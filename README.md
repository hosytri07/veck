(vl-load-com)

;; =========================================================
;; 1. TỰ ĐỘNG LOAD BLOCK TỪ FILE MẪU
;; Thay đổi đường dẫn file dưới đây cho đúng với máy của bạn
;; =========================================================
(defun veck-load-blocks-from-file "F:/Thu vien/1. BANG MAU KHAO SAT 1.500 (2026).dwg"
  (if (findfile path)
    (progn
      (princ (strcat "\nĐang nạp Block từ: " path "..."))
      (command "_.INSERT" (strcat path "=*") nil)
      (princ " Hoàn tất.")
    )
    (princ (strcat "\nKhông tìm thấy file: " path))
  )
)

;; =========================================================
;; VECK – VE CAU KIEN THEO POLYLINE
;; =========================================================

(setq *veck-layer* "CAU_KIEN")
(setq *veck-offset* 0.5)
(setq *veck-text-h* 2.0)

(setq *veck-base-dist* 0.0)
(setq *veck-base-value* 0.0)
(setq *veck-side-base* 1)

(defun z0 (p) (list (car p) (cadr p) 0.0))

(defun veck-layer (n)
  (if (not (tblsearch "LAYER" n))
    (command "_.LAYER" "_M" n "_C" "7" "" "")
  )
)

(defun veck-len (e)
  (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e))
)

(defun veck-tangent-angle (pl d / p der)
  (setq p (vlax-curve-getParamAtDist pl d))
  (setq der (vlax-curve-getFirstDeriv pl p))
  (angle '(0 0 0) der)
)

(defun veck-side-pick (pl side)
  (polar
    (vlax-curve-getStartPoint pl)
    (+ (veck-tangent-angle pl 0.0) (* side (/ pi 2)))
    10.0
  )
)

(defun veck-pick-base-point (pl / p p-on d1 d2)
  (setq p (getpoint "\nPick diem DAU tren polyline goc: "))
  (if (null p) (exit))

  (setq p-on (vlax-curve-getClosestPointTo pl p))
  (setq d1 (distance p-on (vlax-curve-getStartPoint pl)))
  (setq d2 (distance p-on (vlax-curve-getEndPoint pl)))

  (if (< d2 d1)
    (progn
      (command "_.PEDIT" pl "_R" "")
      (setq pl (entlast))
    )
  )
  0.0
)

(defun veck-pick-left-side (pl / p0 p ang v1 v2 cross)
  (setq p0 (vlax-curve-getStartPoint pl))
  (setq p (getpoint "\nPick diem PHIA TRAI tuyen: "))
  (if (null p) (exit))

  (setq ang (veck-tangent-angle pl 0.0))
  (setq v1 (list (cos ang) (sin ang)))
  (setq v2 (list (- (car p) (car p0)) (- (cadr p) (cadr p0))))

  (setq cross (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))
  (if (> cross 0) 1 -1)
)

(defun veck-norm (v)
  (+ *veck-base-dist* (- v *veck-base-value*))
)

(defun veck-offset (pl side)
  (command "_.OFFSET" *veck-offset* pl (veck-side-pick pl side) "")
  (entlast)
)

;; ================== SUB POLYLINE (GREEN) ==================
(defun veck-build-subpline (pl s e / pts d)
  (setq pts '() d s)
  (while (<= d e)
    (setq pts (cons (z0 (vlax-curve-getPointAtDist pl d)) pts))
    (setq d (+ d 1.0))
  )
  (setq pts (reverse pts))

  (if (>= (length pts) 2)
    (entmakex
      (append
        (list
          '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          '(100 . "AcDbPolyline")
          (cons 8 *veck-layer*)
          '(62 . 3)
          (cons 90 (length pts))
        )
        (apply 'append
          (mapcar '(lambda (p) (list (cons 10 p))) pts))
      )
    )
  )
)

(defun veck-text (pt ang str)
  (entmakex
    (list
      '(0 . "TEXT")
      (cons 8 *veck-layer*)
      (cons 10 (z0 pt))
      (cons 40 *veck-text-h*)
      (cons 50 ang)
      (cons 1 str)
    )
  )
)

(defun veck-leader (p0 p1 p2)
  (entmakex
    (list '(0 . "LEADER")
          '(100 . "AcDbEntity")
          '(100 . "AcDbLeader")
          (cons 8 *veck-layer*)
          '(71 . 0)
          '(72 . 0)
          '(73 . 3)
          (cons 10 (z0 p0))
          (cons 10 (z0 p1))
          (cons 10 (z0 p2)))
  )
)

(defun veck-draw-one (pl s e side name / off seg mid ang p1 p2 txtpt)
  (setq off (veck-offset pl side))
  (setq seg (veck-build-subpline off s e))
  (if off (entdel off))

  (if seg
    (progn
      (setq mid (vlax-curve-getPointAtDist seg (/ (veck-len seg) 2)))
      (setq ang (veck-tangent-angle pl (/ (+ s e) 2)))

      (setq p1 (polar mid (+ ang (* side (- (/ pi 2) (/ pi 6)))) 2.0))
      (setq p2 (polar p1 ang 10.0))
      (veck-leader mid p1 p2)

      (setq txtpt (polar p1 ang (+ 5.0 (* 1.2 *veck-text-h*))))
      (veck-text txtpt ang name)

      (veck-text
        (vlax-curve-getStartPoint seg)
        (+ (veck-tangent-angle pl s) (* side (/ pi 2)))
        (strcat "+" (rtos (+ *veck-base-value* s) 2 0))
      )

      (veck-text
        (vlax-curve-getEndPoint seg)
        (+ (veck-tangent-angle pl e) (* side (/ pi 2)))
        (strcat "+" (rtos (+ *veck-base-value* e) 2 0))
      )
    )
  )
)

;; ================== DCL ==================
(setq *veck-dcl*
"veckdlg : dialog {
  label = \"VECK - Bảng nhập cấu kiện\";
  : column {
    : list_box { key = \"lst\"; width = 70; height = 10; }
    : row {
      : edit_box { key = \"s\"; label = \"Lí trình đầu\"; }
      : edit_box { key = \"e\"; label = \"Lí trình cuối\"; }
      : edit_box { key = \"side\"; label = \"Trái/Phải\"; }
      : edit_box { key = \"name\"; label = \"Tên cấu kiện\"; }
    }
    : row {
      : button { key = \"loadcsv\"; label = \"Nạp CSV\"; }
      : button { key = \"add\"; label = \"Thêm\"; }
      : button { key = \"del\"; label = \"Xóa\"; }
    }
    : row {
      : button { key = \"ok\"; label = \"Vẽ\"; is_default = true; }
      : button { key = \"cancel\"; label = \"Hủy\"; is_cancel = true; }
    }
  }
}"
)

(setq *veck-dlg-data* '())

(defun veck-dlg-refresh ()
  (start_list "lst")
  (mapcar
    '(lambda (r)
       (add_list
         (strcat (nth 0 r) " - " (nth 1 r) " - " (nth 2 r) " - " (nth 3 r))
       ))
    *veck-dlg-data*
  )
  (end_list)
)

;; ================== CSV SUPPORT (ONLY DCL SIDE) ==================
(defun veck-trim (s)
  (if s (vl-string-trim " \t\r\n\"" s) "")
)

(defun veck-split (str sep / pos out)
  (setq out '())
  (while (setq pos (vl-string-search sep str))
    (setq out (cons (substr str 1 pos) out))
    (setq str (substr str (+ pos 2)))
  )
  (reverse (cons str out))
)

(defun veck-parse-csv-line (line / cols sep)
  (if (= (substr line 1 1) "\ufeff")
    (setq line (substr line 2))
  )
  (setq sep (if (vl-string-search ";" line) ";" ","))
  (setq cols (veck-split line sep))
  (if (>= (length cols) 4)
    (list
      (veck-trim (nth 0 cols))
      (veck-trim (nth 1 cols))
      (veck-trim (nth 2 cols))
      (veck-trim (nth 3 cols))
    )
    nil
  )
)

(defun veck-dlg-load-csv (/ fn f line row)
  (setq fn (getfiled "Chon file CSV" "" "csv" 0))
  (if fn
    (progn
      (setq f (open fn "r"))
      (while (setq line (read-line f))
        (setq row (veck-parse-csv-line line))
        (if row
          (setq *veck-dlg-data*
            (append *veck-dlg-data* (list row))
          )
        )
      )
      (close f)
      (veck-dlg-refresh)
    )
  )
)

(defun veck-dlg-add ()
  (setq *veck-dlg-data*
    (append *veck-dlg-data*
      (list (list (get_tile "s") (get_tile "e") (get_tile "side") (get_tile "name")))
    )
  )
  (veck-dlg-refresh)
)

(defun veck-dlg-del ()
  (setq *veck-dlg-data*
    (vl-remove
      (nth (atoi (get_tile "lst")) *veck-dlg-data*)
      *veck-dlg-data*
    )
  )
  (veck-dlg-refresh)
)

(defun veck-dlg-run (/ fn f dcl_id res)
  (setq *veck-dlg-data* '())

  (setq fn (vl-filename-mktemp "veck.dcl"))
  (setq f (open fn "w"))
  (write-line *veck-dcl* f)
  (close f)

  (setq dcl_id (load_dialog fn))
  (new_dialog "veckdlg" dcl_id)

  (action_tile "loadcsv" "(veck-dlg-load-csv)")
  (action_tile "add" "(veck-dlg-add)")
  (action_tile "del" "(veck-dlg-del)")
  (action_tile "ok" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (veck-dlg-refresh)

  (setq res (if (= (start_dialog) 1) *veck-dlg-data* nil))
  (unload_dialog dcl_id)
  res
)

(defun veck-draw-from-table (pl table)
  (foreach r table
    (veck-draw-one
      pl
      (veck-norm (atof (nth 0 r)))
      (veck-norm (atof (nth 1 r)))
      (if (= (strcase (nth 2 r)) "L") *veck-side-base* (- *veck-side-base*))
      (nth 3 r)
    )
  )
)

(defun c:VECK (/ pl table tmp cont)
  (setq pl (car (entsel "\nChọn polyline gốc: ")))
  (if (null pl) (exit))

  (setq *veck-base-dist* (veck-pick-base-point pl))
  (setq *veck-base-value* (if (numberp (setq tmp (getreal "\nLí trình đầu tuyến <0>: "))) tmp 0.0))
  (setq *veck-side-base* (veck-pick-left-side pl))
  (setq *veck-offset* (if (numberp (setq tmp (getreal "\nOffset <0.7>: "))) tmp 0.7))
  (setq *veck-text-h* (if (numberp (setq tmp (getreal "\nChiều cao chữ <1.8>: "))) tmp 1.8))

  (veck-layer *veck-layer*)

  (setq cont T)
  (while cont
    (setq table (veck-dlg-run))
    (if table (veck-draw-from-table pl table))
    (setq cont (= (strcase (getstring "\nTiếp tục vẽ? (Có/Không): ")) "Y"))
  )

  (princ "\nVECK hoàn tất.")
  (princ "\nLisp được phát triển bởi Lê Thế Vương Anh - Phòng KHCN-KSTK - Trung tâm Kỹ thuật đường bộ 3")
  (princ)
)
