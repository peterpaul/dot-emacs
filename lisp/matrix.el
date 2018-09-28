(defun matrix-create (cols rows)
  (list cols rows (make-vector (* rows cols) nil)))

(defun matrix-cols (m)
  (nth 0 m))

(defun matrix-rows (m)
  (nth 1 m))

(defun matrix-index (m col row)
  (let ((cols (matrix-cols m))
        (rows (matrix-rows m)))
    (if (>= col cols)
        (error "Invalid column index %d, matrix has %d columns"
               col
               cols)
      (if (>= row rows)
          (error "Invalid row index %d, matrix has %d rows"
                 row
                 rows m)
        (+ (* row cols) col)))))

(defun matrix-get (m col row)
  (aref (nth 2 m)
        (matrix-index m col row)))

(defun matrix-set (m col row value)
  (aset (nth 2 m)
        (matrix-index m col row)
        value))

(defun matrix-max-col-size (m col)
  (let ((res 0))
    (dotimes (r (matrix-rows m))
      (setq res (max res
                     (let ((v (matrix-get m col r)))
                       (if v
                           (length (format "%s" v))
                         0)))))
    res))

(defun matrix-max-col-sizes (m)
  (let ((res (make-vector (matrix-cols m) 0)))
    (dotimes (c (matrix-cols m))
      (aset res c (matrix-max-col-size m c)))
    res))

(defun matrix-render (m)
  "Render a matrix"
  (let ((cols (matrix-cols m))
        (rows (matrix-rows m))
        (max-col-sizes (matrix-max-col-sizes m)))
    (save-excursion
      (dotimes (r rows)
        (dotimes (c cols)
          (insert (format "+-%s-" (make-string (aref max-col-sizes c) ?-))))
        (insert "+\n")
        (dotimes (c cols)
	  (insert (let ((v (matrix-get m c r)))
                    (format (format "| %%%ds " (aref max-col-sizes c))
                            (or v "")))))
        (insert "|\n"))
      (dotimes (c cols)
        (insert (format "+-%s-" (make-string (aref max-col-sizes c) ?-))))
      (insert "+\n"))))

(provide 'matrix)
