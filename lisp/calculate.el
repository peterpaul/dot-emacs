(require 'matrix)

(defun ilog10 (n)
  (let ((c n)
	(r 0))
    (while (>= n 10)
      (setq r (+ 1 r))
      (setq n (/ n 10)))
    r))

(defun number-length (number)
  (+ 1 (ilog10 number)))

(defun number-digit-at (number pos)
  (if (or (< pos 0)
         (> pos (ilog10 number)))
      nil
    (let ((res number))
      (% (dotimes (p pos res)
	   (setq res (/ res 10)))
         10))))

(defun calculate-max-number-length (l)
  (reduce 'max (mapcar (lambda (x) (length (format "%s" x))) l)))

(defun calculate-make-matrix (l)
  (let* ((cols (+ 1 (calculate-max-number-length l)))
         (rows (+ 2 (length l)))
         (res (matrix-create cols rows)))
    (dotimes (r (- rows 2))
      (dotimes (c cols)
        (matrix-set res c (+ 1 r) (number-digit-at (nth r l) (- cols (+ c 1))))))
    res))

(defun calculate-new-sum (l)
  (list l (calculate-make-matrix l)))

(defun calculate-check-result (c r)
  (let ((expected (reduce '+ (nth 0 c))))
    (= expected r)))

(setq abc (list 1 23 456 6789))
(calculate-check-result (calculate-new-sum abc) 7260) ; nil
(calculate-check-result (calculate-new-sum abc) 7269) ; 't
(matrix-render (calculate-make-matrix abc))

(provide 'calculate)
