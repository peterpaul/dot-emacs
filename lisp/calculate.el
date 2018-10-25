;;; -*- lexical-binding: t; -*-
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

(defun calculate-max-index ()
  (let* ((m    (nth 1 *calculate-sum*))
	 (cols (matrix-cols m)))
    (- (* 2 cols) 2)))

(defun calculate-set-value (i value)
  (let* ((m    (nth 1 *calculate-sum*))
	 (cols (matrix-cols m))
	 (rows (matrix-rows m)))
    (if (> i (calculate-max-index))
	(error "Invalid index %d" i)
      (if (oddp i)
	  (let ((r 0)
		(c (- cols (+ 1 (/ i 2)) 1)))
	    (matrix-set m c r value))
	(let ((r (- rows 1))
	      (c (- cols (/ i 2) 1)))
	  (matrix-set m c r value))))))

(defun calculate-render ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (matrix-render (nth 1 *calculate-sum*))
    (if (calculate-finishedp)
        (insert (if (calculate-check-result (calculate-get-result))
                    "Dat is het juiste antwoord!\n\n"
                  "Dat is fout, je moet beter je best doen op school.\n\n"))
      (if (oddp *calculate-index*)
          (insert "Hoeveel onthouden?\n\n")
        (insert "Hoeveel opschrijven?\n\n")))
    (insert "b - Ga terug\nr - Begin opnieuw\nq - Stop rekensom\n")))

(defun calculate-finishedp ()
  (> *calculate-index* (calculate-max-index)))

(defun calculate-check-result (r)
  (let ((expected (reduce '+ (nth 0 *calculate-sum*))))
    (= expected r)))

(defun calculate (times)
  "Start a sum"
  (interactive "nSom grootte: ")
  (switch-to-buffer "calculate")
  (calculate-mode)
  (calculate-init times))

(defun calculate-get-result ()
  (let* ((m (nth 1 *calculate-sum*))
         (cols (matrix-cols m))
         (rows (matrix-rows m))
         (r 0))
    (dotimes (c cols)
      (setq r (+ (* r 10) (or (matrix-get m c (- rows 1))
                             0))))
    r))

(defun calculate-init (times)
  (interactive "nSom grootte: ")
  (setq *calculate-sum* (calculate-new-sum (mapcar (lambda (x) (random 10000))
                                                   (number-sequence 1 times))))
  (setq *calculate-index* 0)
  (calculate-set-value *calculate-index* "?")
  (calculate-render))

(define-derived-mode calculate-mode special-mode "calculate"
  (define-key calculate-mode-map (kbd "0") 'calculate-set-0)
  (define-key calculate-mode-map (kbd "1") 'calculate-set-1)
  (define-key calculate-mode-map (kbd "2") 'calculate-set-2)
  (define-key calculate-mode-map (kbd "3") 'calculate-set-3)
  (define-key calculate-mode-map (kbd "4") 'calculate-set-4)
  (define-key calculate-mode-map (kbd "5") 'calculate-set-5)
  (define-key calculate-mode-map (kbd "6") 'calculate-set-6)
  (define-key calculate-mode-map (kbd "7") 'calculate-set-7)
  (define-key calculate-mode-map (kbd "8") 'calculate-set-8)
  (define-key calculate-mode-map (kbd "9") 'calculate-set-9)
  (define-key calculate-mode-map (kbd "r") 'calculate-init)
  (define-key calculate-mode-map (kbd "b") 'calculate-back))

(defun calculate-back ()
  (interactive)
  (when (> *calculate-index* 0)
    (condition-case nil
        (calculate-set-value *calculate-index* nil)
      (error nil))
    (setq *calculate-index* (- *calculate-index* 1))
    (calculate-set-value *calculate-index* "?"))
  (calculate-render))

(defun calculate-set-entered-value (v)
  (when (not (calculate-finishedp))
    (calculate-set-value *calculate-index* v)
    (condition-case nil
	(calculate-set-value (+ 1 *calculate-index*) "?")
      (error nil))
    (setq *calculate-index* (+ 1 *calculate-index*))
    (calculate-render)))

(defun calculate-set-0 () (interactive) (calculate-set-entered-value 0))
(defun calculate-set-1 () (interactive) (calculate-set-entered-value 1))
(defun calculate-set-2 () (interactive) (calculate-set-entered-value 2))
(defun calculate-set-3 () (interactive) (calculate-set-entered-value 3))
(defun calculate-set-4 () (interactive) (calculate-set-entered-value 4))
(defun calculate-set-5 () (interactive) (calculate-set-entered-value 5))
(defun calculate-set-6 () (interactive) (calculate-set-entered-value 6))
(defun calculate-set-7 () (interactive) (calculate-set-entered-value 7))
(defun calculate-set-8 () (interactive) (calculate-set-entered-value 8))
(defun calculate-set-9 () (interactive) (calculate-set-entered-value 9))

(provide 'calculate)
