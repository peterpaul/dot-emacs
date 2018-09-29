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

(defun calculate-max-index (calc)
  (let* ((m    (nth 1 calc))
	 (cols (matrix-cols m)))
    (- (* 2 cols) 2)))

(defun calculate-set-value (calc i value)
  (let* ((m    (nth 1 calc))
	 (cols (matrix-cols m))
	 (rows (matrix-rows m)))
    (if (> i (calculate-max-index calc))
	(error "Invalid index %d" i)
      (if (oddp i)
	  (let ((r 0)
		(c (- cols (+ 1 (/ i 2)) 1)))
	    (matrix-set m c r value))
	(let ((r (- rows 1))
	      (c (- cols (/ i 2) 1)))
	  (matrix-set m c r value))))))

(defun calculate-render (calc)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (matrix-render (nth 1 calc))
    (if (calculate-finishedp)
        (insert (format "Your answer is... %s"
                        (if (calculate-check-result calc (calculate-get-result))
                            "Correct!"
                          "Incorrect...")))
      (if (oddp *calculate-index*)
          (insert "Hoeveel onthouden?")
        (insert "Hoeveel opschrijven?")))))

(defun calculate-finishedp ()
  (> *calculate-index* (calculate-max-index *calculate-sum*)))

(defun calculate-check-result (c r)
  (let ((expected (reduce '+ (nth 0 c))))
    (= expected r)))

(defun calculate ()
  "Start a sum"
  (interactive)
  (switch-to-buffer "calculate")
  (calculate-mode)
  (calculate-init))

(defun calculate-get-result ()
  (let* ((m (nth 1 *calculate-sum*))
        (cols (matrix-cols m))
        (rows (matrix-rows m))
        (r 0))
    (dotimes (c cols)
      (setq r (+ (* r 10) (or (matrix-get m c (- rows 1))
                             0))))
    r))

(defun calculate-init ()
  (setq *calculate-sum* (calculate-new-sum (list 1 23 456 6789)))
  (setq *calculate-index* 0)
  (calculate-render *calculate-sum*))

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
  )

(defun calculate-set-entered-value (v)
  (when (not (calculate-finishedp))
    (calculate-set-value *calculate-sum* *calculate-index* v)
    (setq *calculate-index* (+ 1 *calculate-index*))
    (calculate-render *calculate-sum*)))

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

;; ;; testscript
;; (setq abc (list 1 23 456 6789))
;; (calculate-check-result (calculate-new-sum abc) 7260) ; nil
;; (calculate-check-result (calculate-new-sum abc) 7269) ; 't
;; (setq cal (calculate-new-sum abc))
;; (calculate-render cal)

;; (dotimes (i 9)
;;   (calculate-set-value cal i i))

;; (calculate-set-value cal 9 9)

(provide 'calculate)
