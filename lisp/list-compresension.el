;; -*- lexical-binding: t -*-

(defun flatten (l)
  (apply #'append l))

(defun explode:tuples-of (a b &optional combiner)
  "Takes list A and value B, and returns a list containing tuples of the values of A and B."
  (let ((combiner (or combiner 'list)))
    (mapcar (lambda (x) (funcall combiner x b))
            a)))

(defun explode-two (a b &optional combiner)
  "Takes lists A and B, and returns tuples of all possible combinations."
  (flatten (mapcar (lambda (x) (explode:tuples-of a x combiner))
                   b)))

(defun explode (l)
  (let ((head (car l))
        (tail (cdr l))
        (len  (length l)))
    (pcase len
      (0 ())
      (1 head)
      (2 (explode-two head (explode tail)))
      (_ (explode-two head (explode tail) 'cons)))))

(defun list-comprehension (list condition &optional collect)
  "Poor mans list comprehension"
  (let ((collect   (or collect   (lambda (x) x))))
    (mapcar collect
            (seq-filter condition list))))

(list-comprehension (number-sequence 1 5)
                    'oddp
                    'square)

(defun square (x) (* x x))

(defun pythagoras (r)
  (list-comprehension
   (explode (list (number-sequence 1 r)
                  (number-sequence 1 r)
                  (number-sequence 1 r)))
   (lambda (x)
     (let ((a (car x))
           (b (cadr x))
           (c (caddr x)))
       (and (> c b a)
          (eq (+ (square a)
                 (square b))
              (square c)))))))

(pythagoras 20)

(explode '((1 2) (a b) ("X" "Y" "Z")))

