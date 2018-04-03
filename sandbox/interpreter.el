;;; -*- lexical-binding: t; -*-



(defun evaluate (exp env)
  (pcase exp
    (`(add ,x ,y)       (+ (evaluate x env) (evaluate y env)))
    (`(call ,fun ,arg)  (funcall (evaluate fun env) (evaluate arg env)))
    (`(fn ,arg ,body)   (lambda (val)
                          (evaluate body (cons (cons arg val) env))))
    ((pred numberp)     exp)
    ((pred symbolp)     (cdr (assq exp env)))
    (_                  (error "Unknown expression %S" exp))))


(defun eval-expr (expr env)
    (pcase expr
      ((pred symbolp)        (env expr))
      (`(lambda (,x) ,body)  (lambda (arg)
			       (eval-expr body (lambda (y)
						 (if (eq x y)
						     arg
						   (env y))))))
      (`(,rator ,rand)       ((eval-expr rator env)
			      (eval-expr rand env))))) 
