#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      
     (op-exp (exp1 exp2 num)
      (let ((val1 (value-of exp1 env))
            (val2 (value-of exp2 env)))
             (let ((num1 (expval->num val1))
                   (num2 (expval->num val2)))
                   (cond
                         ((eq? num 1) (num-val (+ num1 num2)))
                         ((eq? num 2) (num-val (* num1 num2)))
                         ((eq? num 3) (num-val (/ num1 num2)))
                   (else (num-val (- num1 num2)))))))



     
      
      (if-exp (exp1 exp2 conds exps exp3)
              (let ((b1  (value-of exp1 env)))
                (if (expval->bool b1)
                    (value-of exp2 env)
                    (if (null? conds)
                        (value-of exp3 env)
                        (value-of (if-exp (car conds) (car exps) (cdr conds) (cdr exps) exp3) env)))))



       (even?-exp (num)
                  (if (eq? (modulo num 2) 0)
                         (bool-val #t)
                         (bool-val #f)))

      

      (str-exp (str) (string-append "'" (symbol->string (car str)) "'"))
      
      ;;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
      (const-exp (num) (num-val num))
      
      ;;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
      (var-exp (var) (apply-env env var))
     
      ;;\commentbox{\zerotestspec}
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))

      ;;\commentbox{\ma{\theletspecsplit}}
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

        
      )))