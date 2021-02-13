(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    (arr-val
      (arr arr-val?))
    )

  (define-datatype stack stack?
    (stack-type (size number?) (stack-array arr-val?)))

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval->arr
    (lambda (v)
      (cases expval v
        (arr-val (arr) arr)
        (else (expval-extractor-error 'arr v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
  
;;;;;;;;;;;;;;;; array ;;;;;;;;;;;;;;;;
  
  (define arr-val?
    (lambda (value)
      (if (list? value)
          (if (null? value)
              #t
              (reference? (car value)))
          #f)))

  (define newarray
    (lambda (length value)
      (if (= length 0)
          '()
          (cons (newref value) (newarray (- length 1) value)))))

  (define update-array
    (lambda (arr index value)
      (setref! (list-ref arr index) value)))

  (define read-array
    (lambda (arr index)
      (deref (list-ref arr index))))
  
 ;;;;;;;;;;;;;;;; stack ;;;;;;;;;;;;;;;;
  (define dummy -1)
  (define null 'null )
  
  (define new-stack
    (lambda ()
      (let ((size 0)
           (len 10))
        (let ((stk (newarray len dummy)))
          (begin
            (update-array stk 0 size)
            (update-array stk 1 len)
            stk)))))

  (define stack-push
    (lambda (stk val)
      (let ()
        (begin
          (update-array stk (+ (stack-size stk) 2) val)
          (update-array stk 0 (+ 1 (stack-size stk)))))))

  (define stack-pop
    (lambda (stk)
      (if (empty-stack? stk)
          (dummy)
          (let ((rv (read-array stk (+ (stack-size stk) 1))))
            (begin
              (update-array stk (+ (stack-size stk) 1) dummy)
              (update-array stk 0 (- (stack-size stk) 1))
              rv)))))

  (define stack-size
    (lambda (stk)
        (read-array stk 0)))
    
  (define stack-top
    (lambda (stk)
        (read-array stk (+ (stack-size stk) 1))))

  (define empty-stack?
    (lambda (stk)
        (if (= (stack-size stk) 0)
            #t
            #f)))

  (define print-stack
   (lambda (stk)
      (if (empty-stack? stk)
         (display 0)
     (print-stack-helper stk 2))))


 (define print-stack-helper
   (lambda (stk index)
     (if (> (stack-size stk) (- index 2))
       (begin (let ((tmp (read-array stk index)))
            (display tmp)
          (print-queue-helper stk (+ index 1))))
       (display null))))
    
  

 ;;;;;;;;;;;;;;;; queue ;;;;;;;;;;;;;;;;
  (define new-queue
    (lambda ()
      (let ((size 0)
           (len 10)
           (start 3))
        (let ((q (newarray len dummy)))
          (begin
            (update-array q 0 size)
            (update-array q 1 len)
            (update-array q 2 start)
            q)))))

  (define queue-push
    (lambda (q val)
      (let ()
        (begin
          (update-array q (+ (queue-size q) (read-array q 2)) val)
          (update-array q 0 (+ 1 (queue-size q)))))))

  (define queue-pop
    (lambda (q)
      (if (empty-queue? q)
          (dummy)
          (let ((rv (read-array q (read-array q 2))))
            (begin
              (update-array q (read-array q 2) dummy)
              (update-array q 2 (+ (read-array q 2) 1))
              (update-array q 0 (- (queue-size q) 1))
              rv)))))

  (define queue-size
    (lambda (q)
        (read-array q 0)))
    
  (define queue-top
    (lambda (q)
        (read-array q (read-array q 2))))

  (define empty-queue?
    (lambda (q)
        (if (= (queue-size q) 0)
            #t
            #f)))

  (define print-queue
   (lambda (q)
      (if (empty-queue? q)
         (display 0)
     (print-queue-helper q (read-array q 2)))))


 (define print-queue-helper
   (lambda (q index)
     (if (> (+ (queue-size q) (read-array q 2)) index)
       (begin (let ((tmp (read-array q index)))
            (display tmp)
          (print-queue-helper q (+ index 1))))
       (display null))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))


)
