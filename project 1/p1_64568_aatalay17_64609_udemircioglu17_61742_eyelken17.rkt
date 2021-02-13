(module project1 mzscheme
  ;;;;;;;;;;;; Comp 301 Project 1 ;;;;;;;;;;;;;;;;
  ;;; Add group members below
  ;;; Altay Atalay, aatalay17, 64568
  ;;; Umur Demircioglu, udemircioglu17, 64609
  ;;; Ege Yelken, eyelken17, 61742
  ;;; save your file in the format: p1_64568_aatalay17_64609_udemircioglu17_61742_eyelken17.rkt
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  ;; PROJECT 1 Part A | Write your answer below here as a comment
  ;
  ; Binary Represantation 
  ; B ::= (0)
  ;   ::= ((0 | 1)* B 0)
  ; Binary representation of natural numbers. Least significant bit is at left. x = 5 --> (1 0 1 0).
  ;;;;;;;;;;;;;;;;;;;;;;
  ; SignedB ::= (0 . B)
  ;         ::= (1 . B)
  ;         
  ; 
  ;; PROJECT 1 Part B
  ;; First Representation | We added a suffix so that both first and second representations can be tested at once.


  (define create-a
    (lambda (x)
      (cond
        ((eqv? x 0) '(0))
        (else
         (cons (modulo x 2) (create-a (floor (/ x 2))))))))
 
  (define is-zero-a?
    (lambda (x) (equal? x '(0) )))


  (define successor-a
    (lambda (x)
    (if (null? (cdr x)) (cons 1 0)
           (if (eqv? (car x) 0) (cons 1 (cdr x)) (cons 0 (successor-a(cdr x)))))))
  

  ;; Second Representation | We added a -b suffix so that both Unary and BigNum can be tested at once.
  (define create-b
    (lambda (x)
      (cond
        ((pair? x)
         (eqv? (car x) "-") (cons 1 (create-a (cdr x))))
        (else
         (cons 0 (create-a x))))))

  (define is-zero-b?
    (lambda (x) (or (equal? x '(0 0)) (equal? x '(1 0)))))

  (define pred-a
    (lambda (x)
      (cond
        ((eqv? (car x) 0) (cons 1 (pred-a (cdr x))))
        ((eqv? (car x) 1) (cons 0  (cdr x))))))
  
  (define successor-b
    (lambda (x)
      (cond
        ((is-zero-b? x) (cons 0 1))
        ((eqv? (car x) 0) (cons 0 successor-a (cdr x)))
        ((eqv? (car x) 1) (cons 1 (pred-a (cdr x)))))))

  ;; PROJECT 1 Part C | Write your answer below here as a comment
  ;
  ;"create-a"/"create-b" and "successor-a"/"successor-b" are Constructors since they build an element of the data type
  ;and "is-zer0-a?"/"is-zero-b?" is a Predicate type of Observer because it returns a boolean that depends
  ;on the values of the data type.
  
  ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the below function, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
  
  ;; PROJECT 1 Part D | Remove the comments and write test cases.
  (display "First Representation Tests\n")

  (define a (create-a 6))
  (display a)
  (newline)
  (display (is-zero-a? (create-a 6)))
  (newline)
  (display (is-zero-a? (create-a 0)))
  (newline)
  (display (successor-a (create-a 5)))
  (newline)

 ;(equal?? (create-a ) '()) ; should return ?
 ;(equal?? (is-zero-a? '()) #f) ; should return #f
 ;(equal?? (is-zero-a? '()) #t) ; should return #t
 ;(equal?? (successor-a '()) '()) ; should return ?
  (newline)

  
  (display "Second Representation Tests\n")
  (define b (cons "-" 5))
  (display b)
  (newline)
  (display (create-b b))
  (define c (create-b b))
  (newline)
  (display (is-zero-b? (create-b b)))
  (newline)
  (display (is-zero-b? (create-b 0)))
  (newline)
  (display (successor-b c))
  
  ;(equal?? (create-a ) '()) ; should return ?
  ;(equal?? (is-zero-a? '()) #f) ; should return #f
  ;(equal?? (is-zero-a? '()) #t) ; should return #t
  ;(equal?? (successor-a '()) '()) ; should return ?
  (newline)
)