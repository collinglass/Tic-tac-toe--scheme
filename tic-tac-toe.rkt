(define start '((1 2 3) (4 5 6) (7 8 9) 
                 (1 4 7) (2 5 8) (3 6 9) 
                 (1 5 9) (3 5 7)
                 ))

(define subst 
  (lambda (new old l) 
    (cond ((null? l) (quote ())) 
              ((atom? (car l)) 
                 (cond ((eq? (car l) old) 
                              (cons new (subst new old (cdr l)))) 
                           (else (cons (car l) (subst new old (cdr l)))))) 
               (else (cons (subst new old (car l)) 
                                 (subst new old (cdr l)))))))

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define printer2
  (lambda (X)
    (cond
      ((number? X) (display " "))
      (else (display X)))))

(define printer
  (lambda (X)
    (printer2 (car X))
    (display " |")
    (printer2 (cadr X))
    (display " |")
    (printer2 (caddr X))))

(define game
  (lambda (X)
    (and (printer (car X))
         (newline)
         (display "__|__|__")
         (newline)
         (printer (cadr X))
         (newline)
         (display "__|__|__")
         (newline)
         (printer (caddr X)))))

(define newgame
  (let ((A start))
    (game A)))

(define checker?
  (lambda (M)
    (cond
      ((equal? '(O O O) M) #t)
      ((equal? '(X X X) M) #t)
      (else #f))))

(define checkwin?
  (lambda (X)
    (cond
      ((null? X) #f)
      ((checker? (car X)) #t)
      (else (checkwin? (cdr X))))))

(define numchecker?
  (lambda (X)
    (cond
      ((number? (car X)) #t)
      ((number? (cadr X)) #t)
      ((number? (caddr X)) #t)
      (else #f))))

(define checkdraw?
  (lambda (X)
    (cond
      ((null? X) #t)
      ((numchecker? (car X)) #f)
      (else (checkdraw? (cdr X))))))

(define play2
  (lambda (X A)
    (newline)
    (display X)
    (display "'s move:")
    (let ((n (read)))
      (game (subst X n A))
    (cond
      ((checkwin? (subst X n A)) (and (newline) (and (display X) (display " has won!"))))
      ((checkdraw? (subst X n A)) (and (newline) (display "It is a Draw!")))
      ((equal? A (subst X n A)) (and (and (newline) (display "Invalid move!")) (play2 X A)))
      ((equal? 'X X) (play2 'O (subst X n A)))
      (else (play2 'X (subst X n A)))))))

(define play
  (cond
    ((= 1 0) #f)
    (else (newline)
          (display "x's move:")
          (let ((n (read)))
            (game (subst 'X n start))
            ((equal? start (subst 'X n start)) (and (and (newline) (display "Invalid move!")) (play2 'X start)))
            (play2 'O (subst 'X n start))))))
