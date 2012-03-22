#lang racket

(define (digit->integer x) (- (char->integer x ) (char->integer #\0)))
(define (double-seconds-list l)
    (if (empty? l) 
              '()
              (cons (first l) 
                    (cons (* 2 (first (rest l))) 
                          (double-seconds-list(rest (rest l)))))))

(define (double-seconds-string s)
  (double-seconds-list (map digit->integer (string->list s))))
  

(define (add-digits number)
  (apply + (map digit->integer (string->list (number->string number)))))

(define (add-digits-list list)
  (map add-digits list))

(define (check-sum list)
    (modulo (apply + list) 10)) 
  
(define (reverse-string s)
  (list->string (reverse (string->list s))))

(define (luhn-string? cc)
  (= (check-sum
        (add-digits-list (double-seconds-string (reverse-string cc)))) 0))

(define (luhn-list? cc)
  (= (check-sum
        (add-digits-list (double-seconds-list (reverse cc)))) 0))

(add-digits 123)
(add-digits 0)
(add-digits 18)
  
(check-sum '(1 2 3 4))

(double-seconds-string "5555555555554444")
(double-seconds-string "1111111111111111")

(add-digits-list (double-seconds-string "5555555555554444"))
(add-digits-list (double-seconds-string "1111111111111111"))

(luhn-string? "5555555555554444")
(luhn-string? "1111111111111111")




