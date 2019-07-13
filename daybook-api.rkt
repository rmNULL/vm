#lang racket
(require db "./vm.rkt")

(define (open-day amount)
  (query-exec DBCON
              "insert into OpeningBalance(open_date, amount) values (date('now', 'localtime'), $1)"
              amount))

(define (opening-balance date)
  'TO-DO-OPENING-BALANCE)


(define (current-balance)
  (define (opening-balance-today)
    (query-value DBCON
                 "select amount from OpeningBalance where open_date = date('now', 'localtime')"))

  (define (yesterdays-carry)
    (define tc
      (query-list
       DBCON
       "select total from DayBook
        where date = date('now', '-1 day', 'localtime') and payment_method = 'cash'"))
    (if (empty? tc) 0 (last tc)))
  

  ;; find sqlish way to do it later
  (define totals
    (query-list DBCON
                "select total from DayBook
                 where date(date) = date('now', 'localtime') and payment_method = 'cash' "))
  (if (empty? totals)
      (+ (opening-balance-today) (yesterdays-carry))
      (last totals)))

(define (daybook-add #:description desc
                     #:category cat
                     #:amount amt
                     #:payment-method [paid-by "cash"]
                     #:credit/debit cre-or-deb)
  (unless (member cre-or-deb '("credit" "debit"))
    (raise "Argument Error, cre-or-deb should be credit/debit"))
  (when (member cat '("customer" "supplier"))
      ;; log in customer/supplier transactions
    'stub
      )

  (define total
    (if (string=? paid-by "cash")
        ((if (string=? cre-or-deb "credit") + -) (current-balance) amt)
        (current-balance)))
  
  (query-exec DBCON
              (format
               "insert into DayBook(date, description, category, ~a, total, payment_method) values
                (datetime('now', 'localtime'), $1, $2, $3, $4, $5)" cre-or-deb)
              desc cat amt total paid-by))
