#lang racket/base
(require db racket/match "./vm.rkt")
(provide open-day day-opened? daybook-add current-balance)

(define (opening-balance date)
  'TO-DO-OPENING-BALANCE)

(define (current-balance)
  (define (opening-balance-today)
    (define amt
      (query-maybe-value
         DBCON
         "select amount from OpeningBalance where open_date = date('now', 'localtime')"))
    (or amt 0))

  (define (yesterdays-carry)
    (define date:total
      (query-non-null-row
       DBCON
       "select MAX(date), total from DayBook
        where date(date) < date('now', 'localtime') and payment_method = 'cash'"))
    (if date:total (vector-ref date:total 1) 0))
  

  ;; find sqlish way to do it later
  (define date:total
    (query-non-null-row
     DBCON
     "select MAX(date), total from DayBook
      where date(date) = date('now', 'localtime') and payment_method = 'cash' "))
  
  (if date:total
      (vector-ref date:total 1)
      (+ (opening-balance-today) (yesterdays-carry))))

(define (daybook-add #:description desc
                     #:category cat
		     #:person-ref [pid #f]
                     #:amount amt
                     #:payment-method [paid-by "cash"]
                     #:credit/debit cre-or-deb)
  (unless (member cre-or-deb '("credit" "debit"))
    (raise "Argument Error, cre-or-deb should be credit/debit"))
  (when (member cat '("customer" "supplier"))
      (unless pid
        (raise (format "person-ref not given for ~a" cat))))
  

  (define total
    (if (string=? paid-by "cash")
        ((if (string=? cre-or-deb "credit") + -) (current-balance) amt)
        (current-balance)))

  (start-transaction DBCON)
  (query-exec DBCON
              (format
               "insert into DayBook(date, description, category, ~a, total, payment_method) values
                (datetime('now', 'localtime'), $1, $2, $3, $4, $5)" cre-or-deb)
              desc cat amt total paid-by)

  (define daybook-entry (last-insert-rowid))
  (when (member cat '("customer" "supplier"))
      ;; log in customer/supplier transactions
    (ledger-trader-add #:daybook-entry daybook-entry
                       #:trader pid
                       #:role cat))
  (commit-transaction DBCON))

(define (day-opened?)
  (query-maybe-value
         DBCON
         "select 1 from OpeningBalance where open_date = date('now', 'localtime')"))

(define (open-day amount)
  (query-exec DBCON
              "insert into OpeningBalance(open_date, amount) values (date('now', 'localtime'), $1)"
              amount))