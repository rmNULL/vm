#lang racket/base
(require db racket/match racket/string)
(require "./database/connection.rkt")
(provide (all-defined-out))



;; Note --
;; ALL DATETIME ARE STORED IN DEFAULT FORMAT, but retreived in LOCALTIME format.
;;

(define PAT-INT "[0-9]+")
(define RX-INT (pregexp PAT-INT))
(define RX-FLOAT (pregexp (string-append PAT-INT
                                         "(?:" "(?:[.]" PAT-INT ")"
                                         "|" "[.]?"
                                         ")?")))
(define RX-NAME #px"[A-Za-z]+(?:\\s[A-Za-z]*)*")



(define DBCON (init-db))
(define sqlite-true  1 #;(query-value DBCON "select true"))
(define sqlite-false 0 #;(query-value DBCON "select false"))
(define (last-insert-rowid)
  (query-value DBCON "select last_insert_rowid()"))
(define (query-non-null-row conn query . args)
  (define row (apply query-maybe-row conn query args))
  (match row
    [(or #f (vector (== sql-null) ...)) #f]
    [(vector _ ...) row]
    ;; not adding a guard clause as i want it to fail (:
    ))

(struct Contact (label number))
(struct Account (number IFSC bank branch))

(define (select-invoice invoice-no)
  (query-maybe-row  DBCON
                    "select bill_number, date(date, 'localtime'), customer, total
                      from Invoice where number = ?" invoice-no))

(define (invoice-available? invoice#)
  (eq? #F (select-invoice invoice#)))


(define (select-invoices)
  (in-query DBCON "select date(date, 'localtime'), number, bill_number, total from Invoice"))

(define (invoices-produced #:for customer)
  (in-query
   DBCON
   "select date(date, 'localtime'), number, bill_number, total
    from Invoice where customer = ?" customer))

(define (select-lot#s [status "open"])
  (query-list DBCON "select lot from Inventory where status = ?" status))


(define (insert-account! #:account a #:person p)
  (query-exec DBCON
              "insert into BankAccounts(person, account_number, IFSC, bank, branch)
               values($1,$2,$3,$4,$5)"
              p
              (Account-number a)
              (Account-IFSC a)
              (Account-bank a)
              (Account-branch a)))

(define (insert-contact! #:person-id p #:contact c)
  (query-exec DBCON
              "insert into Contacts(person, label, number)
               values ($1, $2, $3)"
              p
              (Contact-label c)
              (Contact-number c)))

(define (update-contact! #:old-number old #:contact c)
  (query-exec DBCON
              "update Contacts set label = $1, number = $2
               where number = $3"
              (Contact-label c) (Contact-number c) old))

(define (contact-labels)
  (query-list DBCON "select label from contact_labels"))

(define (item-packages)
  (query-list DBCON
              "select DISTINCT package from Items where package is not null
               UNION
               select DISTINCT package from  Sold where package is not null"))


(define (delete-contact! #:number number)
  (query-exec DBCON "delete from Contacts where number = ?" number))

(define TAB-ATTRIBS
    #hash((People . (id name address permanent relation))
          (Contacts . (person label number))))


(define (update-person! #:column col #:value val #:person-id person)
  (define col-names (hash-ref TAB-ATTRIBS 'People))
  (when (member col col-names)
    (define col-name (format "~a" col))
    (query-exec
     DBCON
     (string-append "update People set " col-name " = $1 where id = $2")
     val person)
    #t))


(struct Person
  (name address relation contacts bank-accounts))

(define (select-person id)
  (if id
     (query-maybe-row DBCON "select name, address, relation from People where id = ?" id)
     #F))

(define (personal-details #:of person)
  (define row (select-person person))
  (if row
      (let ()
        (define-values (name address relation) (vector->values row))
        (Person name address relation
          (contact-details #:of person)
          (bank-details #:of person)))
      #f))

(define (phone-numbers #:of person)
  (query-list DBCON "select number from Contacts where person = ?" person))

(define (contact-details #:of person)
  (in-query DBCON "select label, number from Contacts where person = ?" person))

(define (bank-details #:of person)
  (in-query DBCON "select  name, number, bank, branch, IFSC from BankAccounts
                   where person = ?" person))

(define (delete-person! person-id)
   (query-exec DBCON "delete from People where id = ?" person-id))
(define (delete-invoice! invoice#)
  (query-exec DBCON "delete from Invoice where number = ?" invoice#))
(define (delete-inventory! lot#)
  (query-exec DBCON "delete from Inventory where lot = ?" lot#))


(define (insert-person! #:name [n #f]
                        #:address [addr #f]
                        #:contacts [cts '()]
                        #:bank_accounts [acts '()]
                        #:relation rel
                        #:permanent? (permanent? sqlite-true))
  (start-transaction DBCON)
  (query-exec DBCON
              "insert into People(name, address, relation, permanent) values($1,$2,$3,$4)"
              (or n "") (or addr "") rel permanent?)
  (define id (last-insert-rowid))
  (for ([c cts]) (insert-contact!  #:contact c #:person id))
  (for ([a acts]) (insert-account! #:account a #:person id))
  (commit-transaction DBCON)
  id)

(define (insert-customer! #:name (n #f)
                          #:address (addr #f)
                          #:contacts (cts '())
                          #:permanent? (permanent? sqlite-true))
  (insert-person! #:name n
                  #:address addr
                  #:contacts cts
                  #:relation "customer"
                  #:permanent? permanent?))

(define (insert-supplier! #:name (n #f)
                          #:address (addr #f)
                          #:contacts [cts '()]
                          #:bank_accounts [acts '()]
                          #:permanent? (permanent? sqlite-true))
  (insert-person! #:name n
                  #:address addr
                  #:contacts cts
                  #:bank_accounts acts
                  #:relation "supplier"
                  #:permanent? permanent?))


(define (select-relation relation #:exclusive? [exclusive? #f])    
  (define pred
    (if exclusive?
        "relation = ?"
        "relation = ? OR relation = 'both'"))
  
  (in-query DBCON
            (format "select id, name, address from People
                     where permanent AND ~a
                     ORDER by lower(name)"
                    pred)
            relation))

(define (supplier? p)
  (define ret (query-maybe-value
               DBCON
               "select ? IN (select id from People where relation in ('supplier', 'both'))" p))
  (= ret sqlite-true))

(define (select-customers) (select-relation "customer"))
(define (select-suppliers) (select-relation "supplier"))
(define (select-x-customers) (select-relation "customer" #:exclusive? #t))
(define (select-x-suppliers) (select-relation "supplier" #:exclusive? #t))

(struct Item
  (name quantity (stock #:mutable) package package-count) #:transparent)


;; Sets up a new lot with given items,
;; associates the lot with the supplier,
;; and returns the saved(/alloted) lot#
;;;;;;;
;;   [lot#] : (or number #f)
;; supplier : number
;;  [items] : list
;; -> number
(define (insert-lot! #:supplier p #:items (items '()) #:lot# (lot# #F))
  (unless (supplier? p)
    (raise "insert-lot!: not a supplier"))
  
  (start-transaction DBCON)
  (if lot#
      (query-exec DBCON
                  "insert into Inventory(lot, supplier, date)
                   values($1, $2, datetime('now'))"
                  lot# p)
      (begin
        (query-exec DBCON
                    "insert into Inventory(supplier, date)
                     values($1, datetime('now'))"
                    p)
        (set! lot# (last-insert-rowid))))
  
  (for ([item items]) (insert-inventory-item! #:item item #:lot lot#))
  (commit-transaction DBCON)
  lot#)


(define (insert-inventory-item!
         #:lot lot
         #:item item)
  (query-exec DBCON
              "insert into Items (lot, name, qty, stock, package, package_count)
               values            ( $1,   $2,  $3,    $4,      $5,            $6)"
              lot
              (Item-name item)
              (Item-quantity item)
              (Item-stock item)
              (Item-package item)
              (Item-package-count item)))

(define (select-lot lot#)
  (in-query DBCON "select datetime(date, 'localtime'), lot, name, status
                   from Inventory JOIN People AS P ON supplier = P.id
                   where lot = ?" lot#))

(define (select-lots [status "open"])
  (in-query DBCON "select datetime(date, 'localtime'), lot, name, status
                   from Inventory JOIN People AS P ON supplier = P.id
                   where status = ?" status))

(define (__select-item lot# name)
  (query-maybe-row DBCON
                   "select name, qty, stock, package, package_count from Items
                     where lot = $1 AND name = $2"
                   lot# name))

(define (select-item lot# name)
  (define i (__select-item lot# name))
  (and i (apply Item (vector->list i))))

(define (select-available-item-names lot#)
  (query-list DBCON
              "select name from Items T JOIN Inventory I ON T.lot = I.lot
               where I.lot = $1 AND stock > 0" lot#))

(define (select-item-names lot#)
  (query-list DBCON
              "select name from Items T JOIN Inventory I ON T.lot = I.lot
               where I.lot = $1" lot#))


;; #:date-to is only considered when #:date-from exists
;; lot# : number 
;; in-stock? : bool  ;; when true consider items in stock only.
;; date-from : seconds
;; date-to : seconds
;; supplier : number ;; supplier id
;; -> (sequence (values lot# Item.*) ... )
;;
(define (filter-inventory #:lot# [lot# #f]
                          #:item-name [item #f]
                          #:in-stock? [in-stock? #f]
                          #:date-from [date-from #f]
                          #:date-to [date-to #f]
                          #:supplier [supplier-id #f])
  ;; DISCLAIMER: BEWARE !!!!!!
  (define qry (list))
  (define args (list))
  (when lot#
    (set! qry  (cons "Inventory.lot = ?" qry))
    (set! args (cons lot# args)))
  (when item
    (set! qry  (cons "Items.name like ?" qry))
    (set! args (cons (format "%~a%" item) args)))
  (when supplier-id
    (set! qry  (cons "Inventory.supplier = ?" qry))
    (set! args (cons supplier-id args)))
  (when in-stock?
    (set! qry  (cons "Items.stock > ?" qry))
    (set! args (cons 0 args)))
  (when date-from
    ;; ASSUMES 'AND' will be interleaved between each query string
    (set!  qry (cons "datetime(?,'unixepoch', 'localtime')" qry))
    (set! args (cons (or date-to (current-seconds)) args))

    (set! qry  (cons "Inventory.date BETWEEN datetime(?,'unixepoch', 'localtime')" qry))
    (set! args (cons date-from args)))

  (define query (string-append "select Items.*  from
                                Inventory JOIN Items ON Inventory.lot = Items.lot"
                               (if (null? qry)
                                   " "
                                   (string-join qry " AND " #:before-first " WHERE "))))
  (apply in-query DBCON query args))


(define (select-items lot#)
  (in-query DBCON
            "select name, qty, stock, package, package_count
             from Items T JOIN Inventory I ON T.lot = I.lot 
             where I.lot = $1"
            lot#))



(struct SellingItem
  (lot name ppu qty package n_packages))

(define (sell! #:to customer #:items s-items  #:bill-number bill
               ;; invoice-no should be a non-existing invoice# in the database
               #:invoice-number [invoice-no #f]
               ;; misc-items (listof (cons desc amt))
               #:misc [misc-exp '()]
               #:total total)
  
  ;; desc-amt = (cons desc amt)
  (define (insert-invoice-miscexp! invoice-no desc-amt)
    (query-exec DBCON "insert into SoldMisc(invoice, description, amount) values($1, $2, $3)"
                invoice-no (car desc-amt) (cdr desc-amt)))
  
  (define (insert-invoice-item! invoice s-item)
    (define-values (name lot ppu qty)
      (values (SellingItem-name s-item) (SellingItem-lot s-item)
              (SellingItem-ppu s-item) (SellingItem-qty s-item)))
  
    (define instock (Item-stock (select-item lot name)))
    (define updated-stock (- instock qty))
  
    (when (>= updated-stock 0)
      (define amount (* ppu qty))
      (define pkg (SellingItem-package s-item))
      (define n_pkgs (SellingItem-n_packages s-item))
      (start-transaction DBCON)
      (query-exec DBCON
                  "insert into Sold
                 (invoice, lot, item, ppu, qty, amount, package, package_count) values
                 (     $1,  $2,   $3,  $4,  $5,     $6,      $7,            $8)"
                  invoice  lot  name  ppu  qty   amount     pkg         n_pkgs)

      (update-item-stock! lot name updated-stock)
      (commit-transaction DBCON)))
  
  (start-transaction DBCON)
  (if invoice-no
      (query-exec
       DBCON
       "insert into
        Invoice(number, bill_number, customer, total, date) values ($1,$2, $3, $4, date('now'))"
       invoice-no bill customer total)
      
      (begin
        (query-exec
         DBCON
         "insert into Invoice(bill_number, customer, total,        date) values
                             (         $1,       $2,    $3, date('now'))"
                                      bill customer  total)
        (set! invoice-no (last-insert-rowid))))
  
  (for ([item s-items])
    (insert-invoice-item! invoice-no item))
  (for ([desc-amt misc-exp])
    (insert-invoice-miscexp! invoice-no desc-amt))
  (commit-transaction DBCON))


(define (update-item-stock! lot item-name stock)
  (query-exec DBCON
              "update Items set stock = $1 where lot = $2 AND name = $3"
              stock lot item-name))


(define lot-taken?
  (let ([qry (prepare DBCON "select 1 from Inventory where lot = ?")])
    (λ (lot#)
      (query-maybe-value DBCON qry lot#))))


(define (ledger-trader-add
         #:daybook-entry daybook-entry
         #:trader pid
         #:role role)  
  (query-exec
   DBCON
   "insert into LedgerTrader(trader, daybook_entry, role) values($1, $2, $3)"
   pid daybook-entry role))

;; client may ask discount on damaged goods for which invoice was already issued
(define (ledger-invoice-add
         #:invoice-no invoice-no
         #:amount amt
         #:description desc)
  (query-exec
   DBCON
   "insert into LedgerInvoice(invoice, description, amount) values($1, $2, $3)"
   invoice-no desc amt))

(define (ledger-add kwargs)
  
  ;; no arguments taken, as arguments are verified in match below
  (define (ledger-add!)
    (define args
      ;; 0 is a safe-guard for credit/debit field
      (map (λ (k) (hash-ref kwargs k 0)) 
	   '(person-ref payment-method role credit debit total description)))
    (apply
     query-exec 
     "insert into MoneyTransaction(date,person, payment_method, role, credit, debit, total, description)
	   values(datetime('now', 'localtime'), $1, $2, $3, $4, $5, $6, $7)"
     args))

  (match kwargs
    [(hash-table ((or 'credit 'debit) amount) ('role role) ('person-ref pid)
		 ('description desc) ('payment-method payment-method)) 
     (ledger-add!)]
    [_ (raise 'invalid-kwargs-given)]))