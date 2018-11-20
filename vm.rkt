#lang racket
(require db)
(provide (all-defined-out))

(define (init-db)
  (define D (sqlite3-connect #:database "vm.db" #:mode 'create))
  (query-exec D "pragma foreign_keys = on")
  D)



(define DBCON (init-db))
(define sqlite-true  1 #;(query-value DBCON "select true"))
(define sqlite-false 0 #;(query-value DBCON "select false"))
(define (last-insert-rowid)
  (query-value DBCON "select last_insert_rowid()"))

(struct Contact (label number))
(struct Account (number IFSC bank branch))

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


(define (delete-contact! #:number number)
  (query-exec DBCON "delete from Contacts where number = ?" number))

(define TAB-ATTRIBS
    #hash((People . (id name address permanent relation))
          (Contacts . (person label number))))

#;(define (select-query #:table table  #:fields fields)
  ;; table is assumed to exist !!
  (define table-name (format "~a" table))
  (define table-attribs (hash-ref TAB-ATTRIBS table))
  
  (define-values (attrs tup)
    (for/lists (attrs tup)
               ([(f v) fields]
                #:when (member f table-attribs))
      (values (format "~a" f) v)))

  (if (empty? attrs)
      (values (string) (list))
      (values
       (string-append "select "
                     (string-join attrs ",")
                     " FROM " table-name
                     " where "
                     (string-join (map (λ (attr) (string-append attr " = ?")) attrs) " AND "))
       tup)))

(define (update-person! #:column col #:value val #:person-id person)
  (define col-names (hash-ref TAB-ATTRIBS 'People))
  (when (member col col-names)
    (define col-name (format "~a" col))
    (query-exec
     DBCON
     (string-append "update People set " col-name " = $1 where id = $2")
     val person)))



(struct Person
  (name address relation contacts bank-accounts))

(define (personal-details #:of person)
  (define-values (name address relation)
    (vector->values
     (query-row DBCON "select name, address, relation from People where id = ?" person)))
  (Person name address relation
          (contact-details #:of person)
          (bank-details #:of person)))

(define (phone-numbers #:of person)
  (query-list DBCON "select number from Contacts where person = ?" person))

(define (contact-details #:of person)
  (in-query DBCON "select label, number from Contacts where person = ?" person))

(define (bank-details #:of person)
  (in-query DBCON "select  name, number, bank, branch, IFSC from BankAccounts
                   where person = ?" person))

(define (invoices-produced #:for customer)
  (in-query
   DBCON
   "select date, number, bill_number, total from Invoice where customer = ?" customer))

(define (delete-person! person-id)
   (query-exec DBCON "delete from People where id = ?" person-id))

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


(define (select-relation relation #:exclusive? [exclusive? false])    
  (define pred
    (if exclusive?
        "relation = ?"
        "relation = ? OR relation = 'both'"))
  
  (in-query DBCON
            (format "select id, name, address from People
                     where permanent AND ~a
                     ORDER by name"
                    pred)
            relation))

(define (supplier? p)
  (query-maybe-value
   DBCON
   "select ? IN (select id from People where relation in ('supplier', 'both'))"))

(define (select-customers) (select-relation "customer"))
(define (select-suppliers) (select-relation "supplier"))
(define (select-x-customers) (select-relation "customer" #:exclusive? true))
(define (select-x-suppliers) (select-relation "supplier" #:exclusive? true))

(define (insert-lot! #:supplier p #:items (items '()) #:lot# (lot# #F))
  (when (supplier? p)
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
    (commit-transaction DBCON)))

(struct Item
  (name quantity (stock #:mutable) ppu package package-count))

(define (insert-inventory-item!
         #:lot lot
         #:item item)
  (query-exec DBCON
              "insert into Items(lot, name, qty, stock, ppu, package, package_count)
               values( $1, $2, $3, $4, $5, $6, $7 )"
              lot
              (Item-name item)
              (Item-quantity item)
              (Item-stock item)
              (Item-ppu item)
              (Item-package item)
              (Item-package-count item)))

(define (select-lots)
  (in-query DBCON "select (lot, date, supplier) from Inventory where status = 'open'"))

(define (select-item lot# name)
  (apply Item
         (vector->list
          (query-row DBCON
                     "select (name, qty, stock, ppu, package, package_count) from Items
                     where lot = $1 AND name = $2"
                     lot# name))))

(define (select-items lot#)
  (in-query DBCON
            "select (name, qty, stock, ppu, package, package_count) from Items
              where lot# = $1"
            lot#))

(struct Selling
  (lot item-name qty package n_packages))

(define (sell! #:to customer #:items s-items  #:bill-number bill)
  (start-transaction DBCON)
  (query-exec DBCON
              "insert into Invoice(bill_number, date, customer) values ($1,date('now'),$2)"
              bill customer)
  (define invoice-no (last-insert-rowid))
  (for ([item s-items])
    (insert-invoice-item! invoice-no item))
  (commit-transaction DBCON))

(define (update-item-stock! lot item-name stock)
  (query-exec DBCON
              "update Items set stock = $1 where lot = $2 AND name = $3"
              stock lot item-name))

(define (insert-invoice-item! invoice s-item)
  (define-values (item-name lot qty)
    (values (Selling-item-name s-item) (Selling-lot s-item) (Selling-qty s-item)))
  (define item (select-item lot item-name))
  (define updated-stock (- (Item-stock item) qty))
  
  (when (>= updated-stock 0)
    (define amount (* (Item-ppu item) qty))
    (start-transaction DBCON)
    (query-exec DBCON
                "insert into Sold(invoice, lot, item, qty, amount, package, package_count)
                 values ($1,$2,$3,$4,$5,$6,$7)"
                lot item-name qty amount (Selling-package s-item) (Selling-n_packages s-item))
    (update-item-stock! lot item-name updated-stock)
    (commit-transaction DBCON)))


(define (close-db D)
  (when (and (connection? D)
             ( connected? D))
    (disconnect D)))