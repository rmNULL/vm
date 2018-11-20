#lang racket/gui
(require "./vm.rkt")
(require "./window/contact.rkt")
(require "./window/invoice.rkt")
(require "./window/common-gui-utils.rkt")


(define main-window
  (new frame% [label "VM"]
       [min-width 350] [min-height 350]
       [stretchable-width #t] [stretchable-height #t]))


(define (transpose matrix)
  (if (empty? matrix)
      matrix
      (apply map list matrix)))

(define header-font (make-font #:family 'decorative  #:weight 'bold #:size 14))
(define title-font (make-font #:style 'slant  #:weight 'normal))



(define (draw-new-customer)
  (draw-customer (insert-customer!)))

(define (draw-customer customer-id #:parent [parent main-window])
  
  (define customer (personal-details #:of customer-id))
  (define-values (name addr contacts)
    (values (Person-name customer) (Person-address customer) (Person-contacts customer)))

  (define f (new frame% [label name] [parent parent] [width 600] [height 600]))

  (define invoice-choices
    (for/list ([(date invoice# bill# total) (invoices-produced #:for customer-id)])
      (list date invoice# bill# total)))

  #;(unless (empty? invoice-choices)
    (define (show-invoice b e)
      (when (eq? (send e get-event-type) 'list-box-dclick)
        (draw-invoice
         (send invoice-list get-data
               (send invoice-list get-selection)))))
    
    (define invoice-list
      (new extended-list-box% [label "Invoices"] [parent f] [choices '()]
           [min-width MIN_WIN_WIDTH] [min-height MIN_WIN_HEIGHT]
           [style '(single column-headers)]
           [columns '("date" "invoice#" "bill#" "total")]
           [callback show-invoice]
           ))
  
    (send/apply invoice-list set (transpose invoice-choices))
    (for ([column invoice-choices]
          [i (length invoice-choices)])
      (send invoice-list set-data i (second column))))
  
  ;; Person Details
  (define name-field
    (new text-field% [label "Name"] #;[enabled #f] [parent f] [init-value name]))
  (define (save-name!)
    (define name-in-field (send name-field get-value))
    (define updated-name
      (if (equal? name name-in-field)
          #f
          name-in-field))
    (when updated-name
      (update-person! #:column 'name #:value updated-name #:person-id customer-id)))
  
  (define address-field
    (new text-field% [label "Address"] [parent f] [init-value addr]))

  (define (save-address!)
    ;; similar to (save-name!), CALL FOR REFACTORING !!!!
    (define address-in-field (send address-field get-value))
    (define updated-address
      (and (not (equal? addr address-in-field))
           address-in-field))
    (when updated-address
      (update-person! #:column 'address #:value updated-address #:person-id customer-id)))
  
  (for ([tf (list name-field address-field)])
    (send tf set-field-background (make-object color% "DarkGray")))
 
  ;; phone numbers
  
  (define contact-card
    (new contact-card% (parent f) (person-id customer-id)))
  (for ([(label number) contacts])
    (send contact-card add-contact label number))

  (when (empty? (send contact-card get-children))
    (send contact-card add-contact))
  
  (define (save-contacts!) (send contact-card update!))

  (define (save-values! b e)
    ;(unless (void? save-invoice!) (save-invoice!))
    (save-name!)
    (save-address!)
    (save-contacts!))
  
  (new button% (parent f) (label "update") (callback save-values!))
  (send f show true)
  f)

(define (draw-customers btn evnt)
  (define f (new frame% [label "customers"] [parent main-window]))

  (define (selected-row) (send customer-list get-selection))
  (define (selected-row-pid)
    (send customer-list get-data (selected-row)))
  
  (define (show-customer btn event)
    (when (and (eq? (send event get-event-type) 'list-box-dclick)
               (selected-row))
      (draw-customer #:parent f (selected-row-pid))))

  (define COLUMN_NAMES '("Name" "Address" "Phone No.s"))
  (define customer-list
    (new extended-list-box% [label #F] [parent f] [choices '()]
         [min-width MIN_WIN_WIDTH] [min-height MIN_WIN_HEIGHT]
         [style '(single column-headers)]
         [columns COLUMN_NAMES]
         [callback show-customer]
         ))
  (for ([column# 3] [width #(100 300 100)])
    (send customer-list set-column-width column# width 0 MIN_WIN_WIDTH))

  (define (fetch-vals-choices)
    (define-values (customer-ids choices)
      (for/lists (customer-ids choices)
                 ([(customer name address) (select-customers)])
      (define pnos
        (string-join (phone-numbers #:of customer) "\n"))
      (values customer (list name address pnos))))
    (values customer-ids (transpose choices)))

  (define (draw-customer-list!)
    (send customer-list clear)
    (for ([(customer-id name addr) (select-customers)])
      (define pnos
        (string-join (phone-numbers #:of customer-id) "\n"))
      (send customer-list append-row (list name addr pnos) customer-id)))

  
  (define (rm-customer b e)
    (define customer-id (selected-row-pid))
    (dialog-prompt "delete customer"
                   "you agree to destroy all the data associated with the customer.
                    This includes Invoices, Bank Accounts etc"
                   (λ (proceed?)
                     (when proceed?
                       (delete-person! customer-id)
                       (draw-customer-list!)))))
  
  (draw-customer-list!)

  (define control-row
    (new horizontal-panel% (parent f) (alignment '[center center])))
  (new button% [label "reload values"] [parent control-row]
       [callback (λ (b e) (draw-customer-list!))])
  (new button% [label "new customer"] [parent control-row]
       [callback (λ (b e) (draw-new-customer))])
  (new button% [label "delete customer"] [parent control-row]
       [callback rm-customer])
  (send f show true)
  
  customer-list)

(define (draw-suppliers button event)
  (void))

(define (draw-inventory button event)
  (void))

(define (draw-ledger button event)
  (void))

(define (draw-invoices button event)
  (void)
  #;(new invoices-frame% [parent main-window]))

(define (draw-accounts button event)
  (void))

(for ((label1 '("Customer" "Inventory" "Ledger"))
      (cb1 (list draw-customers draw-inventory draw-ledger))
      (label2 '("Supplier" "Invoice" "Accounts"))
      (cb2 (list draw-suppliers draw-invoices draw-accounts)))
  (define t (new horizontal-panel% [parent main-window]))

  (new button% [parent t] [font header-font] [label label1]
       [stretchable-width #t] [stretchable-height #t]
       [callback cb1])
  (new button% [parent t] [font header-font] [label label2]
       [stretchable-width #t] [stretchable-height #t]
       [callback cb2]))

(define customer-list (draw-customers 0 0))
(send main-window show true)