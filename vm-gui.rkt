#lang racket/gui
(require "./vm.rkt")
(require "./window/contact.rkt"
         "./window/invoice.rkt"
         "./window/common-gui-utils.rkt")


(define main-window
  (new frame% [label "VM"]
       [min-width 350] [min-height 350]
       [stretchable-width #t] [stretchable-height #t]))

(define (transpose matrix)
  (if (empty? matrix)
      matrix
      (apply map list matrix)))

(define header-font (make-font #:family 'decorative  #:weight 'semibold #:size 18))
(define title-font (make-font #:style 'slant  #:weight 'normal))

(define (draw-new-customer) (draw-customer (insert-customer!)))

(define (draw-customer customer-id #:parent [parent main-window])
  
  (define customer (personal-details #:of customer-id))
  (define-values (name addr contacts)
    (values (Person-name customer) (Person-address customer) (Person-contacts customer)))

  (define f (new frame% [label name] [parent parent] [width 300] [height 300]))

  (define invoice-choices
    (for/list ([(date invoice# bill# total) (invoices-produced #:for customer-id)])
      (list date invoice# bill# total)))
  
  (define invoices-list (new invoices-list% [parent f] [customer-id customer-id]))
  (send invoices-list redraw!)
  
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
    (save-contacts!)
    (send CL redraw!))
  
  (new button% (parent f) (label "update") (callback save-values!))
  (send f show true)
  f)

(define customers-list%
  (class frame%
    (init parent)
    (super-new [parent parent] [label "customers"])

    (define (selected-row) (send customer-list get-selection))
    (define (selected-row-pid)
      (define selection (selected-row))
      (and selection (send customer-list get-data selection)))
  
    (define (show-customer btn event)
      (when (and (eq? (send event get-event-type) 'list-box-dclick)
                 (selected-row))
        (draw-customer #:parent this (selected-row-pid))))

    (define COLUMN_NAMES '("Name" "Address" "Phone No.s"))
    (define customer-list
      (new extended-list-box% [label #F] [parent this] [choices '()]
           [min-width MIN_WIN_WIDTH] [min-height MIN_WIN_HEIGHT]
           [style '(single column-headers)]
           [columns COLUMN_NAMES]
           [callback show-customer]
           ))
    (for ([column# 3] [width #(100 300 100)])
      (send customer-list set-column-width column# width 0 MIN_WIN_WIDTH))

    (inherit is-shown? [-show show])
    (define/public (redraw!)
      (when (is-shown?)
       (send customer-list clear)
        (for ([(customer-id name addr) (select-customers)])
        (define pnos
          (string-join (phone-numbers #:of customer-id) "\n"))
          (send customer-list append-row (list name addr pnos) customer-id))))
  
    (define (rm-customer b e)
      (define customer-id (selected-row-pid))
      (when customer-id
        (dialog-prompt "delete customer"
                     "you agree to destroy all the data associated with the customer.
                    This includes Invoices, Bank Accounts etc"
                     (λ (proceed?)
                       (when proceed?
                         (delete-person! customer-id)
                         (redraw!))))))

    (define control-row
      (new horizontal-panel% (parent this) (alignment '[center center])))
    (new button% [label "reload values"] [parent control-row]
         [callback (λ (b e) (redraw!))])
    (new button% [label "new customer"] [parent control-row]
         [callback (λ (b e) (draw-new-customer))])
    (new button% [label "delete customer"] [parent control-row]
         [callback rm-customer])))

(define (draw-suppliers button event) (void))

(define (draw-inventory button event) (void))

(define (draw-ledger button event) (void))

(define draw-invoice
  (let ([I (new invoices-frame% [min-width MIN_WIN_WIDTH] [parent main-window])])
    (λ (b e)
      (unless (send I is-shown?)
        (send I show true)))))

[define CL (new customers-list% [parent main-window])]
(define draw-customers
    (λ (b e)
      (unless (send CL is-shown?)
        (send CL show true)
        (send CL redraw!))))

(define (draw-accounts button event) (void))

(for ((label1 '("Customer" "Inventory" "Ledger"))
      (cb1 (list draw-customers draw-inventory draw-ledger))
      (label2 '("Supplier" "Invoice" "Accounts"))
      (cb2 (list draw-suppliers draw-invoice draw-accounts)))
  (define t (new horizontal-panel% [parent main-window]))

  (new button% [parent t] [font header-font] [label label1]
       [stretchable-width #t] [stretchable-height #t]
       [callback cb1] #;[enabled (string=? label1 "Customer")])
  (new button% [parent t] [font header-font] [label label2]
       [stretchable-width #t] [stretchable-height #t]
       [callback cb2] #;[enabled (string=? label2 "Invoice")]))

(send main-window show true)