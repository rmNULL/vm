#lang racket/gui
(require "./vm.rkt")

(define-values (MIN_WIN_WIDTH MIN_WIN_HEIGHT) (values 500 500))
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

(define (draw-invoice invoice-no)
  (void))

(define (draw-customer customer-id)
  
  (define customer (personal-details #:of customer-id))
  (define-values (name addr contacts)
    (values (Person-name customer) (Person-address customer) (Person-contacts customer)))

  (define f (new frame% [label name] [parent main-window]))

  (define choices
    (for/list ([(date invoice# bill# total) (invoices-produced #:for customer-id)])
      (list date invoice# bill# total)))

  (define save-invoice! 
    (unless (empty? choices)
      (define (show-invoice b e)
        (when (eq? (send e get-event-type) 'list-box-dclick)
          (draw-invoice
           (send invoice-list get-data
                 (send invoice-list get-selection)))))
    
      (define invoice-list
        (new list-box% [label "Invoices"] [parent f] [choices '()]
             [min-width MIN_WIN_WIDTH] [min-height MIN_WIN_HEIGHT]
             [style '(single column-headers)]
             [columns '("date" "invoice#" "bill#" "total")]
             [callback show-invoice]
             ))
  
      (send/apply invoice-list set (transpose choices))
      (for ([column choices]
            [i (length choices)])
        (send invoice-list set-data i (second column)))
    
      save-invoice!))

  
  ;; Person Details
  (define name-field
    (new text-field% [label "Name"] [parent f] [init-value name]))
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
   (new group-box-panel% [label "Contact Card"] [parent f] [font title-font]
       ))
  (for ([(label number) contacts])
    (new text-field% [label label] [parent contact-card] [init-value number]
         ))
  (define (save-contacts!)
    (for ([number-field (in-list (send contact-card get-children))]
          [(label number) contacts]
          #:when (not (string=? number (send number-field get-value))))
      (define no (send number-field get-value))
      (update-contact-number! #:label label #:number no #:person-id customer-id)
    ))

  (define (save-values! b e)
    (unless (void? save-invoice!) (save-invoice!))
    (save-name!)
    (save-address!)
    (save-contacts!))
  
  (new button% (parent f) (label "update") (callback save-values!))
  (send f show true)
  f)

(define (draw-customers btn evnt)
  (define f (new frame% [label "customers"] [parent main-window]))
  
  (define (show-customer btn event)
    (when (eq? (send event get-event-type) 'list-box-dclick)
      (draw-customer
       (send customer-list get-data
        (send customer-list get-selection)))))

  (define customer-list
    (new list-box% [label #F] [parent f] [choices '()]
         [min-width MIN_WIN_WIDTH] [min-height MIN_WIN_HEIGHT]
         [style '(single column-headers)]
         [columns '("Name" "Address" "Phone No.s")]
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
    (define-values (customer-ids choices)
      (fetch-vals-choices))
    (send/apply customer-list set choices)
    (for ([customer-id customer-ids]
        [i (length customer-ids)])
      (send customer-list set-data i customer-id)))

  (draw-customer-list!)
  
  (new button% [label "reload values"] [parent f]
       [callback (λ (b e) (draw-customer-list!))])
  (send f show true)
  customer-list)


(for ((label1 '("Customer" "Inventory" "Ledger"))
      (label2 '("Supplier" "Invoice" "Account")))
  (define t (new horizontal-panel% [parent main-window]))
  (for ([label (list label1 label2)])
    (new button% [parent t] [font header-font] [label label]
         [stretchable-width #t] [stretchable-height #t])))

(define customer-list (draw-customers 0 0));(send main-window show true)