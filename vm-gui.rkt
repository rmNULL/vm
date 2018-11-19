#lang racket/gui
(require "./vm.rkt")

(define-values (MIN_WIN_WIDTH MIN_WIN_HEIGHT) (values 500 500))
(define main-window
  (new frame% [label "VM"]
       [min-width 350] [min-height 350]
       [stretchable-width #t] [stretchable-height #t]))

(define (dialog-prompt window-name msg continue)
  (define dialog (new dialog% (label window-name)))
 
  (new message% [parent dialog] [label msg])
  (define panel
    (new horizontal-panel% [parent dialog] [alignment '(center center)]))

  ;; if you are adding code(to callback), its time for refactor !!
  (new button% [parent panel] [label "Cancel"] [callback (λ (b e)
                                                           (send dialog show false)
                                                           (continue #F))])
  (new button% [parent panel] [label "Ok"] [callback (λ (b e)
                                                       (send dialog show false)
                                                       (continue #T))])
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))
  (send dialog show #T))


(define contact-panel%
  (class horizontal-panel%
    (define CONTACT-LABELS '("Mobile" "Home" "Other"))
    (init parent
          (number-label (first CONTACT-LABELS))
          (number ""))

    (define @init-num number)
    (define @init-lab number-label)

    (define/public (modified?)
      (or
       (not (string-ci=? @init-lab (get-number-label)))
       (not (string=? @init-num (get-number)))))
    
    (super-new (parent parent) (alignment '(left top)))
 
    (define label-field
      (new combo-field% [parent this] [label #f]
           [choices CONTACT-LABELS] [init-value number-label]
           ))
    
    (define number-field
      (new text-field% [parent this] [label #f] [init-value number]
           [callback (λ (tf _)
                       (define number (send tf get-value))
                       (when (> (string-length number) 16)
                         (send tf set-value (substring number 0 16))))]))

    (new button% [parent this] [label "new"]
         [callback (λ (_b _e) (new contact-panel% (parent parent)))])
    
    (new button% [parent this] [label "delete"]
         [callback (λ (_b _e)
                     ;; definitely, there's a better way to do this.
                     (send parent delete-child this)
                     (when (empty? (send parent get-children))
                       (new contact-panel% (parent parent))))])

    (define/public (get-number-label)
      (send label-field get-value))
    
    (define/public (get-number)
      (send number-field get-value))

    (define/public (delete!)
      (delete-contact! #:number @init-num))

    (define/public (save!)
      (define-values (label number) (values (get-number-label) (get-number)))
      (define save? (and (modified?) (non-empty-string? number)))
      (define pid
        (send (send this get-parent) get-person-id))
      (define in-db? (non-empty-string? @init-num))
      
      (when save?
        (define c (Contact label number))
        (if in-db?
            (update-contact! #:old-number @init-num #:contact c)
            (insert-contact! #:contact c #:person-id pid))
        (set! @init-num number)))))


(define contact-card%
  (class group-box-panel%
    (init parent person-id)
    (define @pid person-id)
    (define/public (get-person-id) @pid)
    
    (define to-delete '())
    (define (empty-to-delete!) (set! to-delete '()))
    (define (push-to-delete child)
      (set! to-delete (cons child to-delete)))
    
    (super-new [parent parent] (label "Contact Card"))
    
    (define/override (delete-child child)
      (when (or (send child modified?)
                (non-empty-string? (send child get-number)))
        (push-to-delete child))
      (super delete-child child))

    (define/public add-contact
      (case-lambda
        [() (new contact-panel% [parent this])]
        [(l n) (new contact-panel% [parent this] [number n] [number-label l])]))

    (define (save!)
      (for ([row (in-list (send this get-children))])
        (send row save!)))

    (define (delete!)
      (for ([row (in-list to-delete)])
        (send row delete!)))

    (define/public (update!)
      (save!)
      (delete!)
      (empty-to-delete!))
    ))




(define (transpose matrix)
  (if (empty? matrix)
      matrix
      (apply map list matrix)))

(define header-font (make-font #:family 'decorative  #:weight 'bold #:size 14))
(define title-font (make-font #:style 'slant  #:weight 'normal))

(define (draw-invoice invoice-no)
  (void))

(define (draw-new-customer)
  (draw-customer (insert-customer!)))

(define (draw-customer customer-id #:parent [parent main-window])
  
  (define customer (personal-details #:of customer-id))
  (define-values (name addr contacts)
    (values (Person-name customer) (Person-address customer) (Person-contacts customer)))

  (define f (new frame% [label name] [parent parent] [width 600] [height 600]))

  (define choices
    (for/list ([(date invoice# bill# total) (invoices-produced #:for customer-id)])
      (list date invoice# bill# total)))

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
      (send invoice-list set-data i (second column))))
  
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
    (new list-box% [label #F] [parent f] [choices '()]
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
    (define-values (customer-ids choices)
      (fetch-vals-choices))
    
    (when (empty? choices)
      (set! choices (make-list (length COLUMN_NAMES) (list))))
    (send/apply customer-list set choices)
    (for ([customer-id customer-ids]
          [i (length customer-ids)])
      (send customer-list set-data i customer-id)))

  
  (define (rm-customer b e)
    (define customer-id (selected-row-pid))
    (println customer-id)
    (dialog-prompt "delete customer"
                   "Continue to delete?"
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
  (void))

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