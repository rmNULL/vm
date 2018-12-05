#lang racket/gui
(require "./common-gui-utils.rkt"
         "./contact.rkt"
         "./invoice.rkt"
         "../vm.rkt")
(provide customers-frame% suppliers-frame%)

(define basic-details-panel%
  (class vertical-panel%
    (init parent person [relation 'customer])
    (super-new (parent parent))
    (define @rel relation)

    (define @pid person)
    (define-values (name addr _)
      (let ([row (select-person @pid)])
        (if row (vector->values row) (values "" "" ""))))

    (define @name-addr (mcons name addr))

    (define/public (get-name) (mcar @name-addr))
    (define/public (get-addr) (mcdr @name-addr))
    
    (define (text-field table-column)
      (define-values (selector modifier!)
        (if (eq? 'name table-column) (values mcar set-mcar!) (values mcdr set-mcdr!)))
      
      (define text-field
        (new text-field% [label (symbol->string table-column)] [parent this]
             [init-value (selector @name-addr)]))
      (send text-field set-field-background (make-object color% "DarkGray"))

      (λ ()
        (define previous-entry (selector @name-addr))
        (define current-entry (send text-field get-value))
        (define changed-entry
          (if (equal? previous-entry current-entry) #f current-entry))

        (when changed-entry
          (update-person! #:column table-column #:value changed-entry #:person-id @pid)
          (set! previous-entry changed-entry)
          (modifier! @name-addr changed-entry)
          )))

    (define save-name! (text-field 'name))
    (define save-address! (text-field 'address))

    (define save-contacts!
      (let ([contact-card (new contact-card% (parent this) (person-id @pid))])
        (λ () (unless (eq? @pid (send contact-card get-person-id))
                (send contact-card set-person-id! @pid))
          (send contact-card update!))))

    (define/public (save!)
      (unless @pid
        (if (eq? @rel 'customer) (insert-customer!) (insert-supplier!))
        (set! @pid (last-insert-rowid)))
      (save-name!)
      (save-address!)
      (save-contacts!))
    ))


(define people-list-box%
  (class extended-list-box%
    (init parent relation)
    (unless (member relation '("customer" "supplier"))
      (raise "~a says, relation must be 'customer' or 'supplier' " this%))

    (define @relation relation)

    (define F% (if (equal? relation "customer") customer-frame% supplier-frame%))
    
    (define (draw-person lb event)
       (when (and (eq? (send event get-event-type) 'list-box-dclick)
                 (send lb selected-row))
         (define id (send lb selected-row-data))
         
         (send (new F% [id id] [parent parent]) show #t)
      ))
    
    (super-new  [parent parent] [label #F] [choices '()]
                [min-width MIN_WIN_WIDTH] [min-height MIN_WIN_HEIGHT]
                [style '(single column-headers)]
                [callback draw-person]
                [columns '("Name" "Address" "Phone no.s")])

    (inherit selected-row selected-row-data clear append-row)

    (define/public (redraw!)
      (clear)
      (for ([(id name addr) (select-relation @relation)])
        (define pnos (string-join (phone-numbers #:of id) "\n"))
        (append-row (list name addr pnos) id)))

    
    (define/public (rm-selected!)
      (define id (selected-row-data))
      (when id
        (dialog-prompt (string-append "delete " @relation)
                       "you agree to destroy all the data associated with this person.
                        This might include Invoices, Bank Accounts etc"
                       (λ (proceed?)
                       (when proceed?
                         (delete-person! id)
                         (redraw!))))))

    ))


(define customer-frame%
  (class frame%
    (init parent [id #f])
    (super-new (parent parent) (label ""))

    (define invoices-list
      (when id
        (new invoices-list% [parent this] [customer-id id] [label "Invoices"])))
  
    (define name-addr-contact (new basic-details-panel% [parent this] [person id]))
    (send this set-label (send name-addr-contact get-name))
    
    
    (inherit get-parent)
    (define (save! b e)
      (send name-addr-contact save!)
      (send b set-label "update!")
      (send (get-parent) redraw!))
  
    (define save-button
      (new button% (parent this) (label (if id "update" "save!")) (callback save!)))

    (define save-and-exit-button
      (new button% (parent this) (label (string-append (if id "update" "save!") "& exit"))
           (callback (λ (b e)
                       (save! b e)
                       (send this show #f)))))
    ))

(define (draw-control-row parent F% L)
  (define control-row
    (new horizontal-panel% (parent parent) (alignment '[center center]) (stretchable-height #f)))
  (new button% [label "reload values"] [parent control-row]
       [callback (λ (b e) (send L redraw!))])
  (new button% [label "new"] [parent control-row]
       [callback  (λ (b e) (send (new F% (parent parent)) show #t))])
  (new button% [label "delete"] [parent control-row]
       [callback (λ (b e) (send L rm-selected!))])
  control-row)

(define customers-frame% 
  (class frame%
    (init parent)
    (super-new (parent parent) (label "Customers"))
    (define F% customer-frame%)
    (define L (new people-list-box% [parent this] [relation "customer"]))

    (define control-row (draw-control-row this F% L))
    
    (define/public (redraw!) (send L redraw!))
    (redraw!)))


(define supplier-frame%
  (class frame%
    (init parent [id #f])
    (super-new (parent parent) (label ""))
  
    (define name-addr-contact (new basic-details-panel% [parent this] [person id] [relation 'supplier]))
    (send this set-label (send name-addr-contact get-name))
    ;;;  bank accounts left out
    
    (inherit get-parent)
    (define (save! b e)
      (send name-addr-contact save!)
      (send b set-label "update!")
      (send (get-parent) redraw!))
  
    (define save-button
      (new button% (parent this) (label (if id "update" "save!")) (callback save!)))

    (define save-and-exit-button
      (new button% (parent this) (label (string-append (if id "update" "save!") "& exit"))
           (callback (λ (b e)
                       (save! b e)
                       (send this show #f)))))
    ))

(define suppliers-frame% 
  (class frame%
    (init parent)
    (super-new (parent parent) (label "Suppliers"))
    (define F% supplier-frame%)
    (define L (new people-list-box% [parent this] [relation "supplier"]))
    
    (define control-row (draw-control-row this F% L))

    (define/public (redraw!) (send L redraw!))

    (redraw!)))
