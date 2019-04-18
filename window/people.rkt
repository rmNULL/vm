#lang racket/gui
(require "./common-gui-utils.rkt"
         "./contact.rkt"
         "./invoice.rkt"
         "../vm.rkt")
(provide ; customers-panel% customers-frame% suppliers-panel% suppliers-frame%
         draw-customers draw-suppliers)

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
      #;(send text-field set-field-background (make-object color% "DarkGray"))

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
         
         (send (new F% [id id] [parent (and (parent . is-a? . frame%) parent)])
               show #t)
      ))
    
    (super-new  [parent parent] [label #F] [choices '()]
                [min-width (* 1/2 MIN_WIN_WIDTH)] [min-height (* 1/2 MIN_WIN_HEIGHT)]
                [style '(single column-headers)]
                [callback draw-person]
                [columns '("Name" "Address" "Phone no.s")])

    (inherit selected-row selected-row-data clear append-row)

    (define/public (redraw!)
      (clear)
      (for ([(id name addr) (select-relation @relation)])
        (define pnos (phone-numbers #:of id))
        (define pno (string-join (take pnos (min (length pnos) 2)) "\n"))
        (append-row (list name addr pno) id)))

    
    (define/public (rm-selected!)
      (define id (selected-row-data))
      (when id
        (dialog-prompt (string-append "delete " @relation)
                       "you agree to destroy all the data associated with this person.
                        \rThis might include Invoices, Bank Accounts etc"
                       (λ (proceed?)
                         (when proceed?
                           (delete-person! id)
                           (redraw!))))))

    ))


(define customer-frame%
  (class frame%
    (init parent [id #f])
    (super-new [parent parent] [label "New Customer"]
               [min-height (* 3/4 MIN_WIN_HEIGHT)] [min-width (* 3/4 MIN_WIN_WIDTH)])

    (define invoices-list
      (when id
        (new invoices-list% [parent this] [customer-id id] [label "Invoices"])))
  
    (define name-addr-contact (new basic-details-panel% [parent this] [person id]))
    (when id (send this set-label (send name-addr-contact get-name)))
    
    (inherit get-parent)
    (define (save!) (send name-addr-contact save!))
    (define (redraw!) (send (get-parent) redraw!))
    (define (close-window) (send this show #f))

    (let ([mb (new menu-bar% [parent this])])
      (define m (new menu% [label "Customer"] [parent mb]))
      (new menu-item% [parent m] [label "Save"] [callback (λ (_m _c) (save!) (redraw!))])
      (new menu-item% [parent m] [label "Save && Exit"]
           [callback (λ (_m _c) (save!) (close-window))])
      (new separator-menu-item% [parent m])
      (new menu-item% [parent m] [label "Cancel && Exit"] [callback (λ (_m _c) (close-window))]))
    ))

(define supplier-frame%
  (class frame%
    (init parent [id #f])
    (super-new [parent parent] [label "New Supplier"]
               [min-height (* 3/4 MIN_WIN_HEIGHT)] [min-width (* 3/4 MIN_WIN_WIDTH)])
    
    (define name-addr-contact (new basic-details-panel% [parent this] [person id]
                                   [relation 'supplier]))
    (when id (send this set-label (send name-addr-contact get-name)))
    ;; TODO: bank accounts left out
    
    (inherit get-parent)
    (define (save!) (send name-addr-contact save!))
    (define (redraw!) (when (get-parent) (send (get-parent) redraw!)))
    (define (close-window) (send this show #f))

    (let ([mb (new menu-bar% [parent this])])
      (define m (new menu% [label "Supplier"] [parent mb]))
      (new menu-item% [parent m] [label "Save"] [callback (λ (_m _c) (save!) (redraw!))])
      (new menu-item% [parent m] [label "Save && Exit"]
           [callback (λ (_m _c) (save!) (close-window))])
      (new separator-menu-item% [parent m])
      (new menu-item% [parent m] [label "Cancel && Exit"] [callback (λ (_m _c) (close-window))]))
    ))

(define (draw-control-row parent F% L)
  (define control-row
    (new horizontal-panel% (parent parent) (alignment '[center center]) (stretchable-height #f)))
  (new button% [label "reload values"] [parent control-row]
       [callback (λ (b e) (send L redraw!))])
  (new button% [label "new"] [parent control-row]
       [callback  (λ (b e) (send (new F% (parent #f)) show #t))])
  (new button% [label "delete"] [parent control-row]
       [callback (λ (b e) (send L rm-selected!))])
  control-row)

(define (draw-customers parent)
    (define F% customer-frame%)
    (define L (new people-list-box% [parent parent] [relation "customer"]))
    (define control-row (draw-control-row parent F% L))
    (send L redraw!))

(define (draw-suppliers parent)
    (define F% supplier-frame%)
    (define L (new people-list-box% [parent parent] [relation "supplier"]))
    (define control-row (draw-control-row parent F% L))
    (send L redraw!))


#|
(define customers-panel%
  (class vertical-panel%
    (init parent)
    (super-new (parent parent))
    (define F% customer-frame%)
    (define L (new people-list-box% [parent this] [relation "customer"]))

    (define control-row (draw-control-row this F% L))
    
    (define/public (redraw!) (send L redraw!))
    (redraw!)))

(define customers-frame% 
  (class frame%
    (init [parent #f])
    (super-new (parent parent) (label "Customers"))
    (define CP (new customers-panel% [parent this]))
    (define/public (redraw!) (send CP redraw!))
    (redraw!)))


(define suppliers-panel% 
  (class vertical-panel%
    (init parent)
    (super-new (parent parent))
    (define F% supplier-frame%)
    (define L (new people-list-box% [parent this] [relation "supplier"]))

    (define control-row (draw-control-row this F% L))
    (define/public (redraw!) (send L redraw!))
    (redraw!)))

(define suppliers-frame% 
  (class frame%
    (init [parent #f])
    (super-new (parent parent) (label "Suppliers"))
    (define SP (new suppliers-panel% [parent this]))
    (define/public (redraw!) (send SP redraw!))
    (redraw!)))
|#

