#lang racket/gui
(require racket/set)
(require "./common-gui-utils.rkt"
         "../vm.rkt")

(define lot-item-row%
  (class horizontal-panel%
    (init parent)
    (super-new (parent parent) (stretchable-height #f))
    
    (define item-inputs
      (for/list ([name-pat `(("Name" . ,RX-NAME)
                             ("Qty(kg)" . ,RX-INT)
                             ("package" . ,RX-NAME)
                             ("no.of packages" . ,RX-INT))])
        (define-values (name pat) (values (car name-pat) (cdr name-pat)))
        (new restricted-text-field% [label name]
             [style '(single vertical-label)] [parent this] [pattern pat])))

    (define/public (non-empty-fields?)
      (andmap (λ (tf) (non-empty-string? (send tf get-value))) item-inputs))

    (define/public (get-item)
      (define final (append (take item-inputs 2)
                            (list (list-ref item-inputs 1))
                            (take-right item-inputs 2)))
      (apply Item (map (λ (tf) (send tf get-value)) final)))
    ))

(define lot-item-list%
  (class group-box-panel%
    (init parent [items #f])
    (super-new (parent parent) (label "Items"))
    (inherit delete-child get-children)

    (define lot-rows (mutable-set))

    (define (add-item)
      (when (non-empty-rows?)
        (define H (new horizontal-panel% (parent this) (stretchable-height #f)))
        (define l (new lot-item-row% [parent H]))
        (new button% [parent H] [label "Add"] [callback (λ (b e) (add-item))])
        (new button% [parent H] [label "Del"]
             [callback (λ (b e)
                         (unless (= (set-count lot-rows) 1)
                           (set-remove! lot-rows l)
                           (delete-child H)))])
        (set-add! lot-rows l)))

    (define/public (non-empty-rows?)
      (stream-andmap (λ (lr) (send lr non-empty-fields?)) (set->stream lot-rows)))

    (define/public (get-items)
      (for/list ([row lot-rows])
          (send row get-item)))
    
    (add-item)))



;; lot# supplier status
(define lot-head-row%
  (class horizontal-panel%
    (init parent)
    (super-new (parent parent) (alignment '(left bottom)) (stretchable-height #f))

    (define supplier-f
      (new choice-data% (label "supplier") (parent this) (style '(vertical-label))))

    (define status-f
      (new message% (label "open for sale") (parent this)
           (stretchable-width #f)
           (stretchable-height #f)))

    
    (define (draw! lot#)
      (define suppliers (if lot# (select-lot-) (select-suppliers))))

    

    

    #;(define/public (get-supplier)
        (if (send supplier-f is-enabled?)
          (send supplier-f get-data (send supplier-f get-selection))
          (void)))

    #;(define/public (get-status) (send status-f get-string-selection))

    #;(define/public (non-empty-fields?)
      (and (number? (get-supplier))
           (non-empty-string? (get-status))))
    
    ))

(define lot-frame%
  (class frame%
    (init [parent #f] [label "New Lot"])
    (super-new (parent parent) (label label) (width MIN_WIN_WIDTH) (height MIN_WIN_HEIGHT))

    (define date-f (new message% (parent this) (label (date-today))))
    (define lot-f
      (new combo-field%
           (parent this) (pattern RX-INT) (label "Lot#") (style '(single vertical-label))
           (stretchable-width #f) (min-width 20)
           (callback void)))
    
    (define head-f (new lot-head-row% [parent this]))
    (define items-f (new lot-item-list% (parent this)))

    
    (define control-panel
      (new horizontal-pane% (parent this) (alignment '(left center)) (stretchable-height #f)))
    
    (new button% (parent control-panel) (label "save!")
         [callback void])))



#|
    (define (all-inputs-filled?)
      (and (send items-f non-empty-rows?)
           (send head-f non-empty-fields?)))

    (define (save!)
      (define filled? (all-inputs-filled?))
        (when filled?
          (let ([supp (send head-f get-supplier)]
                [lot# (send head-f get-lot)])
            (unless (lot-taken? lot#)
              (insert-lot! #:lot# lot# #:supplier supp
                           #:items (send items-f get-items))
              lot#))))
|#