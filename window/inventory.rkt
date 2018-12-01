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
                             #;("Stock" . ,RX-INT)
                             ("ppu" . ,RX-FLOAT)
                             ("package" . ,RX-NAME)
                             ("no.of packages" . ,RX-INT))])
        (define-values (name pat) (values (car name-pat) (cdr name-pat)))
        (new restricted-text-field% [label name]
             [style '(single vertical-label)] [parent this] [pattern pat])))

    (define/public (non-empty-fields?)
      (andmap (λ (tf) (non-empty-string? (send tf get-value))) item-inputs))

    (define/public (get-item)
      (apply Item (map (λ (tf) (send tf get-value)) item-inputs)))
    ))

(define lot-item-list%
  (class group-box-panel%
    (init parent [items #f])
    (super-new (parent parent) (label "Items"))

    (define lot-rows (mutable-set))

    (inherit delete-child get-children)
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
          (map (λ (tf) (send tf get-item)) row)))
    
    (add-item)))



;; lot# supplier status
(define lot-head-row%
  (class horizontal-panel%
    (init parent)
    (super-new (parent parent) (alignment '(left bottom)) (stretchable-height #f))
    
    (define lot-f
      (new restricted-text-field%
           (parent this) (pattern RX-INT) (label "Lot#") (style '(single vertical-label))
           (stretchable-width #f) (min-width 20)))


    (define supplier-f
      (let-values ([(ids names)
                    (for/lists (ids names)
                              ([(id name _a) (select-suppliers)])
                     (values id (format "(~a) ~a" name)))])
        (new choice-data% (label "supplier") (parent this) (datas ids) (choices names)
             (style '(vertical-label))
             (enabled (not (empty? names))))))

    (define status-f
      (new choice% (label "status") (parent this) (choices (list "open" "closed"))
           (style '(vertical-label))
           (enabled #f)))

    (define/public (get-lot) (send lot-f get-string-selection))
    (define/public (get-supplier)
      (when (send supplier-f is-enabled?)
        (send supplier-f get-data (send supplier-f get-selection))))
    (define/public (get-status) (send status-f get-string-selection))

    (define/public (non-empty-fields?)
      (and (non-empty-string? (get-supplier))
           (non-empty-string? (get-lot))
           (non-empty-string? (get-status))))
    
    ))

(define lot-frame%
  (class frame%
    (init [parent #f] [label "New Lot"])
    (super-new (parent parent) (label label) (width MIN_WIN_WIDTH) (height MIN_WIN_HEIGHT))

    (define date-f (new message% (parent this) (label (date-today))))
    (define head-f (new lot-head-row% [parent this]))

    (define items-f (new lot-item-list% (parent this)))

    (define (all-inputs-filled?) (and (send items-f non-empty-rows?)
                                      (send head-f non-empty-fields?)))

    (define (save!)
      (define saved? (all-inputs-filled?))
      (when saved?
        (define-values (supp lot#) (values (send head-f get-supplier) (send head-f get-lot)))
        (insert-lot! #:lot# lot# #:supplier supp
                     #:items (send items-f get-items)))
      saved?)

    (define control-panel
      (new horizontal-pane% (parent this) (alignment '(left center)) (stretchable-height #f)))
    
    (define save-msg (new message% (parent control-panel) (label "unsaved changes")))
    (new button% (parent control-panel) (label "save!")
         [callback (λ (_b _e) (when (save!)
                                (send save-msg set-label "saved")
                                (thread (λ () (sleep 10) (send save-msg set-label "")))))])))


(define inventory-list%
  (class extended-list-box%
    (init parent)
    (super-new [parent parent]
               [label #f]
               [style '(single column-headers vertical-label)]
               #;[min-width MIN_WIN_WIDTH] #;[min-height MIN_WIN_HEIGHT]
               [columns '("date" "lot" "supplier" "status")]
               [choices '()])

    (inherit get-selection get-data append-row)

    (define/public (redraw!)
      (send this clear)
      (for ([(date lot supp-name status) (select-lots)])
        (append-row (list date lot supp-name status) lot)))

    (define/public (rm-selected!)
      (define lot (get-data (get-selection)))
      (dialog-prompt "EMPTY LOT"
                     (format "Remove the lot and all its items?" lot)
                     (λ (proceed?)
                       (when proceed?
                         (delete-inventory! lot)
                         (redraw!)))))
    ))


(module+ main
  (define F (new frame% [label "listing"] (min-width MIN_WIN_WIDTH) (min-height MIN_WIN_HEIGHT)))
  (new inventory-list% (parent F)
       [callback (λ (il e) (void))])
  (define h (new horizontal-panel% (parent F) (alignment '(center center)) (stretchable-height #f)))
  (new button% [label "add"] [parent h]
       [callback (λ (_ e) (define f (new lot-frame% (parent F))) (send f show #t))])
  (send F show true))