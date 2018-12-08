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
    
    (define lot-f
      (new restricted-text-field%
           (parent this) (pattern RX-INT) (label "Lot#") (style '(single vertical-label))
           (stretchable-width #f) (min-width 20)
           (callback (λ (t e)
                       (define lot# (send t get-value))
                       (send t set-field-background (if (lot-taken? lot#) WARN-COLOR SAFE-COLOR))))))


    (define supplier-f
      (let-values ([(ids names)
                    (for/lists (ids names)
                              ([(id name _a) (select-suppliers)])
                     (values id (format "(~a) ~a" id name)))])
        (new choice-data% (label "supplier") (parent this) (datas ids) (choices names)
             (style '(vertical-label))
             (enabled (not (empty? names))))))

    (define status-f
      (new choice% (label "status") (parent this) (choices (list "open" "closed"))
           (style '(vertical-label))
           (enabled #f)))

    (define/public (get-lot) (send lot-f get-value))
    (define/public (get-supplier)
      (when (send supplier-f is-enabled?)
        (send supplier-f get-data (send supplier-f get-selection))))
    (define/public (get-status) (send status-f get-string-selection))

    (define/public (non-empty-fields?)
      (and (number? (get-supplier))
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

    (define control-panel
      (new horizontal-pane% (parent this) (alignment '(left center)) (stretchable-height #f)))
    
    (define save-msg (new message% (parent control-panel) (label "unsaved changes")))
    (new button% (parent control-panel) (label "save!")
         [callback (λ (_b _e)
                     (define lot# (save!))
                     (unless (void? lot#)
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
        (append-row (list (~a date) (~a lot) (~a supp-name) (~a status)) lot)))

    (define/public (rm-selected!)
      (define selection (get-selection))
      (when selection
        (define lot (get-data selection))
        (dialog-prompt "EMPTY LOT"
                       (format "Remove the lot ~a and all its items?" lot)
                       (λ (proceed?)
                         (when proceed?
                           (delete-inventory! lot)
                           (redraw!))))))
    (redraw!)
    ))


(module+ main
  (define F (new frame% [label "listing"] (min-width MIN_WIN_WIDTH) (min-height MIN_WIN_HEIGHT)))
  (define il (new inventory-list% (parent F)
       [callback (λ (il e) (void))]))
  (define h (new horizontal-panel% (parent F) (alignment '(center center)) (stretchable-height #f)))
  (new button% [label "add"] [parent h]
       [callback (λ (_ e) (define f (new lot-frame% (parent F))) (send f show #t))])
  (new button% [label "reload"] [parent h]
       [callback (λ (_ e) (send il redraw!))])
  (new button% [label "del"] [parent h]
       [callback (λ (_ e) (send il rm-selected!))])
  (send F show true))