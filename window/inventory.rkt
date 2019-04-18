#lang racket/gui
(require "./common-gui-utils.rkt"
         "../vm.rkt")

(provide draw-inventory #;inventory-frame% #;inventory-panel%)

(define lot-item-row%
  (class horizontal-panel%
    (init parent title?)
    (super-new (parent parent) (stretchable-height #f))

    (define (add-a-row t e) (send parent add-item-row))

    (define item-inputs
      (for/list ([name-pat `(("Name" . ,RX-NAME)
                             ("Qty(kg)" . ,RX-FLOAT)
                             ("package" . ,RX-NAME)
                             ("no.of packages" . ,RX-INT))])
        (define-values (name pat) (values (car name-pat) (cdr name-pat)))
        ;; BUG: The title disappears when the first row is deleted
        ;; TODO: ^ - ^
        (if title?
            (new restricted-text-field% [label name] [parent this]
                 [pattern pat] [callback add-a-row] [style '(single vertical-label)])
            (new restricted-text-field% [label ""] [parent this]
                 [pattern pat] [callback add-a-row]))))
 

    (define/public (non-empty-fields?)
      (andmap (λ (tf) (non-empty-string? (send tf get-value))) item-inputs))

    (define/public (get-item)
      (define item-fs (append (take item-inputs 2)
                              (list (list-ref item-inputs 1)) ; stock = qty
                              (take-right item-inputs 2)))
      (apply Item (map (λ (tf) (send tf get-value)) item-fs)))
    ))

(define lot-item-list%
  (class group-box-panel%
    (init parent [items #f])
    (super-new (parent parent) (label "Items"))
    (inherit delete-child get-children)

    
    (define/public (add-item-row)
      (when (non-empty-rows?)
        (define L (new lot-item-row% [parent this] [title? (empty? (get-children))]))
        #;(new button% [parent L] [label "Add"] [callback (λ (b e) (add-item-row))])
        (new button% [parent L] [label "Del"]
             [vert-margin 0] [horiz-margin 0] [style '(border)]
             [callback (λ (b e)
                         (unless (empty? (rest (get-children)))
                           (delete-child L)
                           #;(send parent resize 0 0)))])
        ))

    (define/public (non-empty-rows?)
      (for/and ([lr (get-children)]) (send lr non-empty-fields?)))
    ;(stream-andmap (λ (lr) (send lr non-empty-fields?)) (set->stream lot-rows)))

    (define/public (get-items)
      (for/list ([row (get-children)]
                 #:when (send row non-empty-fields?))
        (send row get-item)))
    
    (add-item-row)))

(define create-lot-frame%
  (class frame%
    (init parent)
    (super-new [parent parent] [label "New Lot"]
               [min-height (* 3/4 MIN_WIN_HEIGHT)] [min-width (* 3/4 MIN_WIN_WIDTH)])

    (define lot#-supp-row (new horizontal-panel% [parent this] [stretchable-height #f]))
    #;(define status-msg (new message% [label ""] [parent this] [stretchable-height #f]))

    ;; status : (or/c 'empty 'taken 'available)
    (define (set-lot#-status! status)
      (send lot#-f
            set-field-background 
            (if (or (eq? status 'empty) (eq? status 'taken))
                WARN-COLOR
                DEF-BG-COLOR)))

    (define lot#-f
      (new restricted-text-field%
           [pattern RX-INT]
           [label "Lot Number"] [parent lot#-supp-row] [style '(single vertical-label)]
           [stretchable-width false]
           [callback (λ (tf e)
                       (define lot# (send tf get-value))
                       (set-lot#-status! (cond
                                           [(string=? "" lot#) 'empty]
                                           [(lot-taken? lot#) 'taken]
                                           [else 'available])))]))
    (set-lot#-status! 'empty)

    (define-values (ids names)
      (for/lists (ids names)
                 ([(id name _addr) (select-suppliers)])
        (values id name)))
    
    (define supplier-f
      (new choice-data% (label "supplier")
           (parent lot#-supp-row)
           (style '(vertical-label))
           [datas ids]
           [choices names]
           [stretchable-width false]))

    (define item-list
      (new lot-item-list% [parent this]))
    
    
    (define (lot#-supp-fed?)
      (define lot# (send lot#-f get-value))
      (define lot#? (and (non-empty-string? lot#) (not (lot-taken? lot#))))
      ;; careless checking :(
      (define supp? (send supplier-f get-selection))
      (and supp? lot#?))

    
    (let ([save! (λ ()
                   (when (lot#-supp-fed?)
                     (define items (send item-list get-items))
                     (when (not (empty? items))
                       (define lot# (string->number (send lot#-f get-value)))
                       (define supp (send supplier-f get-data
                                          (send supplier-f get-selection)))
                       (insert-lot! #:supplier supp
                                    #:items items
                                    #:lot# lot#))))]
          [mb (new menu-bar% [parent this])])
      (define m (new menu% [label "Save"] [parent mb]))
      (new menu-item%
           [parent m]
           [label "Save"]
           [callback (λ (m e) (save!))])
      (new menu-item%
           [parent m]
           [label "Save and Exit"]
           [callback (λ (m e) (when (number? (save!)) (send this show #f)))]))
    ))

(define (draw-inventory parent)
    (define search-bar
      (new horizontal-panel% [parent parent] [stretchable-height #f]))

    (define search-lot# (new restricted-text-field%
                             [pattern RX-INT]
                             [label "Lot#"]
                             [parent search-bar]
                             [style '(single vertical-label)]
                             [callback (λ (t e) (when (get-lot#) (set-items-list!)))]))

    (define search-item (new item-input-field%
                             [parent search-bar]
                             [label "Item"]
                             [style '(single vertical-label)]
                             [callback (λ (t e) (when (get-item-name)
                                                  (set-items-list!)))]))

    
    (define search-date-start (new date-input-field%
                                   [label "Date(Start)"]
                                   [parent search-bar]
                                   [style '(single vertical-label)]
                                   [allow-empty? #t]
                                   [callback (λ (t e) (and (seconds? (send t get-converted-value))
                                                           (set-items-list!)))]
                                   ))

    (define search-date-end (new date-input-field%
                                 [label "Date(End)"]
                                 [parent search-bar]
                                 [style '(single vertical-label)]
                                 [allow-empty? #t]
                                 [callback (λ (t e) (and (seconds? (send search-date-start
                                                                         get-converted-value))
                                                     (seconds? (send t get-converted-value))
                                                     (set-items-list!)))]
                                 ))

    (define search-supplier
      (let-values ([(ids names) (for/lists (cids names)
                                           ([(id name _addr) (select-suppliers)])
                                  (values id name))])
        (new choice-data% [label "Supplier"] [style '(vertical-label)] [parent search-bar]
             [datas ids] [choices names]
             [callback (λ (c e) (when (get-supplier-id) (set-items-list!)))])))

    (define toggle-bar
      ;; If the items are are selected, then the selected search item is considered
      (let ([box (new group-box-panel% [label "Consider For Search?"] [parent parent]
           [alignment '(left center)] [stretchable-height #f])])
        (new horizontal-panel% [parent box] [stretchable-height #f])))

    (define toggle-lot# (new check-box% [parent toggle-bar] [label "lot#"]
                             [callback (λ (cb ev) (set-items-list!))]))
    #;(define toggle-item (new check-box% [parent toggle-bar] [label "Item"]
                             [callback (λ (cb ev) (set-items-list!))]))
    (define toggle-supplier (new check-box% [parent toggle-bar] [label "Supplier"]
                                 [callback (λ (cb ev) (set-items-list!))]))
    (define toggle-in-stock? (new check-box% [parent toggle-bar] [label "In Stock?"]
                                  [callback (λ (cb ev) (set-items-list!))]))

    (define (get-lot#) (and (send toggle-lot# get-value)
                            (send search-lot# get-value)))
    (define (get-item-name)
      (define item-name (send search-item get-converted-value))
      (and #;(send toggle-item get-value)
           (if (eq? 'empty item-name) "" item-name)))
    (define (get-in-stock?) (send toggle-in-stock? get-value))
    (define (get-date-from)
      (define d (send search-date-start get-converted-value))
      (and (seconds? d) d))
    (define (get-date-to)
      (define d (send search-date-end get-converted-value))
      (and (seconds? d) d))
    (define (get-supplier-id) (and (send toggle-supplier get-value)
                                   (send search-supplier get-selection-data)))

    (define items-list
      (new extended-list-box%
           [label #f]
           [choices '()] ;; appended later
           [parent parent]
           [columns '("Lot#" "Item" "Qty" "Stock" "Pkg" "Pkg Count")]
           [style '(multiple column-headers)]))
    
    (define (set-items-list!)
      (send items-list clear)
      (define rows
        (filter-inventory #:lot# (get-lot#)
                          #:item-name (get-item-name)
                          #:in-stock? (get-in-stock?)
                          #:date-from (get-date-from)
                          #:date-to (get-date-to)
                          #:supplier (get-supplier-id)))
      (for ([(lot# item qty stk pkg pkgc) rows])
        (define row (list (~a lot#) item (~a qty) (~a stk) pkg (~a pkgc)))
        (send items-list append-row row)))

  (set-items-list!)

  (define CR (new horizontal-panel% [parent parent] [stretchable-height #f]
                  [alignment '(center top)]))
  (new button% [parent CR] [label "Reload"] [callback (λ (b e) (set-items-list!))])
  (new button% [parent CR] [label "Add Lot"]
       [callback (λ (_m _c) (send (new create-lot-frame% [parent #f]) show #t))]))


#|
(define inventory-panel%
  (class vertical-panel%
    (init parent)
    (super-new [parent parent])
    (draw-inventory this)))

(define inventory-frame%
  (class frame%
    (init [parent #f])
    (super-new [label "Inventory"] [parent parent])

    (let ()
      (define mb (new menu-bar% [parent this]))
      (define m (new menu% [label "Add"] [parent mb]))
      (new menu-item% [parent m] [label "Lot"]
           [callback (λ (_m _c)
                       (send (new create-lot-frame% [parent this]) show #t))]))

    (new inventory-panel% [parent this])))
|#


