#lang racket/gui
(require "../vm.rkt")
(require "./common-gui-utils.rkt")

(provide contact-card%)

(define contact-panel%
  (class horizontal-panel%
    (define CONTACT-LABELS (map string-titlecase (contact-labels)))
    (init parent
          (number-label (first CONTACT-LABELS))
          (number ""))

    (define @parent parent)
    (define @init-num number)
    (define @init-lab number-label)

    (define/public (modified?)
      (or
       (not (string-ci=? @init-lab (get-number-label)))
       (not (string-ci=? @init-num (get-number)))))
    
    (super-new (parent parent) (alignment '(left top)) (stretchable-height #f))
 
    (define label-field
      (new combo-field% [parent this] [label #f]
           [choices CONTACT-LABELS] [init-value number-label]))
    
    (define number-field
      (new restricted-text-field%
           [pattern #px"\\d{,16}"]
           [parent this] [label #f] [init-value number]
           [stretchable-width #f]
           ))

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
      (define-values (label number)
        (values (string-downcase (string-normalize-spaces (get-number-label))) (get-number)))
      (define save? (and (modified?) (non-empty-string? number)))
      (define pid (send @parent get-person-id))
      (define in-db? (non-empty-string? @init-num))
      
      (when save?
        (define c (Contact label number))
        (if in-db?
            (update-contact! #:old-number @init-num #:contact c)
            (insert-contact! #:contact c #:person-id pid))
        (set! @init-num number)
        ))
    ))


(define contact-card%
  (class group-box-panel%
    (init parent person-id)
    (define @pid person-id)

    
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
        [(l n) (new contact-panel% [parent this]
                    [number n]
                    [number-label (string-titlecase l)])]))

    (define (save!)
      (for ([row (in-list (send this get-children))])
        (send row save!)))

    (define (clear!)
      (for ([row (in-list (send this get-children))])
        (super delete-child row)))

    (define (delete!)
      (for ([row (in-list to-delete)])
        (send row delete!))
      (empty-to-delete!))

    (define/public (update!)
      ;; clear! should be called only after save!, the order matters
      ;; as clear! deletes the children
      (save!)
      (delete!)
      (clear!)
      (draw!))

    (define (draw!)
      (define contacts (if @pid (contact-details #:of @pid) '()))
      (define added?
        (for/last ([(label number) contacts])
          (add-contact label number)))
      (unless added? (add-contact)))

    (draw!)
    
    (define/public (get-person-id) @pid)
    (define/public (set-person-id! v)
      (set! @pid v)
      (draw!))
    ))