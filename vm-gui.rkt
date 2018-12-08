#lang racket/gui
(require "./window/invoice.rkt"
         "./window/people.rkt")


(define main-window
  (new frame% [label "VM"]
       [min-width 350] [min-height 350]
       [stretchable-width #t] [stretchable-height #t]))

(define header-font (make-font #:family 'decorative #:size 16))
(define title-font (make-font #:style 'slant #:weight 'normal))


(define (draw- F%)
  (let ([F (if F%
            (new F% [min-width 350] [parent main-window])
            (new frame% [min-width 350] [parent main-window] [label "..."]))])
      (λ (b e)
        (unless (send F is-shown?) (send F show true)))))


(for ((label1 '("Customers" "Inventory" "Ledger"))
      (f1% (list customers-frame% #f #f))
      (label2 '("Suppliers" "Invoices" "Accounts"))
      (f2% (list suppliers-frame% #f #f)))
  (define t (new horizontal-panel% [parent main-window]))

  (new button% [parent t] [font header-font] [label label1]
       [stretchable-width #t] [stretchable-height #t]
       [callback (draw- f1%)])
  (new button% [parent t] [font header-font] [label label2]
       [stretchable-width #t] [stretchable-height #t]
       [callback (draw- f2%)]))

(send main-window show true)