#lang racket/gui
(require "./window/invoice.rkt"
         "./window/inventory.rkt"
         "./window/people.rkt")

(define main-window
  (new frame% [label "VM"]
       [min-width 800] [min-height 600]
       [stretchable-width #t] [stretchable-height #t]))
(define icon-path
  (build-path (find-system-path 'home-dir) "vm" "resources" "icons" "logo" "VM_32.png"))
(define VM-ICON (read-bitmap icon-path))
(send main-window set-icon VM-ICON)

(define header-font (make-font #:family 'decorative #:size 16))
(define title-font (make-font #:style 'slant #:weight 'normal))

(define (draw- F%)
  (let ([F (if F%
            (new F% [min-width 800] [min-height 600] [parent main-window])
            (new frame% [min-width 100] [parent main-window] [label "..."]))])
    (send F set-icon VM-ICON)
    (λ (b e)
      (unless (send F is-shown?) (send F show true)))))


(for ((label1 '("Customers" "Inventory" #;"Ledger"))
      (f1% (list customers-frame% inventory-frame% #f))
      (label2 '("Suppliers" "Invoices" #;"Accounts"))
      (f2% (list suppliers-frame% invoices-frame% #f)))
  (define t (new horizontal-panel% [parent main-window]))

  (new button% [parent t] [font header-font] [label label1]
       [stretchable-width #t] [stretchable-height #t]
       [callback (draw- f1%)])
  (new button% [parent t] [font header-font] [label label2]
       [stretchable-width #t] [stretchable-height #t]
       [callback (draw- f2%)]))

(send main-window show true)