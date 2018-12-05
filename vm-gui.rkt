#lang racket/gui
(require "./vm.rkt"
         "./window/contact.rkt"
         "./window/invoice.rkt"
         "./window/people.rkt")


(define main-window
  (new frame% [label "VM"]
       [min-width 350] [min-height 350]
       [stretchable-width #t] [stretchable-height #t]))

(define header-font (make-font #:family 'decorative  #:weight 'semibold #:size 18))
(define title-font (make-font #:style 'slant  #:weight 'normal))


(define (draw- frame%)
  (let ([F (new frame% [min-width 350] [parent main-window])])
      (λ (b e)
        (unless (send F is-shown?) (send F show true)))))


(define draw-customers (draw- customers-frame%))
(define draw-invoices  (draw-  invoices-frame%))
(define draw-suppliers (draw- suppliers-frame%))
(define draw-inventory  (λ (_ a)void))
(define draw-ledger  (λ (_ a)void))
(define draw-accounts (λ (_ a) void))



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

(send main-window show true)