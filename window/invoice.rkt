#lang racket/gui
(require "../vm.rkt")
(require "./common-gui-utils.rkt")


(provide invoices-frame%)

(define (draw-invoice invoice-no)
  (void))


(define invoices-frame%
  (class frame%
    (init parent (title "Invoices"))
    (super-new (parent parent) (label title))

    (define invoice-list
      #;(new list-box% [label "Invoices"] [parent this] [choices '()]
           [min-width MIN_WIN_WIDTH] [min-height MIN_WIN_HEIGHT]
           [style '(single column-headers)]
           [columns '("date" "invoice#" "bill#" "total")]
           [callback show-invoice]
           )(void))

    
    (send this show true)
    ))