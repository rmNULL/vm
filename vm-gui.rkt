#lang racket/gui
(require "./window/invoice.rkt"
         "./window/inventory.rkt"
         "./window/people.rkt"
         "./window/daybook.rkt")

(define main-window
  (new frame% [label "VM"]
       [width 800] [height 600]
       [stretchable-width #t] [stretchable-height #t]))

(define logo-dir (build-path (current-directory) "resources" "icons" "logo"))
(define icon-path (build-path logo-dir "VM_32.png"))
(define VM-ICON (read-bitmap icon-path))
(send main-window set-icon VM-ICON)

;(define header-font (make-font #:family 'decorative #:size 20))
;(define title-font (make-font #:style 'slant #:weight 'normal))

(define (clear-tab-contents) (send T change-children (λ (c) (list))))

(define MAIN-MENU-TITLES-RFN
  `(("Customers" . ,draw-customers)
    ("Inventory" . ,draw-inventory)
    ("Suppliers" . ,draw-suppliers)
    ("Invoices"  . ,draw-invoices)
    ("Day Book"  . ,draw-daybook)))

(define T
  (new tab-panel%
     [parent main-window]
     [choices (map car MAIN-MENU-TITLES-RFN)]
     [callback (λ (t e)
                 (render-T-content (cdr (list-ref MAIN-MENU-TITLES-RFN (send t get-selection)))))]))


(define (render-T-content render-fn)
  (clear-tab-contents)
  (render-fn T))

(define VM-GREETING-SIZE 512)
(define VM-GREETING (read-bitmap (build-path logo-dir (format "VM_~a.png" VM-GREETING-SIZE))))
(void(new (class canvas%
            (super-new)
            (define/override (on-event e)
              (when (send e button-changed?)
                (define t-p% (first MAIN-MENU-TITLES-RFN))
                (render-T-content (cdr t-p%)))))
     [parent T]
     [paint-callback (λ (c dc)
                       ;; sw - screen width
                       (define-values [sw sh] (send c get-client-size))
                       ;; scx - screen center-x
                       (define-values [scx scy] (values (/ sw 2) (/ sh 2)))
                       (define o (/ VM-GREETING-SIZE 2))
                       (send dc draw-bitmap VM-GREETING (- scx 256) (- scy 256)))]))

;;(send main-window resize 0 0)
(send main-window show true)