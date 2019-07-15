#lang racket/gui
;; open-day daybook-add current-balance day-opened?
(require
  "../daybook-api.rkt"
  "./common-gui-utils.rkt")
(provide draw-daybook)


(define PAT-INT "[0-9]+")
(define RX-FLOAT (pregexp (string-append PAT-INT
                                         "(?:" "(?:[.]" "[0-9]{1,2}" ")"
                                         "|" "[.]?"
                                         ")?")))


(define (prompt-opening-balance)  
   ;;(define dialog #f)
   (define input-field #f)
   (define label "Opening Balance")
   (dialog-cb-prompt
    (λ (dialog)
      ;;(set! dialog d)
      (set! input-field
            (new restricted-text-field%
                 [parent dialog] [label label] [pattern RX-FLOAT] [style '(single vertical-label)])))
    label
    (λ (open?)
      (define amount
        (and open? input-field (send input-field get-value)))
      (when (and amount (non-empty-string? amount))
        (open-day (string->number amount))))
    #:true-label "CONFIRM"))

(define (draw-daybook parent)
  (unless (day-opened?)
    (prompt-opening-balance))
  
  
  
  )