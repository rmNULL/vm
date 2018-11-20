#lang racket/gui
(provide (all-defined-out))

(define MIN_WIN_WIDTH  500)
(define MIN_WIN_HEIGHT 500)

(define (dialog-prompt window-name msg continue)
  (define dialog (new dialog% (label window-name)))
 
  (new message% [parent dialog] [label msg])
  (define panel
    (new horizontal-panel% [parent dialog] [alignment '(center center)]))

  ;; if you are adding code(to callback), its time for refactor !!
  (new button% [parent panel] [label "Cancel"] [callback (λ (b e)
                                                           (send dialog show false)
                                                           (continue #F))])
  (new button% [parent panel] [label "Ok"] [callback (λ (b e)
                                                       (send dialog show false)
                                                       (continue #T))])
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))
  (send dialog show true))