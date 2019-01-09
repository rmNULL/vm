#lang racket/gui
(provide (all-defined-out))

(define MIN_WIN_WIDTH  500)
(define MIN_WIN_HEIGHT 300)
(require racket/date)

(define DAYS '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define MONTHS '(#f "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(define (date-today)
  (let* ([now (current-date)]
         [mon (list-ref MONTHS (date-month now))]
         [year (date-year now)]
         [day (date-day now)]
         [wday (list-ref DAYS (date-week-day now))])
    (format "~a ~a, ~a ~a" wday day mon year)))

(define SAFE-COLOR (send the-color-database find-color "Medium Turquoise"))
(define WARN-COLOR (send the-color-database find-color "orange"))
(define DEF-BG-COLOR (send the-color-database find-color "white"))


(define choice-data%
  (class choice%
    (init (label #f))
    (init-field (datas '()) (choices '()))

    (define (check-args choices datas)
      (unless (= (length datas) (length choices))
        (raise "Length of datas and choices must be equal")))

    (check-args choices datas)
    
    (define/public (get-data n) (list-ref datas n))
    (super-new [choices choices] [label label])
 
    (define/public (set cs ds)
      (check-args cs ds)
      (set! choices cs)
      (set! datas ds))))


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


;; provides add row by #of cols in the list-box
(define extended-list-box%
  (class list-box%
    (super-new)
    (inherit get-number get-column-labels (-append append) set-string)

    (define/public (selected-row) (send this get-selection))
    (define/public (selected-row-data)
      (define selection (selected-row))
      (and selection (send this get-data selection)))
    
    (define/public (append-row labels (data #f))
      ;;; REFACTOR ... .. .
      (let ([cols (length (get-column-labels))])
        (unless (andmap label-string? labels)
          (raise (format "expected list of label strings, instead given ~a"
                         labels)))
        (unless (= cols (length labels))
          (raise (format "labels should contain ~a label strings, instead given ~v"
                         cols labels))))
 
      (define row (get-number))
      (-append (first labels) data)
      (for ([label (in-list (rest labels))]
            [column (in-naturals 1)])
        (set-string row label column)))))


(define restricted-text-field%
  (class text-field%
    ;; inefficient . 
    (init pattern [callback #f])
    (inherit get-value set-value)
    (super-new [callback (λ (t e)
                           (define pat-match (regexp-match pattern (get-value)))
                           (define v (if pat-match (first pat-match) ""))
                           (set-value v)
                           (when callback (callback t e)))])))