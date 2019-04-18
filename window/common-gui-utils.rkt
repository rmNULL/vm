#lang racket/gui
(provide (all-defined-out))

(define MIN_WIN_WIDTH  800)
(define MIN_WIN_HEIGHT 600)
(require racket/date)
(require "./enhanced-text-input.rkt")

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
(define WARN-COLOR (send the-color-database find-color "OrangeRed"))
(define DEF-BG-COLOR (send the-color-database find-color "white"))


(define choice-data%
  (class choice%
    (init (label #f))
    (init-field (datas '()) (choices '()))

    (define (check-args choices datas)
      (unless (= (length datas) (length choices))
        (raise "Length of datas and choices must be equal")))

    (check-args choices datas)
    (super-new [choices choices] [label label])
    
    (define/public (get-data n)
      (if (empty? datas) #f (list-ref datas n)))
    
    (inherit get-selection)
    (define/public (get-selection-data)
      (get-data (get-selection)))

    ;; TODO: test, write doc
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
      ;;; TODO: REFACTOR( use contracts )
      (let ([cols (length (get-column-labels))])
        (unless (andmap label-string? labels)
          (error (format "expected list of label strings, instead given ~a"
                         labels)))
        (unless (= cols (length labels))
          (error (format "labels should contain ~a label strings, instead given ~v"
                         cols labels))))
 
      (define row (get-number))
      (-append (first labels) data)
      (for ([label (in-list (rest labels))]
            [column (in-naturals 1)])
        (set-string row label column)))

    (define N (length (get-column-labels)))
    (define win-width (send this get-width))
    (define col-width (floor (* (/ 1 N) win-width)))
    (for ([col N])
      (send this set-column-width col col-width (round (* 1/2 col-width)) win-width))))


(define restricted-text-field%
  (class text-field%
    ;; inefficient . 
    (init pattern [callback #f])
    (inherit get-value set-value)
    (super-new [callback (λ (t e)
                           (define pat-match (regexp-match pattern (get-value)))
                           (define v (if pat-match (first pat-match) ""))
                           (set-value v)
                           (when (and pat-match callback) (callback t e)))])))


(define (string->date str)
  (let* ([t (string-trim str)]
         [m (regexp-match #px"^([0-9]+)/([0-9]+)/([0-9]{4})$" t)])
    (and m
         (let ((day (string->number (list-ref m 1)))
               (month (string->number (list-ref m 2)))
               (year (string->number (list-ref m 3))))
           (with-handlers (((lambda (e) #t) (lambda (e) #f)))
             (find-seconds 0 0 0 day month year))))))

(define (date->string seconds)
  (define date (seconds->date seconds))
  (string-append
   (~a (date-day date) #:width 2 #:left-pad-string "0" #:align 'right)
   "/"
   (~a (date-month date) #:width 2 #:left-pad-string "0" #:align 'right)
   "/"
   (~a (date-year date) #:width 4 #:left-pad-string "0" #:align 'right)))

(define seconds? number?)
(define date-input-field%
  (validating-mixin string->date date->string #;text-field% (cue-mixin "DD/MM/YYYY" text-field%)))


(define item-input-field%
  (validating-mixin identity identity text-field%))

;; TODO: COMMON LABELS FOR ALL UI ELEMENTS
;; (define LABELS #hash())