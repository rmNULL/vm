#lang racket/gui
;; Copyright (c) 2018 Alex Harsanyi

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

(require racket/date)
(provide validating-mixin cue-mixin)

(define (validating-mixin string->data data->string base-class)
  (unless (subclass? base-class text-field%)
    (error "validating-mixin: parent is not derived from text-field%"))
  (class base-class
    (init-field [allow-empty? #f])
    (super-new)

    (define good-bg (send this get-field-background))
    (define bad-bg (make-object color% 255 120 124)) ; red
    
    (define (valid-value? data)
      (let ([t (string-trim data)])
        (or (and allow-empty? (= (string-length t) 0)) (string->data t))))
    
    (define (validate)
      (let ([valid? (valid-value? (send this get-value))])
        (send this set-field-background (if valid? good-bg bad-bg))))

    
    (define/override (on-subwindow-char receiver event)
      (begin0 (super on-subwindow-char receiver event)
        (validate)))

    (define/override (set-value v)
      (super set-value (if (string? v) v (data->string v)))
      (validate))

    (define/public (get-converted-value)
      (let ([v (string-trim (send this get-value))])
        (and (valid-value? v)
             (if (= (string-length v) 0) 'empty (string->data v)))))

    (validate)))

(define cue-text-style
  (let ((grey-text (new style-delta%)))
    (send grey-text set-delta-foreground "gray")
    grey-text))

(define normal-text-style
  (let ((black-text (new style-delta%)))
    (send black-text set-delta-foreground "black")
    black-text))

(define (text-empty? a-text)
  (define snip (send a-text find-first-snip))
  (or (not snip) (= 0 (send snip get-count))))

(define (cue-mixin default-cue base-class)
  (unless (subclass? base-class text-field%)
    (error "cue-mixin: parent is not derived from text-field%"))
  (class base-class
    (init-field [cue default-cue] [callback #f])
    (super-new [callback (lambda (c e) (on-callback c e))])

    (define showing-cue? #f)
    (define editor (send this get-editor))

    (define (clear-cue)
      (when showing-cue?
        (send* editor
          (erase)
          (change-style normal-text-style 'start 'end #f))
        (set! showing-cue? #f)))

    (define (maybe-insert-cue)
      (unless (or showing-cue? (not (text-empty? editor)))
        ;; NOTE: setting showing-cue? after inserting cue is troublesome when a
        ;;  child class inherits this class and passes it a callback.
        ;;  There is a possibility that the child's callback is called even before the child
        ;;  is fully allocated.
        (set! showing-cue? #t)
        (send* editor
          ;; NOTE; change-style will change *selected* text between start and
          ;; end, and make it sticky, so text inserted after 'end will also
          ;; have the same style.  It is simpler to start with an empty text,
          ;; apply the style and than insert the cue, otherwise we would have
          ;; to select the cue, apply the style and un-select it.
          (change-style cue-text-style 'start 'end #f)
          (insert cue)
          (move-position 'home))))

    (define/override (on-subwindow-char receiver event)
      (clear-cue)
      (begin0 (super on-subwindow-char receiver event)
        (queue-callback (lambda () (maybe-insert-cue)))))

    (define (on-callback control event)
      (when (and callback (not showing-cue?))
        (callback control event)))

    (define/override (set-value v)
      (clear-cue)
      (super set-value v)
      (maybe-insert-cue))

    (define/override (get-value)
      (if showing-cue? "" (super get-value)))

    (maybe-insert-cue)))