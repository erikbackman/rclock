#lang racket/gui

(require racket/date)

;;; Linear Algebra
(struct vec2 (v1 v2))
(struct m22 (m11 m12 m21 m22))

(define (vec-map f vec)
  (vec2 (f (vec2-v1 vec))
        (f (vec2-v2 vec))))

(define (mag vec)
  (sqrt (+ (sqr (vec2-v1 vec))
           (sqr (vec2-v2 vec)))))

(define (*vec n vec)
  (vec2 (* n (vec2-v1 vec))
        (* n (vec2-v2 vec))))

(define (/vec n vec)
  (vec2 (/ (vec2-v1 vec) n)
        (/ (vec2-v2 vec) n)))

(define (normalize vec)
  (*vec (/ 1 (mag vec)) vec))

(define (-vec v1 v2)
  (vec2 (- (vec2-v1 v1) (vec2-v1 v2))
        (- (vec2-v2 v1) (vec2-v2 v2))))

(define (+vec v1 v2)
  (vec2 (+ (vec2-v1 v1) (vec2-v1 v2))
        (+ (vec2-v2 v1) (vec2-v2 v2))))

;;; Time/angle
(define π pi)
(define 1second 1000)
(define 1rad 0.0174533)
(define zero-angle (- (/ π 2))) ; as in twelve-o-clock
(define 1min-rad (/ π 30))
(define 1hour-rad (/ π 6))

(define (min/hour min) (/ min 60))
(define (min->rad min) (* min 1min-rad))

(define (hour->rad hour min)
  (if (>= min 60) hour
      (* (+ hour (min/hour min)) 1hour-rad)))

;;; App
(define win-h 400)
(define win-w 400)
(define origin-x (/ win-w 2))
(define origin-y (/ win-h 2))

(define black-brush (new brush% [color "black"]))
(define white-pen (new pen% [color "white"] [width 2] [style 'solid]))

(define (draw-clock-outline dc)
  (send dc set-brush black-brush)
  (send dc set-pen white-pen)
  (send dc draw-ellipse 0 0 win-h win-w))

(define (draw-line dc vec)
  (send dc draw-line origin-x origin-y
        (+ origin-x (vec2-v1 vec))
        (+ origin-y (vec2-v2 vec))))

(define (draw-clock-hand dc mag a)
  (let ([adjusted-angle (+ zero-angle a)])
    (draw-line dc (vec2 (* mag (cos adjusted-angle))
                        (* mag (sin adjusted-angle))))))

(define (draw-minute-hand dc a)
  ;; (70/100)(h/2)
  (draw-clock-hand dc (* 70 (/ win-h 200)) a))

(define (draw-hour-hand dc a)
  (draw-clock-hand dc (* 40 (/ win-h 200)) a))

(define mark-angles
  (stream->list (in-range 0 (* 2 π) (/ π 6))))

(define (draw-mark-at-angle dc angle)
  (let* ([magnitude 10]
         [radius (/ win-h 2)]
         [offset (/ win-h 2)]
         [origin (vec2 offset offset)]
         [x-start (+ offset (* radius (cos angle)))]
         [y-start (+ offset (* radius (sin angle)))]
         [dir (*vec magnitude
                    (normalize
                     (-vec origin (vec2 x-start y-start))))]
         [x-end (+ x-start (vec2-v1 dir))]
         [y-end (+ y-start (vec2-v2 dir))])
    (send dc draw-line x-start y-start x-end y-end)))

(define (draw-clock-background dc)
  (send dc set-smoothing 'smoothed)
  (draw-clock-outline dc)
  (for-each (λ (angle)
              (draw-mark-at-angle dc angle))
            mark-angles))

;; Canvas
;; Ugly OOP UI code >:|
(define clock-canvas%
  (class canvas%
    (define min-angle 0)
    (define hour-angle 0)

    (define (redraw)
      (let ([dc (send this get-dc)])
        (send dc set-background "black")
        (send dc clear)
        (draw-clock-background dc)
        (draw-minute-hand dc min-angle)
        (draw-hour-hand dc hour-angle)))

    (define (close)
      (send
       (send this get-parent) show #f)
      (exit))
    
    (define/public (modify-hand-angles f)
      (let ([new (f hour-angle min-angle)])
        (set!-values (hour-angle min-angle) (values (car new) (cdr new)))
        (redraw)))

    (super-new)
    
    (define/override (on-paint)
      (let [(dc (send this get-dc))]
        (send dc set-alignment-scale 1.0)
        (printf "redraw\n")
        (redraw)
        (super on-paint)))
    (define/override (on-char key-event)
      (let ([keycode (send key-event get-key-code)])
        (cond
          [(equal? keycode 'escape) (close)])))))

(define frame
  (new frame%
       [label "RacketClock"]
       [width win-w]
       [height win-h]))

(define (mil-hour h)
  (cond
    [(zero? h) 12]
    [(> h 13) (- h 12)]
    [else h]))

(define (update-time)
  (let* ([now (current-date)]
         [min (date-minute now)]
         [hour (mil-hour (date-hour now))])
    (send canvas modify-hand-angles
          (λ (_h _m) (cons (hour->rad hour min)
                           (min->rad min))))))

(define canvas (new clock-canvas% [parent frame]))

(define time-update-timer
  (new timer%
       [notify-callback update-time]
       [interval #f]))

(update-time)
(send time-update-timer start (* 60 1second))

(send frame show #t)
