#lang racket/gui

(require racket/date)

;;; Vectors
(struct vec2 (v1 v2))

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

(define (mil-hour h)
  (cond
    [(zero? h) 12]
    [(> h 13) (- h 12)]
    [else h]))

(define (min/hour min) (/ min 60))

(define (min->rad min) (* min 1min-rad))

(define (hour->rad hour min)
  (if (>= min 60) hour
      (* (+ hour (min/hour min)) 1hour-rad)))

;;; Drawing
(define black-brush (new brush% [color "black"]))
(define white-pen (new pen% [color "white"] [width 2] [style 'solid]))

(define (draw-clock-outline dc win-h)
  (send dc set-brush black-brush)
  (send dc set-pen white-pen)
  (send dc draw-ellipse 0 0 win-h win-h))

(define (draw-line dc vec x0)
  (send dc draw-line x0 x0
        (+ x0 (vec2-v1 vec))
        (+ x0 (vec2-v2 vec))))

(define (draw-clock-hand dc mag a win-h)
  (let ([adjusted-angle (+ zero-angle a)])
    (draw-line dc (vec2 (* mag (cos adjusted-angle))
                        (* mag (sin adjusted-angle)))
               (/ win-h 2))))

(define (draw-minute-hand dc a win-h)
  ;; (70/100)(h/2)
  (draw-clock-hand dc (* 80 (/ win-h 200)) a win-h))

(define (draw-hour-hand dc a win-h)
  (draw-clock-hand dc (* 40 (/ win-h 200)) a win-h))

(define mark-angles
  (stream->list (in-range 0 (* 2 π) (/ π 6))))

(define (make-mark-at-angle angle win-h)
  (let* ([magnitude 20]
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
    (list x-start y-start x-end y-end)))

(define (draw-clock-background dc win-h)
  (send dc set-smoothing 'smoothed)
  (draw-clock-outline dc win-h)
  (for-each (λ (angle)
              (match (make-mark-at-angle angle win-h)
                [(list x0 y0 x y) (send dc draw-line x0 y0 x y)]))
            mark-angles))

(define (draw-clock dc win-h min-angle hour-angle)
  (send dc set-background "black")
  (send dc clear)
  (draw-clock-background dc win-h)
  (draw-minute-hand dc min-angle win-h)
  (draw-hour-hand dc hour-angle win-h))

;; Canvas
;; Ugly OOP UI code >:|
(define clock-canvas%
  (class canvas%
    (init draw)
    (init handle-kb-event)

    (define _draw draw)
    (define _handle-kb-event handle-kb-event)
    (define min-angle 0)
    (define hour-angle 0)
    
    (define/public (modify-hand-angles f)
      (let ([new (f hour-angle min-angle)])
        (set!-values (hour-angle min-angle)
                     (values (car new)
                             (cdr new)))
        (send this on-paint)))

    (super-new)
    
    (define/override (on-paint)
      (_draw min-angle hour-angle
            (send this get-height)
            (send this get-dc)))
    
    (define/override (on-char key-event)
      (_handle-kb-event (send key-event get-key-code)))))

;; Main window
(define frame
  (new frame%
       [label "RacketClock"]))

(define canvas
  (new clock-canvas%
       [parent frame]
       
       [draw
        (λ (min-a hour-a win-h dc)
          (send dc set-alignment-scale 1.0)
          (draw-clock dc win-h min-a hour-a))]
       
       [handle-kb-event
        (λ (key)
          (cond [(equal? key 'escape) (send frame show #f)]))]))


;; Timers

(define (update-time)
  (let* ([now (current-date)]
         [min (date-minute now)]
         [hour (mil-hour (date-hour now))])
    (send canvas modify-hand-angles
          (λ (_h _m) (cons (hour->rad hour min)
                           (min->rad min))))))
(define time-update-timer
  (new timer%
       [notify-callback update-time]
       [interval #f]))


;;
(update-time)
(send time-update-timer start (* 60 1second))

(send frame show #t)
