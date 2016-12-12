#lang racket

(provide line-point segment-middle segment-length
	 normal-point bezier-point
	 ->line-func ->ellipse-func)

(define (line-point x0 y0 x1 y1 p)
  (values (+ x0 (* p (- x1 x0)))
	  (+ y0 (* p (- y1 y0)))))

(define (segment-middle x0 y0 x1 y1)
  (line-point x0 y0 x1 y1 1/2))

(define (segment-length x0 y0 x1 y1)
  (let ([xd (- x1 x0)]
	[yd (- y1 y0)])
    (sqrt (+ (* xd xd) (* yd yd)))))

(define (normal-point x0 y0 x1 y1 distance)
  (let-values ([(xm ym) (segment-middle x0 y0 x1 y1)])
    (let* ([xd (- x1 x0)]
	   [yd (- y1 y0)]
	   [sd (segment-length x0 y0 x1 y1)]
	   [dx (* distance (/ yd sd))]
	   [dy (* distance (/ xd sd))])
      (values (- xm dx) (+ ym dy)))))

(define (bezier-point x0 y0 x1 y1 x2 y2 x3 y3 t)
  (let* ([ct (- 1 t)]
         [c0 (* ct ct ct)]
         [c1 (* 3 ct ct t)]
         [c2 (* 3 ct t t)]
         [c3 (* t t t)])
    (values (+ (* c0 x0) (* c1 x1) (* c2 x2) (* c3 x3))
            (+ (* c0 y0) (* c1 y1) (* c2 y2) (* c3 y3)))))

(define ((->line-func x0 y0 a) x)
  (+ (* a (- x x0)) y0))

(define ((->ellipse-func cx cy w h) t)
  (values (+ cx (* 1/2 w (cos (- t))))
	  (+ cy (* 1/2 h (sin (- t))))))
