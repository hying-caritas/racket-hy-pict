#lang racket

(require racket/draw)
(require pict)

(require hy/sequence)
(require "geometry.rkt")

(provide parse-pict-pos
	 ->find-cord ->find-line-point ->find-normal-point
	 ->find-hoffset ->find-voffset ->find-rotate-offset
	 ->find-by-segments
	 ->find-ellipse-point ->find-rectangle-point
	 pt-superimpose place-pict place-picts
	 pict-box ellipse-box
	 filled-rect-box
	 blank-for
         call-with-pen with-pen call-with-pen-color with-pen-color
         call-with-brush with-brush
         path-pict make-path ->find-path-point find-path-middle
         add-line add-arrow-line add-arrows-line
         brace
         save-pict)

(provide (all-from-out "geometry.rkt"))

(define (parse-pict-pos pict x-or-obj y-or-find)
  (if (procedure? y-or-find)
      (y-or-find pict x-or-obj)
      (values x-or-obj y-or-find)))

(define ((->find-cord x y) pict obj)
  (values x y))

(define ((->find-line-point gx0 gy0 gx1 gy1 p) pict obj)
  (let-values ([(x0 y0) (parse-pict-pos pict gx0 gy0)]
	       [(x1 y1) (parse-pict-pos pict gx1 gy1)])
    (line-point x0 y0 x1 y1 p)))

(define ((->find-normal-point gx0 gy0 gx1 gy1 distance) pict obj)
  (let-values ([(x0 y0) (parse-pict-pos pict gx0 gy0)]
	       [(x1 y1) (parse-pict-pos pict gx1 gy1)])
    (normal-point x0 y0 x1 y1 distance)))

(define ((->find-hoffset gx0 gy0 gx1 gy1 [p 1]) pict obj)
  (let-values ([(x0 y0) (parse-pict-pos pict gx0 gy0)]
	       [(x1 y1) (parse-pict-pos pict gx1 gy1)])
    (values (* p (- x1 x0)) 0)))

(define ((->find-voffset gx0 gy0 gx1 gy1 [p 1]) pict obj)
  (let-values ([(x0 y0) (parse-pict-pos pict gx0 gy0)]
	       [(x1 y1) (parse-pict-pos pict gx1 gy1)])
    (values 0 (* p (- y1 y0)))))

(define ((->find-rotate-offset gdx gdy [p 1]) pict obj)
  (let-values ([(dx dy) (parse-pict-pos pict gdx gdy)])
    (values (* p dy) (* p dx))))

(define ((->find-by-segments gxo gyo . segments-spec) pict obj)
  (define (parse-segment-spec segment-spec)
    (let ([len (length segment-spec)])
      (case len
	[(2 3)
	 (let-values ([(dx dy) (parse-pict-pos pict
					       (first segment-spec)
					       (second segment-spec))])
	   (values dx dy
		   (if (= len 3)
		       (third segment-spec)
		       1)))
	 ]
	[(4 5)
	 (let-values ([(x0 y0) (parse-pict-pos pict
					       (first segment-spec)
					       (second segment-spec))]
		      [(x1 y1) (parse-pict-pos pict
					       (third segment-spec)
					       (fourth segment-spec))])
	   (values (- x1 x0) (- y1 y0)
		   (if (= len 5)
		       (fifth segment-spec)
		       1)))])))
  (let-values ([(xo yo) (parse-pict-pos pict gxo gyo)])
    (let loop ([x xo]
	       [y yo]
	       [segments-spec segments-spec])
      (if (empty? segments-spec)
	  (values x y)
	  (let-values ([(dx dy p) (parse-segment-spec (first segments-spec))])
	    (loop (+ x (* p dx)) (+ y (* p dy)) (cdr segments-spec)))))))

(define ((->find-ellipse-point angle) pict pellipse)
  (let-values ([(cx cy) (cc-find pict pellipse)])
    ((->ellipse-func cx cy
		     (pict-width pellipse)
		     (pict-height pellipse))
     angle)))

(define (normalize-distance distance total)
  (let* ([q (exact-floor (/ distance total))]
	 [r (- distance (* q total))])
    (if (< r 0) (+ r total) r)))

(define ((->find-rectangle-point distance) pict rect)
  (let* ([nd (normalize-distance distance 4)]
	 [edge (exact-floor nd)]
	 [edge-distance (- nd edge)])
    (let-values ([(sfind efind)
		  (case edge
		    [(0) (values lt-find lb-find)]
		    [(1) (values lb-find rb-find)]
		    [(2) (values rb-find rt-find)]
		    [(3) (values rt-find lt-find)])])
      ((->find-line-point rect sfind rect efind edge-distance)
       pict rect))))

;; (pt-superimpose pict gx gy ...)
(define (pt-superimpose . pict-gx-gy-list)
  (define (move-pict pict left top)
    (if (and (zero? left) (zero? top))
        pict
        (hb-append (blank left (+ top (pict-height pict))) pict)))
  (let* ([pxys
          (for/list ([pgxy (in-slice 3 pict-gx-gy-list)])
            (let-values ([(x y) (apply parse-pict-pos pgxy)])
              (list (car pgxy) x y)))]
         [xs (map second pxys)]
         [ys (map third pxys)]
         [xb (apply max xs)]
         [yb (apply max ys)]
         [ls (map (λ (x) (- xb x)) xs)]
         [ts (map (λ (y) (- yb y)) ys)]
         [picts
          (for/list ([pxy pxys]
                     [l ls]
                     [t ts])
            (move-pict (car pxy) l t))])
    (apply lt-superimpose picts)))

(define (place-pict pict obj find pgx pgy)
  (pt-superimpose obj obj find pict pgx pgy))

(define (place-picts pict . obj-find-pgx-pgy-list)
  (for/fold ([pict pict])
            ([obj-find-pgx-pgy (in-slice 4 obj-find-pgx-pgy-list)])
    (apply place-pict pict obj-find-pgx-pgy)))

(define (pict-box content #:margin [margin 4]
		  #:width [width #f] #:height [height #f]
		  #:shape [shape rectangle]
		  #:linestyle [style 'solid])
  (let* ((in-width (pict-width content))
	 (in-height (pict-height content))
	 (dmargin (* 2 margin))
	 (out-width (if width
			width
			(+ in-width dmargin)))
	 (out-height (if height
			 height
			 (+ in-height dmargin))))
    (cc-superimpose content
		    (linestyle style (shape out-width out-height)))))

(define (ellipse-box content #:margin [margin 8]
		     #:width [width #f] #:height [height #f])
  (pict-box content #:margin margin
	    #:width width #:height height #:shape ellipse))

(define filled-rect-box
  (case-lambda
    [(w h color) (filled-rectangle w h #:color color)]
    [(bp color) (filled-rectangle (pict-width bp)
				  (pict-height bp)
				  #:color color)]))

(define (blank-for pict)
  (blank (pict-width pict) (pict-height pict)))

(define (call-with-pen dc pen func)
  (if pen
      (let ([old-pen (send dc get-pen)])
        (dynamic-wind
          (λ () (send dc set-pen pen))
          func
          (λ () (send dc set-pen old-pen))))
      (func)))

(define-syntax-rule (with-pen dc pen body ...)
  (call-with-pen dc pen (λ () body ...)))

(define (call-with-pen-color dc color func)
  (if color
      (let ([old-pen (send dc get-pen)])
        (dynamic-wind
          (λ ()
            (send dc set-pen color
                  (send old-pen get-width)
                  (send old-pen get-style)))
          func
          (λ ()
            (send dc set-pen old-pen))))
      (func)))

(define-syntax-rule (with-pen-color dc color body ...)
  (call-with-pen-color dc color (λ () body ...)))

(define (call-with-brush dc brush func)
  (if brush
      (let ([old-brush (send dc get-brush)])
        (dynamic-wind
          (λ () (send dc set-brush brush))
          func
          (λ () (send dc set-brush old-brush))))
      (func)))

(define-syntax-rule (with-brush dc brush body ...)
  (call-with-brush dc brush (λ () body ...)))

(define (parse-node-spec node-spec)
  (match node-spec
    [(list x y) (values x y #f #f #f #f)]
    [(list x y a) (values x y a a #f #f)]
    [(list x y ia oa) (values x y ia oa #f #f)]
    [(list x y ia oa p) (values x y ia oa p p)]
    [(list x y ia oa ip op) (values x y ia oa ip op)]))

(define (parse-path-spec path-spec)
  (for/list ([(x0 y0 ia0 oa0 ip0 op0 x1 y1 ia1 oa1 ip1 op1)
              (in-consecutive (sequence-map parse-node-spec
                                            (in-stream path-spec)))])
    (if (or oa0 ia1)
        (let* ([oa0 (or oa0
                        (atan (- y0 y1) (- x1 x0)))]
               [ia1 (or ia1
                        (atan (- y0 y1) (- x1 x0)))]
               [d (segment-length x0 y0 x1 y1)]
               [op0d (* (or op0 1/3) d)]
               [ip1d (* (or ip1 1/3) d)]
               [xm0 (+ x0 (* op0d (cos oa0)))]
               [ym0 (- y0 (* op0d (sin oa0)))]
               [xm1 (- x1 (* ip1d (cos ia1)))]
               [ym1 (+ y1 (* ip1d (sin ia1)))])
          `((,x0 ,y0) (,xm0 ,ym0) (,xm1 ,ym1) (,x1 ,y1)))
        `((,x0 ,y0) (,x1 ,y1)))))

(define (offset-path-desc path-desc xoff yoff)
  (for/list ([seg (in-list path-desc)])
    (for/list ([point (in-list seg)])
      (list (+ (first point) xoff) (+ (second point) yoff)))))

(define (pair-values point-pair)
  (apply values (apply append point-pair)))

(define (path-desc-first-pair path-desc)
  (pair-values (take (first path-desc) 2)))

(define (path-desc-last-pair path-desc)
  (pair-values
   (let ([seg (last path-desc)])
     (if (= (length seg) 2)
         seg
         (drop seg 2)))))

(struct path-pict pict (path-desc))

(define (draw-arrow dc x y angle size [solid? #t])
  (define (->point p)
    (let* ([x (car p)]
           [y (cadr p)]
           [d (sqrt (+ (* x x) (* y y)))]
           [a (if (and (zero? x) (zero? y))
                  0
                  (atan y x))])
      (make-object point%
                   (* d size 1/2 (cos (+ a angle)))
                   (* d size 1/2 (- (sin (+ a angle)))))))
  (send dc draw-polygon
        (map ->point
             `((0 0)
               (-2 -1)
               (-3/2 0)
               (-2 1)))
        x y))

(define (make-path path-spec
                   #:width [width #f]
                   #:height [height #f]
                   #:filled [filled #f]
                   #:pen [pen #f]
                   #:pen-color [pen-color #f]
                   #:pen-width [pen-width 0]
                   #:pen-style [pen-style #f]
                   #:pen-cap [pen-cap #f]
                   #:pen-join [pen-join #f]
                   #:brush [brush #f]
                   #:brush-color [brush-color #f]
                   #:brush-style [brush-style #f]
                   #:brush-spec [brush-spec #f]
                   #:start-arrow [start-arrow #f]
                   #:end-arrow [end-arrow #f]
                   #:arrow-size [arrow-size
                                 (if (or start-arrow end-arrow)
                                     10
                                     0)]
                   #:solid-arrow? [solid-arrow? #t])
  (let*-values
      ([(path-desc)
        (parse-path-spec path-spec)]
       [(build-dc-path)
        (lambda (path-desc)
          (define path (new dc-path%))
          (send/apply path move-to (caar path-desc))
          (for ([seg (in-list path-desc)])
            (if (= (length seg) 2)
                (send/apply path line-to (second seg))
                (send/apply path curve-to (flatten (cdr seg)))))
          path)]
       [(path) (build-dc-path path-desc)]
       [(l t w h) (send path get-bounding-box)]
       [(l t w h) (let* ([hsize (/ arrow-size 2)]
                         [dlt hsize]
                         [dwh (+ (if start-arrow hsize 0)
                                 (if end-arrow hsize 0)
                                 1
                                 (* 2 pen-width))])
                    (values (- l dlt) (- t dlt)
                            (+ w dwh) (+ h dwh)))]
       [(width) (or width w)]
       [(height) (or height h)]
       [(path-desc)
        (offset-path-desc path-desc (- l) (- t))]
       [(path) (build-dc-path path-desc)]
       [(draw)
        (lambda (dc x y)
          (let* ([old-pen (send dc get-pen)]
                 [pen
                  (or pen
                      (send the-pen-list find-or-create-pen
                            (or pen-color (send old-pen get-color))
                            pen-width
                            (or pen-style (send old-pen get-style))
                            (or pen-cap (send old-pen get-cap))
                            (or pen-join (send old-pen get-join))))]
                 [old-brush (send dc get-brush)]
                 [brush
                  (or brush
                      (send the-brush-list find-or-create-brush
                            (or brush-color (send old-brush get-color))
                            (if filled
                                (or brush-style (send old-brush get-style))
                                'transparent)))])
            (with-pen dc pen
              (with-brush dc brush
                (send dc draw-path path x y)))
            (when (or start-arrow end-arrow)
              (let* ([pen (send the-pen-list find-or-create-pen
                                (send pen get-color)
                                (if solid-arrow? 0 (send pen get-width))
                                'solid)]
                     [brush (send the-brush-list find-or-create-brush
                                  (if solid-arrow?
                                      (send pen get-color)
                                      (send brush get-color))
                                  'solid)])
                (with-pen dc pen
                  (with-brush dc brush
                    (when start-arrow
                      (let*-values ([(x0 y0 x1 y1) (path-desc-first-pair path-desc)]
                                    [(a) (atan (- y0 y1) (- x1 x0))])
                        (draw-arrow dc (+ x x0) (+ y y0) (+ a pi) arrow-size)))
                    (when end-arrow
                      (let*-values ([(x0 y0 x1 y1) (path-desc-last-pair path-desc)]
                                    [(a) (atan (- y0 y1) (- x1 x0))])
                        (draw-arrow dc (+ x x1) (+ y y1) a arrow-size)))))))))])
    (path-pict `(prog ,draw ,height) width height 0 0 null #f #f path-desc)))

(define (find-path-point pict path t)
  (let*-values
      ([(path-desc) (path-pict-path-desc path)]
       [(n) (exact-floor t)]
       [(rt) (- t n)]
       [(n rt) (if (and (= n (length path-desc)) (= rt 0))
                   (values (sub1 n) 1)
                   (values n rt))]
       [(l t) (lt-find pict path)]
       [(seg) (list-ref path-desc n)]
       [(x y) (let ([params (append (flatten seg) (list rt))])
                (if (= (length seg) 4)
                    (apply bezier-point params)
                    (apply line-point params)))])
    (values (+ l x) (+ t y))))

(define ((->find-path-point t) pict path)
  (find-path-point pict path t))

(define (find-path-middle pict path)
  (let ([path-desc (path-pict-path-desc path)])
    (find-path-point pict path (* 0.5 (length path-desc)))))

(define (find-path-start pict path)
  (find-path-point pict path 0))

(define (find-path-end pict path)
  (let ([path-desc (path-pict-path-desc path)])
    (find-path-point pict path (length path-desc))))

(define (gpath-spec->path-spec pict gpath-spec)
  (for/list ([node-spec gpath-spec])
    (let-values ([(x y) (apply parse-pict-pos pict (take node-spec 2))])
      (list* x y (drop node-spec 2)))))

(define (add-line pict gpath-spec
                  #:start-arrow [start-arrow #f]
                  #:end-arrow [end-arrow #f]
                  #:arrow-size [arrow-size
                                (if (or start-arrow end-arrow)
                                    10
                                    0)]
                  #:line-width [line-width 0]
                  #:color [color #f]
                  #:style [style 'solid]
                  #:under? [under? #f])
  (let* ([path-spec (gpath-spec->path-spec pict gpath-spec)]
         [path (make-path path-spec
                          #:pen-color color
                          #:pen-width line-width
                          #:pen-style style
                          #:start-arrow start-arrow
                          #:end-arrow end-arrow
                          #:arrow-size arrow-size)]
         [origin (take (car path-spec) 2)]
         [find-origin (apply ->find-cord origin)]
         [npict
          (if under?
              (pt-superimpose pict #f find-origin
                              path path (->find-path-point 0))
              (pt-superimpose path path (->find-path-point 0)
                              pict #f find-origin))])
    (values npict path)))

(define (add-arrow-line arrow-size pict gpath-spec
                        #:start-arrow [start-arrow #f]
                        #:line-width [line-width 0]
                        #:color [color #f]
                        #:style [style #f]
                        #:under? [under? #f])
  (add-line pict gpath-spec
            #:start-arrow start-arrow
            #:end-arrow (not start-arrow)
            #:arrow-size arrow-size
            #:line-width line-width
            #:color color
            #:style style
            #:under? under?))

(define (add-arrows-line arrow-size pict gpath-spec
                         #:line-width [line-width 0]
                         #:color [color #f]
                         #:style [style #f]
                         #:under? [under? #f])
  (add-line pict gpath-spec
            #:start-arrow #t
            #:end-arrow #t
            #:arrow-size arrow-size
            #:line-width line-width
            #:color color
            #:style style
            #:under? under?))

(define (brace w [h (* 1/4 w)])
  (let ([rw (sub1 w)]
        [rh (sub1 h)])
    (make-path `((0 ,rh ,(* 1/2 pi))
                 (,(* 1/8 rw) ,(* 0.5 rh) 0 0 1/3 5/5)
                 (,(* 1/2 rw) 0 ,(* 2/5 pi) ,(* -2/5 pi))
                 (,(* 7/8 rw) ,(* 0.5 rh) 0 0 5/5 1/3)
                 (,rw ,rh ,(* -1/2 pi))))))

(define (save-pict pict file-name [kind 'png])
  (send (pict->bitmap pict) save-file file-name kind))
