(include "class.scm")
(include "scm-lib-macro.scm")
(load "scm-lib.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; object system version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class colorfull () (slot: color))

(define-class point () (slot: x) (slot: y))
(define-class circle (point) (slot: radius))

(define-class object (point) (slot: velocity))
(define-class apple (colorfull object))
(define-class ant   (object))

(define-generic (shape obj))
(define-method  (shape (obj apple))
  (make-circle (object-x obj) (object-y obj) 4))
(define-method  (shape (obj ant))
  (make-point (object-x obj) (object-y obj)))

(define-generic (collision? obj1 obj2))
(define-method  (collision? (obj1 object) (obj2 object))
  (collision? (shape obj1) (shape obj2)))

(define (cart-distance p1 p2)
  (let ((x1 (point-x p1))
        (y1 (point-y p1))
        (x2 (point-x p2))
        (y2 (point-y p2)))
    (sqrt (+ (expt (- x2 x1) 2) (+ (expt (- y2 y1) 2))))))
(define-method (collision? (obj1 circle) (obj2 circle))
  (<= (cart-distance obj1 obj2)
      (+ (circle-radius obj1) (circle-radius obj2))))

(define-method (collision? (obj1 point) (obj2 point))
  (and (= (point-x obj1) (point-x obj2))
       (= (point-y obj1) (point-y obj2))))

(define-method (collision? (obj1 point) (obj2 circle))
  (<= (cart-distance obj1 obj2)
      (circle-radius obj2)))
(define-method (collision? (obj1 circle) (obj2 point))
  (collision? obj2 obj1))



(define-generic (animate obj))
(define-method (animate (obj object))
  (update! obj object x
           (lambda (old-x) (+ old-x (point-x (object-velocity obj)))))
  (update! obj object y
           (lambda (old-y) (+ old-y (point-y (object-velocity obj)))))
  (if (exists (lambda (other-obj) (collision? obj other-obj))
              (all-objects))
      (number-of-collisions (+ (number-of-collisions) 1)))
  ((dyn-behaviour)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-type version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type dt-point x y)
(define-type dt-circle x y radius)
(define-type dt-apple color x y velocity)
(define-type dt-ant   x y velocity)

(define (dt-shape obj)
  (cond ((dt-apple? obj)
         (make-dt-circle (dt-apple-x obj) (dt-apple-y obj) 4))
        ((dt-ant? obj)
         (make-dt-point (dt-ant-x obj) (dt-ant-y obj)))))

(define (dt-cart-distance p1 p2)
  (define (pointify p) (if (dt-circle? p)
                           (make-dt-point (dt-circle-x p) (dt-circle-y p))
                           p))
  (let* ((p1 (pointify p1))
         (p2 (pointify p2))
         (x1 (dt-point-x p1))
         (y1 (dt-point-y p1))
         (x2 (dt-point-x p2))
         (y2 (dt-point-y p2)))
    (sqrt (+ (expt (- x2 x1) 2) (+ (expt (- y2 y1) 2))))))

(define (dt-collision? obj1 obj2)
  (cond ((or (and (dt-apple? obj1) (dt-apple? obj2))
             (and (dt-apple? obj1) (dt-ant? obj2))
             (and (dt-ant? obj1)   (dt-apple? obj2))
             (and (dt-ant? obj1)   (dt-ant? obj2)))
         (dt-collision? (dt-shape obj1) (dt-shape obj2)))
        ((and (dt-circle? obj1) (dt-circle? obj2))
         (<= (dt-cart-distance obj1 obj2)
             (+ (dt-circle-radius obj1) (dt-circle-radius obj2))))
        ((and (dt-point? obj1) (dt-point? obj2))
         (and (= (dt-point-x obj1) (dt-point-x obj2))
              (= (dt-point-y obj1) (dt-point-y obj2))))
        ((and (dt-point? obj1) (dt-circle? obj2))
         (<= (dt-cart-distance obj1 obj2)
             (dt-circle-radius obj2)))
        ((and (dt-circle? obj1) (dt-point? obj2))
         (dt-collision? obj2 obj1))))

(define (dt-animate obj)
  (cond ((dt-apple? obj) (dt-animate-apple obj))
        ((dt-ant? obj)   (dt-animate-ant obj))))

(define (dt-animate-apple obj)
  (dt-apple-x-set! obj (+ (dt-apple-x obj)
                           (dt-point-x (dt-apple-velocity obj))))
  (dt-apple-y-set! obj (+ (dt-apple-y obj)
                           (dt-point-y (dt-apple-velocity obj))))
  (if (exists (lambda (other-obj) (dt-collision? obj other-obj))
              (all-objects))
      (number-of-collisions (+ (number-of-collisions) 1)))
  ((dyn-behaviour)))
(define (dt-animate-ant obj)
  (dt-ant-x-set! obj (+ (dt-ant-x obj)
                           (dt-point-x (dt-ant-velocity obj))))
  (dt-ant-y-set! obj (+ (dt-ant-y obj)
                           (dt-point-y (dt-ant-velocity obj))))
  (if (exists (lambda (other-obj) (dt-collision? obj other-obj))
              (all-objects))
      (number-of-collisions (+ (number-of-collisions) 1)))
  ((dyn-behaviour)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-instances nb apple-creator ant-creator point-creator)
  (define limit (* 2 nb))
  (define max-velo 5)
  (define (gen-inst)
    (let ((prob (random-real)))
      (cond ((< prob 0.5)
             (apple-creator 'green
                            (random-integer limit)
                            (random-integer limit)
                            (point-creator (random-integer max-velo)
                                           (random-integer max-velo))))
            (else
             (ant-creator  (random-integer limit)
                           (random-integer limit)
                           (point-creator (random-integer max-velo)
                                          (random-integer max-velo)))))))
  (let loop ((i 0) (instances '()))
    (if (< i nb)
        (loop (+ i 1) (cons (gen-inst) instances))
        instances)))





(define number-of-collisions (make-parameter #f))
(define all-objects          (make-parameter #f))
(define dyn-behaviour        (make-parameter #f))

(define (simple-sim-oo-main)
  (parameterize ((dyn-behaviour (lambda () 'nothing))
                 (number-of-collisions 0)
                 (all-objects (generate-instances
                               20 make-apple make-ant make-point)))
   (for iteration 0 (< iteration 5)
        (for-each (lambda (obj) (animate obj))
                  (all-objects)))))
(define (simple-sim-dt-main)
  (parameterize ((dyn-behaviour (lambda () 'nothing))
                 (number-of-collisions 0)
                 (all-objects (generate-instances
                               20 make-dt-apple make-dt-ant make-dt-point)))
   (for iteration 0 (< iteration 5)
        (for-each (lambda (obj) (dt-animate obj))
                  (all-objects)))))

;; The complex sim uses a thread-sleep to simulate some complex
;; behaviour that do not use much of the object system
(define (complex-sim-oo-main)
  (parameterize ((dyn-behaviour (lambda () (thread-sleep! 0.005)))
                 (number-of-collisions 0)
                 (all-objects (generate-instances
                               20 make-apple make-ant make-point)))
   (for iteration 0 (< iteration 5)
        (for-each (lambda (obj) (animate obj))
                  (all-objects)))))
(define (complex-sim-dt-main)
  (parameterize ((dyn-behaviour (lambda () (thread-sleep! 0.005)))
                 (number-of-collisions 0)
                 (all-objects (generate-instances
                               20 make-dt-apple make-dt-ant make-dt-point)))
   (for iteration 0 (< iteration 5)
        (for-each (lambda (obj) (dt-animate obj))
                  (all-objects)))))