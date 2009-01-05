(include "class.scm")
(load "scm-lib.scm")

(define-class point () (slot: x) (slot: y))
(define-class circle (point) (slot: radius))

(define-class colorfull () (slot: color))

(define-class object () (slot: x) (slot: y) (slot: velocity))
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
    (sqrt (+ (expt (- x2 x1)) (+ (expt (- y2 y1)))))))
(define-method (collision? (obj1 circle) (obj2 circle))
  (<= (cart-distance obj1 obj2)
      (+ (circle-radius obj1) (circle-radius obj2))))

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
              all-objects)
      (thread-sleep! 0.1)
      (thread-sleep! 0.05)))

(define (generate-instances nb)
  (define limit (* 2 nb))
  (define max-velo 5)
  (define (gen-inst)
    (let ((prob (random-real)))
      (cond ((< prob 0.5)
             (make-apple 'green
                         (random-integer limit)
                         (random-integer limit)
                         (make-point (random-integer max-velo)
                                     (random-integer max-velo))))
            (else
             (make-ant  (random-integer limit)
                         (random-integer limit)
                         (make-point (random-integer max-velo)
                                     (random-integer max-velo)))))))
  (let loop ((i 0) (instances '()))
    (if (< i nb)
        (loop (+ i 1) (cons (gen-inst) instances))
        instances)))

(define all-objects (generate-instances 50))

(define (main)
  (for iteration 0 (< iteration 1000)
       (for-each (lambda (obj) (animate obj))
                 all-objects)))

(time (main))