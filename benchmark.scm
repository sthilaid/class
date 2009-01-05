(include "scm-lib-macro.scm")
(include "class.scm")

(define-class A () (slot: a))
(define-class B (A) (slot: b))

(define-generic (f x))
(define-method (f (x B)) 'toto)
(define (g x) 'tutu)

(define-type C c)



(define upper-bound 1000000)
(define oo-delay #f)
(define deftype-delay #f)

(define (t) (time->seconds (current-time)))

(display "Benchmark suite for the object system")
(newline)(newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Slot access benchmark ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "**** Slot access benchmark **** ") (newline)
(let ((init-t (t))
      (obj (make-B 1 2)))
  (for i 0 (< i upper-bound)
       (+ (B-b obj) (B-b obj)))
  (let ((delta-t (- (t) init-t)))
    (set! oo-delay delta-t)
    #;(pp `(Delay of ,upper-bound object slot access: ,delta-t))))

(let ((init-t (t))
      (obj (make-C 1)))
  (for i 0 (< i upper-bound)
       (+ (C-c obj) (C-c obj)))
  (let ((delta-t (- (t) init-t)))
    (set! deftype-delay delta-t)
    #;(pp `(Delay of ,upper-bound define-type slot access: ,delta-t))))

(pp `(oo-system/define-type ratio: ,(/ oo-delay deftype-delay)))
(newline)


;;;;;;;;;;;;;;;;;;;;;; Generic function call benchmark ;;;;;;;;;;;;;;;;;;;;;;;

(display "**** Generic function call benchmark **** ") (newline)
(let ((init-t (t))
      (obj (make-B 1 2)))
  (for i 0 (< i upper-bound)
       (f obj))
  (let ((delta-t (- (t) init-t)))
    (set! oo-delay delta-t)
    #;(pp `(Delay of ,upper-bound gen fun calls: ,delta-t))))

(let ((init-t (t))
      (obj (make-C 1)))
  (for i 0 (< i upper-bound)
       (g obj))
  (let ((delta-t (- (t) init-t)))
    (set! deftype-delay delta-t)
    #;(pp `(Delay of ,upper-bound normal fun calls: ,delta-t))))

(pp `(gen-fun/fun ratio: ,(/ oo-delay deftype-delay)))
(newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; General usage benchmark ;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (generate-instances)
  )