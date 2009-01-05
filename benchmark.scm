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

;; Slot access benchmark
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

;; Generic function call benchmark
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