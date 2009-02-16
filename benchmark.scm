(include "scm-lib-macro.scm")
(include "class.scm")
(load "bench-sim")

(define-class B () (slot: b))
(define-class D (B) (slot: d))
(define-class E (B) (slot: e))

(define-generic (f x))
(define-method (f (x B)) 'toto)
(define-method (f (x D)) 'tourlou)
(define (g x) 'tutu)

(define-type C c)



(define upper-bound 1000000)
(define oo-delay #f)
(define deftype-delay #f)


;;;;;;;;;;;;;;;;;;;; Abstraction of the benchmarking process ;;;;;;;;;;;;;;;;;;

(define (t) (time->seconds (current-time)))
(define bench-table (make-table test: eq?))
(define (run-benchmark #!optional (id 'all))
  (cond ((eq? id 'all) (##table-for-each (lambda (_ b) (b)) bench-table))
        ((table-ref bench-table id #f) => (lambda (b) (b)))
        (else (error "unknown benchmark"))))
(define-macro (define-benchmark id desc oo-thunk normal-thunk)
  `(table-set!
    bench-table ',id
    (lambda ()
      (define oo-delay #f)
      (define normal-delay #f)
      (display "**** ") (display ,desc) (display " **** ") (newline)
      (let ((init-t (t)))
        (,oo-thunk)
        (let ((delta-t (- (t) init-t)))
          (set! oo-delay delta-t)))
      (let ((init-t (t)))
        (,normal-thunk)
        (let ((delta-t (- (t) init-t)))
          (set! normal-delay delta-t)))
      (pp `(,',id (oo/normal ratio): ,(/ oo-delay normal-delay)))
      (newline))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Benchmarks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Slot access benchmark ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-benchmark slot-access "Slot access benchmark"
  (lambda () (let ((obj (make-B 2)))
               (for i 0 (< i upper-bound)
                    (+ (B-b obj) (B-b obj)))))
  (lambda () (let ((obj (make-C 1)))
               (for i 0 (< i upper-bound)
                    (+ (C-c obj) (C-c obj))))))


;;;;;;;;;;;;;;;;;;;; Generic function call benchmark ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-benchmark genfun-call "Generic function call benchmark"
  (lambda () (let ((obj (make-B 2)))
               (for i 0 (< i upper-bound)
                    (f obj))))
  (lambda () (let ((obj (make-C 1)))
               (for i 0 (< i upper-bound)
                    (g obj)))))

;;;;;;;;;;;;;;;;;; Polymorphic function call benchmark ;;;;;;;;;;;;;;;;;;;;;;;

(define-benchmark genfun-call "Polymorphic function call benchmark"
  (lambda () (let ((obj (make-E 2 3)))
               (for i 0 (< i upper-bound)
                    (f obj))))
  (lambda () (let ((obj (make-C 1)))
               (for i 0 (< i upper-bound)
                    (g obj)))))

;;;;;;;;;;;;;;;;;;;; Instance creation benchmark ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-benchmark instance-creation "Instance creation benchmark"
  (lambda () (for i 0 (< i upper-bound) (make-B 1)))
  (lambda () (for i 0 (< i upper-bound) (make-C 1))))
    
;;;;;;;;;;;;;;;;;;;;;; General usage benchmark ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-benchmark simple-sim "Simple simulation benchmark"
  simple-sim-oo-main
  simple-sim-dt-main)

(define-benchmark complex-sim
  "A simulation benchmark with some non-trivial element behaviour"
  complex-sim-oo-main
  complex-sim-dt-main)