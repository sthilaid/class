;; File that include a test suite for the object system provided with
;; class.scm. The tests can be ran with (run-tests).

(include "scm-lib-macro.scm")
(include "test-macro.scm")
(include "class.scm")
(load "scm-lib.scm")
(load "test.scm")

;; (pp (lambda () (define-class A ()      (slot: a) (class-slot: csa))))
(define-class A ()      (slot: a) (class-slot: csa))
(define-class B (A)     (slot: b))
(define-class C ()      (slot: c))
(define-class D (C)     (slot: d))
(define-class E (B D)   (slot: e))
(define-class F (B C))

;; Another similar hierarchy
(define-class <A> ()    (slot: a))
(define-class <B> (<A>) (slot: b))
(define-class <C> (<B>) (slot: c))
(define-class <D> ()    (slot: d))
(define-class <E> (<D>) (slot: e))
(define-class <F> (<C> <E>) (slot: f))

(define-generic test)
(define-method (test (a A)) (number->string (A-a a)))
(define-method (test (b B)) (symbol->string (B-b b)))
(define-method (test (c C)) (number->string (+ (C-c c) 5)))
(define-method (test (e E)) 'e)

(define-generic test2)
(define-method (test2 (a1 A) (a2 A)) (+ (A-a a1) (A-a a2)))
(define-method (test2 (b1 B) (b2 B)) (symbol-append (B-b b1) (B-b b2)))
(define-method (test2 (a A)  (c C))  (+ (A-a a) (C-c c)))

(define-generic test3)
(define-method (test3 (o A)) 'a)
(define-method (test3 (o B)) 'b)
(define-method (test3 (o E)) 'e)

(define-generic h)
(define-method (h x y) x)
(define-method (h (x A) (y A)) (A-a x))

(define-generic TotO!)
(define-method (TotO! (x A)) (A-a x))
(define-method (TotO! (x B)) (B-b x))
(define-method (TotO! (x A) (y A)) (+ (TotO! x) (TotO! y)))
(define-method (TotO! (x B) (y B)) (+ (TotO! x) (TotO! y)))


(define-test simple-instance-slots "aaabbccdde" 'ok
  (let ((obj (make-E 'a 'b 'c 'd 'e)))
    (display (A-a obj))
    (display (B-a obj))
    (display (E-a obj))
    (display (B-b obj))
    (display (E-b obj))
    (display (C-c obj))
    (display (E-c obj))
    (display (D-d obj))
    (display (E-d obj))
    (display (E-e obj)))
  'ok)

(define-test simple-class-slots "12333allo44allo7allo7toto" 'ok
  (let ((obj1 (make-A 1))
        (obj2 (make-A 2))
        (obj3 (make-B 5 6)))
    (A-csa-set! 3)
    (B-csa-set! 'allo)
    (display (A-a obj1))
    (display (A-a obj2))
    (display (A-csa))
    (display (A-csa obj1))
    (display (A-csa obj2))
    (display (A-csa obj3))
    (A-csa-set! 4)
    (display (A-csa))
    (display (A-csa obj1))
    (display (A-csa obj3))
    (A-csa-set! obj1 7)
    (display (A-csa obj1))
    (display (A-csa obj3))
    (A-csa-set! obj3 'toto)
    (display (A-csa obj1))
    (display (A-csa obj3)))
  'ok)

(define-test test-generic-simple "10dix" 'ok
  (display (test (make-A 10)))
  (display (test (make-B 10 'dix)))
  'ok)

(define-test test-generic-redefinition "toto!titi!" 'ok
  (let ((a (make-A 10)))
    (define-generic toto)
    (define-method (toto (obj A)) 'toto!)
    (display (toto a))
    (define-method (toto (obj A)) 'titi!)
    (display (toto a))
    'ok))

;; Testing that the generic function test is correctly "polymorphised"
;; such that when calling it with some object instances that have no
;; direct equivalent instance, the most specific found is used.
(define-test test-generic-polymorphism "deux6deux" 'ok
  (let ((b (make-B 1 'deux))
        (d (make-D 1 2))
        (e (make-F 1 'deux 3)))
    (display (test b))
    (display (test d))
    (display (test e)))
  'ok)

(define-test test-generic-polymorphism-2 "2deuxdeuxdeuxdeux2" 'ok
  (let ((a (make-A 1))
        (b (make-B 1 'deux))
        (d (make-D 1 2))
        (e (make-F 1 'deux 3)))
    (display (test2 a b))
    (display (test2 b b))
    (display (test2 e e))
    (display (test2 a d)))
  'ok)

(define-test test-predicate-simple "yesnono" 'ok
  (let ((a (make-A 1)))
    (display (if (A? a)     'yes 'no))
    (display (if (C? a)     'yes 'no))
    (display (if (C? 'toto) 'yes 'no))
    'ok))

(define-test test-predicate-hierarchy "yyyyynnyy" 'ok
  (let ((d (make-D 'c 'd))
        (e (make-E 'a 'b 'c 'd 'e)))
    (display (if (A? e) 'y 'n))
    (display (if (B? e) 'y 'n))
    (display (if (C? e) 'y 'n))
    (display (if (D? e) 'y 'n))
    (display (if (E? e) 'y 'n))
    (display (if (A? d) 'y 'n))
    (display (if (B? d) 'y 'n))
    (display (if (C? d) 'y 'n))
    (display (if (D? d) 'y 'n))
    'ok))

(define-test test-any-type-generics "okokok" 'ok
  (display (h (make-A 'ok) (make-A 11)))
  (if (integer? (h 10 11)) (display 'ok) (display 'no))
  (if (vector? (h (make-C 10) (make-C 11))) (display 'ok) (display 'no))
  'ok)

(define-test test-instance-of "#t#f#t#t#t#f#f#t#f#f" 'ok
  (let ((a (make-A 1))
        (b (make-B 1 2)))
    (display (A? a))
    (display (B? a))
    (display (A? b))
    (display (B? b))
    (display (instance-of? a 'A))
    (display (instance-of? a 'B))
    (display (instance-of? b 'A))
    (display (instance-of? b 'B))
    (display (instance-of? a '*))
    (display (instance-of? b any-type)))
  'ok)

(define-test test-describe "" 'ok
  (let ((a (make-A 1))
        (b (make-B 2 3)))
    (A-csa-set! 'allo)
    (B-csa-set! 'salut)
    (if (and
         (equal? (describe a)
                 '((slot: a = 1) (class-slot: csa = allo)))
         (equal? (describe b)
                 '((slot: a = 2) (class-slot: csa = salut) (slot: b = 3))))
        'ok
        'no)))

(define-test test-update! "" 'ok
  (let ((obj (make-A 1)))
    (update! obj A a (lambda (x) (+ x 1)))
    (if (= (A-a obj) 2)
        'ok
        'no)))

(define-test test-equal-and-copy "#t#t" #f
  (let ((o1 (make-A 1))
        (o2 (make-B 1 2)))
    (display (equal? o2 (make-B 1 2)))
    (display (equal? o2 (object-light-copy o2)))
    (equal? o1 o2))) 


(define-test test-casts "abeaa" 'ok
  (let ((obj-a (make-A 1))
        (obj-b (make-B 1 2))
        (obj-e (make-E 1 2 3 4 5)))
    (display (test3 obj-a))
    (display (test3 obj-b))
    (display (test3 obj-e))
    (display (test3 cast: '(A) obj-b))
    (display (test3 cast: '(A) obj-e))
    'ok))

;; Testing slot hooks
(define-class hooked ()
  (slot:       s
               (read-hooks:  (lambda (obj x) (display "r") (display x)))
               (write-hooks: (lambda (obj x) (display "w") (display x))))
  (class-slot: cs
               (read-hooks:  (lambda (x) (display "r") (display x)))
               (write-hooks: (lambda (x) (display "w") (display x)))))

(define-test test-hooks "wallor1w2r2rallowsalutrsalut" 'ok
  (let ((o (make-hooked 1)))
    (hooked-cs-set! 'allo)
    (hooked-s o)
    (hooked-s-set! o 2)
    (hooked-s o)
    (hooked-cs)
    (hooked-cs-set! 'salut)
    (hooked-cs)
    'ok))

(define-test test-multiple-arity-genfun "1921810" 'ok
  (let ((a (make-A 1))
        (b (make-B 3 9)))
    (display (TotO! a))
    (display (TotO! b))
    (display (TotO! a a))
    (display (TotO! b b))
    (display (TotO! a b))
    'ok))

;; can't use local class defs
(define-test test-local-declarations "" 'ok
  ((lambda ()
     (define-generic test)
     (define-method (test (x A)) 'ok)
     (test (make-A 1)))))

(define-class weird-complex-number () (slot: real) (slot: imag)
  (constructor: (lambda (obj)
                  (weird-complex-number-real-set! obj 0)
                  (weird-complex-number-imag-set! obj 0)))
  (constructor: (lambda (obj real)
                  (weird-complex-number-real-set! obj real)
                  (weird-complex-number-imag-set! obj (* real real))))
  (constructor: (lambda (obj real imag)
                  (weird-complex-number-real-set! obj real)
                  (weird-complex-number-imag-set! obj imag))))
(define-test test-constructors "003932003932" 'ok
  (define-generic disp)
  (define-method (disp (n1 weird-complex-number))
    (display (weird-complex-number-real n1))
    (display (weird-complex-number-imag n1)))

  (disp (init! (make-weird-complex-number-instance)))
  (disp (init! (make-weird-complex-number-instance) 3))
  (disp (init! (make-weird-complex-number-instance) 3 2))
  (disp (new weird-complex-number))
  (disp (new weird-complex-number 3))
  (disp (new weird-complex-number 3 2))
  'ok)

(define-test test-set-fields! "6734567345" 'ok
  (let* ((e (new E 1 2 3 4 5)))
    (let ((test (set-fields! e E ((a 6) (b 7)))))
      (display (E-a e))
      (display (E-b e))
      (display (E-c e))
      (display (E-d e))
      (display (E-e e)))
    (display (E-a e))
    (display (E-b e))
    (display (E-c e))
    (display (E-d e))
    (display (E-e e)))
  'ok)

(define-test test-call-next-method "6" 12
  (begin
    (define-generic intern-test)
    (define-method (intern-test (x <A>)) (<A>-a x))
    (define-method (intern-test (x <B>)) (+ (<B>-b x) (call-next-method)))
    (define-method (intern-test (x <C>)) (+ (<C>-c x) (call-next-method)))
    (define-method (intern-test (x <D>)) (<D>-d x))
    (define-method (intern-test (x <E>)) (+ (<E>-e x) (call-next-method)))
    (define-method (intern-test (x <F>)) (+ (<F>-f x) (call-next-method)))
    (display (intern-test (new <C> 1 2 3)))
    (intern-test (new <F> 1 2 3 4 5 6))))
