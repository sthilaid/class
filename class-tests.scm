;; File that include a test suite for the object system provided with
;; class.scm. The tests can be ran with (run-tests).

(include "scm-lib-macro.scm")
(include "test-macro.scm")
(include "class.scm")
(load "scm-lib.scm")
(load "test.scm")


(define-class A ()      (slot: a) (class-slot: csa))
(define-class B (A)     (slot: b))
(define-class C ()      (slot: c))
(define-class D (C)     (slot: d))
(define-class E (B D)   (slot: e))
(define-class F (B C))

(define-generic (test obj))
(define-method (test (a A)) (number->string (A-a a)))
(define-method (test (b B)) (symbol->string (B-b b)))
(define-method (test (c C)) (number->string (+ (C-c c) 5)))

(define-generic (test2 o1 o2))
(define-method (test2 (a1 A) (a2 A)) (+ (A-a a1) (A-a a2)))
(define-method (test2 (b1 B) (b2 B)) (symbol-append (B-b b1) (B-b b2)))
(define-method (test2 (a A)  (c C))  (+ (A-a a) (C-c c)))

(define-generic (h x y))
(define-method (h x y) x)
(define-method (h (x A) (y A)) (A-a x))

;; (setup-generic-functions!)


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

(define-test simple-class-slots "1234" 'ok
  (define-class A () (slot: a) )
  (let ((obj1 (make-A 1))
        (obj2 (make-A 2)))
    (A-csa-set! 3)
    (display (A-a obj1))
    (display (A-a obj2))
    (display (A-csa))
    (A-csa-set! 4)
    (display (A-csa)))
  'ok)

(define-test test-generic-simple "10dix" 'ok
  (display (test (make-A 10)))
  (display (test (make-B 10 'dix)))
  'ok)

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
    (display (test2 a e)))
  'ok)

(define-test test-predicate-simple "yesno" 'ok
  (let ((a (make-A 1)))
    (display (if (A? a) 'yes 'no))
    (display (if (C? a) 'yes 'no))
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

(define-test test-instance-of "A#fAB#f#f#f#t#f#f" 'ok
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
    (display (instance-of? a 'any-type))
    (display (instance-of? b 'any-type)))
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

  




;; (pp (lambda () (setup-generic-functions!)))