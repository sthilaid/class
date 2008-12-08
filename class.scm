;; Very simple object system which focuses on runtime speed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expansion context setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; these 2 macros sets up the context in which the class system is
;; used. If the iterative-method-dev is chosen (default), the generic
;; function (polymorphism) are reset at each new method definition,
;; which enables iterative developpement in a repl. This is not
;; suitable for compiled code (exploses the code size), so the manual
;; mode lets the user decide when to generate the methods with a
;; manual call to (setup-generic-functions!) in the user code.
;;
;; These macros should be called just after including this file.

(define-macro (set-iterative-method-developpement!)
  (set! mode 'iterative))

(define-macro (set-manual-method-developpement!)
  (set! mode 'manual))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro expansion time env
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initializes the global define-class macro-expension-time
;; environnment. This macro must be called
(define-macro (init)
  ;; macro exp time librairy
  (eval
   '(begin
      ;; method expansion mode
      (define mode 'iterative) ; iterative as default
      
      ;; starts to 2 because 0/1 are reserved for the class id and supers
      (define desc-index 1) 
      (define class-table (make-table test: eq?))
      (define meth-table (make-table test: eq?))
      (define (next-desc-index)
        (set! desc-index (+ desc-index 1))
        desc-index)
      
      (define (meth-name sign) (if (not (list? sign))
                                   (error 'bad-signature-syntax)
                                   (car sign)))
      (define any-type 'any-type)

      (define (symbol-append s1 . ss)
          (string->symbol (apply string-append
                                 (symbol->string s1)
                                 (map symbol->string ss))))

      ;; Returns the super-class id, if found.
      (define (macro-is-subclass? class-id super-id)
        ;; Warning: This might return true even if its not a subclass if
        ;; an old superclass as been redefined...
        (or (and (eq? class-id super-id) class-id)
            (exists (lambda (class-super)
                      (macro-is-subclass? class-super super-id))
                    (class-desc-supers (class-info-desc
                                        (table-ref class-table class-id))))
            (eq? super-id any-type)))


      ;;;;;;;;;;;;;;; Naming convention abstractions ;;;;;;;;;;;;;;;
      (define (gen-accessor-name class-name var)
        (symbol-append class-name '- var))
      (define (gen-setter-name class-name var)
        (symbol-append class-name '- var '-set!))
      (define (gen-predicate-name class-name)
        (symbol-append class-name '?))
      (define (class-desc-name  class-name)
        (symbol-append class-name '-class-descriptor))


      (define (gen-method-desc-name sign)
        (symbol-append (meth-name sign) '-meth-desc))

      (define (gen-method-table-name name)
        (symbol-append name '-meth-table))

      (define (rt-class-table-name) 'rt-class-table)
      


      ;;;;;;;;;;;;;;; Data structure used ;;;;;;;;;;;;;;;

      (define (make-class-info field-indices descriptor)
        (vector field-indices descriptor))
      (define (class-info-fi info) (vector-ref info 0))
      (define (class-info-desc info) (vector-ref info 1))
      
      (define (make-class-desc id supers num-fields)
        (let ((desc (make-vector (+ num-fields 2) 'unknown-slot)))
          (vector-set! desc 0 id)
          (vector-set! desc 1 supers)
          desc))
      (define (class-desc-id desc) (vector-ref desc 0))
      (define (class-desc-supers desc) (vector-ref desc 1))
      (define (class-desc-indices-vect desc) (vector-ref desc 2))


      (define (make-slot type index) (cons type index))
      (define (is-class-slot? slot-info)
        (and (pair? slot-info)
             (eq? (slot-type slot-info) class:)))
      (define (is-instance-slot? slot-info)
        (and (pair? slot-info)
             (eq? (slot-type slot-info) instance:)))
      (define (slot-type slot-info)
        (car slot-info))
      (define (slot-index slot-info)
        (cdr slot-info))

        
      (define (make-generic-function name args)
        (vector name args '()))
      (define (generic-function-name gf) (vector-ref gf 0))
      (define (generic-function-args gf) (vector-ref gf 1))
      (define (generic-function-instances gf) (vector-ref gf 2))
      (define (generic-function-instances-add! gf instance)
        (vector-set! gf 2 (cons instance (generic-function-instances gf))))
      
      (define make-method vector) ; (make-method id types body)
      (define (method-id meth) (vector-ref meth 0))
      (define (method-types meth) (vector-ref meth 1))
      (define (method-body meth) (vector-ref meth 2))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime lib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (init-rt)
  `(begin
     (define (class-desc-id desc) (vector-ref desc 0))
     (define (class-desc-supers desc) (vector-ref desc 1))
     (define (class-desc-indices-vect desc) (vector-ref desc 2))

     ;; Runtime class table
     (define ,(rt-class-table-name) (make-table test: eq?))


     (define (object? obj)
       (and (vector? obj)
            (vector? (vector-ref obj 0))
            (symbol? (class-desc-id (vector-ref obj 0)))))

     (define (cast obj type) (vector cast: obj type))
     (define (cast? obj)
       (and (vector? obj)
            (eq? (vector-ref obj 0) cast:)
            (= (vector-length obj) 3)))
     (define (cast-obj  c) (vector-ref c 1))
     (define (cast-type c) (vector-ref c 2))
     (define (uncast obj) (if (cast? obj) (cast-obj obj) obj))
       
     ;; FIXME: VERY BAD object verification..
     (define (get-class-id obj)
       (cond
        ((object? obj)
         (let ((id (class-desc-id (vector-ref obj 0))))
           ;; this test ensures that its a valid class
           (if (table-ref ,(rt-class-table-name) id #f)
               id
               'any-type)))
        ((cast? obj) (cast-type obj))
        (else 'any-type)))

     ;; This produces a "light" copy because the fiels are simply
     ;; copied by value, not deeply replicated. Thus a pointer to a
     ;; data structure will be copied as a pointer to the same data
     ;; structure.
     (define (object-light-copy obj)
       (let* ((len (vector-length obj))
              (new-obj (make-vector len #f)))
         (let loop ((i 0))
           (if (< i len)
               (begin (vector-set! new-obj i (vector-ref obj i))
                      (loop (+ i 1)))))
         new-obj))

     (define (instance-of? obj class-id)
       (eq? (vector-ref obj 0)
            (table-ref ,(rt-class-table-name) class-id (gensym))))

     (define (is-subclass? class-id super-id)
       ;; Warning: This might return true even if its not a subclass if
       ;; an old superclass as been redefined...
       (or (and (eq? class-id super-id) class-id)
           (exists (lambda (class-super) (is-subclass? class-super super-id))
                   (class-desc-supers
                    (table-ref ,(rt-class-table-name) class-id)))
           (eq? super-id 'any-type)))

     (define-generic (describe obj))

     ;; A usefull function provided by Marc :)
     (define (add-pp-method! type-predicate transformer)
       (let* ((old-wr
               ##wr)
              (new-wr
               (lambda (we obj)
                 (old-wr we
                         (if (type-predicate obj)
                             (transformer obj)
                             obj)))))
         (set! ##wr new-wr)))

     'object-system-loaded
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-class name supers . fields) 
  (define temp-field-table (make-table test: eq?))
  
  (define obj (gensym 'obj))
  (define val (gensym 'val))

  ;; todo
  ;; puts the fields into the temp table. The class fields MUST be
  ;; processed AFTER that the super class's fields where processed.
  (define (process-field! field)
    (cond
     ((and (list? field)         ; FIXME: should this test be removed?
           (= (length field) 2)
           (eq? (car field) slot:))
      (let ((slot-name (cadr field)))
        ;; If a field is already provided by a super class
        ;; then the super class's is used ...
        (if (not (table-ref temp-field-table slot-name #f))
            (table-set! temp-field-table
                        slot-name
                        (make-slot instance: (next-desc-index))))))
     ((and (list? field)         ; FIXME: should this test be removed?
           (= (length field) 2)
           (eq? (car field) class-slot:))
      (let ((slot-name (cadr field)))
        (if (not (table-ref temp-field-table slot-name #f))
            (table-set! temp-field-table
                        slot-name
                        (make-slot class: (next-desc-index))))))))

  (define (gen-accessors field-indices)
    (define (gen-accessor field slot-info)
      (cond
       ((is-class-slot? slot-info)
        (let ((index (slot-index slot-info)))
          `(define (,(gen-accessor-name name field))
             (vector-ref ,(class-desc-name name) ,index))))

       ((is-instance-slot? slot-info)
        (let ((index (slot-index slot-info)))
          `(define (,(gen-accessor-name name field) ,obj)
             (vector-ref ,obj
                         (vector-ref (vector-ref ,obj 0) ,index)))))))
    ;; Generate a list of all the accesssors
    (if (not (pair? field-indices))
        '()
        (let ((field (caar field-indices))
              (index (cdar field-indices)))
          (cons (gen-accessor field index)
                (gen-accessors (cdr field-indices))))))
  
  (define (gen-setters field-indices)
    (define (gen-setter field slot-info)
      (cond
       ((is-class-slot? slot-info)
        (let ((index (slot-index slot-info)))
          `(define (,(gen-setter-name name field) ,val)
             (vector-set! ,(class-desc-name name) ,index ,val))))
       
       ((is-instance-slot? slot-info)
        (let ((index (slot-index slot-info)))
          `(define (,(gen-setter-name name field) ,obj ,val)
             (vector-set! ,obj
                          (vector-ref (vector-ref ,obj 0) ,index)
                          ,val))))))
    ;; Generate a list of all the setters
    (if (not (pair? field-indices))
        '()
        (let ((field (caar field-indices))
              (index (cdar field-indices)))
          (cons (gen-setter field index)
                (gen-setters (cdr field-indices))))))

  ;; field-indices are expected to be sorted from lower index to
  ;; highest index
  (define (gen-descriptor field-indices)
    (define-macro (instance-index++)
      (define i (gensym 'i))
      `(let ((,i instance-index))
         (set! instance-index (+ instance-index 1))
         ,i))
    (let* ((instance-index 1)
           (desc (make-class-desc name supers
                                  (+ (slot-index
                                      (cdar (take-right field-indices 1)))
                                     1))))
      ;; For each field, insert the instance index if it is an
      ;; instance slot or, simply add an unbound notice for a class
      ;; slot into the descriptor
      (for-each
       (lambda (fi)
         (let ((index (slot-index fi)))
           (cond ((is-instance-slot? fi)
                  (vector-set! desc index (instance-index++)))
                 ((is-class-slot? fi)
                  (vector-set! desc index 'unbound-class-slot))
                 (else
                  (error "Unknown slot type")))))
       (map cdr field-indices))
      desc))

  ;; field-indices are expected to be sorted from lower index to
  ;; highest index
  (define (gen-instantiator field-indices)
    (define obj (gensym 'obj))
    (define instance-field-indices
      (filter (lambda (field-index)
                (is-instance-slot? (cdr field-index)))
              field-indices))
    ;; Transforms a macro time vector to a vector runtime declaration
    (define (vector->vector v)
      (let ((code '())
            (len  (vector-length v)))
       (let loop ((i (- (vector-length v) 1))
                  (code '()))
         (if (>= i 0)
             (loop (- i 1) (cons `(quote ,(vector-ref v i)) code))
             (cons 'vector code)))))
    `(begin
       ;; Class descriptor is put in a global var
       (define ,(class-desc-name name)
         ,(vector->vector (class-info-desc (table-ref class-table name))))
       (define (,(symbol-append 'make- name) ,@(map car instance-field-indices))
         (vector ,(class-desc-name name)
                 ,@(map car instance-field-indices)))))

  (define (gen-predicate)
    (define obj (gensym 'obj))
    `(begin
       ;; Class descriptor is put in a global var
       (define (,(gen-predicate-name name) ,obj)
         (is-subclass? (class-desc-id (vector-ref ,obj 0))
                       ',name))))

  (define (gen-printfun field-indices)
    ;; not clean hehe, obj uses a gensym but not the rest of the code...
    ;; i dont believe that the codes need hygiene here anyways...
    (define obj (gensym 'obj))
    (define (field->list f)
      (let ((fname (car f))
            (slot-info (cdr f)))
        (if (is-instance-slot? slot-info)
            (list 'list slot: `',fname ''=
              (list (gen-accessor-name name fname) obj))
            (list 'list class-slot: `',fname ''=
              (list (gen-accessor-name name fname))))))
    
    `(define-method (describe (,obj ,name))
       (list ,@(map field->list field-indices))))
  
  (define (sort-field-indices field-indices)
    (define-macro (indice-comp op)
      (let ((x (gensym 'x)) (y (gensym 'y)))
        `(lambda (,x ,y) (,op (slot-index (cdr ,x))
                              (slot-index (cdr ,y))))))
    (quick-sort (indice-comp <) (indice-comp =) (indice-comp >)
                field-indices))

  (include "scm-lib-macro.scm")
  (include "scm-lib.scm")

  ;; Process the super classes' slots
  (for-each
   (lambda (super)
     (let ((super-field-indices
            (cond
             ((table-ref class-table super #f) =>
              (lambda (desc) (class-info-fi desc)))
             (else (error
                    (to-string
                     (show "Inexistant super class: " super)))))))
       (for-each
        (lambda (field-index)
          (if (not (table-ref temp-field-table (car field-index) #f))
              (table-set! temp-field-table
                          (car field-index) (cdr field-index))
              (error
               (to-string
                (show "Field already defined: " (car field-index))))))
        super-field-indices)))
   supers)

  ;; Process this class's slots
  (for-each (lambda (field)
              ;; If a field is already provided by a super class
              ;; then the super class's is used ...
              (process-field! field))
            fields)
  (let* ((field-indices (sort-field-indices (table->list temp-field-table)))
         (class-desc (gen-descriptor field-indices)))

    (table-set! class-table name (make-class-info field-indices class-desc))
    `(begin ,@(gen-accessors field-indices)
            ,@(gen-setters field-indices)
            ,(gen-predicate)
            ,(gen-instantiator field-indices)
            ;; Create a generi print utility (must be at the end)
            ,(gen-printfun field-indices)
            ;; here the class descriptor is put in a global table
            ;; but it is also available via its variable ,(class-desc-name name)
            (table-set! ,(rt-class-table-name)
                        ',name
                        ,(class-desc-name name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-generic signature)
  (define name (meth-name signature))
  (define (parse-arg arg)
    (cond ((and (list? arg) (symbol? (car arg)) (symbol? (cadr arg)))
           (car arg))
          (else arg)))
  (define (args) (map parse-arg (cdr signature)))
  
  (table-set! meth-table name (make-generic-function name (args)))
  ''ok
  #;
  `(begin
     (define ,(gen-method-table-name name) (make-table test: equal?))
     (define (,name ,@(args))
       (let ((types (map get-class-id (list ,@(args)))))
         (cond
          ((table-ref ,(gen-method-table-name name) types #f)
           => (lambda (method) (method ,@(args))))
          (else
           (error (string-append "Unknown method: "
                                 (with-output-to-string
                                   ""
                                   (lambda ()
                                     (pretty-print `(,,name ,@types))))))))))))





(define-macro (define-method signature bod . bods)
  (define (name) (meth-name signature))
  (define unknown-meth-error 'unknown-meth)
  (define (parse-arg arg)
    (cond ((and (list? arg) (symbol? (car arg)) (symbol? (cadr arg)))
           (let ((var (car arg))
                 (type (cadr arg)))
             (values var type)))
          (else (values arg any-type))))
  ;; Returns 2 values: the ordrered list of arguments and the ordered
  ;; list of their types.
  (define (parse-args args) (map-values parse-arg args))
  (include "scm-lib-macro.scm")
  (include "scm-lib.scm")
  
  (with-exception-catcher
   (lambda (e)
     (pp `(received error: ,e))
     (if (eq? e unknown-meth-error)
         (error (to-string (show "Generic method was not defined: " (name))))
         (raise e)))
   (lambda ()
     (cond
      ((table-ref meth-table (name) #f) =>
       (lambda (gen-fun)
         (receive (args types) (parse-args (cdr signature))
                  (generic-function-instances-add!
                   gen-fun
                   (make-method (name) types `(lambda ,args ,bod ,@bods)))
                  (case mode
                   ((iterative) '(setup-generic-functions!))
                   ((manual)    ''ok)
                   (else        (error "unknown method expansion mode."))))))
      (else (raise unknown-meth-error))))))

(define-macro (setup-generic-functions!)
  (define all-generic-functions (map cdr (table->list meth-table)))
  `(begin
     ;; Setup the tables and meta-functions that will search for the
     ;; appropriate generic function instance. The search should be in
     ;; about O(1) since its just a lookup into a pre-calculated table.
     ,@(map
        (lambda (gen-fun)
          (let ((name (generic-function-name gen-fun))
                (args (generic-function-args gen-fun)))
           `(begin
              (define ,(gen-method-table-name name) (make-table test: equal?))
              (define (,name ,@args)
                (let ((types (map get-class-id (list ,@args))))
                  (cond
                   ((table-ref ,(gen-method-table-name name) types #f)
                    => (lambda (method)
                         (apply method (map uncast ,(cons 'list args)))))
                   (else
                    (error (string-append
                            "Unknown method: "
                            (with-output-to-string
                              ""
                              (lambda ()
                                (pretty-print `(,,name ,@types)))))))))))))
        all-generic-functions)
     
     ;; Insert the declared generic functions into their respective tables
     (begin
       ,@(map (lambda (gen-fun)
                (let ((name (generic-function-name gen-fun))
                      (instances (generic-function-instances gen-fun)))
                  `(begin
                     ,@(map (lambda (method)
                              `(table-set! ,(gen-method-table-name name)
                                           ',(method-types method)
                                           ,(method-body method)))
                            instances))))
              all-generic-functions))

     (begin 'DEGUG--begin-of-polymorphing!--) ;; DEBUGING
     
     ;; Insert the most specific generic function instance for all
     ;; possible of valid type combinations for the arguments. (not
     ;; efficient, but made at compile time).
     (polymorphize-methods!)

     (add-pp-method!
      (lambda (obj) (not (eq? (get-class-id obj) 'any-type)))
      describe)))




(define-macro (polymorphize-methods!)
  (define (find-sub-classes class-id)
    (filter (lambda (current-class)
              (macro-is-subclass? current-class class-id))
            (map car (table->list class-table) )))

  (define (find-super-classes class-id)
    (filter (lambda (current-class)
              (macro-is-subclass? class-id current-class))
            (map car (table->list class-table))))

  (define (get-super-numbers class-id)
    (if (eq? class-id any-type)
        0
        (length
         (apply generic-multi-union
                eq?
                (map find-super-classes
                     (class-desc-supers (class-info-desc
                                         (table-ref class-table class-id))))))))
  
  (define (class-comparator fun)
    (lambda (c1 c2)
      (fun (get-super-numbers c1) (get-super-numbers c2))))

  (define (sort-class-specific-order class-lst)
    (quick-sort (class-comparator <) (class-comparator =) (class-comparator >)
                class-lst))

  ;; FIXME: This comparator will certainly lead to strange ordering
  ;; with complicated hierarchy. This is just a temporary fix...
  (define (method-comparator fun)
    (lambda (m1 m2)
      (fun (apply + (map get-super-numbers (method-types m1)))
           (apply + (map get-super-numbers (method-types m2))))))
  
  (define (sort-methods method-lst)
    (quick-sort (method-comparator <)
                (method-comparator =)
                (method-comparator >)
                method-lst))
  
  (load "scm-lib.scm")

  ;; runtime generated code that will insert the most specific method
  ;; instance for subclasses without generic method instances.
  `(begin
     ,@(apply
        reverse-append
        (map
         (lambda (gen-method)
           (let ((gen-meth-name (generic-function-name gen-method))
                 (gen-meth-instances (generic-function-instances gen-method)))
             (apply
              reverse-append
              (map
               (lambda (method)
                 (map
                  (lambda (possible-types)
                    (if (not (exists (lambda (m) (equal? possible-types
                                                         (method-types m)))
                                     gen-meth-instances))
                        `(table-set! ,(gen-method-table-name gen-meth-name)
                                     ',possible-types
                                     ,(method-body method))
                        ''nothing-to-add))
                  (apply cartesian-product
                         (map find-sub-classes (method-types method)))))
               ;; Must reverse the list because of the reverse-append
               ;; calls! The order of the table-set! are important as
               ;; the one with the most specific instance should
               ;; appear last!
               (reverse (sort-methods gen-meth-instances))))))
         ;; filter the empty genric functions (with no instances)
         (filter (lambda (m) (not (null? (generic-function-instances m))))
                 (map cdr (table->list meth-table)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (update! obj class field f)
  (let ((objval (gensym 'objval)))
   `(let ((,objval ,obj))
      (,(gen-setter-name class field) ,objval
       (,f (,(gen-accessor-name class field) ,objval))))))


;;;;;;;;;;;;;;;;;;;;;;;; Runtime stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;

(init)
(init-rt)

