
;; Very simple object system which focuses on runtime speed.

(include "class_.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define-class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-class name supers . fields) 
  (define temp-field-table (make-table test: eq?))
  
  (define obj (gensym 'obj))
  (define val (gensym 'val))

  (define constructor-list '())

  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  (define-macro (to-string e1 . es)
    `(with-output-to-string "" (lambda () ,e1 ,@es)))

  ;; puts the fields into the temp table. The class fields MUST be
  ;; processed AFTER that the super class's fields where processed.
  (define (process-field! field)
    (if (not (list? field))
        (else (error (to-string (show "ill-formed slot declaration: "
                                      field))))
        (case (car field)
          ((slot: class-slot:)
           (let ((slot-type (car field))
                 (slot-name (cadr field))
                 (slot-options (cddr field)))
             ;; If a field is already provided by a super class
             ;; then the super class's is used ...

             (if (not (table-ref temp-field-table slot-name #f))
                 (table-set! temp-field-table
                             slot-name
                             (make-slot slot-type
                                        (next-desc-index)
                                        slot-options
                                        #f ; not inherited if processed...
                                        )))))
          ((constructor:)
           (set! constructor-list (cons (cadr field) constructor-list)))

          (else (error (to-string (show "ill-formed slot declaration: "
                                        field)))))))

  (define (gen-accessors field-indices)
    (define (gen-accessor field slot-info)
      ;; link to the super class accessor if inherited!
      (if (and (slot-inherited?   slot-info)
               (is-instance-slot? slot-info))
          `(define ,(gen-accessor-name name field)
             ,(gen-accessor-name (slot-inherited? slot-info) field))
          (let* ((index (slot-index slot-info))
                 (read-hooks (slot-read-hooks? slot-info)))
            (cond ((is-class-slot? slot-info)
                   (let ((hook-call `(hook slot-value))
                         (direct-slot-access
                          `(vector-ref ,(class-desc-name name) ,index))
                         (indirect-slot-access
                          `(vector-ref (instance-class-descriptor ,obj)
                                       ,index)))
                     ;; An optional argument which must be an instance can
                     ;; be given. If this argument is present, then a
                     ;; dynamic fetch of the class field will be made.
                     `(define (,(gen-accessor-name name field)
                               #!optional (,obj #f))
                        (let ((slot-access
                               (if ,obj
                                   ,indirect-slot-access
                                   ,direct-slot-access)))
                          ,(if read-hooks
                               `(let ((slot-value slot-access))
                                  (for-each (lambda (hook) ,hook-call)
                                            (list ,@read-hooks))
                                  slot-value)
                               'slot-access)))))
                  ((is-instance-slot? slot-info)
                   (let ((hook-call `(hook ,obj slot-value))
                         (slot-access
                          `(vector-ref ,obj
                                       (vector-ref
                                        (instance-class-descriptor ,obj)
                                        ,index))))
                     `(define (,(gen-accessor-name name field) ,obj)
                        ,(if read-hooks
                             `(let ((slot-value ,slot-access))
                                (for-each (lambda (hook) ,hook-call)
                                          (list ,@read-hooks))
                                slot-value)
                             slot-access))))))))
    ;; Generate a list of all the accesssors
    (if (not (pair? field-indices))
        '()
        (let ((field (caar field-indices))
              (index (cdar field-indices)))
          (cons (gen-accessor field index)
                (gen-accessors (cdr field-indices))))))
  
  (define (gen-setters field-indices)
    (define (gen-setter field slot-info)
      ;; link to the super class setter if inherited!
      (if (and (slot-inherited?   slot-info)
               (is-instance-slot? slot-info))
          `(define ,(gen-setter-name name field)
             ,(gen-setter-name (slot-inherited? slot-info) field))
          (let* ((index (slot-index slot-info))
                 (write-hooks (slot-write-hooks? slot-info)))
            (cond ((is-class-slot? slot-info)
                   (let ((hook-call `(hook ,val))
                         (direct-slot-set!
                          `(vector-set! ,(class-desc-name name) ,index ,val))
                         (indirect-slot-set!
                          `(vector-set! (instance-class-descriptor ,obj)
                                        ,index ,val)))
                     `(define (,(gen-setter-name name field)
                               ,obj
                               #!optional (val? #f))
                        (let* ((,val (if val? val? ,obj))
                               (slot-set! (if val?
                                              ,indirect-slot-set!
                                              ,direct-slot-set!)))
                          ,(if write-hooks
                               `(begin (for-each (lambda (hook) ,hook-call)
                                                 (list ,@write-hooks))
                                       slot-set!)
                               'slot-set!)))))
                  ((is-instance-slot? slot-info)
                   (let ((hook-call `(hook ,obj ,val))
                         (slot-set!
                          `(vector-set!
                            ,obj
                            (vector-ref (instance-class-descriptor ,obj) ,index)
                            ,val)))
                     `(define (,(gen-setter-name name field) ,obj ,val)
                        ,(if write-hooks
                             `(begin (for-each (lambda (hook) ,hook-call)
                                               (list ,@write-hooks))
                                     ,slot-set!)
                             slot-set!))))
                  (else (error "gen-setters: unknown class slot"))))))
    
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
    (let* ((instance-index 1) ; 0 -> class-desc
           (desc (make-class-desc
                  name supers
                  (if (pair? field-indices)
                      ;; must substract 1 here because the index
                      ;; includes the clas id and supers' place
                      ;; holders (but id has index 0)
                      (- (slot-index
                          (cdar (take-right field-indices 1)))
                         1)
                      0))))
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

  (define (gen-class-descriptor field-indices)
    ;; Transforms a macro time vector to a vector runtime declaration
    (define (vector->vector v)
      (let ((code '())
            (len  (vector-length v)))
       (let loop ((i    (- (vector-length v) 1))
                  (code '()))
         (if (>= i 0)
             (loop (- i 1) (cons `(quote ,(vector-ref v i)) code))
             (cons 'vector code)))))
    ;; Class descriptor is put in a global var
    `(define ,(class-desc-name name)
       ,(vector->vector (class-info-desc (table-ref mt-class-table name)))))

  ;; field-indices are expected to be sorted from lower index to
  ;; highest index
  (define (gen-instantiator field-indices)
    (define obj (gensym 'obj))
    (define instance-field-indices
      (filter (lambda (field-index)
                (is-instance-slot? (cdr field-index)))
              field-indices))
    
    `(begin
       ;; Usual make-[classname] 'a la' define-type constructor
       (define (,(symbol-append 'make- name) ,@(map car instance-field-indices))
         (vector ,(class-desc-name name)
                 ,@(map car instance-field-indices)))
       ;; The instantiator creates an un-initialized instance
       (define (,(gen-instantiator-name name))
         (vector ,(class-desc-name name)
                 ,@(map (lambda (x)
                          (list 'quote
                                (list 'quote
                                      (symbol-append 'un-initialized-
                                                     name
                                                     '-field))))
                        instance-field-indices)))
       ;; init! instances used as construtors for intitializing instances
       ,@(if (null? constructor-list)
             (list
              `(define-method (init! (obj ,name)
                                     ,@(map car instance-field-indices))
                 ,@(map (lambda (fi)
                          `(,(gen-setter-name name (car fi))
                            obj
                            ,(car fi)))
                        instance-field-indices)
                 obj))
             (map (lambda (constructor)
                    ;; parse the constructor so that it is a lambda
                    ;; expression of at least one var:
                    ;; (lambda (obj-arg . args) . body)
                    (cond ((and (list? constructor)
                                (>= (length constructor) 3)
                                (eq? (car constructor) 'lambda)
                                (list? (cadr constructor))
                                (>= (length (cadr constructor)) 1))
                           (let* ((cons-obj-arg (caadr constructor))
                                  (const-args (cdadr constructor))
                                  (const-body (cddr constructor)))
                             `(define-method (init! (,cons-obj-arg ,name)
                                                    ,@const-args)
                                ,@const-body
                                ,cons-obj-arg)))
                          (else (error "Ill-formed constructor"))))
                  constructor-list))))

  (define (gen-predicate)
    (define obj (gensym 'obj))
    `(begin
       ;; Class descriptor is put in a global var
       (define (,(gen-predicate-name name) ,obj)
         (and (instance-object? ,obj)
              (is-subclass? (class-desc-id (instance-class-descriptor ,obj))
                            ',name)))))

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

  (begin ; body
    
   ;; Process the super classes' *inherited slots*
   (for-each
    (lambda (super)
      (let ((super-field-indices
             (cond
              ((table-ref mt-class-table super #f) =>
               (lambda (desc) (class-info-fi desc)))
              (else (error
                     (to-string
                      (show "Inexistant super class: " super)))))))
        (for-each
         (lambda (field-index)
           (if (not (table-ref temp-field-table (car field-index) #f))
               (let ((super-slot (cdr field-index)))
                 (table-set! temp-field-table
                             (car field-index)
                             (make-slot (slot-type       super-slot)
                                        (slot-index      super-slot)
                                        (slot-options    super-slot)
                                        super ; now inherited
                                        )))
               (error
                (to-string
                 (show "Field already defined: " (car field-index))))))
         super-field-indices)))
    supers)

   ;; Process this class's slots
   (for-each process-field! fields)

   (let* ((field-indices (sort-field-indices (table->list temp-field-table)))
         (class-desc (gen-descriptor field-indices)))

    (table-set! mt-class-table name (make-class-info field-indices class-desc))
    `(begin ,(gen-class-descriptor field-indices)
            ,@(gen-accessors field-indices)
            ,@(gen-setters field-indices)
            ,(gen-predicate)
            ;; here the class descriptor is put in a global table
            ;; but it is also available via its variable ,(class-desc-name name)
            (table-set! rt-class-table
                        ',name
                        ,(class-desc-name name))
            ;; Generic function that are generated must come after the
            ;; descriptor was put into the runtime class table!
            ,(gen-instantiator field-indices)
            ,(gen-printfun field-indices)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-generic name)
  (let ((args 'toto))
    (table-set! mt-meth-table name (make-mt-generic-function name args))
    `(begin
       (define ,(gen-method-table-name name)
         (make-generic-function ',name ',args))
       (define (,name #!key (cast #f) #!rest args)
         ;; the retrieval of types is slower then when the number of
         ;; parameters was constant. Now (map get-class-id...) is
         ;; performed at runtime.
         (let ((types
                (cond
                 ((pair? cast) cast)
                 (else (map get-class-id args)))))
           (cond
            ((or (generic-function-get-instance ,(gen-method-table-name name)
                                                types)
                 (find-polymorphic-instance? ,(gen-method-table-name name)
                                             types))
             => (lambda (method)
                  (parameterize ((___call-next-method
                                  (lambda ()
                                    (apply ,name
                                           cast:
                                           (map get-supers
                                                (current-method-types))
                                           args))))
                    (apply (method-body method) args))))
            (else
             (error (string-append
                     "Unknown method: "
                     (with-output-to-string
                       ""
                       (lambda ()
                         (pretty-print `(,',name ,@types)))))))))))))

(define-macro (define-method signature bod . bods)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  (define-macro (to-string e1 . es)
    `(with-output-to-string "" (lambda () ,e1 ,@es)))
  (define (name) (meth-name signature))
  (define unknown-meth-error 'unknown-meth)
  (define (parse-arg arg)
    (cond 
     ;; Match a specific value
     ((and (list? arg) (= (length arg) 2)
           (symbol? (car arg))
           (match-type? (cadr arg)))
      (values (car arg) (cadr arg)))
     ;; Match a specific class tree
     ((and (list? arg) (= (length arg) 2)
           (symbol? (car arg)) (symbol? (cadr arg)))
      (let ((var (car arg))
            (type (cadr arg)))
        (values var type)))
     ;; Otherwise match anything
     (else (values arg any-type))))
  ;; Returns 2 values: the ordrered list of arguments and the ordered
  ;; list of their types.
  (define (parse-args args) (map-values parse-arg args))
  (with-exception-catcher
   (lambda (e)
     (if (eq? e unknown-meth-error)
         (error (to-string (show "Generic method was not defined: " (name))))
         (raise e)))
   ;; TODO: Add arity verification
   (lambda ()
     (cond
      ((table-ref mt-meth-table (name) #f) =>
       (lambda (gen-fun)
         (receive (args types) (parse-args (cdr signature))
           (let ((parameterized-body
                  `(lambda ,args
                     (parameterize ((current-method-types ',types))
                       ,bod ,@bods))))
             ;; macro exp time book-keeping
            (mt-generic-function-instances-add!
             gen-fun
             (make-method (name) types parameterized-body))
            ;; runtime book-keeping
            `(generic-function-instances-add!
              ,(gen-method-table-name (name))
              (make-method ',(name) ',types ,parameterized-body))))))
      (else
       `(begin
          (define-generic ,(name))
          (define-method ,signature ,bod ,@bods)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime lib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Warning: The class descriptor data structure should not be
;; modified, or if so, some code in the generation of the descriptor
;; will need to be modified (because the rest of the other vector
;; fields are used by the indexes of all the fields of the objects.
(define (instance-class-descriptor obj) (vector-ref obj 0))

(define (class-desc-id desc)           (vector-ref desc 0))
(define (class-desc-supers desc)       (vector-ref desc 1))
(define (class-desc-indices-vect desc) (vector-ref desc 2))

(define (make-generic-function name args)
  (vector name args (make-table test: equal?) '()))
(define (generic-function-name gf)             (vector-ref gf 0))
(define (generic-function-args gf)             (vector-ref gf 1))
(define (generic-function-instances gf)        (vector-ref gf 2))
(define (generic-function-sorted-instances gf) (vector-ref gf 3))
(define (generic-function-instances-list gf)
  (##table-foldl rcons '() (lambda (k v) v)
                 (generic-function-instances gf)))
(define (generic-function-get-instance gf types)
  (table-ref (generic-function-instances gf) types #f))
(define (generic-function-instances-add! gf instance)
  (let ((new-method? (table-ref (generic-function-instances gf)
                                (method-types instance)
                                #f)))
    (table-set! (generic-function-instances gf)
                (method-types instance)
                instance)
    ;; If a method with the same types was not previously used, we can
    ;; re-use the same sorted-list, but we can't if its not the
    ;; case...
    (vector-set! gf 3
                 (sort-methods
                  (if new-method?
                      (cons instance
                            (generic-function-sorted-instances gf))
                      (generic-function-instances-list gf)))))
  
  ;; Update the sorted list of
  )
(define (generic-function-instances-number gf)
  (table-length (generic-function-instances gf)))

(define make-method vector)          ; (make-method id types body)
(define (method-id meth) (vector-ref meth 0))
(define (method-types meth) (vector-ref meth 1))
(define (method-body meth) (vector-ref meth 2))

(define any-type '*)
(define (any-type? ty) (eq? ty any-type))
(define (make-external-object val) (cons val any-type))
(define (external-object? ty) (and (pair? ty)
                                   (eq? (cdr ty) any-type)))
(define external-object-value car)

(define match-type match-value:)
(define (make-match-type val) (list match-type val))
(define (match-type? obj) (and (list? obj) (eq? (car obj) match-type)))
(define match-type-value cadr)


;; Runtime class table
(define rt-class-table (make-table test: eq?))

(define (find-class? id)
  (table-ref rt-class-table id #f))

(define (instance-object? obj)
  (and (vector? obj)
       (vector? (instance-class-descriptor obj))
       (symbol? (class-desc-id (instance-class-descriptor obj)))))

;; Temporarily removed because slows the code down and cannot support
;; cast to a list of classes lightly.
(define (assert-cast args types)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  (define-macro (to-string e1 . es)
    `(with-output-to-string "" (lambda () ,e1 ,@es)))
  (define error-str )

  (if (not (= (length args) (length types)))
      (error (string-append "Cannot perform cast: actual parameter number "
                            "differs from cast types number.")))
  (for-each
   (lambda (arg type)
     (let ((class-id (get-class-id arg)))
       (if (not (is-subclass? class-id type))
           (error (to-string (show "Cannot perform cast: "
                                   class-id " is not a sublclass of " type))))))
   args
   types)
  types)

;; FIXME: VERY BAD object verification..
(define (get-class-id obj)
  (cond
   ((instance-object? obj) (class-desc-id (instance-class-descriptor obj)))
   (else (make-external-object obj))))

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
  (eq? (instance-class-descriptor obj)
       (table-ref rt-class-table class-id (gensym))))

(define (is-subclass? class-id super-id)
  (or (eq? class-id super-id)
      (any-type? super-id)
      (if (match-type? super-id)
          (and (external-object? class-id)
               (equal? (external-object-value class-id)
                       (match-type-value super-id)))
          ;; else regular class type
          (and (not (any-type? class-id))
           (memq super-id
                 (class-desc-supers
                  (table-ref rt-class-table class-id)))
           #t))))

(define (get-super-numbers type)
    (cond
     ((or (any-type? type) (external-object? type))
      0)
     ;; match types request are priotary toward any-type requests...
     ((match-type? type)
      1)
     (else
      (length (class-desc-supers (find-class? type))))))

(define (get-supers type)
  (if (or (any-type? type) (external-object? type) (match-type? type))
      any-type
      (class-desc-supers (find-class? type))))

(define (sort-methods method-lst)
    (define (method-comparator fun)
      (lambda (m1 m2)
        (fun (apply + (map get-super-numbers (method-types m1)))
             (apply + (map get-super-numbers (method-types m2))))))

    ;; Note the reversed order of comparators to get the most specific
    ;; gf instances first.
    (quick-sort (method-comparator >)
                (method-comparator =)
                (method-comparator <)
                method-lst))

(define (equivalent-types? instance-types param-types)
    (if (pair? instance-types)
        (and (if (and (pair? (car param-types))
                      (not (external-object? (car param-types))))
                 ;; a list of param types is used to implement the
                 ;; call-next-method functionnality by providing the list
                 ;; of the super classes here...
                 (let ((instance-type (car instance-types)))
                   (exists (lambda (x) (is-subclass? x instance-type))
                           (car param-types)))
                 (is-subclass? (car param-types) (car instance-types)))
             (equivalent-types? (cdr instance-types) (cdr param-types)))
        #t))

;; Will find the "best" or most specific instance of the generic
;; function genfun that corresponds to the actual parameter's types
(define (find-polymorphic-instance? genfun types)
  (let ((args-nb (length types))
        (sorted-instances (generic-function-sorted-instances genfun)))
    (exists (lambda (method) (equivalent-types? (method-types method)
                                                types))
            (filter (lambda (i) (= (length (method-types i)) args-nb))
                    sorted-instances))))

;; The call-next-method works well with single inheritance, but might
;; give unexpected results with multiple inheritance, as the
;; next-method called will depend on the sort-methods function which
;; does *not* discriminate method instances with equal number of super
;; classes and thus might choose arbitriraly which one will be called.
(define ___call-next-method  (make-parameter #f))
(define current-method-types (make-parameter #f))
(define (call-next-method) ((___call-next-method)))


(define-generic describe)
(define-generic init!)

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
