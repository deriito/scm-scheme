(define (define-data-type type-name field-names)
  (begin
    (let ((data-type-def
            (c-define-data-type type-name field-names))
          (data-type-def-sym
            (string->symbol (string-append
                              "data-type-def:"
                              (symbol->string type-name)))))
      (begin
        (eval `(define ,data-type-def-sym ,data-type-def))
        (gen-other-procedures type-name (list->vector field-names) data-type-def-sym)))))

(define (gen-other-procedures type-name field-names-v data-type-def)
  (begin
    (gen-constructor type-name field-names-v data-type-def)
    (gen-predicate type-name data-type-def)
    (gen-accessor type-name field-names-v data-type-def)
    (gen-modifier type-name field-names-v data-type-def)))

(define (gen-constructor type-name field-names-v data-type-def)
  (let* ((procname
          (string->symbol (string-append
                            "make-"
                            (symbol->string type-name))))
         (field-num (vector-length field-names-v))
         (field-names (vector->list field-names-v))
         (arg2 (append (list 'vector) field-names)))
    (eval `(define ,procname
              (lambda ,field-names
                (c-make-instance ,data-type-def ,arg2))))))

(define (gen-predicate-name type-name)
  (string->symbol (string-append
                    (symbol->string type-name)
                    "?")))

(define (gen-predicate type-name data-type-def)
  (let ((procname (gen-predicate-name type-name)))
    (eval `(define ,procname
              (lambda (obj)
                (c-data-type-predicate ,data-type-def obj))))))

(define (gen-accessor type-name field-names-v data-type-def)
  (let loop ((i 0))
    (if (< i (vector-length field-names-v))
      (let ((procname
              (string->symbol (string-append
                                (string-append
                                  (symbol->string type-name)
                                  "-")
                                (symbol->string (vector-ref field-names-v i))))))
        (begin
          (eval `(define ,procname
                    (lambda (obj)
                      (if (,(gen-predicate-name type-name) obj)
                        (c-data-type-accessor obj ,(+ i 1))
                        (error "accessor: wrong type of obj")))))
          (loop (+ i 1))))
      #t)))

(define (gen-modifier type-name field-names-v data-type-ref)
  (let loop ((i 0))
    (if (< i (vector-length field-names-v))
      (let ((procname
              (string->symbol (string-append
                                "set"
                                (string-append
                                  "-"
                                  (string-append
                                    (symbol->string type-name)
                                    (string-append
                                      "-"
                                      (string-append
                                        (symbol->string (vector-ref field-names-v i))
                                        "!")))))))
             (procname-bakup
               (string->symbol (string-append
                                 "set"
                                 (string-append
                                   "-"
                                   (string-append
                                     (symbol->string type-name)
                                     (string-append
                                       "-"
                                       (string-append
                                         (symbol->string (vector-ref field-names-v i))
                                         "!-bakup")))))))
             (procname-with-wb
               (string->symbol (string-append
                                 "set"
                                 (string-append
                                   "-"
                                   (string-append
                                     (symbol->string type-name)
                                     (string-append
                                       "-"
                                       (string-append
                                         (symbol->string (vector-ref field-names-v i))
                                         "!-with-wb"))))))))
        (begin
          (eval `(define ,procname
                         (lambda (obj value)
                           (if (,(gen-predicate-name type-name) obj)
                             (c-data-type-modifier obj ,(+ i 1) value)
                             (error "modifier: wrong type of obj")))))
          (eval `(define ,procname-bakup ,procname))
          (eval `(define ,procname-with-wb
                         (lambda (obj value)
                           (if (,(gen-predicate-name type-name) obj)
                             (c-data-type-modifier-with-wb obj ,(+ i 1) value)
                             (error "modifier-with-wb: wrong type of obj")))))
          (loop (+ i 1))))
      #t)))

;; ====================== utils for define-data-type ======================

;; LinkedList
(define-data-type 'linked-list-node '(entry next-node))

(define-data-type 'linked-list '(size pred-proc-sym first-node last-node))

(define (new-linked-list accepted-type-name-sym)
  (let ((pred-proc-sym (string->symbol (string-append (symbol->string accepted-type-name-sym) "?"))))
    (make-linked-list 0 pred-proc-sym '() '())))

(define (linked-list-add linked-list value)
  (if (not ((eval (linked-list-pred-proc-sym linked-list)) value))
    (begin
      (error (string-append
               "ERROR: You are trying to add a object of wrong type to linked-list<"
               (symbol->string (linked-list-pred-proc-sym linked-list))
               ">\n"))
      #f)
    (begin
      (let ((tmp-node (make-linked-list-node value '())))
        (begin
          (if (null? (linked-list-first-node linked-list))
            (begin
              (set-linked-list-first-node! linked-list tmp-node)
              (set-linked-list-last-node! linked-list tmp-node))
            (begin
              (set-linked-list-node-next-node! (linked-list-last-node linked-list) tmp-node)
              (set-linked-list-last-node! linked-list tmp-node)))
          (set-linked-list-size! linked-list (+ 1 (linked-list-size linked-list))))))))

(define (get-linked-list-size linked-list)
  (linked-list-size linked-list))

(define (linked-list-ref linked-list index)
  (let ((list-size (linked-list-size linked-list)))
    (if (or (null? (linked-list-first-node linked-list)) (< index 0) (> index (- list-size 1)))
      (error "ERROR: Index out of bounds!")
      (let loop ((i 0)
                  (tmp-node (linked-list-first-node linked-list)))
        (if (= i index)
          (linked-list-node-entry tmp-node)
          (loop (+ i 1) (linked-list-node-next-node tmp-node)))))))

(define (linked-list-set! linked-list index value)
  (let ((list-size (linked-list-size linked-list)))
    (cond
      ((not ((eval (linked-list-pred-proc-sym linked-list)) value))
        (begin
          (error (string-append
                   "ERROR: You are trying to set a object of wrong type to linked-list<"
                   (symbol->string (linked-list-pred-proc-sym linked-list))
                   ">\n"))
          #f))
      ((or (null? (linked-list-first-node linked-list)) (< index 0) (> index (- list-size 1)))
        (error "ERROR: Index out of bounds!"))
      (else
        (let loop ((i 0)
                    (tmp-node (linked-list-first-node linked-list)))
          (if (= i index)
            (set-linked-list-node-entry! tmp-node value)
            (loop (+ i 1) (linked-list-node-next-node tmp-node))))))))

(define (linked-list-rm-ref linked-list index)
  (let ((list-size (linked-list-size linked-list)))
    (if (or (null? (linked-list-first-node linked-list)) (< index 0) (> index (- list-size 1)))
      (error "ERROR: Index out of bounds!")
      (let loop ((i 0)
                  (prev-node '())
                  (curr-node (linked-list-first-node linked-list))
                  (post-node (linked-list-node-next-node (linked-list-first-node linked-list))))
        (if (= i index)
          (begin
            (set-linked-list-node-next-node! curr-node '())
            (cond
              ((and (null? prev-node) (null? post-node))
                (begin
                  (set-linked-list-first-node! linked-list '())
                  (set-linked-list-last-node! linked-list '())))
              ((and (null? prev-node) (not (null? post-node)))
                (set-linked-list-first-node! linked-list post-node))
              ((and (not (null? prev-node)) (null? post-node))
                (begin
                  (set-linked-list-node-next-node! prev-node '())
                  (set-linked-list-last-node! linked-list prev-node)))
              (else
                (set-linked-list-node-next-node! prev-node post-node)))
            (set-linked-list-size! linked-list (- list-size 1)))
          (loop (+ i 1) curr-node post-node (linked-list-node-next-node post-node)))))))

;; ArrayList
(define-data-type 'array-list '(size pred-proc-sym data))

(define init-array-list-data-capacity 10)
(define default-array-list-grow-time 2)

(define (new-array-list accepted-type-name-sym)
  (let ((pred-proc-sym (string->symbol (string-append (symbol->string accepted-type-name-sym) "?"))))
    (make-array-list 0 pred-proc-sym (make-vector init-array-list-data-capacity '()))))

(define (array-list-add array-list value)
  (if (not ((eval (array-list-pred-proc-sym array-list)) value))
    (begin
      (error (string-append
               "ERROR: You are trying to add a object of wrong type to array-list<"
               (symbol->string (array-list-pred-proc-sym array-list))
               ">\n"))
      #f)
    (begin
      ((lambda (array-list)
         (let ((curr-size (array-list-size array-list))
                (curr-capacity (vector-length (array-list-data array-list))))
           (cond
             ((>= curr-size curr-capacity)
               (let ((new-vector (make-vector (* curr-capacity default-array-list-grow-time) '()))
                      (curr-vector (array-list-data array-list)))
                 (begin
                   (let loop ((i 0))
                     (cond
                       ((< i curr-size)
                         (vector-set! new-vector i (vector-ref curr-vector i)))))
                   (set-array-list-data! array-list new-vector)))))))
        array-list)
      (let ((curr-size (array-list-size array-list)))
        (begin
          (vector-set! (array-list-data array-list) curr-size value)
          (set-array-list-size! array-list (+ curr-size 1)))))))

(define (get-array-list-size array-list)
  (array-list-size array-list))

(define (get-array-list-data-capacity array-list)
  (vector-length (array-list-data array-list)))

(define (array-list-ref array-list index)
  (let ((list-size (array-list-size array-list)))
    (if (or (<= list-size 0) (< index 0) (> index (- list-size 1)))
      (error "ERROR: Index out of bounds!")
      (vector-ref (array-list-data array-list) index))))

(define (array-list-set! array-list index value)
  (cond
    ((not ((eval (array-list-pred-proc-sym array-list)) value))
      (begin
        (error (string-append
                 "ERROR: You are trying to set a object of wrong type to array-list<"
                 (symbol->string (array-list-pred-proc-sym array-list))
                 ">\n"))
        #f))
    ((or (<= (array-list-size array-list) 0) (< index 0) (> index (- (array-list-size array-list) 1)))
      (error "ERROR: Index out of bounds!"))
    (else
      (vector-set! (array-list-data array-list) index value))
    ))

(define (array-list-rm-ref array-list index)
  (let ((list-size (array-list-size array-list))
         (curr-capacity (vector-length (array-list-data array-list))))
    (if (or (<= list-size 0) (< index 0) (> index (- list-size 1)))
      (error "ERROR: Index out of bounds!")
      (let ((new-vector (make-vector curr-capacity '()))
             (curr-vector (array-list-data array-list)))
        (begin
          (let loop ((i 0))
            (cond
              ((< i list-size)
                (begin
                  (cond
                    ((< i index)
                      (vector-set! new-vector i (vector-ref curr-vector i)))
                    ((> i index)
                      (vector-set! new-vector (- i 1) (vector-ref curr-vector i))))
                  (loop (+ i 1))))))
          (set-array-list-data! array-list new-vector)
          (set-array-list-size! array-list (- list-size 1)))))))

;; HashMap
;; TODO

;; ====================== utils for define-data-type ======================