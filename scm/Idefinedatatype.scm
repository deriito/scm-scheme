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

(define (new-linked-list-node entry next-node)
  (let ((new-instance (make-linked-list-node '() '())))
    (begin
      (set-linked-list-node-entry! new-instance entry)
      (set-linked-list-node-next-node! new-instance next-node)
      new-instance)))

(define-data-type 'linked-list '(size pred-proc-sym first-node last-node))

(define (new-linked-list accepted-type-name-sym)
  (let ((pred-proc-sym (string->symbol (string-append (symbol->string accepted-type-name-sym) "?")))
         (new-instance (make-linked-list '() '() '() '())))
    (begin
      (set-linked-list-size! new-instance 0)
      (set-linked-list-pred-proc-sym! new-instance pred-proc-sym)
      (set-linked-list-first-node! new-instance '())
      (set-linked-list-last-node! new-instance '())
      new-instance)))

(define (linked-list-add linked-list value)
  (if (not ((eval (linked-list-pred-proc-sym linked-list)) value))
    (begin
      (error (string-append
               "ERROR: You are trying to add a object of wrong type to linked-list<"
               (symbol->string (linked-list-pred-proc-sym linked-list))
               ">\n"))
      #f)
    (begin
      (let ((tmp-node (new-linked-list-node value '())))
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

(define (linked-list-index-of linked-list obj)
  (let ((found-index -1))
    (begin
      (let loop ((i 0)
                  (curr-node (linked-list-first-node linked-list)))
        (if (not (null? curr-node))
          (if (eq? obj (linked-list-node-entry curr-node))
            (set! found-index i)
            (loop (+ i 1) (linked-list-node-next-node curr-node)))))
      found-index)))

(define (linked-list-rm-obj linked-list obj)
  (let ((found-index (linked-list-index-of linked-list obj)))
    (if (>= found-index 0)
      (linked-list-rm-ref linked-list found-index))))

(define (linked-list-print linked-list)
  (begin
    (display "[")
    (let loop ((curr-node (linked-list-first-node linked-list))
                (i 0)
                (list-size (get-linked-list-size linked-list)))
      (if (not (null? curr-node))
        (begin
          (display (linked-list-node-entry curr-node))
          (if (< i (- list-size 1))
            (display ", "))
          (loop (linked-list-node-next-node curr-node) (+ i 1) list-size))))
    (display "]\n")))

;; ArrayList
(define-data-type 'array-list '(size pred-proc-sym data))

(define init-array-list-data-capacity 10)
(define default-array-list-grow-time 2)

(define (new-array-list accepted-type-name-sym)
  (let ((pred-proc-sym (string->symbol (string-append (symbol->string accepted-type-name-sym) "?")))
         (new-instance (make-array-list '() '() '())))
    (begin
      (set-array-list-size! new-instance 0)
      (set-array-list-pred-proc-sym! new-instance pred-proc-sym)
      (set-array-list-data! new-instance (make-vector init-array-list-data-capacity '()))
      new-instance)))

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

(define (array-list-index-of array-list obj)
  (let ((found-index -1)
         (list-size (get-array-list-size array-list)))
    (begin
      (let loop ((i 0))
        (cond
          ((< i list-size)
            (if (eq? obj (array-list-ref array-list i))
              (set! found-index i)
              (loop (+ i 1))))))
      found-index)))

(define (array-list-rm-obj array-list obj)
  (let ((found-index (array-list-index-of array-list obj)))
    (if (>= found-index 0)
      (array-list-rm-ref array-list found-index))))

(define (array-list-print array-list)
  (begin
    (display "[")
    (let loop ((i 0)
                (list-size (get-array-list-size array-list)))
      (cond
        ((< i list-size)
          (begin
            (display (array-list-ref array-list i))
            (if (< i (- list-size 1))
              (display ", "))
            (loop (+ i 1) list-size)))))
    (display "]\n")))

;; HashMap
(define-data-type 'hash-map-entry '(k v))

(define (new-hash-map-entry k v)
  (let ((new-instance (make-hash-map-entry '() '())))
    (begin
      (set-hash-map-entry-k! new-instance k)
      (set-hash-map-entry-v! new-instance v)
      new-instance)))

(define-data-type 'hash-map-internal '(size key-pred-proc-sym value-pred-proc-sym used-bucket-size buckets-vector))

(define (new-hash-map-internal size key-pred-proc-sym value-pred-proc-sym used-bucket-size buckets-vector)
  (let ((new-instance (make-hash-map-internal '() '() '() '() '())))
    (begin
      (set-hash-map-internal-size! new-instance size)
      (set-hash-map-internal-key-pred-proc-sym! new-instance key-pred-proc-sym)
      (set-hash-map-internal-value-pred-proc-sym! new-instance value-pred-proc-sym)
      (set-hash-map-internal-used-bucket-size! new-instance used-bucket-size)
      (set-hash-map-internal-buckets-vector! new-instance buckets-vector)
      new-instance)))

(define-data-type 'hash-map '(hash-map-internal))

(define init-buckets-capacity 16)
(define buscket-capacity-expand-time 2)

(define (new-hash-map-internal-with-capacity key-pred-proc-sym value-pred-proc-sym capacity)
  (new-hash-map-internal 0 key-pred-proc-sym value-pred-proc-sym 0 (make-vector capacity '())))

(define (new-hash-map-with-buckets-capacity key-pred-proc-sym value-pred-proc-sym capacity)
  (let ((new-instance (make-hash-map '())))
    (begin
      (set-hash-map-hash-map-internal!
        new-instance
        (new-hash-map-internal-with-capacity key-pred-proc-sym value-pred-proc-sym capacity))
      new-instance)))

(define (new-hash-map accepted-key-type-name-sym accepted-value-type-name-sym)
  (let ((key-pred-proc-sym (string->symbol (string-append (symbol->string accepted-key-type-name-sym) "?")))
         (value-pred-proc-sym (string->symbol (string-append (symbol->string accepted-value-type-name-sym) "?"))))
    (new-hash-map-with-buckets-capacity key-pred-proc-sym value-pred-proc-sym init-buckets-capacity)))

(define (hash-map-internal-simple-put hash-map-internal key value)
  (letrec ((buckets-vector (hash-map-internal-buckets-vector hash-map-internal))
            (key-hash (hash key (vector-length buckets-vector)))
            (entry-list (vector-ref buckets-vector key-hash)))
    (begin
      (let ((entry-to-add (new-hash-map-entry key value)))
        (if (null? entry-list)
          (begin
            (set! entry-list (new-linked-list 'hash-map-entry))
            (linked-list-add entry-list entry-to-add)
            (vector-set! buckets-vector key-hash entry-list)
            (set-hash-map-internal-used-bucket-size! hash-map-internal (+ 1 (hash-map-internal-used-bucket-size hash-map-internal)))
            (set-hash-map-internal-size! hash-map-internal (+ 1 (hash-map-internal-size hash-map-internal))))
          (let ((have-found-key #f))
            (begin
              (let loop ((curr-list-node (linked-list-first-node entry-list)))
                (cond
                  ((not (null? curr-list-node))
                    (if (eq? (hash-map-entry-k (linked-list-node-entry curr-list-node)) key)
                      (begin
                        (set! have-found-key #t)
                        (set-linked-list-node-entry! curr-list-node entry-to-add))
                      (loop (linked-list-node-next-node curr-list-node))))))
              (cond
                ((not have-found-key)
                  (begin
                    (linked-list-add entry-list entry-to-add)
                    (set-hash-map-internal-size! hash-map-internal (+ 1 (hash-map-internal-size hash-map-internal)))))))))))))

(define (hash-map-simple-put hash-map key value)
  (hash-map-internal-simple-put (hash-map-hash-map-internal hash-map) key value))

(define (hash-map-put hash-map key value)
  (letrec ((hash-map-internal (hash-map-hash-map-internal hash-map))
            (key-pred-proc-sym (hash-map-internal-key-pred-proc-sym hash-map-internal))
            (value-pred-proc-sym (hash-map-internal-value-pred-proc-sym hash-map-internal)))
    (cond
      ((or (not ((eval key-pred-proc-sym) key)) (not ((eval value-pred-proc-sym) value)))
        (error (string-append
                 "ERROR: A wrong key or value is put in the hash-map<"
                 (symbol->string key-pred-proc-sym)
                 ", "
                 (symbol->string value-pred-proc-sym)
                 ">")))
      (else
        (begin
          ((lambda (hash-map)
             (letrec ((hash-map-internal (hash-map-hash-map-internal hash-map))
                       (buckets-vector (hash-map-internal-buckets-vector hash-map-internal))
                       (buckets-capacity (vector-length buckets-vector))
                       (used-buckets-size (hash-map-internal-used-bucket-size hash-map-internal)))
               (cond
                 ((>= used-buckets-size buckets-capacity)
                   (letrec ((new-buckets-capacity (* buckets-capacity buscket-capacity-expand-time))
                             (key-pred-proc-sym (hash-map-internal-key-pred-proc-sym hash-map-internal))
                             (value-pred-proc-sym (hash-map-internal-value-pred-proc-sym hash-map-internal))
                             (new-hash-map-internal (new-hash-map-internal-with-capacity key-pred-proc-sym value-pred-proc-sym new-buckets-capacity))
                             (new-buckets-vector (hash-map-internal-buckets-vector new-hash-map-internal)))
                     (begin
                       (let loop ((i 0))
                         (cond
                           ((< i buckets-capacity)
                             (begin
                               (let ((entry-list (vector-ref buckets-vector i)))
                                 (cond
                                   ((not (null? entry-list))
                                     (let loop-internal ((curr-list-node (linked-list-first-node entry-list)))
                                       (cond
                                         ((not (null? curr-list-node))
                                           (begin
                                             (hash-map-internal-simple-put
                                               new-hash-map-internal
                                               (hash-map-entry-k (linked-list-node-entry curr-list-node))
                                               (hash-map-entry-v (linked-list-node-entry curr-list-node)))
                                             (loop-internal (linked-list-node-next-node curr-list-node)))))))))
                               (loop (+ i 1))))))
                       (set-hash-map-hash-map-internal! hash-map new-hash-map-internal)))))))
            hash-map)
          (hash-map-simple-put hash-map key value))))))

(define (get-hash-map-size hash-map)
  (hash-map-internal-size (hash-map-hash-map-internal hash-map)))

(define (get-hash-map-buckets-vector-capacity hash-map)
  (vector-length (hash-map-internal-buckets-vector (hash-map-hash-map-internal hash-map))))

(define (get-hash-map-used-bucket-size hash-map)
  (hash-map-internal-used-bucket-size (hash-map-hash-map-internal hash-map)))

(define (hash-map-get hash-map key)
  (letrec ((hash-map-internal (hash-map-hash-map-internal hash-map))
            (key-pred-proc-sym (hash-map-internal-key-pred-proc-sym hash-map-internal)))
    (cond
      ((not ((eval key-pred-proc-sym) key))
        (error "hash-map-get: Wrong type of the key givened!"))
      (else
        (letrec ((buckets-vector-capacity (get-hash-map-buckets-vector-capacity hash-map))
                  (key-hash (hash key buckets-vector-capacity))
                  (buckets-vector (hash-map-internal-buckets-vector hash-map-internal))
                  (entry-list (vector-ref buckets-vector key-hash)))
          (if (null? entry-list)
            '()
            (let loop ((i 0)
                        (curr-list-node (linked-list-first-node entry-list)))
              (cond
                ((not (null? curr-list-node))
                  (if (eq? key (hash-map-entry-k (linked-list-node-entry curr-list-node)))
                    (hash-map-entry-v (linked-list-node-entry curr-list-node))
                    (loop (+ i 1) (linked-list-node-next-node curr-list-node))))
                ((and (null? curr-list-node) (= i (get-linked-list-size entry-list)))
                  '())))))))))

(define (hash-map-rm hash-map key)
  (letrec ((hash-map-internal (hash-map-hash-map-internal hash-map))
            (key-pred-proc-sym (hash-map-internal-key-pred-proc-sym hash-map-internal)))
    (cond
      ((not ((eval key-pred-proc-sym) key))
        (error "hash-map-get: Wrong type of the key givened!"))
      (else
        (letrec ((buckets-vector-capacity (get-hash-map-buckets-vector-capacity hash-map))
                  (key-hash (hash key buckets-vector-capacity))
                  (buckets-vector (hash-map-internal-buckets-vector hash-map-internal))
                  (entry-list (vector-ref buckets-vector key-hash)))
          (if (null? entry-list)
            #t
            (let ((found-index -1))
              (begin
                (let loop ((i 0)
                            (curr-list-node (linked-list-first-node entry-list)))
                  (cond
                    ((not (null? curr-list-node))
                      (if (eq? key (hash-map-entry-k (linked-list-node-entry curr-list-node)))
                        (set! found-index i)
                        (loop (+ i 1) (linked-list-node-next-node curr-list-node))))))
                (if (>= found-index 0)
                  (begin
                    (linked-list-rm-ref entry-list found-index)
                    (set-hash-map-internal-size! hash-map-internal (- (hash-map-internal-size hash-map-internal) 1))
                    (cond
                      ((= 0 (get-linked-list-size entry-list))
                        (begin
                          (vector-set! buckets-vector key-hash '())
                          (set-hash-map-internal-used-bucket-size! hash-map-internal (- (hash-map-internal-used-bucket-size hash-map-internal) 1)))))))))))))))

(define (hash-map-print hash-map)
  (let ((hash-map-internal (hash-map-hash-map-internal hash-map)))
    (begin
      (display "{")
      (let ((entry-idx 0)
             (map-size (get-hash-map-size hash-map)))
        (let external-loop ((i 0))
          (if (< i (get-hash-map-buckets-vector-capacity hash-map))
            (let ((entry-list (vector-ref (hash-map-internal-buckets-vector hash-map-internal) i)))
              (if (not (null? entry-list))
                (let loop ((curr-node (linked-list-first-node entry-list)))
                  (if (not (null? curr-node))
                    (let ((entry (linked-list-node-entry curr-node)))
                      (begin
                        (display (hash-map-entry-k entry))
                        (display "=")
                        (display (hash-map-entry-v entry))
                        (if (< entry-idx (- map-size 1))
                          (display ", "))
                        (set! entry-idx (+ entry-idx 1))
                        (loop (linked-list-node-next-node curr-node))))
                    (external-loop (+ i 1))))
                (external-loop (+ i 1)))))))
      (display "}\n"))))

;; ====================== utils for define-data-type ======================
