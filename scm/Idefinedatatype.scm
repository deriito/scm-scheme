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
         (procname2
           (string->symbol (string-append
                             (symbol->string procname)
                             "-not-init")))
         (field-num (vector-length field-names-v))
         (field-names (vector->list field-names-v))
         (arg2 (append (list 'vector) field-names)))
    (begin
      (eval `(define ,procname
                     (lambda ,field-names
                       (c-make-instance ,data-type-def ,arg2))))
      (eval `(define (,procname2)
               (c-make-instance ,data-type-def '()))))))

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
                        (error (string-append
                                 ,(symbol->string type-name)
                                 "'s accessor: wrong type of obj"))))))
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
                                         "!-with-wb")))))))
             (c-modifier-with-wb-procname
               (string->symbol (string-append
                                 "c-data-type-modifier-with-wb-of-field-"
                                 (number->string i)))))
        (begin
          (eval `(define ,procname
                         (lambda (obj value call-site-info) ;; 引数call-site-infoは使わない，WB付きのmodifierの引数個数と同じために
                           (c-data-type-modifier obj ,i value)))) ;; 型検査は省略, 人工チェックの代わりに
          (eval `(define ,procname-bakup ,procname))
          (eval `(define ,procname-with-wb
                         (lambda (obj value call-site-info)
                           (,c-modifier-with-wb-procname obj value call-site-info)))) ;; 型検査は省略, 人工チェックの代わりに
          (loop (+ i 1))))
      #t)))

;; ====================== utils for define-data-type ======================

;; LinkedList
(define-data-type 'linked-list-node '(entry next-node))

(define (new-linked-list-node entry next-node call-site-info)
  (let ((new-instance (make-linked-list-node '() '())))
    (begin
      (set-linked-list-node-entry! new-instance entry call-site-info)
      (set-linked-list-node-next-node! new-instance next-node call-site-info)
      new-instance)))

(define-data-type 'linked-list '(size pred-proc-sym first-node last-node))

(define (new-linked-list accepted-type-name-sym call-site-info)
  (let ((pred-proc-sym (string->symbol (string-append (symbol->string accepted-type-name-sym) "?")))
         (new-instance (make-linked-list '() '() '() '())))
    (begin
      (set-linked-list-size! new-instance 0 call-site-info)
      (set-linked-list-pred-proc-sym! new-instance pred-proc-sym call-site-info)
      (set-linked-list-first-node! new-instance '() call-site-info)
      (set-linked-list-last-node! new-instance '() call-site-info)
      new-instance)))

(define (linked-list-add linked-list value call-site-info)
  (if (not ((eval (linked-list-pred-proc-sym linked-list)) value))
    (begin
      (error (string-append
               "ERROR: You are trying to add a object of wrong type to linked-list<"
               (symbol->string (linked-list-pred-proc-sym linked-list))
               ">\n"))
      #f)
    (begin
      (let ((tmp-node (new-linked-list-node value '() call-site-info)))
        (begin
          (if (null? (linked-list-first-node linked-list))
            (begin
              (set-linked-list-first-node! linked-list tmp-node call-site-info)
              (set-linked-list-last-node! linked-list tmp-node call-site-info))
            (begin
              (set-linked-list-node-next-node! (linked-list-last-node linked-list) tmp-node call-site-info)
              (set-linked-list-last-node! linked-list tmp-node call-site-info)))
          (set-linked-list-size! linked-list (+ 1 (linked-list-size linked-list)) call-site-info))))))

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

(define (linked-list-set! linked-list index value call-site-info)
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
            (set-linked-list-node-entry! tmp-node value call-site-info)
            (loop (+ i 1) (linked-list-node-next-node tmp-node))))))))

(define (linked-list-rm-ref linked-list index call-site-info)
  (let ((list-size (linked-list-size linked-list)))
    (if (or (null? (linked-list-first-node linked-list)) (< index 0) (> index (- list-size 1)))
      (error "ERROR: Index out of bounds!")
      (let loop ((i 0)
                  (prev-node '())
                  (curr-node (linked-list-first-node linked-list))
                  (post-node (linked-list-node-next-node (linked-list-first-node linked-list))))
        (if (= i index)
          (begin
            (set-linked-list-node-next-node! curr-node '() call-site-info)
            (cond
              ((and (null? prev-node) (null? post-node))
                (begin
                  (set-linked-list-first-node! linked-list '() call-site-info)
                  (set-linked-list-last-node! linked-list '() call-site-info)))
              ((and (null? prev-node) (not (null? post-node)))
                (set-linked-list-first-node! linked-list post-node call-site-info))
              ((and (not (null? prev-node)) (null? post-node))
                (begin
                  (set-linked-list-node-next-node! prev-node '() call-site-info)
                  (set-linked-list-last-node! linked-list prev-node call-site-info)))
              (else
                (set-linked-list-node-next-node! prev-node post-node call-site-info)))
            (set-linked-list-size! linked-list (- list-size 1) call-site-info))
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

(define (linked-list-rm-obj linked-list obj call-site-info)
  (let ((found-index (linked-list-index-of linked-list obj)))
    (if (>= found-index 0)
      (linked-list-rm-ref linked-list found-index call-site-info))))

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
(define-data-type 'array-list-data-block '(data))

(define (new-array-list-data-block data call-site-info)
  (let ((new-instance (make-array-list-data-block '())))
    (begin
      (set-array-list-data-block-data! new-instance data call-site-info)
      new-instance)))

(define-data-type 'array-list '(size pred-proc-sym data-blocks))

(define init-array-list-data-capacity 10)
(define default-array-list-grow-size 6)

(define (new-array-list-with-capacity accepted-type-name-sym capacity call-site-info)
  (let ((pred-proc-sym (string->symbol (string-append (symbol->string accepted-type-name-sym) "?")))
         (new-instance (make-array-list '() '() '())))
    (begin
      (set-array-list-size! new-instance 0 call-site-info)
      (set-array-list-pred-proc-sym! new-instance pred-proc-sym call-site-info)
      (set-array-list-data-blocks! new-instance (make-vector capacity '()) call-site-info)
      new-instance)))

(define (new-array-list accepted-type-name-sym call-site-info)
  (new-array-list-with-capacity accepted-type-name-sym init-array-list-data-capacity call-site-info))

(define (array-list-add array-list value call-site-info)
  (if (not ((eval (array-list-pred-proc-sym array-list)) value))
    (begin
      (error (string-append
               "ERROR: You are trying to add a object of wrong type to array-list<"
               (symbol->string (array-list-pred-proc-sym array-list))
               ">\n"))
      #f)
    (begin
      ((lambda (array-list call-site-info)
         (let ((curr-size (array-list-size array-list))
                (curr-capacity (vector-length (array-list-data-blocks array-list))))
           (cond
             ((>= curr-size curr-capacity)
               (let ((new-vector (make-vector (+ curr-capacity default-array-list-grow-size) '()))
                      (curr-vector (array-list-data-blocks array-list)))
                 (begin
                   (let loop ((i 0))
                     (cond
                       ((< i curr-size)
                         (begin
                           (vector-set! new-vector i (vector-ref curr-vector i))
                           (loop (+ i 1))))))
                   (set-array-list-data-blocks! array-list new-vector call-site-info)))))))
        array-list call-site-info)
      (let ((curr-size (array-list-size array-list))
             (new-data-block (new-array-list-data-block value call-site-info)))
        (begin
          (vector-set! (array-list-data-blocks array-list) curr-size new-data-block)
          (set-array-list-size! array-list (+ curr-size 1) call-site-info))))))

(define (get-array-list-size array-list)
  (array-list-size array-list))

(define (get-array-list-data-capacity array-list)
  (vector-length (array-list-data-blocks array-list)))

(define (array-list-ref array-list index)
  (let ((list-size (array-list-size array-list)))
    (if (or (<= list-size 0) (< index 0) (> index (- list-size 1)))
      (error "[array-list-ref] ERROR: Index out of bounds!")
      (array-list-data-block-data (vector-ref (array-list-data-blocks array-list) index)))))

(define (array-list-set! array-list index value call-site-info)
  (cond
    ((not ((eval (array-list-pred-proc-sym array-list)) value))
      (begin
        (error (string-append
                 "[array-list-set!] ERROR: You are trying to set a object of wrong type to array-list<"
                 (symbol->string (array-list-pred-proc-sym array-list))
                 ">\n"))
        #f))
    ((or (<= (array-list-size array-list) 0) (< index 0) (> index (- (array-list-size array-list) 1)))
      (error "[array-list-set!] ERROR: Index out of bounds!"))
    (else
      (set-array-list-data-block-data! (vector-ref (array-list-data-blocks array-list) index) value call-site-info))
    ))

(define (array-list-rm-ref array-list index call-site-info)
  (let ((list-size (array-list-size array-list))
         (curr-capacity (vector-length (array-list-data-blocks array-list))))
    (if (or (<= list-size 0) (< index 0) (> index (- list-size 1)))
      (error "[array-list-rm-ref] ERROR: Index out of bounds!")
      (let ((new-vector (make-vector curr-capacity '()))
             (curr-vector (array-list-data-blocks array-list)))
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
          (set-array-list-data-blocks! array-list new-vector call-site-info)
          (set-array-list-size! array-list (- list-size 1) call-site-info))))))

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

(define (array-list-rm-obj array-list obj call-site-info)
  (let ((found-index (array-list-index-of array-list obj)))
    (if (>= found-index 0)
      (array-list-rm-ref array-list found-index call-site-info))))

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
(define-data-type 'hash-entry '(k v))

(define (new-hash-entry k v call-site-info)
  (let ((new-instance (make-hash-entry '() '())))
    (begin
      (set-hash-entry-k! new-instance k call-site-info)
      (set-hash-entry-v! new-instance v call-site-info)
      new-instance)))

(define-data-type 'hash-internal '(size key-pred-proc-sym value-pred-proc-sym used-bucket-size buckets))

(define (new-hash-internal size key-pred-proc-sym value-pred-proc-sym buckets call-site-info)
  (let ((new-instance (make-hash-internal '() '() '() '() '())))
    (begin
      (set-hash-internal-size! new-instance size call-site-info)
      (set-hash-internal-key-pred-proc-sym! new-instance key-pred-proc-sym call-site-info)
      (set-hash-internal-value-pred-proc-sym! new-instance value-pred-proc-sym call-site-info)
      (set-hash-internal-used-bucket-size! new-instance 0 call-site-info)
      (set-hash-internal-buckets! new-instance buckets call-site-info)
      new-instance)))

(define-data-type 'hash-map '(hash-internal))

(define init-buckets-capacity 16)
(define bucket-capacity-expand-size 6)

(define (new-hash-internal-with-capacity key-pred-proc-sym value-pred-proc-sym capacity call-site-info)
  (new-hash-internal
    0
    key-pred-proc-sym
    value-pred-proc-sym
    (make-vector capacity '())
    call-site-info))

(define (new-hash-map-with-buckets-capacity key-pred-proc-sym value-pred-proc-sym capacity call-site-info)
  (let ((new-instance (make-hash-map '())))
    (begin
      (set-hash-map-hash-internal!
        new-instance
        (new-hash-internal-with-capacity key-pred-proc-sym value-pred-proc-sym capacity call-site-info)
        call-site-info)
      new-instance)))

(define (new-hash-map accepted-key-type-name-sym accepted-value-type-name-sym call-site-info)
  (let ((key-pred-proc-sym (string->symbol (string-append (symbol->string accepted-key-type-name-sym) "?")))
         (value-pred-proc-sym (string->symbol (string-append (symbol->string accepted-value-type-name-sym) "?"))))
    (new-hash-map-with-buckets-capacity key-pred-proc-sym value-pred-proc-sym init-buckets-capacity call-site-info)))

(define (hash-internal-simple-put hash-internal key value call-site-info)
  (letrec ((buckets (hash-internal-buckets hash-internal))
            (key-hash (hash key (vector-length buckets)))
            (entry-list (vector-ref buckets key-hash)))
    (begin
      (let ((entry-to-add (new-hash-entry key value call-site-info)))
        (if (null? entry-list)
          (begin
            (set! entry-list (new-linked-list 'hash-entry call-site-info))
            (linked-list-add entry-list entry-to-add call-site-info)
            (vector-set! buckets key-hash entry-list) ;; line-num-recording on vector is not supported
            (set-hash-internal-used-bucket-size! hash-internal (+ 1 (hash-internal-used-bucket-size hash-internal)) call-site-info)
            (set-hash-internal-size! hash-internal (+ 1 (hash-internal-size hash-internal)) call-site-info))
          (let ((have-found-key #f))
            (begin
              (let loop ((curr-list-node (linked-list-first-node entry-list)))
                (cond
                  ((not (null? curr-list-node))
                    (if (eq? (hash-entry-k (linked-list-node-entry curr-list-node)) key)
                      (begin
                        (set! have-found-key #t)
                        (set-linked-list-node-entry! curr-list-node entry-to-add call-site-info))
                      (loop (linked-list-node-next-node curr-list-node))))))
              (cond
                ((not have-found-key)
                  (begin
                    (linked-list-add entry-list entry-to-add call-site-info)
                    (set-hash-internal-size! hash-internal (+ 1 (hash-internal-size hash-internal)) call-site-info)))))))))))

(define (hash-map-simple-put hash-map key value call-site-info)
  (hash-internal-simple-put (hash-map-hash-internal hash-map) key value call-site-info))

(define (hash-map-put hash-map key value call-site-info)
  (letrec ((hash-internal (hash-map-hash-internal hash-map))
            (key-pred-proc-sym (hash-internal-key-pred-proc-sym hash-internal))
            (value-pred-proc-sym (hash-internal-value-pred-proc-sym hash-internal)))
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
          ((lambda (hash-map call-site-info)
             (letrec ((hash-internal (hash-map-hash-internal hash-map))
                       (buckets (hash-internal-buckets hash-internal))
                       (buckets-capacity (vector-length buckets))
                       (used-buckets-size (hash-internal-used-bucket-size hash-internal)))
               (cond
                 ((>= used-buckets-size buckets-capacity)
                   (letrec ((new-buckets-capacity (+ buckets-capacity bucket-capacity-expand-size))
                             (key-pred-proc-sym (hash-internal-key-pred-proc-sym hash-internal))
                             (value-pred-proc-sym (hash-internal-value-pred-proc-sym hash-internal))
                             (new-hash-internal (new-hash-internal-with-capacity key-pred-proc-sym value-pred-proc-sym new-buckets-capacity call-site-info))
                             (new-buckets (hash-internal-buckets new-hash-internal)))
                     (begin
                       (let loop ((i 0))
                         (cond
                           ((< i buckets-capacity)
                             (begin
                               (let ((entry-list (vector-ref buckets i)))
                                 (cond
                                   ((not (null? entry-list))
                                     (let loop-internal ((curr-list-node (linked-list-first-node entry-list)))
                                       (cond
                                         ((not (null? curr-list-node))
                                           (begin
                                             (hash-internal-simple-put
                                               new-hash-internal
                                               (hash-entry-k (linked-list-node-entry curr-list-node))
                                               (hash-entry-v (linked-list-node-entry curr-list-node))
                                               call-site-info)
                                             (loop-internal (linked-list-node-next-node curr-list-node)))))))))
                               (loop (+ i 1))))))
                       (set-hash-map-hash-internal! hash-map new-hash-internal call-site-info)))))))
            hash-map call-site-info)
          (hash-map-simple-put hash-map key value call-site-info))))))

(define (get-hash-map-size hash-map)
  (hash-internal-size (hash-map-hash-internal hash-map)))

(define (get-hash-map-buckets-capacity hash-map)
  (vector-length (hash-internal-buckets (hash-map-hash-internal hash-map))))

(define (get-hash-map-used-bucket-size hash-map)
  (hash-internal-used-bucket-size (hash-map-hash-internal hash-map)))

(define (hash-map-get hash-map key)
  (letrec ((hash-internal (hash-map-hash-internal hash-map))
            (key-pred-proc-sym (hash-internal-key-pred-proc-sym hash-internal)))
    (cond
      ((not ((eval key-pred-proc-sym) key))
        (error "hash-map-get: Wrong type of the key givened!"))
      (else
        (letrec ((buckets-capacity (get-hash-map-buckets-capacity hash-map))
                  (key-hash (hash key buckets-capacity))
                  (buckets (hash-internal-buckets hash-internal))
                  (entry-list (vector-ref buckets key-hash)))
          (if (null? entry-list)
            '()
            (let loop ((i 0)
                        (curr-list-node (linked-list-first-node entry-list)))
              (cond
                ((not (null? curr-list-node))
                  (if (eq? key (hash-entry-k (linked-list-node-entry curr-list-node)))
                    (hash-entry-v (linked-list-node-entry curr-list-node))
                    (loop (+ i 1) (linked-list-node-next-node curr-list-node))))
                ((and (null? curr-list-node) (= i (get-linked-list-size entry-list)))
                  '())))))))))

(define (hash-map-rm hash-map key call-site-info)
  (letrec ((hash-internal (hash-map-hash-internal hash-map))
            (key-pred-proc-sym (hash-internal-key-pred-proc-sym hash-internal)))
    (cond
      ((not ((eval key-pred-proc-sym) key))
        (error "hash-map-get: Wrong type of the key givened!"))
      (else
        (letrec ((buckets-capacity (get-hash-map-buckets-capacity hash-map))
                  (key-hash (hash key buckets-capacity))
                  (buckets (hash-internal-buckets hash-internal))
                  (entry-list (vector-ref buckets key-hash)))
          (if (null? entry-list)
            #t
            (let ((found-index -1))
              (begin
                (let loop ((i 0)
                            (curr-list-node (linked-list-first-node entry-list)))
                  (cond
                    ((not (null? curr-list-node))
                      (if (eq? key (hash-entry-k (linked-list-node-entry curr-list-node)))
                        (set! found-index i)
                        (loop (+ i 1) (linked-list-node-next-node curr-list-node))))))
                (if (>= found-index 0)
                  (begin
                    (linked-list-rm-ref entry-list found-index call-site-info)
                    (set-hash-internal-size! hash-internal (- (hash-internal-size hash-internal) 1) call-site-info)
                    (cond
                      ((= 0 (get-linked-list-size entry-list))
                        (begin
                          (vector-set! buckets key-hash '())
                          (set-hash-internal-used-bucket-size! hash-internal (- (hash-internal-used-bucket-size hash-internal) 1) call-site-info))))))))))))))

(define (hash-map-print hash-map)
  (let ((hash-internal (hash-map-hash-internal hash-map)))
    (begin
      (display "{")
      (let ((entry-idx 0)
             (map-size (get-hash-map-size hash-map)))
        (let external-loop ((i 0))
          (if (< i (get-hash-map-buckets-capacity hash-map))
            (let ((entry-list (vector-ref (hash-internal-buckets hash-internal) i)))
              (if (not (null? entry-list))
                (let loop ((curr-node (linked-list-first-node entry-list)))
                  (if (not (null? curr-node))
                    (let ((entry (linked-list-node-entry curr-node)))
                      (begin
                        (display (hash-entry-k entry))
                        (display "=")
                        (display (hash-entry-v entry))
                        (if (< entry-idx (- map-size 1))
                          (display ", "))
                        (set! entry-idx (+ entry-idx 1))
                        (loop (linked-list-node-next-node curr-node))))
                    (external-loop (+ i 1))))
                (external-loop (+ i 1)))))))
      (display "}\n"))))

;; ====================== utils for define-data-type ======================
