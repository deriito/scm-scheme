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
                        (c-data-type-accessor obj ,i)
                        (error "wrong type of obj")))))
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
                                        "!"))))))))
        (begin
          (eval `(define ,procname
                    (lambda (obj value)
                      (if (,(gen-predicate-name type-name) obj)
                        (c-data-type-modifier obj ,i value)
                        (error "wrong type of obj")))))
          (loop (+ i 1))))
      #t)))
