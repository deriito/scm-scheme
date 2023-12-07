(define-data-type 'Node '(left right i j))

(define (newNode l r callSite)
  (let ((ni (make-Node-not-init)))
    (begin
      (set-Node-left! ni l callSite)
      (set-Node-right! ni r callSite)
      (set-Node-i! ni 0 callSite)
      (set-Node-j! ni 0 callSite)
      ni)))

(define (newNode l r callSite)
  (let ((ni (make-Node-not-init)))
    (begin
      (set-Node-left! ni l callSite)
      (set-Node-right! ni r callSite)
      (set-Node-i! ni 0 callSite)
      (set-Node-j! ni 0 callSite)
      ni)))

(define (newEmptyNode callSite)
  (newNode '() '() callSite))

;  Build tree top down, assigning to older objects.
(define (Populate iDepth thisNode)
  (if (<= iDepth 0)
    #f
    (let ((iDepth (- iDepth 1)))
      (set-Node-left! thisNode (newEmptyNode 29) 29)
      (set-Node-right! thisNode (newEmptyNode 30) 30)
      (Populate iDepth (Node-left thisNode))
      (Populate iDepth (Node-right thisNode)))))

;  Build tree bottom-up
(define (MakeTree iDepth)
  (if (<= iDepth 0)
    (newEmptyNode 37)
    (newNode (MakeTree (- iDepth 1))
      (MakeTree (- iDepth 1)) 39)))

(define-data-type 'A0 '(f1 f2))

(define (newA0 f1 f2 callSite)
  (let ((ni (make-A0-not-init)))
    (begin
      (set-A0-f1! ni f1 callSite)
      (set-A0-f2! ni f2 callSite)
      ni)))

(define-data-type 'A1 '(f1 f2))

(define (newA1 f1 f2 callSite)
  (let ((ni (make-A1-not-init)))
    (begin
      (set-A1-f1! ni f1 callSite)
      (set-A1-f2! ni f2 callSite)
      ni)))

(define-data-type 'A2 '(f1 f2))

(define (newA2 f1 f2 callSite)
  (let ((ni (make-A2-not-init)))
    (begin
      (set-A2-f1! ni f1 callSite)
      (set-A2-f2! ni f2 callSite)
      ni)))

(define-data-type 'A3 '(f1 f2))

(define (newA3 f1 f2 callSite)
  (let ((ni (make-A3-not-init)))
    (begin
      (set-A3-f1! ni f1 callSite)
      (set-A3-f2! ni f2 callSite)
      ni)))

(define-data-type 'A4 '(f1 f2))

(define (newA4 f1 f2 callSite)
  (let ((ni (make-A4-not-init)))
    (begin
      (set-A4-f1! ni f1 callSite)
      (set-A4-f2! ni f2 callSite)
      ni)))

(define-data-type 'A5 '(f1 f2))

(define (newA5 f1 f2 callSite)
  (let ((ni (make-A5-not-init)))
    (begin
      (set-A5-f1! ni f1 callSite)
      (set-A5-f2! ni f2 callSite)
      ni)))

(define-data-type 'A6 '(f1 f2))

(define (newA6 f1 f2 callSite)
  (let ((ni (make-A6-not-init)))
    (begin
      (set-A6-f1! ni f1 callSite)
      (set-A6-f2! ni f2 callSite)
      ni)))

(define-data-type 'A7 '(f1 f2))

(define (newA7 f1 f2 callSite)
  (let ((ni (make-A7-not-init)))
    (begin
      (set-A7-f1! ni f1 callSite)
      (set-A7-f2! ni f2 callSite)
      ni)))

(define-data-type 'A8 '(f1 f2))

(define (newA8 f1 f2 callSite)
  (let ((ni (make-A8-not-init)))
    (begin
      (set-A8-f1! ni f1 callSite)
      (set-A8-f2! ni f2 callSite)
      ni)))

(define-data-type 'A9 '(f1 f2))

(define (newA9 f1 f2 callSite)
  (let ((ni (make-A9-not-init)))
    (begin
      (set-A9-f1! ni f1 callSite)
      (set-A9-f2! ni f2 callSite)
      ni)))

(define-data-type 'B0 '(f1 f2))

(define (newB0 f1 f2 callSite)
  (let ((ni (make-B0-not-init)))
    (begin
      (set-B0-f1! ni f1 callSite)
      (set-B0-f2! ni f2 callSite)
      ni)))

(define-data-type 'B1 '(f1 f2))

(define (newB1 f1 f2 callSite)
  (let ((ni (make-B1-not-init)))
    (begin
      (set-B1-f1! ni f1 callSite)
      (set-B1-f2! ni f2 callSite)
      ni)))

(define-data-type 'B2 '(f1 f2))

(define (newB2 f1 f2 callSite)
  (let ((ni (make-B2-not-init)))
    (begin
      (set-B2-f1! ni f1 callSite)
      (set-B2-f2! ni f2 callSite)
      ni)))

(define-data-type 'B3 '(f1 f2))

(define (newB3 f1 f2 callSite)
  (let ((ni (make-B3-not-init)))
    (begin
      (set-B3-f1! ni f1 callSite)
      (set-B3-f2! ni f2 callSite)
      ni)))

(define-data-type 'B4 '(f1 f2))

(define (newB4 f1 f2 callSite)
  (let ((ni (make-B4-not-init)))
    (begin
      (set-B4-f1! ni f1 callSite)
      (set-B4-f2! ni f2 callSite)
      ni)))

(define-data-type 'B5 '(f1 f2))

(define (newB5 f1 f2 callSite)
  (let ((ni (make-B5-not-init)))
    (begin
      (set-B5-f1! ni f1 callSite)
      (set-B5-f2! ni f2 callSite)
      ni)))

(define-data-type 'B6 '(f1 f2))

(define (newB6 f1 f2 callSite)
  (let ((ni (make-B6-not-init)))
    (begin
      (set-B6-f1! ni f1 callSite)
      (set-B6-f2! ni f2 callSite)
      ni)))

(define-data-type 'B7 '(f1 f2))

(define (newB7 f1 f2 callSite)
  (let ((ni (make-B7-not-init)))
    (begin
      (set-B7-f1! ni f1 callSite)
      (set-B7-f2! ni f2 callSite)
      ni)))

(define-data-type 'B8 '(f1 f2))

(define (newB8 f1 f2 callSite)
  (let ((ni (make-B8-not-init)))
    (begin
      (set-B8-f1! ni f1 callSite)
      (set-B8-f2! ni f2 callSite)
      ni)))

(define-data-type 'B9 '(f1 f2))

(define (newB9 f1 f2 callSite)
  (let ((ni (make-B9-not-init)))
    (begin
      (set-B9-f1! ni f1 callSite)
      (set-B9-f2! ni f2 callSite)
      ni)))

(define-data-type 'C0 '(f1 f2))

(define (newC0 f1 f2 callSite)
  (let ((ni (make-C0-not-init)))
    (begin
      (set-C0-f1! ni f1 callSite)
      (set-C0-f2! ni f2 callSite)
      ni)))

(define-data-type 'C1 '(f1 f2))

(define (newC1 f1 f2 callSite)
  (let ((ni (make-C1-not-init)))
    (begin
      (set-C1-f1! ni f1 callSite)
      (set-C1-f2! ni f2 callSite)
      ni)))

(define-data-type 'C2 '(f1 f2))

(define (newC2 f1 f2 callSite)
  (let ((ni (make-C2-not-init)))
    (begin
      (set-C2-f1! ni f1 callSite)
      (set-C2-f2! ni f2 callSite)
      ni)))

(define-data-type 'C3 '(f1 f2))

(define (newC3 f1 f2 callSite)
  (let ((ni (make-C3-not-init)))
    (begin
      (set-C3-f1! ni f1 callSite)
      (set-C3-f2! ni f2 callSite)
      ni)))

(define-data-type 'C4 '(f1 f2))

(define (newC4 f1 f2 callSite)
  (let ((ni (make-C4-not-init)))
    (begin
      (set-C4-f1! ni f1 callSite)
      (set-C4-f2! ni f2 callSite)
      ni)))

(define-data-type 'C5 '(f1 f2))

(define (newC5 f1 f2 callSite)
  (let ((ni (make-C5-not-init)))
    (begin
      (set-C5-f1! ni f1 callSite)
      (set-C5-f2! ni f2 callSite)
      ni)))

(define-data-type 'C6 '(f1 f2))

(define (newC6 f1 f2 callSite)
  (let ((ni (make-C6-not-init)))
    (begin
      (set-C6-f1! ni f1 callSite)
      (set-C6-f2! ni f2 callSite)
      ni)))

(define-data-type 'C7 '(f1 f2))

(define (newC7 f1 f2 callSite)
  (let ((ni (make-C7-not-init)))
    (begin
      (set-C7-f1! ni f1 callSite)
      (set-C7-f2! ni f2 callSite)
      ni)))

(define-data-type 'C8 '(f1 f2))

(define (newC8 f1 f2 callSite)
  (let ((ni (make-C8-not-init)))
    (begin
      (set-C8-f1! ni f1 callSite)
      (set-C8-f2! ni f2 callSite)
      ni)))

(define-data-type 'C9 '(f1 f2))

(define (newC9 f1 f2 callSite)
  (let ((ni (make-C9-not-init)))
    (begin
      (set-C9-f1! ni f1 callSite)
      (set-C9-f2! ni f2 callSite)
      ni)))

(define-data-type 'LeakPath '(first last len))

(define (newLeakPath first last len callSite)
  (let ((ni (make-LeakPath-not-init)))
    (begin
      (set-LeakPath-first! ni first callSite)
      (set-LeakPath-last! ni last callSite)
      (set-LeakPath-len! ni len callSite)
      ni)))

(define LEAK_PATHS (new-hash-map 'number 'LeakPath 321))

(define (createLeakPathNode pathIdx nodeIdx)
  (let* ((pathName (cond
                    ((= pathIdx 0)
                      "A")
                    ((= pathIdx 1)
                      "B")
                    ((= pathIdx 2)
                      "C")
                     (else
                       (error "Wrong nodexIdx!"))))
          (nodeConstructorSym (string->symbol (string-append
                                                 "make-"
                                                 pathName
                                                 (number->string nodeIdx)
                                                 "-not-init"))))
    ((eval nodeConstructorSym))))

(define (getModifierByIdx pathIdx nodeIdx)
  (let* ((pathName (cond
                     ((= pathIdx 0)
                       "A")
                     ((= pathIdx 1)
                       "B")
                     ((= pathIdx 2)
                       "C")
                     (else
                       (error "Wrong nodexIdx!"))))
          (fieldName (if (even? nodeIdx)
                       "f1"
                       "f2"))
          (nodeModifierSym (string->symbol (string-append
                                                 "set-"
                                                 pathName
                                                 (number->string nodeIdx)
                                                 "-"
                                                 fieldName
                                                 "!"))))
    (eval nodeModifierSym)))

(define (addPathNode pathIdx)
  (let ((leakPath (hash-map-get LEAK_PATHS pathIdx)))
    (if (null? leakPath)
      (let ((newLeakPathNode (createLeakPathNode pathIdx 0)))
        (hash-map-put LEAK_PATHS pathIdx (newLeakPath newLeakPathNode newLeakPathNode 1 342) 342))
      (let ((currentPathLen (LeakPath-len leakPath)))
        (if (>= currentPathLen 10)
          (let ((newLeakPathNode (createLeakPathNode pathIdx 0)))
            (hash-map-put LEAK_PATHS pathIdx (newLeakPath newLeakPathNode newLeakPathNode 1 370) 370))
          (let ((newLeakPathNode (createLeakPathNode pathIdx currentPathLen)))
            ((getModifierByIdx pathIdx (- currentPathLen 1)) (LeakPath-last leakPath) newLeakPathNode 372)
            (set-LeakPath-last! leakPath newLeakPathNode 373)
            (set-LeakPath-len! leakPath (+ 1 currentPathLen) 374)
            (if (>= (+ 1 currentPathLen) 10)
              (begin
                (set-LeakPath-last! leakPath '() 377)
                (assert-dead newLeakPathNode))
              #t)))))))

(define (tak x y z)
  (cond
    ((<= x y)
      z)
    (else
      (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))

(define (test-func)
  (do ((start-time (current-time) start-time))
    ((>= (- (current-time) start-time) 3600))
    (begin
      (addPathNode 0)
      (addPathNode 1)
      (addPathNode 2)
      (tak 20 10 0))))

(define (main)
  (start-record-exec-cost-time)
  (test-func)
  (end-record-exec-cost-time))


