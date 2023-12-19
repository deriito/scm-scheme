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

(define-data-type 'A0 '(f1 f2))
(define-data-type 'A1 '(f1 f2))
(define-data-type 'A2 '(f1 f2))
(define-data-type 'A3 '(f1 f2))
(define-data-type 'A4 '(f1 f2))
(define-data-type 'A5 '(f1 f2))
(define-data-type 'A6 '(f1 f2))
(define-data-type 'A7 '(f1 f2))
(define-data-type 'A8 '(f1 f2))
(define-data-type 'A9 '(f1 f2))

(define-data-type 'B0 '(f1 f2))
(define-data-type 'B1 '(f1 f2))
(define-data-type 'B2 '(f1 f2))
(define-data-type 'B3 '(f1 f2))
(define-data-type 'B4 '(f1 f2))
(define-data-type 'B5 '(f1 f2))
(define-data-type 'B6 '(f1 f2))
(define-data-type 'B7 '(f1 f2))
(define-data-type 'B8 '(f1 f2))
(define-data-type 'B9 '(f1 f2))

(define-data-type 'C0 '(f1 f2))
(define-data-type 'C1 '(f1 f2))
(define-data-type 'C2 '(f1 f2))
(define-data-type 'C3 '(f1 f2))
(define-data-type 'C4 '(f1 f2))
(define-data-type 'C5 '(f1 f2))
(define-data-type 'C6 '(f1 f2))
(define-data-type 'C7 '(f1 f2))
(define-data-type 'C8 '(f1 f2))
(define-data-type 'C9 '(f1 f2))

(define-data-type 'D0 '(f1 f2))
(define-data-type 'D1 '(f1 f2))
(define-data-type 'D2 '(f1 f2))
(define-data-type 'D3 '(f1 f2))
(define-data-type 'D4 '(f1 f2))
(define-data-type 'D5 '(f1 f2))
(define-data-type 'D6 '(f1 f2))
(define-data-type 'D7 '(f1 f2))
(define-data-type 'D8 '(f1 f2))
(define-data-type 'D9 '(f1 f2))

(define-data-type 'E0 '(f1 f2))
(define-data-type 'E1 '(f1 f2))
(define-data-type 'E2 '(f1 f2))
(define-data-type 'E3 '(f1 f2))
(define-data-type 'E4 '(f1 f2))
(define-data-type 'E5 '(f1 f2))
(define-data-type 'E6 '(f1 f2))
(define-data-type 'E7 '(f1 f2))
(define-data-type 'E8 '(f1 f2))
(define-data-type 'E9 '(f1 f2))

(define-data-type 'LeakPath '(first last len))

(define (newLeakPath first last len callSite)
  (let ((ni (make-LeakPath-not-init)))
    (begin
      (set-LeakPath-first! ni first callSite)
      (set-LeakPath-last! ni last callSite)
      (set-LeakPath-len! ni len callSite)
      ni)))

(define LEAK_PATHS (new-hash-map 'number 'LeakPath 89))

(define (getPathNameByIdx pathIdx)
  (if (or (< pathIdx 0) (>= pathIdx 5))
    (error "Wrong pathIdx!\n")
    (string (integer->char (+ pathIdx 65)))))

(define (createLeakPathNode pathIdx nodeIdx)
  (let* ((pathName (getPathNameByIdx pathIdx))
          (nodeConstructorSym (string->symbol (string-append
                                                 "make-"
                                                 pathName
                                                 (number->string nodeIdx)
                                                 "-not-init"))))
    ((eval nodeConstructorSym))))

(define (getModifierByIdx pathIdx nodeIdx)
  (let* ((pathName (getPathNameByIdx pathIdx))
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
        (hash-map-put LEAK_PATHS pathIdx (newLeakPath newLeakPathNode newLeakPathNode 1 123) 123))
      (let ((currentPathLen (LeakPath-len leakPath)))
        (if (>= currentPathLen 10)
          (let ((newLeakPathNode (createLeakPathNode pathIdx 0)))
            (hash-map-put LEAK_PATHS pathIdx (newLeakPath newLeakPathNode newLeakPathNode 1 127) 127))
          (let ((newLeakPathNode (createLeakPathNode pathIdx currentPathLen)))
            ((getModifierByIdx pathIdx (- currentPathLen 1)) (LeakPath-last leakPath) newLeakPathNode 129)
            (set-LeakPath-last! leakPath newLeakPathNode 130)
            (set-LeakPath-len! leakPath (+ 1 currentPathLen) 131)
            (if (>= (+ 1 currentPathLen) 10)
              (begin
                (set-LeakPath-last! leakPath '() 134)
                (assert-dead newLeakPathNode)
                )
              #t)))))))

(define (test-func kStretchTreeDepth num-of-times)
  (let* ((kLongLivedTreeDepth (- kStretchTreeDepth 2))
          (kMinTreeDepth      4)
          (kMaxTreeDepth      kLongLivedTreeDepth))

    ;  Build tree top down, assigning to older objects.
    (define (Populate iDepth thisNode)
      (if (<= iDepth 0)
        #f
        (let ((iDepth (- iDepth 1)))
          (set-Node-left! thisNode (newEmptyNode 149) 149)
          (set-Node-right! thisNode (newEmptyNode 150) 150)
          (Populate iDepth (Node-left thisNode))
          (Populate iDepth (Node-right thisNode)))))

    ;  Build tree bottom-up
    (define (MakeTree iDepth)
      (if (<= iDepth 0)
        (newEmptyNode 157)
        (newNode (MakeTree (- iDepth 1))
          (MakeTree (- iDepth 1)) 159)))

    ;  Nodes used by a tree of a given size
    (define (TreeSize i)
      (- (expt 2 (+ i 1)) 1))

    ;  Number of iterations to use for a given tree depth
    (define (NumIters i)
      (quotient (* 2 (TreeSize kStretchTreeDepth))
        (TreeSize i)))

    (define (TimeConstruction depth)
      (let ((iNumIters (NumIters depth)))
        (do ((i 0 (+ i 1)))
          ((>= i iNumIters))
          (Populate depth (newEmptyNode 174)))
        (do ((i 0 (+ i 1)))
          ((>= i iNumIters))
          (MakeTree depth))))

    ;  Stretch the memory space quickly
    (MakeTree kStretchTreeDepth)

    ;  Create a long lived object
    (display (string-append
               " Creating a long-lived binary tree of depth "
               (number->string kLongLivedTreeDepth)
               "\n"))
    (let ((longLivedTree (newEmptyNode 187)))
      (Populate kLongLivedTreeDepth longLivedTree))

    ;  Start long time running
    (display "A long-time-running phase started\n")
    (do ((ntimes 0 (+ ntimes 1)))
      ((>= ntimes num-of-times))
      (begin
        (do ((tmpPathIdx 0 (+ tmpPathIdx 1)))
          ((>= tmpPathIdx 5))
          (addPathNode tmpPathIdx))
        (do ((d kMinTreeDepth (+ d 2)))
          ((> d kMaxTreeDepth))
          (TimeConstruction d))))))

(define (main)
  (start-record-exec-cost-time)
  (test-func 18 1200)
  (end-record-exec-cost-time))

(define (loop-main n)
  (do ((i 0 (+ i 1))) ((>= i n)) (main)))
