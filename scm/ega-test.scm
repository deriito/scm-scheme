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

(define-data-type 'LeakPath '(first last len))

(define (newLeakPath first last len callSite)
  (let ((ni (make-LeakPath-not-init)))
    (begin
      (set-LeakPath-first! ni first callSite)
      (set-LeakPath-last! ni last callSite)
      (set-LeakPath-len! ni len callSite)
      ni)))

(define LEAK_PATHS (new-hash-map 'number 'LeakPath 84))

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
        (hash-map-put LEAK_PATHS pathIdx (newLeakPath newLeakPathNode newLeakPathNode 1 129) 129))
      (let ((currentPathLen (LeakPath-len leakPath)))
        (if (>= currentPathLen 10)
          (let ((newLeakPathNode (createLeakPathNode pathIdx 0)))
            (hash-map-put LEAK_PATHS pathIdx (newLeakPath newLeakPathNode newLeakPathNode 1 133) 133))
          (let ((newLeakPathNode (createLeakPathNode pathIdx currentPathLen)))
            ((getModifierByIdx pathIdx (- currentPathLen 1)) (LeakPath-last leakPath) newLeakPathNode 135)
            (set-LeakPath-last! leakPath newLeakPathNode 136)
            (set-LeakPath-len! leakPath (+ 1 currentPathLen) 137)
            (if (>= (+ 1 currentPathLen) 10)
              (begin
                (set-LeakPath-last! leakPath '() 140)
                (assert-dead newLeakPathNode)
                )
              #t)))))))

(define (tak x y z)
  (cond
    ((<= x y)
      z)
    (else
      (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))

(define (test-func kStretchTreeDepth running-seconds)
  (let ((kLongLivedTreeDepth (- kStretchTreeDepth 2))
         (kShortLivedTreeDepth (- kStretchTreeDepth 12)))

    ;  Stretch the memory space quickly
    (MakeTree kStretchTreeDepth)

    ;  Create a long lived object
    (display (string-append
               " Creating a long-lived binary tree of depth "
               (number->string kLongLivedTreeDepth)
               "\n"))
    (let ((longLivedTree (newEmptyNode 164)))
      (Populate kLongLivedTreeDepth longLivedTree))

    ;  Start long time running
    (display "A long-time-running phase started\n")
    (do ((start-time (current-time) start-time))
      ((>= (- (current-time) start-time) running-seconds))
      (begin
        (addPathNode 0)
        (addPathNode 1)
        (addPathNode 2)
        (Populate kShortLivedTreeDepth (newEmptyNode 174))
        (tak 20 10 0)
        (MakeTree kShortLivedTreeDepth)))))

(define (main)
  (start-record-exec-cost-time)
  (test-func 18 120)
  (end-record-exec-cost-time))
