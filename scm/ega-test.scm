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

(define-data-type 'F0 '(f1 f2))
(define-data-type 'F1 '(f1 f2))
(define-data-type 'F2 '(f1 f2))
(define-data-type 'F3 '(f1 f2))
(define-data-type 'F4 '(f1 f2))
(define-data-type 'F5 '(f1 f2))
(define-data-type 'F6 '(f1 f2))
(define-data-type 'F7 '(f1 f2))
(define-data-type 'F8 '(f1 f2))
(define-data-type 'F9 '(f1 f2))

(define-data-type 'G0 '(f1 f2))
(define-data-type 'G1 '(f1 f2))
(define-data-type 'G2 '(f1 f2))
(define-data-type 'G3 '(f1 f2))
(define-data-type 'G4 '(f1 f2))
(define-data-type 'G5 '(f1 f2))
(define-data-type 'G6 '(f1 f2))
(define-data-type 'G7 '(f1 f2))
(define-data-type 'G8 '(f1 f2))
(define-data-type 'G9 '(f1 f2))

(define-data-type 'H0 '(f1 f2))
(define-data-type 'H1 '(f1 f2))
(define-data-type 'H2 '(f1 f2))
(define-data-type 'H3 '(f1 f2))
(define-data-type 'H4 '(f1 f2))
(define-data-type 'H5 '(f1 f2))
(define-data-type 'H6 '(f1 f2))
(define-data-type 'H7 '(f1 f2))
(define-data-type 'H8 '(f1 f2))
(define-data-type 'H9 '(f1 f2))

(define-data-type 'I0 '(f1 f2))
(define-data-type 'I1 '(f1 f2))
(define-data-type 'I2 '(f1 f2))
(define-data-type 'I3 '(f1 f2))
(define-data-type 'I4 '(f1 f2))
(define-data-type 'I5 '(f1 f2))
(define-data-type 'I6 '(f1 f2))
(define-data-type 'I7 '(f1 f2))
(define-data-type 'I8 '(f1 f2))
(define-data-type 'I9 '(f1 f2))

(define-data-type 'J0 '(f1 f2))
(define-data-type 'J1 '(f1 f2))
(define-data-type 'J2 '(f1 f2))
(define-data-type 'J3 '(f1 f2))
(define-data-type 'J4 '(f1 f2))
(define-data-type 'J5 '(f1 f2))
(define-data-type 'J6 '(f1 f2))
(define-data-type 'J7 '(f1 f2))
(define-data-type 'J8 '(f1 f2))
(define-data-type 'J9 '(f1 f2))

(define-data-type 'K0 '(f1 f2))
(define-data-type 'K1 '(f1 f2))
(define-data-type 'K2 '(f1 f2))
(define-data-type 'K3 '(f1 f2))
(define-data-type 'K4 '(f1 f2))
(define-data-type 'K5 '(f1 f2))
(define-data-type 'K6 '(f1 f2))
(define-data-type 'K7 '(f1 f2))
(define-data-type 'K8 '(f1 f2))
(define-data-type 'K9 '(f1 f2))

(define-data-type 'L0 '(f1 f2))
(define-data-type 'L1 '(f1 f2))
(define-data-type 'L2 '(f1 f2))
(define-data-type 'L3 '(f1 f2))
(define-data-type 'L4 '(f1 f2))
(define-data-type 'L5 '(f1 f2))
(define-data-type 'L6 '(f1 f2))
(define-data-type 'L7 '(f1 f2))
(define-data-type 'L8 '(f1 f2))
(define-data-type 'L9 '(f1 f2))

(define-data-type 'M0 '(f1 f2))
(define-data-type 'M1 '(f1 f2))
(define-data-type 'M2 '(f1 f2))
(define-data-type 'M3 '(f1 f2))
(define-data-type 'M4 '(f1 f2))
(define-data-type 'M5 '(f1 f2))
(define-data-type 'M6 '(f1 f2))
(define-data-type 'M7 '(f1 f2))
(define-data-type 'M8 '(f1 f2))
(define-data-type 'M9 '(f1 f2))

(define-data-type 'N0 '(f1 f2))
(define-data-type 'N1 '(f1 f2))
(define-data-type 'N2 '(f1 f2))
(define-data-type 'N3 '(f1 f2))
(define-data-type 'N4 '(f1 f2))
(define-data-type 'N5 '(f1 f2))
(define-data-type 'N6 '(f1 f2))
(define-data-type 'N7 '(f1 f2))
(define-data-type 'N8 '(f1 f2))
(define-data-type 'N9 '(f1 f2))

(define-data-type 'O0 '(f1 f2))
(define-data-type 'O1 '(f1 f2))
(define-data-type 'O2 '(f1 f2))
(define-data-type 'O3 '(f1 f2))
(define-data-type 'O4 '(f1 f2))
(define-data-type 'O5 '(f1 f2))
(define-data-type 'O6 '(f1 f2))
(define-data-type 'O7 '(f1 f2))
(define-data-type 'O8 '(f1 f2))
(define-data-type 'O9 '(f1 f2))

(define-data-type 'P0 '(f1 f2))
(define-data-type 'P1 '(f1 f2))
(define-data-type 'P2 '(f1 f2))
(define-data-type 'P3 '(f1 f2))
(define-data-type 'P4 '(f1 f2))
(define-data-type 'P5 '(f1 f2))
(define-data-type 'P6 '(f1 f2))
(define-data-type 'P7 '(f1 f2))
(define-data-type 'P8 '(f1 f2))
(define-data-type 'P9 '(f1 f2))

(define-data-type 'Q0 '(f1 f2))
(define-data-type 'Q1 '(f1 f2))
(define-data-type 'Q2 '(f1 f2))
(define-data-type 'Q3 '(f1 f2))
(define-data-type 'Q4 '(f1 f2))
(define-data-type 'Q5 '(f1 f2))
(define-data-type 'Q6 '(f1 f2))
(define-data-type 'Q7 '(f1 f2))
(define-data-type 'Q8 '(f1 f2))
(define-data-type 'Q9 '(f1 f2))

(define-data-type 'R0 '(f1 f2))
(define-data-type 'R1 '(f1 f2))
(define-data-type 'R2 '(f1 f2))
(define-data-type 'R3 '(f1 f2))
(define-data-type 'R4 '(f1 f2))
(define-data-type 'R5 '(f1 f2))
(define-data-type 'R6 '(f1 f2))
(define-data-type 'R7 '(f1 f2))
(define-data-type 'R8 '(f1 f2))
(define-data-type 'R9 '(f1 f2))

(define-data-type 'S0 '(f1 f2))
(define-data-type 'S1 '(f1 f2))
(define-data-type 'S2 '(f1 f2))
(define-data-type 'S3 '(f1 f2))
(define-data-type 'S4 '(f1 f2))
(define-data-type 'S5 '(f1 f2))
(define-data-type 'S6 '(f1 f2))
(define-data-type 'S7 '(f1 f2))
(define-data-type 'S8 '(f1 f2))
(define-data-type 'S9 '(f1 f2))

(define-data-type 'T0 '(f1 f2))
(define-data-type 'T1 '(f1 f2))
(define-data-type 'T2 '(f1 f2))
(define-data-type 'T3 '(f1 f2))
(define-data-type 'T4 '(f1 f2))
(define-data-type 'T5 '(f1 f2))
(define-data-type 'T6 '(f1 f2))
(define-data-type 'T7 '(f1 f2))
(define-data-type 'T8 '(f1 f2))
(define-data-type 'T9 '(f1 f2))

(define-data-type 'LeakPath '(first last len))

(define (newLeakPath first last len callSite)
  (let ((ni (make-LeakPath-not-init)))
    (begin
      (set-LeakPath-first! ni first callSite)
      (set-LeakPath-last! ni last callSite)
      (set-LeakPath-len! ni len callSite)
      ni)))

(define LEAK_PATHS (new-hash-map 'number 'LeakPath 271))

(define (getPathNameByIdx pathIdx)
  (if (or (< pathIdx 0) (>= pathIdx 20))
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
        (hash-map-put LEAK_PATHS pathIdx (newLeakPath newLeakPathNode newLeakPathNode 1 305) 305))
      (let ((currentPathLen (LeakPath-len leakPath)))
        (if (>= currentPathLen 10)
          (let ((newLeakPathNode (createLeakPathNode pathIdx 0)))
            (hash-map-put LEAK_PATHS pathIdx (newLeakPath newLeakPathNode newLeakPathNode 1 309) 309))
          (let ((newLeakPathNode (createLeakPathNode pathIdx currentPathLen)))
            ((getModifierByIdx pathIdx (- currentPathLen 1)) (LeakPath-last leakPath) newLeakPathNode 311)
            (set-LeakPath-last! leakPath newLeakPathNode 312)
            (set-LeakPath-len! leakPath (+ 1 currentPathLen) 313)
            (if (>= (+ 1 currentPathLen) 10)
              (begin
                (set-LeakPath-last! leakPath '() 316)
                ;; (assert-dead newLeakPathNode) ;; baseがアサーションを提供していない
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
    (let ((longLivedTree (newEmptyNode 340)))
      (Populate kLongLivedTreeDepth longLivedTree))

    ;  Start long time running
    (display "A long-time-running phase started\n")
    (do ((start-time (current-time) start-time))
      ((>= (- (current-time) start-time) running-seconds))
      (begin
        (do ((tmpPathIdx 0 (+ tmpPathIdx 1))) ((>= tmpPathIdx 5)) (addPathNode tmpPathIdx))
        (Populate kShortLivedTreeDepth (newEmptyNode 349))
        (do ((tmpPathIdx 5 (+ tmpPathIdx 1))) ((>= tmpPathIdx 10)) (addPathNode tmpPathIdx))
        (tak 20 10 0)
        (do ((tmpPathIdx 10 (+ tmpPathIdx 1))) ((>= tmpPathIdx 15)) (addPathNode tmpPathIdx))
        (MakeTree kShortLivedTreeDepth)
        (do ((tmpPathIdx 15 (+ tmpPathIdx 1))) ((>= tmpPathIdx 20)) (addPathNode tmpPathIdx))))))

(define (main)
  (start-record-exec-cost-time)
  (test-func 18 1800)
  (end-record-exec-cost-time))

(define (loop-main n)
  (do ((i 0 (+ i 1))) ((>= i n)) (main)))
