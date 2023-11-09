;; Perfoemance test program on https://www.larcenists.org/benchmarksAboutR6.html

;  This is adapted from a benchmark written by John Ellis and Pete Kovac
;  of Post Communications.
;  It was modified by Hans Boehm of Silicon Graphics.
;  It was translated into Scheme by William D Clinger of Northeastern Univ.
;  Last modified 24 November 2007 (translated into R6RS Scheme).
;
;       This is no substitute for real applications.  No actual application
;       is likely to behave in exactly this way.  However, this benchmark was
;       designed to be more representative of real applications than other
;       Java GC benchmarks of which we are aware.
;       It attempts to model those properties of allocation requests that
;       are important to current GC techniques.
;       It is designed to be used either to obtain a single overall performance
;       number, or to give a more detailed estimate of how collector
;       performance varies with object lifetimes.  It prints the time
;       required to allocate and collect balanced binary trees of various
;       sizes.  Smaller trees result in shorter object lifetimes.  Each cycle
;       allocates roughly the same amount of memory.
;       Two data structures are kept around during the entire process, so
;       that the measured performance is representative of applications
;       that maintain some live in-memory data.  One of these is a tree
;       containing many pointers.  The other is a large array containing
;       double precision floating point numbers.  Both should be of comparable
;       size.
;
;       The results are only really meaningful together with a specification
;       of how much memory was used.  It is possible to trade memory for
;       better time performance.  This benchmark should be run in a 32 MB
;       heap, though we don't currently know how to enforce that uniformly.

; In the Java version, this routine prints the heap size and the amount
; of free memory.  There is no portable way to do this in Scheme; each
; implementation needs its own version.

(define (run-benchmark2 name thunk)
  (display name)
  (newline)
  (thunk))

(define (print-diagnostics)
  (display " Total memory available= ???????? bytes")
  (display "  Free memory= ???????? bytes")
  (newline))

(define-data-type 'class-node '(left right i j))

(define (new-class-node l r . call-site)
  (let ((ni (make-class-node-not-init)))
    (begin
      (set-class-node-left! ni l call-site)
      (set-class-node-right! ni r call-site)
      (set-class-node-i! ni 0 call-site)
      (set-class-node-j! ni 0 call-site)
      ni)))

(define (new-class-node0 . call-site)
  (new-class-node '() '() call-site))

(define k-stretch-tree-depth 18) ;; about 16Mb
(define k-long-lived-tree-depth 16) ;; about 4Mb
;; (define k-array-size 500000) ;; about 4Mb
(define k-array-size 50000) ;; about 0.4Mb
(define k-min-tree-depth 4)
(define k-max-tree-depth 16)

;; Nodes used by a tree of a given size
(define (tree-size i)
  (- (expt 2 (+ i 1)) 1))

;; Number of iterations to use for a given tree depth
(define (numiters i)
  (inexact->exact (floor (/ (* 2 (tree-size k-stretch-tree-depth)) (tree-size i)))))

;; Build tree top down, assigning to older objects.
(define (populate i-depth this-node)
  (if (<= i-depth 0)
    #f
    (let ((i-depth (- i-depth 1)))
      (begin
        (set-class-node-left! this-node (new-class-node0 71) 71)
        (set-class-node-right! this-node (new-class-node0 72) 72)
        (populate i-depth (class-node-left this-node))
        (populate i-depth (class-node-right this-node))))))

;; Build tree bottom-up
(define (make-tree i-depth)
  (if (<= i-depth 0)
    (new-class-node0 79)
    (new-class-node (make-tree (- i-depth 1)) (make-tree (- i-depth 1)) 80)))

(define (time-construction depth)
  (let ((i-numiters (numiters depth)))
    (display (string-append "Creating "
               (number->string i-numiters)
               " trees of depth "
               (number->string depth)))
    (newline)
    (run-benchmark2
      "GCBench: Top down construction"
      (lambda ()
        (do ((i 0 (+ i 1)))
          ((>= i i-numiters))
          (populate depth (new-class-node0 99)))))
    (run-benchmark2
      "GCBench: Bottom up construction"
      (lambda ()
        (do ((i 0 (+ i 1)))
          ((>= i i-numiters))
          (make-tree depth))))))

(define (main)
  (start-record-exec-cost-time)
  (display "Garbage Collector Test")
  (newline)
  (display (string-append
             " Stretching memory with a binary tree of depth "
             (number->string k-stretch-tree-depth)))
  (newline)
  (print-diagnostics)
  (run-benchmark2
    "GCBench: Main"
    (lambda ()
      ; Stretch the memory space quickly
      (make-tree k-stretch-tree-depth)

      ; Create a long lived object
      (display (string-append
                 " Creating a long-lived binary tree of depth "
                 (number->string k-long-lived-tree-depth)))
      (newline)
      (let ((long-lived-tree (new-class-node0 131)))
        (populate k-long-lived-tree-depth long-lived-tree)

        ; Create long-lived list, filling half of it
        (display (string-append
                   " Creating a long-lived array of "
                   (number->string k-array-size)
                   " inexact reals"))
        (newline)
        (let ((lst (new-linked-list 'number 140)))
          (do ((i 0 (+ i 1)))
            ((>= i  (inexact->exact (floor (/ k-array-size 2)))))
            (linked-list-add lst (+ i 1) 143))
          (print-diagnostics)

          (do ((d k-min-tree-depth (+ d 2)))
            ((> d k-max-tree-depth))
            (time-construction d))

          (if (or (eq? long-lived-tree '())
                (let ((n (min 1000
                           (- (get-linked-list-size lst)
                             1))))
                  (not (= (linked-list-ref lst n)
                         (+ n 1)))))
            (begin (display "Failed") (newline)))
          ;  fake reference to LongLivedTree
          ;  and array
          ;  to keep them from being optimized away
          ))))
  (print-diagnostics)
  (end-record-exec-cost-time))
