;; Test program based on "lusearch" in dapaco benchmark 2006-10-MR2

;; TermInfo.java
(define-data-type 'term-info '(doc-freq freq-pointer prox-pointer skip-offset))

(define (new-term-info call-site)
  (let ((new-instance (make-term-info-not-init)))
    (begin
      (set-term-info-doc-freq! new-instance 0 call-site)
      (set-term-info-freq-pointer! new-instance 0 call-site)
      (set-term-info-prox-pointer! new-instance 0 call-site)
      (set-term-info-skip-offset! new-instance 0 call-site)
      new-instance)))

(define (new-term-info-by-old-term-info ti call-site)
  (let ((new-instance (make-term-info-not-init)))
    (begin
      (set-term-info-doc-freq! new-instance (term-info-doc-freq ti) call-site)
      (set-term-info-freq-pointer! new-instance (term-info-freq-pointer ti) call-site)
      (set-term-info-freq-pointer! new-instance (term-info-prox-pointer ti) call-site)
      (set-term-info-skip-offset! new-instance (term-info-skip-offset ti) call-site)
      new-instance)))

;; SegmentTermEnum.java
(define-data-type 'segment-term-enum '(term-info))

(define (new-segment-term-enum call-site)
  (let ((ni (make-segment-term-enum-not-init)))
    (begin
      (set-segment-term-enum-term-info! ni (new-term-info call-site) call-site)
      ni)))

(define (segment-term-enum-clone ste call-site)
  (let ((clone '())
         (nti (new-term-info-by-old-term-info (segment-term-enum-term-info ste) 35)))
    (begin
      (set! clone (new-segment-term-enum 37))
      (set-segment-term-enum-term-info! clone nti 38)
      (assert-dead nti)
      clone)))

(define thread-local-hash (new-hash-map 'number 'thread-local 42))

;; ThreadLocal.java  ;; 这里当作一个普通的类来看待不考虑多线程
(define-data-type 'thread-local '(cache))

(define (new-thread-local thread-id call-site)
  (let ((ni (hash-map-get thread-local-hash thread-id)))
    (if (null? ni)
      (begin
        (set! ni (make-thread-local-not-init))
        (hash-map-put thread-local-hash thread-id ni call-site)
        (set-thread-local-cache! ni '() call-site)
        ni)
      ni)))

(define (thread-local-get tl)
  (thread-local-cache tl))

(define (thread-local-set tl obj call-site)
  (set-thread-local-cache! tl obj call-site))

;; TermInfosReader.java
(define-data-type 'term-infos-reader '(enumerators orig-enum))

(define (new-term-infos-reader thread-id call-site)
  (let ((ni (make-term-infos-reader-not-init)))
    (begin
      (set-term-infos-reader-enumerators! ni (new-thread-local thread-id call-site) call-site)
      (set-term-infos-reader-orig-enum! ni (new-segment-term-enum call-site) call-site)
      ni)))

(define (terms term-infos-reader)
  (segment-term-enum-clone (term-infos-reader-orig-enum term-infos-reader) 65))

(define (get-enum term-infos-reader)
  (let* ((enumerators (term-infos-reader-enumerators term-infos-reader))
          (term-enum (thread-local-get enumerators)))
    (begin
      (cond ((null? term-enum)
              (begin
                (set! term-enum (terms term-infos-reader))
                (thread-local-set enumerators term-enum 83))))
      term-enum)))

(define (scan-enum term-infos-reader term)
  (let ((enumerator (get-enum term-infos-reader)))
    (segment-term-enum-term-info enumerator))) ;; 省略扫描过程

(define (term-infos-reader-get tir term)
  (let ((enumerator (get-enum tir)))
    (scan-enum tir term)))

;; IndexSearcher.java
(define-data-type 'index-searcher '(reader close-reader similarity))

(define (new-index-searcher reader call-site)
  (let ((ni (make-index-searcher-not-init)))
    (begin
      (set-index-searcher-reader! ni reader call-site)
      (set-index-searcher-close-reader! ni #f call-site)
      (set-index-searcher-similarity! ni (get-default-similarity) call-site)
      ni)))

(define (search0 searcher query filter)
  (new-hits searcher query filter 106))

(define (search searcher query)
  (search0 searcher query '()))

(define (searcher-get-similarity searcher)
  (index-searcher-similarity searcher))

(define (searcher-doc-freq searcher term)
  (reader-doc-freq (index-searcher-reader searcher) term))

(define (searcher-max-doc searcher)
  (reader-max-doc (index-searcher-reader searcher)))

;; Hits.java
(define-data-type 'hits '(weight searcher filter sort len))

(define (new-hits s q f call-site)
  (let ((ni (make-hits-not-init)))
    (begin
      (set-hits-weight! ni (query-weight q s) call-site)
      (set-hits-searcher! ni s call-site)
      (set-hits-filter! ni f call-site)
      (set-hits-len! ni 0 call-site)
      ni)))

;; TermQuery.java
(define-data-type 'term-query '(term))

(define (new-term-query t call-site)
  (let ((ni (make-term-query-not-init)))
    (begin
      (set-term-query-term! ni t call-site)
      ni)))

(define (create-weight query searcher)
  (new-term-weight query searcher 142))

(define (query-weight query searcher)
  (let ((weight (create-weight query searcher)))
    (begin
      weight)))

(define (query-get-similarity searcher)
  (searcher-get-similarity searcher))

;; class TermWeight in TermQuery.java
(define-data-type 'term-weight '(similarity value idf query-norm query-weight))

(define (new-term-weight query searcher call-site)
  (let ((ni (make-term-weight-not-init)))
    (begin
      (set-term-weight-similarity! ni (query-get-similarity searcher) call-site)
      (set-term-weight-idf! ni (similarity-idf (term-query-term query) searcher) call-site)
      (set-term-weight-value! ni 0 call-site)
      (set-term-weight-query-norm! ni 0 call-site)
      (set-term-weight-query-weight! ni 0 call-site)
      ni)))

;; DefaultSimilarity.java
(define-data-type 'default-similarity '(norm-table))

(define (new-default-similarity call-site)
  (let ((ni (make-default-similarity-not-init)))
    (begin
      (set-default-similarity-norm-table! ni '() call-site)
      ni)))

(define (get-default-similarity)
  (new-default-similarity 175))

(define (default-similarity-idf doc-freq num-docs)
  (+ doc-freq num-docs)) ;; 这里的算法和原来的文件并不一样只是临时的

(define (similarity-idf term searcher)
  (default-similarity-idf (searcher-doc-freq searcher term) (searcher-max-doc searcher)))

;; Term.java
(define-data-type 'term '(field text))

(define (new-term field text call-site)
  (let ((ni (make-term-not-init)))
    (begin
      (set-term-field! ni field call-site)
      (set-term-text! ni text call-site)
      ni)))

;; SegmentReader.java
(define-data-type 'segment-reader '(term-infos-reader))

(define (new-segment-reader thread-id call-site)
  (let ((ni (make-segment-reader-not-init)))
    (begin
      (set-segment-reader-term-infos-reader! ni (new-term-infos-reader thread-id call-site) call-site)
      ni)))

(define (reader-doc-freq reader term)
  (let ((ti (term-infos-reader-get (segment-reader-term-infos-reader reader) term)))
    (if (not (null? ti))
      (term-info-doc-freq ti)
      0)))

(define (reader-max-doc reader)
  100)


;; LusearchHarness.java in dacapobench
(define (run-query thread-id)
  (let* ((searcher
           (new-index-searcher (new-segment-reader thread-id 215) 215))
          (query
           (new-term-query
             (new-term
               (linked-list-ref test-data (random-0-n (get-linked-list-size test-data)))
               (linked-list-ref test-data (random-0-n (get-linked-list-size test-data)))
               221) 221)))
    (begin
      (search searcher query))))

;; test data
(define test-data (new-linked-list 'string 226))
(linked-list-add test-data "a2b" 227)
(linked-list-add test-data "a2c" 228)
(linked-list-add test-data "a2d" 229)
(linked-list-add test-data "a2e" 230)
(linked-list-add test-data "a2f" 231)
(linked-list-add test-data "a2g" 232)
(linked-list-add test-data "a2h" 233)
(linked-list-add test-data "a2i" 234)
(linked-list-add test-data "a2j" 235)
(linked-list-add test-data "a2k" 236)
(linked-list-add test-data "a2l" 237)

(define (run)
  (let loop ((i 0))
    (cond
      ((< i 100)
        (begin
          (run-query i)
          (gc)
          (loop (+ i 1)))))))
