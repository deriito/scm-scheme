;; Test program based on "eclipse bug #115789"

;; CompareEditorInput.java
(define-data-type 'compare-editor-input '(f-message))

(define (new-compare-editor-input string-message . call-site-info)
  (let ((new-instance (make-compare-editor-input '())))
    (begin
      (set-compare-editor-input-f-message! new-instance string-message call-site-info)
      new-instance)))


;; NavigationHistoryEditorInfo.java
(define-data-type 'navigation-history-editor-info '(editor-id editor-input ref-count))

(define (new-navigation-history-editor-info editor-part . call-site-info)
  (let ((new-instance (make-navigation-history-editor-info '() '() 0)))
    (begin
      (set-navigation-history-editor-info-editor-id! new-instance (editor-part-editor-id editor-part) call-site-info)
      (set-navigation-history-editor-info-editor-input! new-instance (editor-part-editor-input editor-part) call-site-info)
      (set-navigation-history-editor-info-ref-count! new-instance 0 call-site-info)
      new-instance)))

(define (editor-info-handle-part-closed editor-info)
  (begin
    (set-navigation-history-editor-info-editor-id! editor-info '() 26)
    (set-navigation-history-editor-info-editor-input! editor-info '() 27)))


;; NavigationHistoryEntry.java
(define-data-type 'navigation-history-entry '(page editor-info))

(define (new-navigation-history-entry page editor-info . call-site-info)
  (let ((new-instance (make-navigation-history-entry '() '())))
    (begin ;; setterにライトバリアしか実装していないため, fieldの初期化するにはsetterを使う
      (set-navigation-history-entry-page! new-instance page call-site-info)
      (set-navigation-history-entry-editor-info! new-instance editor-info call-site-info)
      new-instance)))

(define (history-entry-dispose navi-history-entry)
  (set-navigation-history-entry-editor-info! navi-history-entry '() 41))


;; NavigationHistory.java
(define-data-type 'navigation-history '(history editors page))

(define history-entry-capacity 50)

(define (new-navigation-history page . call-site-info)
  (let ((new-instance (make-navigation-history '() '() '())))
    (begin
      (set-navigation-history-history! new-instance (new-linked-list 'navigation-history-entry) call-site-info)
      (set-navigation-history-editors! new-instance (new-linked-list 'navigation-history-editor-info) call-site-info)
      (set-navigation-history-page! new-instance page 54)
      new-instance)))

(define (update-navigation-history navi-history editor-part)
  (letrec ((editor-id (editor-part-editor-id editor-part))
            (editor-input (editor-part-editor-input editor-part))
            (editors (navigation-history-editors navi-history))
            (editors-list-size (get-linked-list-size editors))
            (info '())
            (history-entries-to-rm (new-linked-list 'navigation-history-entry 63))
            (history-list (navigation-history-history navi-history))
            (history-list-size (get-linked-list-size history-list)))
    (begin
      (let loop ((i 0))
        (if (< i editors-list-size)
          (if (and
                (equal? editor-id (navigation-history-editor-info-editor-id (linked-list-ref editors i)))
                (eqv? editor-input (navigation-history-editor-info-editor-input (linked-list-ref editors i))))
            (set! info (linked-list-ref editors i))
            (loop (+ i 1)))))
      (if (not (null? info))
        (begin
          ;; (assert-dead info) ;; assert-dead
          (editor-info-handle-part-closed info)))
      (let loop ((i 0))
        (cond
          ((and (not (null? info)) (< i history-list-size))
            (let ((entry (linked-list-ref history-list i)))
              (if (eqv? info (navigation-history-entry-editor-info entry))
                (begin
                  (linked-list-add history-entries-to-rm entry 84)
                  (history-entry-dispose entry) ;; Should be "(dispose-entry navi-history entry)"
                  (loop (+ i 1)))
                (loop (+ i 1)))))))
      (let loop ((i 0))
        (cond
          ((< i (get-linked-list-size history-entries-to-rm))
            (begin
              (linked-list-rm-obj history-list (linked-list-ref history-entries-to-rm i))
              (loop (+ i 1)))))))))

(define (create-entry navi-history page part)
  (let ((editor-id (editor-part-editor-id part))
         (editor-input (editor-part-editor-input part))
         (info '())
         (navi-history-edi-infos (navigation-history-editors navi-history)))
    (begin
      (let loop ((i 0))
        (cond
          ((< i (get-linked-list-size navi-history-edi-infos))
            (begin
              (set! info (linked-list-ref navi-history-edi-infos i))
              ;; (assert-dead info) ;; assert-dead
              (if (and (equal? editor-id (navigation-history-editor-info-editor-id info))
                    (eqv? editor-input (navigation-history-editor-info-editor-input info)))
                (set-navigation-history-editor-info-ref-count! info (+ (navigation-history-editor-info-ref-count info) 1) 109)
                (begin
                  (set! info '())
                  (loop (+ i 1))))))))
      (cond
        ((null? info)
          (begin
            (set! info (new-navigation-history-editor-info part 116))
            (assert-dead info) ;; assert-dead
            (set-navigation-history-editor-info-ref-count! info (+ (navigation-history-editor-info-ref-count info) 1) 118)
            (linked-list-add navi-history-edi-infos info 119))))
      (new-navigation-history-entry page info 120))))

(define (dispose-entry navi-history navi-history-entry)
  (let ((editor-info (navigation-history-entry-editor-info navi-history-entry)))
    (cond
      ((not (null? editor-info))
        (begin
          (set-navigation-history-editor-info-ref-count! editor-info (- (navigation-history-editor-info-ref-count editor-info) 1) 127)
          (cond
            ((<= (navigation-history-editor-info-ref-count editor-info) 0)
              (linked-list-rm-obj (navigation-history-editors navi-history) editor-info)))
          (history-entry-dispose navi-history-entry))))))

(define (do-add navi-history entry)
  (letrec ((entry-list (navigation-history-history navi-history))
            (entry-list-size (get-linked-list-size entry-list)))
    (begin
      (if (>= entry-list-size history-entry-capacity)
        (let ((oldest-entry (linked-list-ref entry-list 0)))
          (begin
            (dispose-entry navi-history oldest-entry)
            (linked-list-rm-ref entry-list 0))))
      (linked-list-add entry-list entry 142))))

(define (add-entry navi-history editor-part)
  (letrec ((page (navigation-history-page navi-history))
            (new-entry (create-entry navi-history page editor-part)))
    (do-add navi-history new-entry)))

(define (mark-editor navi-history editor-part)
  (add-entry navi-history editor-part))


;; EditorPart.java
(define-data-type 'editor-part '(editor-id editor-input))

(define (new-editor-part editor-id . call-site-info)
  (let ((new-instance (make-editor-part '() '())))
    (begin
      (set-editor-part-editor-id! new-instance editor-id call-site-info)
      (set-editor-part-editor-input! new-instance '() call-site-info)
      new-instance)))


;; WorkbenchPage.java
(define-data-type 'work-bench-page '(editors navigation-history))

(define (new-work-bench-page . call-site-info)
  (let ((new-instance (make-work-bench-page '() '())))
    (begin
      (set-work-bench-page-editors! new-instance (new-linked-list 'editor-part) call-site-info)
      (set-work-bench-page-navigation-history! new-instance (new-navigation-history new-instance) call-site-info)
      new-instance)))

(define (open-editor page input editor-id)
  (let ((editors (work-bench-page-editors page))
         (navi-history (work-bench-page-navigation-history page))
         (editor-part '()))
    (begin
      (let loop ((i 0)
                  (list-size (get-linked-list-size editors)))
        (if (< i list-size)
          (if (and
                (equal? editor-id (editor-part-editor-id (linked-list-ref editors i)))
                (eqv? input (editor-part-editor-input (linked-list-ref editors i))))
            (set! editor-part (linked-list-ref editors i))
            (loop (+ i 1) list-size))))
      (if (null? editor-part)
        (begin
          (set! editor-part (new-editor-part editor-id))
          (set-editor-part-editor-input! editor-part input 190)
          (linked-list-add editors editor-part 191)))
      (mark-editor navi-history editor-part))))

(define (try-find-exist-editor-input page input-context)
  (let ((editors (work-bench-page-editors page))
         (found-input '()))
    (begin
      (let loop ((i 0)
                  (list-size (get-linked-list-size editors)))
        (if (< i list-size)
          (if (equal? input-context (compare-editor-input-f-message (editor-part-editor-input (linked-list-ref editors i))))
            (set! found-input (editor-part-editor-input (linked-list-ref editors i)))
            (loop (+ i 1) list-size))))
      found-input)))

(define (close-editor page input editor-id)
  (let ((editors (work-bench-page-editors page))
         (navi-history (work-bench-page-navigation-history page))
         (found-editor-part-idx -1))
    (begin
      (let loop ((i 0)
                  (list-size (get-linked-list-size editors)))
        (if (< i list-size)
          (if (and
                (equal? editor-id (editor-part-editor-id (linked-list-ref editors i)))
                (eq? input (editor-part-editor-input (linked-list-ref editors i))))
            (set! found-editor-part-idx i)
            (loop (+ i 1) list-size))))
      (if (>= found-editor-part-idx 0)
        (let ((found-editor-part (linked-list-ref editors found-editor-part-idx)))
          (begin
            (update-navigation-history navi-history found-editor-part)
            (linked-list-rm-ref editors found-editor-part-idx)))))))


;; CompareUI.java
(define (open-compare-editor-on-page compare-editor-input work-bench-page)
  (open-editor work-bench-page compare-editor-input "compare-editor"))


;; close compare editor
(define (close-compare-editor-on-page compare-editor-input work-bench-page)
  (close-editor work-bench-page compare-editor-input "compare-editor"))

;; CompareAction.java
(define (run-compare work-bench-page input-context)
  (let ((input (try-find-exist-editor-input work-bench-page input-context)))
    (begin
      (if (null? input)
        (set! input (new-compare-editor-input input-context)))
      (open-compare-editor-on-page input work-bench-page)
      (set! input '()) ;; don't reuse this input!
      )))


;; close editor part
(define (stop-compare work-bench-page input-context)
  (let ((input (try-find-exist-editor-input work-bench-page input-context)))
    (if (not (null? input))
      (close-compare-editor-on-page input work-bench-page))))


;; test data
(define input-context-string-list (new-linked-list 'string))
(linked-list-add input-context-string-list "a2b")
(linked-list-add input-context-string-list "a2c")
(linked-list-add input-context-string-list "a2d")
(linked-list-add input-context-string-list "a2e")
(linked-list-add input-context-string-list "a2f")
(linked-list-add input-context-string-list "a2g")
(linked-list-add input-context-string-list "a2h")
(linked-list-add input-context-string-list "a2i")
(linked-list-add input-context-string-list "a2j")
(linked-list-add input-context-string-list "a2k")
(linked-list-add input-context-string-list "a2l")


;; Simulation Running
(define work-bench-page (new-work-bench-page 269))

(let loop ((i 0))
  (if (< i 50)
    (begin
      (run-compare work-bench-page (linked-list-ref input-context-string-list (random-0-n (get-linked-list-size input-context-string-list))))
      (stop-compare work-bench-page (linked-list-ref input-context-string-list (random-0-n (get-linked-list-size input-context-string-list))))
      ;; (gc)
      (loop (+ i 1)))))
