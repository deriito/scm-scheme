;; Test program based on "eclipse bug #115789"

;; CompareEditorInput.java
(define-data-type 'compare-editor-input '(f-message))

(define (new-compare-editor-input string-message)
  (let ((new-instance (make-compare-editor-input '())))
    (begin
      (set-compare-editor-input-string-message! new-instance string-message)
      new-instance)))


;; NavigationHistoryEditorInfo.java
(define-data-type 'navigation-history-editor-info '(editor-id editor-input ref-count))

(define (new-navigation-history-editor-info editor-part)
  (let ((new-instance (make-navigation-history-editor-info '() '() 0)))
    (begin
      (set-navigation-history-editor-info-editor-id! new-instance (editor-part-editor-id editor-part))
      (set-navigation-history-editor-info-editor-input! new-instance (editor-part-editor-input editor-part))
      (set-navigation-history-editor-info-ref-count! new-instance 0)
      new-instance)))

(define (editor-info-handle-part-closed editor-info)
  (begin
    (set-navigation-history-editor-info-editor-id! editor-info '())
    (set-navigation-history-editor-info-editor-input! editor-info '())))


;; NavigationHistoryEntry.java
(define-data-type 'navigation-history-entry '(page editor-info))

(define (new-navigation-history-entry page editor-info)
  (let ((new-instance (make-navigation-history-entry '() '())))
    (begin ;; setterにライトバリアしか実装していないため, fieldの初期化するにはsetterを使う
      (set-navigation-history-entry-page! new-instance page)
      (set-navigation-history-entry-editor-info! new-instance editor-info)
      new-instance)))

(define (history-entry-dispose navi-history-entry)
  (set-navigation-history-entry-editor-info! navi-history-entry '()))


;; NavigationHistory.java
(define-data-type 'navigation-history '(history editors page))

(define history-entry-capacity 50)

(define (new-navigation-history page)
  (let ((new-instance (make-navigation-history '() '() '())))
    (begin
      (set-navigation-history-history! new-instance (new-array-list 'navigation-history-entry))
      (set-navigation-history-editors! new-instance (new-array-list 'navigation-history-editor-info))
      (set-navigation-history-page! new-instance page)
      new-instance)))

(define (update-navigation-history navi-history editor-part)
  (letrec ((editor-id (editor-part-editor-id editor-part))
            (editor-input (editor-part-editor-input editor-part))
            (editors (navigation-history-editors navi-history))
            (editors-list-size (get-array-list-size editors))
            (info '())
            (history-entry-idxs-to-rm (new-array-list 'number))
            (history-list (navigation-history-history navi-history))
            (history-list-size (get-array-list-size history-list)))
    (begin
      (let loop ((i 0))
        (if (< i editors-list-size)
          (if (and
                (equal? editor-id (navigation-history-editor-info-editor-id (array-list-ref editors i)))
                (eqv? editor-input (navigation-history-editor-info-editor-input (array-list-ref editors i))))
            (set! info (array-list-ref editors i))
            (loop (+ i 1)))))
      (if (not (null? info))
        (editor-info-handle-part-closed info))
      (let loop ((i 0))
        (cond
          ((< i history-list-size)
            (let ((entry (array-list-get history-list i)))
              (cond
                ((eqv? info (navigation-history-entry-editor-info entry))
                  (begin
                    (array-list-add history-entry-idx-to-rm i)
                    (history-entry-dispose entry) ;; Should be "(dispose-entry navi-history entry)"
                    (loop (+ i 1))))
                (else
                  (loop (+ i 1))))))))
      (let loop ((i 0))
        (cond
          ((< i (get-array-list-size history-entry-idxs-to-rm))
            (begin
              (array-list-rm-ref history-list (array-list-get history-entry-idxs-to-rm i))
              (loop (+ i 1)))))))))

(define (create-entry navi-history page part)
  (let ((editor-id (editor-part-editor-id part))
         (editor-input (editor-part-editor-input part))
         (info '())
         (navi-history-edi-infos (navigation-history-editors navi-history)))
    (begin
      (let loop ((i 0))
        (cond
          ((< i (get-array-list-size navi-history-edi-infos))
            (begin
              (set! info (array-list-ref navi-history-edi-infos i))
              (if (and (equal? editor-id (navigation-history-editor-info-editor-id info))
                    (eqv? editor-input (navigation-history-editor-info-editor-input info)))
                (set-navigation-history-editor-info-ref-count! info (+ (navigation-history-editor-info-ref-count info) 1))
                (begin
                  (set! info '())
                  (loop (+ i 1))))))))
      (cond
        ((null? info)
          (begin
            (set! info (new-navigation-history-editor-info part))
            (set-navigation-history-editor-info-ref-count! info (+ (navigation-history-editor-info-ref-count info) 1))
            (array-list-add navi-history-edi-infos info))))
      (new-navigation-history-entry page info))))

(define (dispose-entry navi-history navi-history-entry)
  (let ((editor-info (navigation-history-entry-editor-info navi-history-entry)))
    (cond
      ((not (null? editor-info))
        (begin
          (set-navigation-history-editor-info-ref-count! editor-info (- (navigation-history-editor-info-ref-count editor-info) 1))
          (cond
            ((<= (navigation-history-editor-info-ref-count editor-info) 0)
              (array-list-rm-obj (navigation-history-editors navi-history) editor-info)))
          (history-entry-dispose navi-history-entry))))))

(define (do-add navi-history entry)
  (letrec ((entry-list (navigation-history-history navi-history))
            (entry-list-size (get-array-list-size entry-list)))
    (begin
      (if (>= entry-list-size history-entry-capacity)
        (let ((oldest-entry (array-list-ref entry-list 0)))
          (begin
            (dispose-entry navi-history oldest-entry)
            (array-list-rm-ref entry-list 0))))
      (array-list-add entry-list entry))))

(define (add-entry navi-history editor-part)
  (letrec ((page (navigation-history-page navi-history))
            (new-entry (create-entry navi-history page editor-part)))
    (do-add navi-history new-entry)))

(define (mark-editor navi-history editor-part)
  (add-entry navi-history editor-part))


;; EditorPart.java
(define-data-type 'editor-part '(editor-id editor-input))

(define (new-editor-part editor-id)
  (let ((new-instance (make-editor-part '() '())))
    (begin
      (set-editor-part-editor-id! new-instance editor-id)
      (set-editor-part-editor-input! new-instance '())
      new-instance)))


;; WorkbenchPage.java
(define-data-type 'work-bench-page '(editors navigation-history))

(define (new-work-bench-page)
  (let ((new-instance (make-work-bench-page '() '())))
    (begin
      (set-work-bench-page-editors! new-instance (new-array-list 'editor-part))
      (set-work-bench-page-navigation-history! new-instance (new-navigation-history new-instance))
      new-instance)))

(define (open-editor page input editor-id)
  (let ((editors (work-bench-page-editors page))
         (navi-history (work-bench-page-navigation-history page))
         (editor-part '()))
    (begin
      (let loop ((i 0)
                  (list-size (get-array-list-size editors)))
        (if (< i list-size)
          (if (and
                (equal? editor-id (editor-part-editor-id (array-list-ref editors i)))
                (eq? input (editor-part-editor-input (array-list-ref editors i))))
            (set! editor-part (array-list-ref editors i))
            (loop (+ i 1) list-size))))
      (if (null? editor-part)
        (begin
          (set! editor-part (new-editor-part editor-id))
          (set-editor-part-editor-input! editor-part input)
          (array-list-add editors editor-part)))
      (mark-editor navi-history editor-part))))

(define (try-find-exist-editor-input page input-context)
  (let ((editors (work-bench-page-editors page))
         (found-input '()))
    (begin
      (let loop ((i 0)
                  (list-size (get-array-list-size editors)))
        (if (< i list-size)
          (if (equal? input-context (compare-editor-input-f-message (editor-part-editor-input (array-list-ref editors i))))
            (set! found-input (editor-part-editor-input (array-list-ref editors i)))
            (loop (+ i 1) list-size))))
      found-input)))

(define (close-editor page input editor-id)
  (let ((editors (work-bench-page-editors page))
         (navi-history (work-bench-page-navigation-history page))
         (found-editor-part-idx -1))
    (begin
      (let loop ((i 0)
                  (list-size (get-array-list-size editors)))
        (if (< i list-size)
          (if (and
                (equal? editor-id (editor-part-editor-id (array-list-ref editors i)))
                (eq? input (editor-part-editor-input (array-list-ref editors i))))
            (set! found-editor-part-idx i)
            (loop (+ i 1) list-size))))
      (if (>= found-editor-part-idx 0)
        (let ((found-editor-part (array-list-ref editors found-editor-part-idx)))
          (begin
            (update-navigation-history navi-history found-editor-part)
            (array-list-rm-ref editors found-editor-part-idx)))))))


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
        (set! input (new-compare-editor-input string-message input-context)))
      (open-compare-editor-on-page input work-bench-page)
      (set! input '()) ;; don't reuse this input!
      )))


;; close editor part
(define (stop-compare work-bench-page input-context)
  (let ((input (try-find-exist-editor-input page input-context)))
    (if (not (null? input))
      (close-compare-editor-on-page input work-bench-page))))


;; test data
(define input-context-string-list (new-array-list 'string))
(array-list-add input-context-string-list "a2b")
(array-list-add input-context-string-list "a2c")
(array-list-add input-context-string-list "a2d")
(array-list-add input-context-string-list "a2e")
(array-list-add input-context-string-list "a2f")
(array-list-add input-context-string-list "a2g")
(array-list-add input-context-string-list "a2h")
(array-list-add input-context-string-list "a2i")
(array-list-add input-context-string-list "a2j")
(array-list-add input-context-string-list "a2k")
(array-list-add input-context-string-list "a2l")


;; Simulation Running
(define work-bench-page (new-work-bench-page))
(run-compare work-bench-page (array-list-ref input-context-string-list 0))
(stop-compare work-bench-page (array-list-ref input-context-string-list 0))
