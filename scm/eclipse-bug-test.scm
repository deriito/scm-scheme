;; Test program based on "eclipse bug #115789"


;; CompareEditorInput.java
(define-data-type 'compare-editor-input '(f-message))

(define (new-compare-editor-input string-message)
  (let ((new-instance (make-compare-editor-input '())))
    (begin
      (set-compare-editor-input-string-message! new-instance string-message)
      new-instance)))


;; NavigationHistoryEditorInfo.java
(define-data-type 'navigation-history-editor-info '(editor-input ref-count))

(define (new-navigation-history-editor-info editor-input)
  (let ((new-instance (make-navigation-history-editor-info '() '())))
    (begin
      (set-navigation-history-editor-info-editor-input! new-instance editor-input)
      (set-navigation-history-editor-info-ref-count! new-instance 0)
      new-instance)))


;; NavigationHistoryEntry.java
(define-data-type 'navigation-history-entry '(editor-info))

(define (new-navigation-history-entry editor-info)
  (let ((new-instance (make-navigation-history-entry '())))
    (begin ;; setterにライトバリアしか実装していないため, fieldの初期化するにはsetterを使う
      (set-navigation-history-entry-editor-info! new-instance editor-info)
      new-instance)))

(define (history-entry-handle-part-closed navi-history-entry)
  #f)

(define (history-entry-dispose navi-history-entry)
  (set-navigation-history-entry-editor-info! navi-history-entry '()))


;; NavigationHistory.java
(define-data-type 'navigation-history '(history editors))

(define (new-navigation-history)
  (let ((new-instance (make-navigation-history '() '())))
    (begin
      (set-navigation-history-history! new-instance (new-array-list 'navigation-history-entry))
      (set-navigation-history-editors! new-instance (new-array-list 'navigation-history-editor-info))
      new-instance)))

(define (update-navigation-history navi-history)
  (letrec ((history-entry-idxs-to-rm (new-array-list 'number))
         (history-list (navigation-history-history navi-history))
         (history-list-size (get-array-list-size history-list)))
    (begin
      (let loop ((i 0))
        (cond
          ((< i history-list-size)
            (let ((entry (array-list-get history-list i)))
              (cond
                ((not (history-entry-handle-part-closed entry))
                  (begin
                    (array-list-add history-entry-idx-to-rm i)
                    (history-entry-dispose entry)
                    (loop (+ i 1))))
                (else
                  (loop (+ i 1))))))))
      (let loop ((i 0))
        (cond
          ((< i (get-array-list-size history-entry-idxs-to-rm))
            (begin
              (array-list-rm-ref history-list (array-list-get history-entry-idxs-to-rm i))
              (loop (+ i 1)))))))))


;; WorkbenchPage.java
(define-data-type 'work-bench-page '(navigation-history))

(define (new-work-bench-page)
  (let ((new-instance (make-work-bench-page '())))
    (begin
      (set-work-bench-page-navigation-history! new-instance (new-navigation-history))
      new-instance)))
