;; Test program based on "eclipse bug #115789"


;; CompareEditorInput.java
(define-data-type 'compare-editor-input '(f-message))

(define (new-compare-editor-input string-message)
  (make-compare-editor-input string-message))


;; NavigationHistoryEditorInfo.java
(define-data-type 'navigation-history-editor-info '(editor-input ref-count))

(define (new-navigation-history-editor-info editor-input)
  (make-navigation-history-editor-info editor-input 0))


;; NavigationHistoryEntry.java
(define-data-type 'navigation-history-entry '(editor-info))

(define (new-navigation-history-entry editor-info)
  (make-navigation-history-entry editor-info))


;; NavigationHistory.java
(define-data-type 'navigation-history '(history editors))

(define (new-navigation-history)
  (make-navigation-history (new-array-list 'navigation-history-entry) (new-array-list 'navigation-history-editor-info)))

