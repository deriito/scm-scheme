(define-data-type 'test-a '(f1 f2))
(define (new-test-a f1v f2v)
  (let ((new-instance (make-test-a '() '())))
    (begin
      (set-test-a-f1! new-instance f1v 5)
      (set-test-a-f2! new-instance f2v 6)
      new-instance)))

(define test-list (new-linked-list 'test-a 9))

(define (simple-test-func)
  (let ((ta (new-test-a 111 222)))
    (begin
      (assert-dead ta)
      (linked-list-add test-list ta 15)
      (gc))))