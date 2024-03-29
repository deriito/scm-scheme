(define-data-type 'test-a '(f1 f2))
(define (new-test-a f1v f2v)
  (let ((new-instance (make-test-a '() '())))
    (begin
      (set-test-a-f1! new-instance f1v 5)
      (set-test-a-f2! new-instance f2v 6)
      new-instance)))

(define test-hash-map (new-hash-map 'number 'test-a 9))

(define (simple-test-func)
  (let ((ta (new-test-a 111 222)))
    (begin
      (assert-dead ta)
      (hash-map-put test-hash-map 1 ta 15)
      (hash-map-put test-hash-map 2 ta 16)
      (hash-map-put test-hash-map 3 ta 17)
      (hash-map-put test-hash-map 4 ta 18)
      (hash-map-put test-hash-map 5 ta 19)
      (hash-map-put test-hash-map 6 ta 20)
      (hash-map-put test-hash-map 7 ta 21)
      (hash-map-put test-hash-map 8 ta 22)
      (hash-map-put test-hash-map 9 ta 23)
      (hash-map-put test-hash-map 10 ta 24)
      (gc))))