(define (func)
  (start-record-gc-cost-time)
  (do ((i 0 (+ i 1)))
    ((>= i 50))
    (begin
      (display (string-append
                 (number->string i)
                 " 秒\n"))
      (gc)
      (thread-sleep 1))))
