;; TAK function : John Macarthy's variation of TARAI function
;; https://www.nue.org/nue/#tak-function

(define (tak x y z)
  (cond
    ((<= x y)
      z)
    (else
      (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))

(define (tak-with-display x y z)
  (begin
    (display (string-append
               "(tak "
               (number->string x)
               " "
               (number->string y)
               " "
               (number->string z)
               ")\n"))
    (cond
      ((<= x y)
        z)
      (else
        (tak-with-display
          (tak-with-display (- x 1) y z)
          (tak-with-display (- y 1) z x)
          (tak-with-display (- z 1) x y))))))
