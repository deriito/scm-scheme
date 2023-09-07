;; TARAI funciton: Original definition of TAK Function
;; https://www.nue.org/nue/#tak-function

(define (tarai x y z)
  (cond
    ((<= x y)
      y)
    (else
      (tarai (tarai (- x 1) y z) (tarai (- y 1) z x) (tarai (- z 1) x y)))))

(define (tarai-with-display x y z)
  (begin
    (display (string-append
               "(tarai "
               (number->string x)
               " "
               (number->string y)
               " "
               (number->string z)
               ")\n"))
    (cond
      ((<= x y)
        y)
      (else
        (tarai-with-display
          (tarai-with-display (- x 1) y z)
          (tarai-with-display (- y 1) z x)
          (tarai-with-display (- z 1) x y))))))
