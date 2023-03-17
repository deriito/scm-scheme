;;;; "iso8601.scm" ISO-8601 time conversion routines
;;; Copyright (C) 2014 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(require 'posix-time)
(require 'printf)
(require 'scanf)

;;@code{(require 'iso-8601)}
;;@ftindex iso-8601

;;@body
;;@1 is the time in seconds since 00:00:00 GMT, January 1, 1970.
;;@0 returns an expanded ISO 8601 format string for the date and time.
(define (time->iso-8601 time)
  (let ((tms (cddr (cdddr (reverse (vector->list (gmtime time)))))))
    ;;(print 'tms tms)
    (apply sprintf #f "%04d-%02d-%02dT%02d:%02d:%02d"
	   (+ 1900 (car tms)) (+ 1 (cadr tms)) (cddr tms))))

;;@body
;;@1 is a time in seconds since 00:00:00 GMT, January 1, 1970.
;;@0 returns a compact ISO 8601 format string for the date and time.
(define (time->iso8601 time)
  (let ((tms (cddr (cdddr (reverse (vector->list (gmtime time)))))))
    ;;(print 'tms tms)
    (apply sprintf #f "%04d%02d%02dT%02d%02d%02d" (+ 1900 (car tms)) (+ 1 (cadr tms)) (cddr tms))))

;;@body
;;@1 is a string in ISO 8601 format, either compact or expanded.
;;@0 returns that time in seconds since 00:00:00 GMT, January 1, 1970.
(define (iso-8601->time str)
  (define tim (make-vector 9 0))
  (define (ttry format)
    (sscanf str
	    format
	    (vector-ref tim 5)
	    (vector-ref tim 4)
	    (vector-ref tim 3)
	    (vector-ref tim 2)
	    (vector-ref tim 1)
	    (vector-ref tim 0)))
  (cond ((or (<= 3 (ttry "%04d-%02d-%02dT%02d:%02d:%02d") 6)
	     (<= 3 (ttry "%04d%02d%02dT%02d%02d%02d") 6))
	 (vector-set! tim 5 (+ -1900 (vector-ref tim 5)))
	 (vector-set! tim 4 (+ -1 (vector-ref tim 4)))
	 (gmktime tim))
	(else #f)))
