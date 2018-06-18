#lang racket/base

(provide (struct-out tuple))

(struct tuple (content)
  #:property prop:equal+hash
  (list (lambda (a b eql?)
          (eql? (tuple-content a)
                (tuple-content b)))
        (lambda (a hc)
          (hc (tuple-content a)))
        (lambda (a hc)
          (hc (tuple-content a))))
  #:property prop:custom-print-quotable
  'never
  #:property prop:custom-write
  (lambda (t out mode)
    (write-string "(values" out)
    (for ([e (in-vector (tuple-content t))])
      (write-string " " out)
      (case mode
        [(#t) (write e out)]
        [(#f) (display e out)]
        [else
         (print e out mode)]))
    (write-string ")" out)))
