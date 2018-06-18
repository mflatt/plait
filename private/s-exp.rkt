#lang racket/base

(provide (struct-out s-exp))

(struct s-exp (content)
  #:property prop:equal+hash
  (list (lambda (a b eql?)
          (eql? (s-exp-content a)
                (s-exp-content b)))
        (lambda (a hc)
          (hc (s-exp-content a)))
        (lambda (a hc)
          (hc (s-exp-content a))))
  #:property prop:custom-print-quotable
  'never
  #:property prop:custom-write
  (lambda (s out depth)
    (write-string "`" out)
    (write (s-exp-content s) out)))
