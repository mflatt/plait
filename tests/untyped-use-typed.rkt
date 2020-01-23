#lang plait #:untyped

(require "basic.rkt")

(test "hi"
      (type-case Linked-List (llnode "hi" (none))
        [(llnode s next) s]))
