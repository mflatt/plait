#lang racket/base

(require racket/runtime-path
         (for-syntax racket/base))

(provide (all-from-out racket/runtime-path)
         (for-syntax quote))
