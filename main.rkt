#lang racket/base

(require syntax/parse
         nanopass/base)

(define (stx-number? stx)
  (if (syntax? stx)
      (number? (syntax-e stx))
      #f))

(define-language L0
  (terminals
   (identifier (name param))
   (stx-number (num)))
  (Expr (expr)
        num
        name
        (let name expr)
        (λ (param* ...) expr)
        (expr expr* ...)))

(define-pass parse : * (stx) -> L0 ()
  (Expr : * (stx) -> Expr (expr)
        (syntax-parse stx
          #:literals (let λ)
          ; let form
          [(let name:id expr)
           `(let ,#'name ,(parse #'expr))]
          ; lambda form
          [(λ (param*:id ...) expr)
           `(λ (,(syntax->list #'(param* ...)) ...) ,(parse #'expr))]
          [(f arg* ...)
           `(,(parse #'f) ,(map parse (syntax->list #'(arg* ...))) ...)]
          ; literal expression
          [x #:when (or (identifier? stx)
                        (stx-number? stx))
             #'x]
          [else (error 'syntax "unknown form: ~a" stx)]))
  (Expr stx))

(parse #'(let a 1))
(parse #'(let id (λ (x) x)))
(parse #'(let a-id (id a)))
