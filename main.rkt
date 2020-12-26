#lang racket/base

(require syntax/parse
         nanopass/base)

(define (stx-number? stx)
  (if (syntax? stx)
      (number? (syntax-e stx))
      #f))

(define-language L0
  (terminals
   (syntax (stx))
   (identifier (name param))
   (stx-number (num)))
  (Expr (expr)
        num
        name
        (let stx name expr) => (let name expr)
        (λ stx (param* ...) expr) => (λ (param* ...) expr)
        (stx expr expr* ...) => (expr expr* ...)))

(define-pass parse : * (stx) -> L0 ()
  (Expr : * (stx) -> Expr (expr)
        (syntax-parse stx
          #:literals (let λ)
          ; let form
          [(let name:id expr)
           `(let ,stx ,#'name ,(parse #'expr))]
          ; lambda form
          [(λ (param*:id ...) expr)
           `(λ ,stx (,(syntax->list #'(param* ...)) ...) ,(parse #'expr))]
          [(f arg* ...)
           `(,stx ,(parse #'f) ,(map parse (syntax->list #'(arg* ...))) ...)]
          ; literal expression
          [x #:when (ormap (λ (pred?) (pred? stx)) (list identifier? stx-number?))
             #'x]
          [else (error 'syntax "unknown form: ~a" stx)]))
  (Expr stx))

(parse #'(let a 1))
(parse #'(let id (λ (x) x)))
(parse #'(let a-id (id a)))
(parse #'(let no-arg (foo)))
