#lang racket

(require data/monad data/applicative)
(require megaparsack megaparsack/text)

(struct ast () #:transparent)
(struct let ast (name expr) #:transparent)

(struct expr () #:transparent)
(struct call expr (f arg*) #:transparent)
(struct binary expr (op lhs rhs) #:transparent)

(define lexeme/p
  ;;; lexeme would take at least one space or do nothing
  ; hidden/p helps parser won't report something like: expected a space/p by hidden it
  (do (hidden/p (or/p (many+/p space/p) void/p))
    (pure (λ () 'lexeme))))
(define must/lexeme/p
  ;;; must/lexeme treat cannot lexme as error
  (do (hidden/p (many+/p space/p))
    (pure (λ () 'lexeme))))

(define (keyword/p keyword)
  (do (string/p keyword)
    (lexeme/p)
    (pure keyword)))
(define identifier/p
  (do [id <- (syntax-box/p (many+/p letter/p))]
    (lexeme/p)
    ; rewrap output, from `(many+/p letter/p)` we would a list of char, using `list->string` get the corresponding string
    (pure (syntax-box (list->string (syntax-box-datum id)) (syntax-box-srcloc id)))))

(define expr/call/p
  (do (keyword/p "apply")
    (must/lexeme/p)
    [e* <- (many/p expr/p #:sep must/lexeme/p)]
    (pure (call (car e*) (cdr e*)))))
(define unary/p
  (do [expr <- (or/p (try/p expr/call/p)
                     (try/p (syntax-box/p (or/p (string/p "true") (string/p "false"))))
                     identifier/p
                     (syntax-box/p integer/p))]
    (lexeme/p)
    (pure expr)))
(define (binary/p high-level/p op-list)
  (do [e <- high-level/p]
    [es <- (many/p (do [op <- (or/p (one-of/p op-list) void/p)]
                     (lexeme/p)
                     [e <- high-level/p]
                     (pure (list op e))))]
    (pure (foldl
           (λ (op+rhs lhs)
             (match op+rhs
               [(list op rhs)
                (binary op lhs rhs)]))
           e es))))
(define (table/p base/p list-of-op-list)
  (if (empty? list-of-op-list)
      base/p
      (table/p (binary/p base/p (car list-of-op-list)) (cdr list-of-op-list))))
(define expr/p
  (table/p unary/p
           '((#\* #\/)
             (#\+ #\-)
             ("==" ))))

(define binding/p
  (do (keyword/p "let")
    [name <- identifier/p]
    (keyword/p "=")
    [expr <- expr/p]
    (pure (let name expr))))

(parse-string binding/p "let a= apply foo 1")
