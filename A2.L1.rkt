#lang racket #| Compile A2's Language L0 to A2's Language L1 |#

(provide debruijn index L0→L1)
(module+ test (require rackunit))

#| A2's language L0 is A1's language L0 with one additional conditional expression. |#

; If <e1> is true then evaluate <e2>, else evaluate <e3>.
#;(L0: if <e1> <e2> <e3>)

#| A2's language L1 is A1's language L1 with one additional conditional expression. |#

; The nth if expression.
#;(L1: if <n> <e1> <e2> <e3>)

#| DeBruijn Indexing of an L0 Expression

 This is the same as in A1. |#

(define (debruijn e [env '()]) ; Takes an optional second argument, which defaults to the empty list.
  (define (debruijn′ e) (debruijn e env))
  e)

#| Indexing of a Debruijnized L0 Expression

 For the A1 subset of L0 this is the same.
 The new conditional expressions are also given unique indices. |#

(define ((counter [c 0]))
  (set! c (add1 c))
  (sub1 c))

; For a debruijned L0 expression e, give each λ expression a unique index,
;  and each if expression a unique index.
(define (index e [λ-count (counter)] [if-count (counter)])
  (define (index′ e) (index e λ-count if-count))
  e)

#| L0→L1

 For an L0 expression: debruijnizes, indexes, and replaces remaining ‘L0:’ tags with ‘L1:’. |#

; non terminals ( don't like giant match patterns )

(define-syntax-rule (L0/close-context e ...)
  (λ (f env count-λ count-if) (f e ... env count-λ count-if)))

(define (L0/lambda? e)
  (match e
    [`(L0: λ (,x) ,M) (L0/close-context x M)]
    [_ #false]))

(define (L0/app? e)
  (match e
    [`(L0: app ,M ,N) (L0/close-context M N)]
    [_ #false]))

(define (L0/var? e)
  (match e
    [`(L0: var ,x) (L0/close-context x)]
    [_ #false]))

(define (L0/datum? e)
  (match e
    [`(L0: datum ,l) (L0/close-context l)]
    [_ #false]))

(define (L0/set? e)
  (match e
    [`(L0: set! ,x ,M) (L0/close-context x M)]
    [_ #false]))

(define (L0/if? e)
  (match e
    [`(L0: if ,L ,M ,N) (L0/close-context L M N)]
    [_ #false]))


; L0 → L1 productions

(define (L0→L1/lambda x M env count-λ count-if)
  (let ([M′ (L0→L1 M (append (list x) env) count-λ count-if)])
    `(L1: λ ,(count-λ) ,M′)))

(define (L0→L1/app M N env count-λ count-if)
  `(L1: app ,(L0→L1 M env count-λ count-if) ,(L0→L1 N env count-λ count-if)))

(define (L0→L1/var x env count-λ count-if)
  `(L1: var ,(L0/debruijn x env)))

(define (L0→L1/datum l env count-λ count-if)
  `(L1: datum ,l))

(define (L0→L1/set x M env count-λ count-if)
  `(L1: set! ,(L0/debruijn x env) ,(L0→L1 M env count-λ count-if)))

(define (L0→L1/if L M N env count-λ count-if)
  (let ([L′ (L0→L1 L env count-λ count-if)]
        [M′ (L0→L1 M env count-λ count-if)]
        [N′ (L0→L1 N env count-λ count-if)])
  `(L1: if ,(count-if) ,L′ ,M′ ,N′)))

(define (L0/debruijn x [env '()])
  (or (index-of env x) x))

; Maybe monad

(define (⊥? e) (equal? e '⊥))
(define-syntax-rule (>> e1 e2) (if (⊥? e1) '⊥ e2))
(define-syntax-rule (>>= e1 e2) ((λ (r) (>> r (e2 r))) e1))

; L0 → L1 compiler

(define (L0→L1 e [env '()] [count-λ (counter)] [count-if (counter)])
  (>>=
   (cond [(L0/lambda? e) => (λ (g) (g L0→L1/lambda env count-λ count-if))]
         [(L0/app? e)    => (λ (g) (g L0→L1/app env count-λ count-if))]
         [(L0/var? e)    => (λ (g) (g L0→L1/var env count-λ count-if))]
         [(L0/datum? e)  => (λ (g) (g L0→L1/datum env count-λ count-if))]
         [(L0/set? e)    => (λ (g) (g L0→L1/set env count-λ count-if))]
         [(L0/if? e)     => (λ (g) (g L0→L1/if env count-λ count-if))]
         [else '⊥])
   identity))

(define eieio
  '(L0: λ (z)
        (L0: app
             (L0: λ (y) (L0: app (L0: var y) (L0: λ (x) (L0: var x))))
             (L0: λ (x) (L0: app (L0: var z) (L0: var x))))))
