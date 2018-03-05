#lang racket #| Compile L1 to sequential language L2 |#

(provide (struct-out compiled:L2) L1→L2)
(module+ test (require rackunit))

#| A2's language L2 is A1's language L2 with three additional expressions. |#

; Jumps to a named statement address, if result is false.
#;(L2: label <name>)      ; Name the current statement location.
#;(L2: jump <name>)       ; Jump/goto a statement location.
#;(L2: jump_false <name>) ; Jump/goto a statement location, when result is false.

#| Compiling L1 to L2 |#

; One additional compilation rule is for L1's new conditional.
#;{(L1: if <n> <e1> <e2> <e3>) →
                               code: {<code-for-e1>
                                      (L2: jump_false else_<n>)
                                      <code-for-e2>
                                      (L2: jump end_<n>)
                                      (L2: label else_<n>)
                                      <code-for-e3>
                                      (L2: label end_<n>)}
                               λs: {<λs-for-e1>
                                    <λs-for-e2>
                                    <λs-for-e3>}}

; A second compilation rule passes through references to an additional function.
#;{(L1: var call/ec) → (L2: closure call_ec)}

#| L1→L2 |#

(struct compiled:L2 (code λs) #:transparent)

; Produce a symbol of the form lambda_<n>.
(require (only-in racket/syntax format-symbol))
(define (lambda_ n)
  (format-symbol "lambda_~a" n))

(define (else_ n)
  (format-symbol "else_~a" n))

(define (end_ n)
  (format-symbol "end_~a" n))


; L1 non terminals

(define (L1/lambda? e)
  (match e
    [`(L1: λ ,n ,M) (λ (f) (f n M))]
    [_ #false]))
            
(define (L1/app? e)
  (match e
    [`(L1: app ,N ,M) (λ (f) (f N M))]
    [_ #false]))

(define (L1/var? e)
  (match e
    [`(L1: var ,x) (λ (f) (f x))]
    [_ #false]))

(define (L1/set? e)
  (match e
    [`(L1: set! ,n ,M) (λ (f) (f n M))]
    [_ #false]))

(define (L1/datum? e)
  (match e
    [`(L1: datum ,l) (λ (f) (f l))]
    [_ #false]))

(define (L1/ec? e)
  (match e
    ['(L1: var call/ec) (λ (f) (f))]
    [_ #false]))

(define (L1/if? e)
  (match e
    [`(L1: if ,n ,e1 ,e2 ,e3) (λ (f) (f n e1 e2 e3))]
    [_ #false]))

; L1 → compiled:L2 productions

(define (L1→L2/datum l)
  (compiled:L2 `((L2: set_result ,l)) '()))

(define (L1→L2/var n)
  (match n
    ['+ (compiled:L2 '((L2: closure make_add)) '())]
    ['* (compiled:L2 '((L2: closure make_multiply)) '())]
    ['< (compiled:L2 '((L2: closure make_less_than)) '())]
    [_ (compiled:L2 `((L2: variable ,n)) '())]))

(define (L1→L2/lambda n M)
  (let ([M′ (L1→L2 M)])
    (compiled:L2
     `((L2: closure (,@(lambda_ n))))
     `((,(lambda_ n) ,(compiled:L2-code M′)) ,@(compiled:L2-λs M′)))))

(define (L1→L2/app N M)
  (let ([N′ (L1→L2 N)] [M′ (L1→L2 M)])
    (compiled:L2
     `(,@(compiled:L2-code N′)
       (L2: push_result)
       ,@(compiled:L2-code M′)
       (L2: call))
     `(,@(compiled:L2-λs N′)
       ,@(compiled:L2-λs M′)))))

(define (L1→L2/set n N)
  (let ([N′ (L1→L2 N)])
    (compiled:L2
     `(,@(compiled:L2-code N′)
       (L2: set ,n))
     `(,@(compiled:L2-λs N′)))))

(define (L1→L2/if n e1 e2 e3)
  (let ([e1′ (L1→L2 e1)]
        [e2′ (L1→L2 e2)]
        [e3′ (L1→L2 e3)])
    (compiled:L2
     `(,@(compiled:L2-code e1′)
       (L2: jump_false ,(else_ n))
       ,@(compiled:L2-code e2′)
       (L2: jump ,(end_ n))
       (L2: label ,(else_ n))
       ,@(compiled:L2-code e3′)
       (L2: label ,(end_ n)))
     `(,@(compiled:L2-λs e1′)
       ,@(compiled:L2-λs e2′)
       ,@(compiled:L2-λs e3′)))))

(define (L1→L2/ec)
  (compiled:L2 '((L2: closure call_ec)) '()))

; L1 → compiled:L2 compiler

(define (L1→L2 e)
  (cond [(L1/lambda? e) => (λ (g) (g L1→L2/lambda))]
        [(L1/app? e)    => (λ (g) (g L1→L2/app))]
        [(L1/set? e)    => (λ (g) (g L1→L2/set))]
        [(L1/ec? e)     => (λ (g) (g L1→L2/ec))]
        [(L1/var? e)    => (λ (g) (g L1→L2/var))]
        [(L1/datum? e)  => (λ (g) (g L1→L2/datum))]        
        [(L1/if? e)     => (λ (g) (g L1→L2/if))]
        [else '⊥]))

