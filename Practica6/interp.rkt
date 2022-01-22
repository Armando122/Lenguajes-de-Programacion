#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")
;; Búsqueda en el ambiente de evaluación.
;; lookup: symbol Env → AST
(define (lookup sub-id env)
   (match env
      [(mtSub) 
         (error 'lookup "Variable libre")]
      [(aSub id value rest-env)
         (if (symbol=? id sub-id)
             value
             (lookup sub-id rest-env))]))

;; Aplicación de puntos estrictos.
;; strict: AST-Value → AST-Value
(define (strict expr)
   (match expr
      [(exprV expr env) (strict (interp expr env))]
      [else expr]))


;; Análisis semántico
;; interp: AST Env → AST-Value
(define (interp expr env)
    (match expr
    [(id i) (lookup i env)]
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(lisT elems) (listV (map (λ (v) (strict (interp v env))) elems))]
    [(op f args) (opf f (map (λ (v) (strict (interp v env))) args))]
    [(iF expr then-expr else-expr) (if (boolV-b (strict (interp expr env)))
                                       (interp then-expr env)
                                       (interp else-expr env))]
    [(fun params body) (closureV params body env)]
    [(recS bindings body) (interp body (crea-ambiente-recursivo bindings env))]
    [(app fun-expr args)
     (let [(fun-val (strict (interp fun-expr env)))]
       (interp
        (closureV-body fun-val)
        (crea-ambiente (closureV-param fun-val) args env)))]))


;; Función auxiliar que creo un ambiente y empareja los parametros formales con los argumentos
;; crea-ambiente: list closureV-param -> list CFBAEL -> Env -> Env
(define (crea-ambiente params args env)
  (match params
    ['() env]
    [(cons x xs)
     (if (empty? args)
         (error "Sin argumentos")
         (crea-ambiente xs (cdr args) (aSub x (exprV (car args) env) env)))]))
(require racket/trace)

;; Función que crea el ambiente recursivo.
;; crea-ambiente-recursivo: listof Binding -> Env
(define (crea-ambiente-recursivo bindings env)
  (if (empty? bindings)
      env
      (let* ([value-holder (box (numV 1729))]
             [new-env (aSub (binding-id (car bindings)) value-holder env)]
             [named-expr-val (interp (binding-value (car bindings)) new-env)])
        (begin
          (set-box! value-holder named-expr-val)
          (crea-ambiente-recursivo (cdr bindings) new-env)))))



;; Función auxiliar que dada un operación y una lista de argumentos,
;; aplica dicha aperación a los argumentos
;; opf: procedure -> list -> CFBAEL-Value
(define (opf f l)
  (let ([result (apply f (map (λ (v) (match v
                                       [(? numV?) (numV-n v)]
                                       [(? boolV?) (boolV-b v)]
                                       [(? listV?) (listV-elems v)])) l))])
    (cond
      [(contiene-en-lista? (list + - * /
                                 modulo expt add1 sub1
                                 < > <= >= =
                                 zero? min max expt) f)
       (numV result)]
      [(contiene-en-lista? (list empty? car cdr) f) (listV result)]
      [else (boolV result)])))





;; Función auxiliar que verifica si un elemento pertenece a una lista.
;; lista-contiene: list any -> boolean
(define (contiene-en-lista? l e)
  (match l
    ['() #f]
    [(cons x xs) (if (equal? e x) #t (contiene-en-lista? xs e))]))



  

(define (call-interp expr)
  (let* ([arg (fun 'x (app (id 'f) (app (id 'x) (id 'x))))]
         [Y (fun 'f (app arg arg))])
    (interp expr (aSub 'Y (exprV Y (mtSub)) (mtSub)))))
