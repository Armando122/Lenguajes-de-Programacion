#lang plai

(require "grammars.rkt")

;; Búsqueda en el ambiente de evaluación.
;, lookup: symbol Env → AST
(define (lookup sub-id env)
   (match env
      [(mtSub) 
         (error 'lookup "Variable libre")]
      [(aSub id value rest-env)
         (if (symbol=? id sub-id)
             value
             (lookup sub-id rest-env))]))

;; Análisis semántico
;; interp: AST Env → AST-Value
(define (interp expr env)
   (type-case AST expr
    [id (i) (lookup i env)]
    [num (n) (numV n)]
    [op (f args) (interp-op f (for/list ([i args]) (convers (interp i env))))]
    [fun (p b) (closureV p b env)]
    [app (f arg) (interp-app f arg env)]))

;;Un conversor de AST-Value a AST.
(define (convers v)
  (type-case AST-Value v
    [closureV (p d e) (fun p d)]
    [numV (n) n]))

;;Funcion auxiliar que nos permite interpretar las operaciones.
(define (interp-op f lst)
  (let ([n (length lst)])
    (cond
      [(= n 1) (numV (f (first lst)))]
      [(= n 2) (numV (f (first lst) (second lst)))]
      [else   (numV (apply f lst))])))

;;Nos permite interpretar las aplicaciones de funcion.
(define (interp-app f a env)
  (type-case AST f
    [fun (p b) (interp b ((reverse p) (reverse (for/list ([i a]) (interp i env))) env))]
    [id (i) (interp (app (convers (lookup i env)) a) (convers (lookup i env)))]
    [else (error "interapp: Esto no debio pasar")]))
