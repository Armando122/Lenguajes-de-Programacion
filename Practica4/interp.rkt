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
   "escribe aquí tu código")