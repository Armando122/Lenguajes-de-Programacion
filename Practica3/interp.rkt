#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Algoritmo de sustitución.
;; subst: AST symbol AST → AST
(define (subst expr sub-id value)
  (type-case AST expr
    [id (l) (if (symbol=? l sub-id)
                value
                expr)]
    [num (n) expr]
    [op (ope list) (op ope (subst-op list sub-id value))]
    [with (list-bin body) (if)]
    [with*])
  )

;;Función subst-op que recibe una lista correspondiente
;;a una expresión op y devuelve la sustitución sobre cada elemento.
;;subst-op: listof-AST symbol AST -> listof AST
(define (subst-op l sub-id value)
  (match l
    [empty empty]
    [(cons x xs) (cons (subst x sub-id value) (subst-op xs sub-id value))]
    )
  )



 ;;Análisis semántico
 ;;interp: AST → number
(define (interp expr)
  "escribe aquí tu codigo")

