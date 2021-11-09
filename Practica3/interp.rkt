#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Algoritmo de sustitución.
;; subst: AST symbol AST → AST
(define (subst expr sub-id value)
  (type-case AST expr
    [id (i) (if (symbol=? i sub-id)
                value
                expr)]
    [num (n) expr]
    [op (ope list) (op ope (subst-op list sub-id value))]
    [with (list-bin body) (if (busca-id list-bin sub-id)
                              (with
                               (subst-with list-bin sub-id value) body)
                              (with
                               (subst-with list-bin sub-id value)
                               (subst body sub-id value))
                              )]
    [with* (list-bin body) (if (busca-id list-bin sub-id)
                              (with*
                               (subst-with list-bin sub-id value) body)
                              (with*
                               (subst-with list-bin sub-id value)
                               (subst body sub-id value))
                              )])
  )

;;Función subst-op que recibe una lista correspondiente
;;a una expresión op y devuelve la sustitución sobre cada elemento.
;;subst-op: listof-AST symbol AST -> listof AST
(define (subst-op l sub-id value)
  (cond
    [(empty? l) empty]
    [else (cons (subst (first l) sub-id value) (subst-op (cdr l) sub-id value))]
    )
  )

;;Función busca-id, devuelve true si la lista de binding
;;contiene el sub-id false en otro caso.
;;busca-id: listof-binding symbol -> boolean
(define (busca-id l sub-id)
  (cond
    [(empty? l) #f]
    [(equal? (binding-id (first l)) sub-id) #t]
    [else (busca-id (cdr l) sub-id)])
  )

;;Función subst-with, realiza la sustitución sobre
;;los argumentos de la expresión with.
;;subst-with: listof-binding symbol AST -> listof-binding
(define (subst-with l sub-id value)
  (cond
    [(empty? l) empty]
    [else (cons
      (binding (binding-id (first l))
               (subst (binding-value (first l)) sub-id value))
      (subst-with (cdr l) sub-id value))])
  )



 ;;Análisis semántico
 ;;interp: AST → number
(define (interp expr)
  "escribe aquí tu codigo")

