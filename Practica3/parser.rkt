#lang plai

(require "grammars.rkt")

;; Función que realiza un mapeo entre símbolos y funciones.
;; elige: symbol → procedure
(define (elige s)
   (match s
      ['+ +]
      ['- -]
      ['* *]
      ['/ /]
      ['modulo modulo]
      ['expt expt]
      ['add1 add1]
      ['sub1 sub1]))

;; Análisis sintáctico
;; parse: s-expression → AST
(define (parse sexp)
  (match sexp
    [(? symbol?) (id sexp)]
    [(? number?) (num sexp)]
    [(list 'with idvalues body) (with (crearBindings idvalues) (parse body))]
    [(list 'with* idvalues body) (with* (crearBindings idvalues) (parse body))]
    [(cons s args) (op (elige s) (map parse args))]
    )
  )

(define (crearBindings l)
  (match l
    ['() '()]
    [(cons x xs) (cons (binding (car x) (parse (cadr x))) (crearBindings xs))]
    )
  )
 