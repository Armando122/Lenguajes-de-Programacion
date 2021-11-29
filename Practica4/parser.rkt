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
;; parse: s-expression → SAST
(define (parse sexp)
   (match sexp
     [(? symbol?) (idS sexp)]
     [(? number?) (numS sexp)]
     [(list 'with idvalues body) (withS (crearBindings idvalues) (parse body))]
     [(list 'with* idvalues body) (withS* (crearBindings idvalues) (parse body))]
     [(list 'fun funValues body) (funS funValues (parse body))]
     [(cons (list 'fun funValues body) args) (appS (funS funValues (parse body)) (map parse args))]
     [(cons s args) (opS (elige s) (map parse args))]))

(define (crearBindings l)
  (match l
    ['() '()]
    [(cons x xs) (cons (binding (car x) (parse (cadr x))) (crearBindings xs))]
    )
  )
 
