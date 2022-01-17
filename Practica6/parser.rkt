#lang plai

(require "grammars.rkt")

;; Función que realiza un mapeo entre símbols y funciones.
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
      ['sub1 sub1]
      ['not not]
      ['and (λ args (foldr (λ (x y) (and x y)) #t args))]
      ['or  (λ args (foldr (λ (x y) (or x y)) #f args))]
      ['< <]
      ['> >]
      ['<= <=]
      ['>= >=]
      ['= =]
      ['zero? zero?]
      ['empty? empty?]
      ['car car]
      ['cdr cdr]))

;; Análisis sintáctico
;; parse: s-expression → SAST
(define (parse sexp)
   (match sexp
     ['true (boolS #t)]
     ['false (boolS #f)]
     ['empty (listS '())]
     [(? symbol?) (idS sexp)]
     [(? number?) (numS sexp)]
     [(cons 'list values) (listS (map parse values))]
     [(list 'if c t e) (ifS (parse c) (parse t) (parse e))]
     [(cons 'cond cases) (condS (crearCases cases))]
     [(list 'with idvalues body) (withS (crearBindings idvalues) (parse body))]
     [(list 'with* idvalues body) (withS* (crearBindings idvalues) (parse body))]
     [(list 'fun funValues body) (funS funValues (parse body))]
     [(cons (list 'fun funValues body) args) (appS (funS funValues (parse body)) (map parse args))]
     [(list 'rec fun body) (recS (crearBindings fun) (parse body))]
     [(cons s args) (opS (elige s) (map parse args))]))

(define (crearCases l)
  (match l
    ['() '()]
    [(cons x xs) (cons (crearCondition x) (crearCases xs)) ])
  )

(define (crearCondition p)
  (let ([x (car p)])
    (match x
      ['else (else-cond (parse (cadr p)))]
      [x (condition (parse (car p)) (parse (cadr p)))])))

(define (crearBindings l)
  (match l
    ['() '()]
    [(cons x xs) (cons (binding (car x) (parse (cadr x))) (crearBindings xs))]
    )
  )