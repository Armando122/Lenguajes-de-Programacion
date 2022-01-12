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
 "completa la función")