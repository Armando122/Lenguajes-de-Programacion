#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Elimina el azúcar sintáctica de las expresiones FWAE.
;; desugar: SAST → AST
(define (desugar expr)
   (type-case SAST expr
     [idS (i) (id i)]
     [numS (n) (num n)]
     [boolS (b) (bool b)]
     [listS (l) (listT (map (lambda (x) (desugar x)) l))]
     [opS (opr list) (op opr (map (lambda (x) (desugar x)) l))]
     [ifS (test-expr then-expr else-expr)
          (iF (desugar test-expr)
              (desugar then-expr)
              (desugar else-expr))]
     [condS (list-cond) (desugar-cond list-cond)])
  )

;; Función auxiliar desugar-cond, que transforma
;; una expresión condS en expresiones iF
;; anidadas, la expresión condS debe contener una rama
;; then y una rama else.
;; desugar-cond: listof-condition -> AST
(define (desugar-cond conditions)
  (cond
    [(else-cond? (car conditions))
     (desugar (else-cond-else-expr (car conditions)))]
    [else
     (iF
      (desugar (condition-test-expr (car conditions)))
      (desugar (condition-then-expr (car conditions)))
      (desugar-cond (cdr conditions)))]
    )
  )

