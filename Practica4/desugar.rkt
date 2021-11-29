#lang plai

(require "parser.rkt")
(require "grammars.rkt")

;; Elimina el azúcar sintáctica de las expresiones FWAE.
;; desugar: SAST → AST
(define (desugar expr)
  (type-case SAST expr
    [idS (i) (id i)]
    [numS (n) (num n)]
    [opS (operator list) (op operator list)]
    [withS (list-bin body) (desugar-withS list-bin body list-bin)])
  )


;; Función auxiliar que convierte los valores de una
;; expresión withS en funciones currificadas.
;; desugar-withS: litof-binding SAST listof-binding -> AST
(define (desugar-withS idvalues body values)
  (match values
    [empty (desugar-withS1 idvalues body)]
    [else
     (app (desugar-withS idvalues body (cdr values))
          (desugar (binding-value (first values))))])
  )

;; Función auxiliar que convierte una lista de binding
;; en funciones.
;; desugar-withS1: listof-binding AST -> AST
(define (desugar-withS1 idvalues body)
  (match idvalues
    [empty (desugar body)]
    [else
     (fun (binding-id (first idvalues))
          (desugar-withS1 (cdr idvalues) body))])
  )