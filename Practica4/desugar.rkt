#lang plai

(require "parser.rkt")
(require "grammars.rkt")

;; Elimina el azúcar sintáctica de las expresiones FWAE.
;; desugar: SAST → AST
(define (desugar expr)
  (match expr
    [(list 'withS idvalues body) (desugar-withS idvalues body)])
  )


;; Función auxiliar que convierte los valores de una
;; expresión withS en funciones currificadas.
;; desugar-withS: litof-binding SAST -> AST
(define (desugar-withS idvalues body)
  (match idvalues
    [empty (desugar body)]
    [else
     (app (fun (binding-id (first idvalues))
               (desugar-withS (cdr idvalues) body))
          (binding-value (first idvalues)))])
  )