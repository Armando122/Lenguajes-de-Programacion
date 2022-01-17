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
     [listS (l) (lisT (map (lambda (x) (desugar x)) l))]
     [opS (opr list) (op opr (map (lambda (x) (desugar x)) list))]
     [ifS (test-expr then-expr else-expr)
          (iF (desugar test-expr)
              (desugar then-expr)
              (desugar else-expr))]
     [condS (list-cond) (desugar-cond list-cond)]
     [withS (list-bin body)
            (let* ([n-body (desugar body)]
                   [n-expr (desugar-withS list-bin n-body (reverse list-bin))])
              n-expr
              )]
     [withS* (list-bin body) (desugar (reduce-withS* list-bin body))]
     [recS (list-bin body) (desugar
                            (desugar-recS list-bin body))]
     [funS (list-param body) (currifica-funS list-param body)]
     [appS (fun-expr list-args) (asocia-appS fun-expr (reverse list-args))])
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

;; Función auxiliar que convierte los valores de una
;; expresión withS en funciones currificadas.
;; desugar-withS: litof-binding SAST listof-binding -> AST
(define (desugar-withS idvalues body values)
  (cond 
    [(empty? values) (desugar-withS1 idvalues body)]
    [else
     (app (desugar-withS idvalues body (cdr values))
          (desugar (binding-value (first values))))])
  )

;; Función auxiliar que convierte una lista de binding
;; en funciones.
;; desugar-withS1: listof-binding AST -> AST
(define (desugar-withS1 idvalues body)
  (cond
    [(empty? idvalues) body]
    [else
     (fun (binding-id (first idvalues))
          (desugar-withS1 (cdr idvalues) body))])
  )

;; Función auxiliar que reduce una expresión withS*
;; a expresiones withS anidadas.
;; reduce-withS*: listof-binding SAST -> AST
(define (reduce-withS* idvalues body)
  (cond
    [(empty? idvalues) body]
    [else
     (withS (list (first idvalues)) (reduce-withS* (cdr idvalues) body))])
  )

;; Función auxiliar que
;; desugar-recS: listof-binding SAST -> SAST
(define (desugar-recS l body)
  (cond
    [(empty? l) body]
    [else
     (let* ([a (first l)]
            [id-l (binding-id a)]
            [value-l (binding-value a)])
       (withS (list
            (binding id-l (funS (list id-l) (Y value-l))))
             (desugar-recS (cdr l) body))
           )
       ])
  )

;; Función auxiliar que currifica una expresión
;; funS en expresiones fun anidadas.
;; currifica-funS: listof-symbol SAST -> AST
(define (currifica-funS params body)
  (cond
    [(empty? params) (desugar body)]
    [else
     (fun (first params) (currifica-funS (cdr params) body))])
  )

;; Función auxiliar que simplifica una expresión
;; appS en aplicaciones de función unitarias.
;; asocia-appS: SAST listof-SAST -> AST
(define (asocia-appS fun-exp args)
  (cond
    [(empty? args) (desugar fun-exp)]
    [else
     (app
      (asocia-appS fun-exp (cdr args))
      (desugar (first args)))])
  )



