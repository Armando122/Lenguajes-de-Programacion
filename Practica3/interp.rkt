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
                               (subst-with1 list-bin sub-id value) body)
                              (with
                               (subst-with1 list-bin sub-id value)
                               (subst body sub-id value))
                              )]
    [with* (list-bin body) (if (busca-id list-bin sub-id)
                              (with*
                               (subst-with2 list-bin sub-id value) body)
                              (with*
                               (subst-with1 list-bin sub-id value)
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

;;Función subst-with1, realiza la sustitución sobre
;;los argumentos de la expresión with.
;;subst-with1: listof-binding symbol AST -> listof-binding
(define (subst-with1 l sub-id value)
  (cond
    [(empty? l) empty]
    [else (cons
      (binding (binding-id (first l))
               (subst (binding-value (first l)) sub-id value))
      (subst-with1 (cdr l) sub-id value))])
  )

;;Función subst-with2, realiza la sustitución sobre
;;los argumentos de la expresión with.
;;subst-with2: listof-binding symbol AST -> listof-binding
(define (subst-with2 l sub-id value)
  (cond
    [(empty? l) empty]
    [(symbol=?
      (binding-id (first l))
      sub-id)
     (cons
      (binding (binding-id (first l))
               (subst (binding-value (first l)) sub-id value))
      (cdr l))]
    [else (cons
      (first l)
      (subst-with2 (cdr l) sub-id value))])
  )

 ;;Análisis semántico
 ;;interp: AST → number
(define (interp expr)
    (type-case AST expr
    [id (i) (error "Variable libre")]
    [num (n) n]
    [op (p l) (let ([operands (for/list ([i l]) (cond
                                                    [(num? i) (num-n i)]
                                                    [(AST? i) (interp i)]
                                                    [else i]))])
                (apply p operands))]
    [with (bindings body) (interp (subst-with body (remove-duplicates bindings)))]
    [with* (bindings body) (if (id? (binding-value (car bindings)))
                               (error "Variable " (binding-id (car bindings)) "not defined.")
                               (interp (subst-with body (clean-bindings bindings))))]))


;; Función auxiliar que prepara una lista de bindings de with*
;; clean-bindings: (list-of binding) -> (list-of Binding)
(define (clean-bindings bindings)
  (remove-duplicates
   (append (subst-bindings (cdr bindings) (list (car bindings)))
           (list (car bindings)))))


;; Función auxiliar que sustitye de forma recursiva todos los valores dentro de la
;; lista de bindings que recibe with* y los reacomoda en orden inverso
;; subst-bindings: AST (list-of binding) -> AST
(define (subst-bindings pending ready)
  (cond
    [(empty? pending) ready]
    [(let* ([first-binding (car pending)]
            [first-binding-value (binding-value first-binding)]
            [subst-value (subst-with first-binding-value ready)]
            [new-binding (binding (binding-id first-binding) subst-value)])
       (if (id? subst-value)
           (error "Variable " (id-i subst-value) "not defined")
           (subst-bindings (cdr pending) (append (list new-binding) ready))))]))


;; Función auxiliar para eliminar los bindings con id duplicadas
;; (Nos quedamos con las primeras apariciones solamente)
;; remove-duplicates: (list-of binding) -> (list-of binding)
(define (remove-duplicates bindings)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (equal? (binding-id x) (binding-id z)))) y)))
         empty
         bindings))

;; Función auxiliar que sustituye todas las apariciones de identificadores
;; dentro de una expresión con los elementos contenidos en la lista de bindings
;; subst-with: AST (list-of Binding) -> AST
(define (subst-with body bindings)
  (cond
    [(empty? bindings) body]
    [(subst-with (subst body
                       (binding-id (car bindings))
                       (binding-value (car bindings)))
                (cdr bindings))]))

