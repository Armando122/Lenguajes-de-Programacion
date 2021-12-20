#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")
;; Búsqueda en el ambiente de evaluación.
;; lookup: symbol Env → AST
(define (lookup sub-id env)
   (match env
      [(mtSub) 
         (error 'lookup "Variable libre")]
      [(aSub id value rest-env)
         (if (symbol=? id sub-id)
             value
             (lookup sub-id rest-env))]))

;; Aplicación de puntos estrictos.
;; strict: AST-Value → AST-Value
(define (strict expr)
   (match expr
      [(exprV expr env) (strict (interp expr env))]
      [else expr]))


;; Análisis semántico
;; interp: AST Env → AST-Value
(define (interp expr env)
    (type-case CFWBAE expr
    [id (i) (lookup i env)]
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [op (f args) (interp-op f (for/list ([i args]) (cc (interp i env))))]
    [iF (c t e) (if (erroriF-aux (cc (interp c env)))
                     (interp t env)
                     (interp e env))]
    [fun (p b) (closure p b env)]
    [app (f arg) (interp-app f arg env)]))
    
    ;;Funcion auxiliar que nos permite interpretar las operaciones.
(define (interp-op f lst)
  (cond
    [(equal? f +) (interp-op-num f lst)]
    [(equal? f -) (interp-op-num f lst)]
    [(equal? f /) (interp-op-num f lst)]
    [(equal? f *) (interp-op-num f lst)]
    [(equal? f sub1) (interp-op-num f lst)]
    [(equal? f add1) (interp-op-num f lst)]
    [(equal? f modulo) (interp-op-num f lst)]
    [(equal? f expt) (interp-op-num f lst)]
    [else (interp-op-bool f lst)]))
    
    ;;Convierte CFWAE-Value a CFWAE.
(define (cc v)
  (type-case CFWBAE-Value v
    [closure (p d e) (fun p d)]
    
    ;;Nos permite interpretar las aplicaciones de funcion.
(define (interp-app f a ds)
  (type-case CFWBAE f
    [fun (p b) (interp b (def (reverse p) (reverse (for/list ([i a]) (interp i ds))) ds))]
    [id (i) (interp (app (cc (lookup i ds)) a) (cs (lookup i ds)))]
    [else (error "interapp: Esto no debio pasar")]))
    
    ;;Funcion que nos permite agregar a al "cache de sustituciones" nuevos elementos.
(define (def p a ds)
  (if (empty? p)
      ds
      (aSub (car p) (car a) (def (cdr p) (cdr a) ds))))
      
      
    [numV (n) n]
    [boolV (b) b]))
    
    ;;Nos ayuda a detectar si a un iF se le ha pasado un valor booleano o no.
(define (erroriF-aux n)
  (if (boolean? n)
      n
      (error "interp: Símbolo no esperado la condicional de if, no es un booleano")))
      
     ;;Nos permite extraer el "cache de sustitucion" de un closure.
(define (cs v)
  (type-case CFWBAE-Value v
    [closure (p d e) e]
    [else error "cs: Esto no debio pasar"]))
    
    ;;Funcion auxiliar que nos permite interpretar las operaciones que devuelven numeros.
(define (interp-op-num f lst)
  (let ([n (length lst)])
    (cond
      [(= n 1) (numV (f (first lst)))]
      [(= n 2) (numV (f (first lst) (second lst)))]
      [else   (numV (apply f lst))])))
      
      ;;Funcion auxiliar que nos permite interpretar las operaciones que devuelven bool.
(define (interp-op-bool f lst)
  (let ([n (length lst)])
    (cond
      [(= n 1) (boolV (f (first lst)))]
      [(= n 2) (boolV (f (first lst) (second lst)))]
      [else   (boolV (apply f lst))])))
