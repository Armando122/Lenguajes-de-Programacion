#lang plai
;;Propuesta de distribución
;; Armando - 1. area para esas figuras. 2.Vagon, pasjaeros.
;; Liprandi - 1. triangulo, cuadrado, rectangulo, area y perimetro de esa fugras. 2. peso-carbon, peso-aproximado
;;Sebastian - 1. circulo, elipse, area y permietro. 2. tren, sin-cama

;;Tipo abstracto Figura que se utiliza para trabajar
;;con figuras geométricas.
;;(rombo a D d) donde a, D y d son números y representan
;;              el lado, diagonal mayor y diagonal menor.
;;(paralelogramo a b h) donde a, b y h son números reales; y
;;              representan los lados y altura.
(define-type Figura
  [rombo (a number?) (D number?) (d number?)]
  [paralelogramo (a number?) (b number?) (h number?)])

;;Función que dada una Figura regresa el perímetro
;;de esta.
;;perimetro: Figura -> number
(define (perimetro f)
  (match f
    [(rombo a D d) (* a 4)]
    [(paralelogramo a b h) (* 2 (+ a b))])
  )

;;Función que dada una figura calcule el área
;;de ésta.
;;area: Figura -> number
(define (area f)
  (match f
    [(rombo a D d) (/ (* D d) 2)]
    [(paralelogramo a b h) (* b h)])
  )


;;Función que dada una figura regrese su perímetro.
;;perimetro: Figura -> number
