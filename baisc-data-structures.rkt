;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname baisc-data-structures) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; For more information on how to make modules, check out the following link:
; https://docs.racket-lang.org/guide/module-basics.html

; A Sprite is one of these guys:
(define-struct sprite [image edges])
; Where image is the actual Image that gets placed on a canvas
; and edges is a [Listof Inequalitites] where each Inequality defines an edge of the image
;
; INTERPRETATION: The idea is that we need to have a structure that associates the image with its
; edges.

; Constructs a Sprite from a list of points (auto-generates the Inequalities for the edges)
; make-sprite-from-points: Image [Listof Posns] -> Sprite
(define (make-sprite-from-points)
  ;TODO -- Write function
  
  #false)

; An Inequality is one of these guys:
(define-struct inequality [m b direction])
; Where m represents the slope of the line equation
; and b represents the y-intercept of the equation
; and direction is one of the following functions:
;    - >
;    - >=
;    - <
;    - <=
; TODO -- Would it make more sense to define these as symbols, passed functions, or lambdas?

; Evaluates an Inequality for a given point
; eval-inequality: Point Inequality -> Boolean
(define (eval-inequality)
  ;TODO -- Write function
  #false)
