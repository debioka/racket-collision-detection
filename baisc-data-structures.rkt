;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname baisc-data-structures) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; For more information on how to make modules, check out the following link:
; https://docs.racket-lang.org/guide/module-basics.html

; A Sprite is one of these guys:
(define-struct sprite [image loe center])
; Where image is the actual Image that gets placed on a canvas
; - loes is a [List-of Edges]
; - edges is a [List-of Inequalitites] where each Inequality defines an edge of the image parameteried by the center of the image
; - center is some arbitrary point to use as refrence. It is best

; INTERPRETATION: The idea is that we need to have a structure that associates the image with its
; edges.

; Constructs a Sprite from a list of points in order by conection(auto-generates the Inequalities for the edges)
; make-sprite-from-points: Image [Listof Posns] -> Sprite
(define (make-sprite-from-points)
  ;TODO -- Write function
  
  #false)
  
; convex? : Posn Posm Posn -> Boolean
; is the angle /_ posnA posnB posnC les than 180 going counter clockwise?
; calculates by determining if the cross product BA X BC is positive in the Z
; ... direction where BA and BC are vectors in 3 space with Z = 0

(define (convex? posnA posnB posnC)
  (local [; disp-vec : Posn Posn -> Posn
          ; makes displacement vector posn1 - posn2.
          (define (disp-vec posn1 posn2) (make-posn (- (posn-x posn1)
                                                       (posn-x posn2))
                                                    (- (posn-y posn1)
                                                       (posn-y posn2))))
          (define vecBA (disp-vec posnA posnB))
          (define vecBC (disp-vec posnC posnB))
          (define vecBA_X_vecBC (- (* (posn-x vecBA) (posn-y vecBC))
                                   (* (posn-y vecBA) (posn-x vecBC))))]
    (cond [(zero?     vecBA_X_vecBC) (error "points are collinear")]
          [(positive? vecBA_X_vecBC) #t]
          [(negative? vecBA_X_vecBC) #f]
          [else (error "something went wrong...")])))

(check-expect (convex? (make-posn 1 0)
                       (make-posn 0 0)
                       (make-posn 0 1))
              #t)
(check-expect (convex? (make-posn -3 -1)
                       (make-posn 0 0)
                       (make-posn 2 -1))
              #t)
(check-expect (convex? (make-posn 1 0)
                       (make-posn 0 0)
                       (make-posn 2 -1))
              #f)
(check-expect (convex? (make-posn 1 0)
                       (make-posn 0 0)
                       (make-posn 0 -1))
              #f)
(check-error  (convex? (make-posn -1 0)
                       (make-posn 1 4)
                       (make-posn 5 12))
              "points are collinear")



                                                               

; An Inequality is one of these guys:
(define-struct inequality [m b direction])
; Where m represents the slope of the line equation
; and b is a [posn -> posn] and generate the y-intercept of the equation based on the position of the center
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
