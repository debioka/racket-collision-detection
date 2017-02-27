;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname colion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; an Line is a (list Number Number Number)
; - where (list a b c) represents the equation aX + bY = c
(define line-x first)
(define line-y second)
(define line-c third)

; split-sprite : [List-of Points] -> [List-of [List-of Points]]   (use point itself that is use first convex point
; splits the sprite's vertecies into parts which are each ... :'(
#;(define (split-shape lop)
    (local [(define lop-total (append lop
                                      (list (first lop)
                                            (second lop))))
            (define (recusion lop)
              (cond [(< (length lop) 3) empty]
                    [(convex? (first lop) (second lop) (third lop))
                     (cons (first lop) (recursion))]
                    [else (...

; point-of-intersection : Line Line -> Posn   (not neccessary anymore)
; the point of intersection between the line AB and CD
; computes via solution of matrix of two equations. Assumes that the two lines are not parallel
(define (point-of-intersection equation1 equation2)
  (cond [(and (zero? (line-y equation1))
              (zero? (line-x equation2)))
         (make-posn (/ (line-c equation1) (line-x equation1))
                    (/ (line-c equation2) (line-y equation2)))]
        [(zero? (line-y equation1))
         (point-of-intersection equation1 
                               (sub-line equation1
                                         (/ (line-x equation2) (line-x equation1))
                                         equation2))]
        [(zero? (line-y equation2))
         (point-of-intersection equation2
                                equation1)]
        [else
         (point-of-intersection (sub-line equation2
                                          (/(line-y equation1) (line-y equation2))
                                          equation1)
                                equation2)]))
(check-expect (point-of-intersection (list 2 3 7)
                                     (list 3 5 11))
              (make-posn 2 1))


; sub-line: Line Number Line -> Line
; subtracts the first line from the second line with a constant multiple
(define (sub-line sub-l multiplier base-l)
  (list (- (line-x base-l)
           (* (line-x sub-l) multiplier))
        (- (line-y base-l)
           (* (line-y sub-l) multiplier))
        (- (line-c base-l)
           (* (line-c sub-l) multiplier))))

(check-expect (sub-line (list 1 2 3) 2 (list 9 9 9))
              (list                          7 5 3))

; make-equation : Posn Posn -> Line
; eqation of line that passes through two posns
(define (make-equation posn1 posn2)
  (local [(define a (- (posn-y posn1) (posn-y posn2)))
          (define b (- (posn-x posn1) (posn-x posn2)))
          (define c (+ (* a (posn-x posn1)) (* b (posn-y posn1))))]
    (list a b c)))

(check-expect (make-equation (make-posn 0 3) (make-posn -4 -5))
              (list 8 4 12))

