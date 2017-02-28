;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname collisions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; an Angl is a (make-angl Posn Posn Posn)
;;; - where the posns go in order of endpoint first line, center, and endpoint of second line
;;;   and the angle is measured counter-clockwise
(define-struct angl [point1 center point2])

;;; a Perimiter is a (make-perim LoA Bool)
;;; - where Bool is whether the perimtere describes a convex shape
;;; - and LoA is one of
;;; - (list Angle Angle Angle)
;;; - (cons Angle Perimiter)
(define-struct perim [LoA convex?])

; split-sprite : [List-of Points] -> [List-of [List-of Points]]
; splits the sprite's vertecies into parts which are each convex
#;
(define (split-shape lop)
  (local [(define perim (make-perimiter lop)) ; perimeter of shape
          ;; find-concave : perim -> Nat
          ;; index of first concave
          (define (find-concave perim) ...)

          ;; cycle-list : nat [List-of X] -> [List-of X]
          ;; cylces perim so nat'th element is first ellement
          ;; example: (cycle-list : 2 (list 1 2 3 4)) -> (list 3 4 0 1 2)
          (define (cycle-list n lst) ...)

          ;; one-path : perim -> perim perim
          ;; cuts on concave section off starting with the first angle
          (define (one-path perim))

          ;; recur until find-concave is false forever or something
   ....]))


;;; make-perimeter : [List-of Posns] -> Perimeter
;;; converts every vertex to an angle with the posn as the center
(define (make-perimeter lop)
  (local [;; format-lop : [List-of X] -> [List-of X]
          ;; last element is added to the beginning and the first element is apendded to the end
          ;; of the list
          ;; - list is formated to represent rapping of points since list represent continuous object
          (define (format-list lst)
            (append (cons (last lst) lst) (list (first lst))))
          ;; recursive-fun : [List-of Posns] -> Perimeter
          ;; takes in fromated lop and replaces every point of object with angle
          (define (make-loa lop)
            (cond [(empty? (rest (rest lop))) empty]
                  [else (cons (make-angl (first  lop)
                                         (second lop)
                                         (third  lop))
                              (make-loa (rest lop)))]))
          (define loa (make-loa (format-list lop)))] ; list of angles
    (make-perim loa (andmap convex? loa))))


(check-expect (make-perimeter (list (make-posn 0 0) (make-posn 0 2)
                                    (make-posn 2 2) (make-posn 1 1)
                                    (make-posn 2 0)))                 ; shape that looks like [<
              (make-perim (list (make-angl (make-posn 2 0) (make-posn 0 0) (make-posn 0 2))
                                (make-angl (make-posn 0 0) (make-posn 0 2) (make-posn 2 2))
                                (make-angl (make-posn 0 2) (make-posn 2 2) (make-posn 1 1))
                                (make-angl (make-posn 2 2) (make-posn 1 1) (make-posn 2 0))
                                (make-angl (make-posn 1 1) (make-posn 2 0) (make-posn 0 0)))
                          #false))
(check-expect (make-perimeter (list (make-posn 0 0) (make-posn 0 1)
                                    (make-posn 1 1) (make-posn 1 0))) ; square
              (make-perim (list (make-angl (make-posn 1 0) (make-posn 0 0) (make-posn 0 1))
                                (make-angl (make-posn 0 0) (make-posn 0 1) (make-posn 1 1))
                                (make-angl (make-posn 0 1) (make-posn 1 1) (make-posn 1 0))
                                (make-angl (make-posn 1 1) (make-posn 1 0) (make-posn 0 0)))
                          #true))

;;; last : [List-of X] -> X
;;; last element of list
(define (last lst)
  (cond [(empty? lst)        (error "last expected a non-empty list but recieved empty")]
        [(empty? (rest lst)) (first lst)]
        [else                (last (rest lst))]))

(check-expect (last (list 2 5 6)) 6)
(check-expect (last (list 3))     3)

;;; convex? : Angl -> Boolean
;;; is the angle less than 180?
;;; calculates by determining if the cross product BA X BC is positive in the Z
;;; ... direction where BA and BC are vectors in 3 space with Z = 0

(define (convex? angl)
  (local [;; disp-vec : Posn Posn -> Posn
          ;; makes displacement vector from posn2 to posn1.
          (define (disp-vec posn1 posn2) (make-posn (- (posn-x posn1)
                                                       (posn-x posn2))
                                                    (- (posn-y posn1)
                                                       (posn-y posn2))))
          (define vecBA (disp-vec (angl-point1 angl) (angl-center angl)))
          (define vecBC (disp-vec (angl-point2 angl) (angl-center angl)))
          (define vecBA_X_vecBC (- (* (posn-x vecBA) (posn-y vecBC))    ; the z component of cross
                                   (* (posn-y vecBA) (posn-x vecBC))))] ; product 
    (cond [(zero?     vecBA_X_vecBC) (error "points are collinear")]
          [(positive? vecBA_X_vecBC) #t]
          [(negative? vecBA_X_vecBC) #f]
          [else (error "something went wrong with cconvex?...")])))

(check-expect (convex? (make-angl (make-posn 1 0)
                                  (make-posn 0 0)
                                  (make-posn 0 1)))
              #t)
(check-expect (convex? (make-angl (make-posn -3 -1)
                                  (make-posn 0 0)
                                  (make-posn 2 -1)))
              #t)
(check-expect (convex? (make-angl (make-posn 1 0)
                                  (make-posn 0 0)
                                  (make-posn 2 -1)))
              #f)
(check-expect (convex? (make-angl (make-posn 1 0)
                                  (make-posn 0 0)
                                  (make-posn 0 -1)))
              #f)
(check-error  (convex? (make-angl (make-posn -1 0)
                                  (make-posn 1 4)
                                  (make-posn 5 12)))
              "points are collinear")


