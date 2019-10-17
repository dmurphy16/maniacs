;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |hw 6|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; HOMEWORK 6 - Deirdre Murphy and Ahmed Altunisi

(require 2htdp/universe)
;; EXERCISE 1

; sum-x-coords : [List-of Posn] -> Number
; Sum all the x-coordinates in the list of positions
(check-expect (sum-x-coords empty) 0)
(check-expect (sum-x-coords
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 8)
(define (sum-x-coords lop)
  (cond
    [(empty? lop) 0]
    [(cons? lop)
     (+ (posn-x (first lop))
        (sum-x-coords (rest lop)))]))
 
; mult-distances : [List-of Posn] -> Number
; Multiply all the distances from each position to the origin
(check-expect (mult-distances empty) 1)
(check-expect (mult-distances
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 65)
(define (mult-distances lop)
  (cond
    [(empty? lop) 1]
    [(cons? lop)
     (* (distance-to-origin (first lop))
        (mult-distances (rest lop)))]))
 
; distance-to-origin : Posn -> Number
; Produces the distance from this position to the origin
(check-within (distance-to-origin (make-posn 2 2)) (sqrt 8) 1e-06)
(check-expect (distance-to-origin (make-posn 3 4)) 5)
(define (distance-to-origin p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

; operate-posn: [List-of Posn] Number [Number Number -> Number] [Posn -> Number] -> Number
; either sums all x-coordinates in list of posns or multiplies all distances
; from posns to origin depending on given operation
(check-expect (operate-posn empty 0 + posn-x) 0)
(check-expect (operate-posn
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))
               0 + posn-x) 8)
(check-expect (operate-posn empty 1 * distance-to-origin) 1)
(check-expect (operate-posn
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))
               1 * distance-to-origin) 65)

(define (operate-posn lop base math-op posn-op)
  (cond
    [(empty? lop) base]
    [(cons? lop)
     (math-op (posn-op (first lop))
              (operate-posn (rest lop) base math-op posn-op))]))

; sum-x-coords.v2: [List-of Posn] -> Number
; Sum all the x-coordinates in the list of positions
(check-expect (sum-x-coords empty) 0)
(check-expect (sum-x-coords
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 8)

(define (sum-x-coords.v2 lop)
  (operate-posn lop 0 + posn-x))

; mult-distances.v2 : [List-of Posn] -> Number
; Multiply all the distances from each position to the origin
(check-expect (mult-distances empty) 1)
(check-expect (mult-distances
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 65)
(define (mult-distances.v2 lop)
  (operate-posn lop 1 * distance-to-origin))


; biggest-difference: [List-of Posns] -> Number
; produces the largest difference between a posn's x and y values
; in a given list of posns
(check-expect (biggest-difference empty) 0)
(check-expect (biggest-difference
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 7)

(define (biggest-difference lop)
  (operate-posn lop 0 max posn-diff))

; posn-diff: Posn -> Number
; returns the difference between a posn's x and y
(check-expect (posn-diff (make-posn 3 5)) 2)
(check-expect (posn-diff (make-posn 6 0)) 6)
(check-expect (posn-diff (make-posn 7 2)) 5)

(define (posn-diff p)
  (abs (- (posn-x p) (posn-y p))))

; EXERCISE 2

; my-animate: [Number-> Image} -> Number   
; re-implements animate function as a big-bang function


(define (my-animate imagefunc)
  (big-bang 0
    [to-draw imagefunc]
    [on-tick add1]))
   

;; EXERCISE 3
(define LOS-0 (cons "hello" empty))
(define LOS-1 (cons "dog" LOS-0))
(define LOS-2 (cons "zoo" LOS-1))
(define LOS-3 (cons "apple" LOS-2))
(define LOS-4 (cons "cat" LOS-3))


; earliest: [Non-Empty List of Strings] [String String -> Boolean] -> String
; outputs string that comes earliest in non-empty list according to given function
(check-expect (earliest LOS-0 string<?) "hello")
(check-expect (earliest LOS-2 string>?) "zoo")
(check-expect (earliest LOS-4 last-empty?) "hello")

(define (earliest nelos before?)
  (cond
    [(empty? (rest nelos)) (first nelos)]
    [(cons? (rest nelos)) (if (before? (first nelos) (earliest (rest nelos) before?))
                              (first nelos)
                              (earliest (rest nelos) before?))]))

; earliest.v1: [Non-Empty List of Strings] [String String -> Boolean] -> String
; outputs string from given non-empty list that comes earliest lexographically
(check-expect (earliest.v1 LOS-0) "hello")
(check-expect (earliest.v1 LOS-4) "apple")

(define (earliest.v1 nelos)
  (earliest nelos string<?))

; earliest.v2: [Non-Empty List of Strings] [String String -> Boolean] -> String
; outputs string from given non-empty list that comes last lexographically
(check-expect (earliest.v2 LOS-0) "hello")
(check-expect (earliest.v2 LOS-4) "zoo")

(define (earliest.v2 nelos)
  (earliest nelos string>?))

; earliest.v3: [Non-Empty List of Strings] [String String -> Boolean] -> String
; outputs string from given non-empty list that comes last in non-empty list
(check-expect (earliest.v3 LOS-0) "hello")
(check-expect (earliest.v3 LOS-3) "hello")
(define (earliest.v3 nelos)
  (earliest nelos last-empty?))

; last-empty?: String String -> Boolean
; returns true if second string is empty string, false otherwise
(check-expect (last-empty? "hi" "bye") false)
(check-expect (last-empty? "cat" "") true)
(define (last-empty? s1 s2)
  (if (string=? "" s2) true false))

;; EXERCISE 4
(define-struct cup [oz color material])
 
; A Cup is a (make-cup NonNegNumber String String)
; and represents a cup's capacity in fluid ounces, color, and material
 
(define CUP1 (make-cup 10 "brown" "wood"))
(define CUP2 (make-cup 8 "brown" "ceramic"))
(define CUP3 (make-cup 10 "red" "plastic"))
(define CUP4 (make-cup 6 "clear" "plastic"))
(define CUP5 (make-cup 8 "blue" "wood"))
 
(define CUPS
  (cons CUP1
        (cons CUP2
              (cons CUP3
                    (cons CUP4
                          (cons CUP5 empty))))))

(check-expect (create-cup-binning CUPS "brown" cup-color)
              (list (make-cup 10 "brown" "wood") (make-cup 8 "brown" "ceramic")))

(define (create-cup-binning cup color property)
  (cond
    [(empty? cup) empty]
    [(cons? cup)
     (if (string=? color (property (first cup)))
         (cons (first cup) (create-cup-binning (rest cup) color property))
         (create-cup-binning (rest cup) color property))]))

; create-binning: (X Y) [List-of X] [X -> Y] [Y Y[-> Boolean] -> [List-of Bins] 
; produces a binning of elements X based on their propterties, Y

(define-struct bin [property lox])

(define BIN-1 (make-bin "brown" CUPS 
(define BIN-2 (make-bin 10 (list CUP1 CUP3 CUP2 CUP4 CUP5)))
(define BIN-3 (make-bin "plastic" (list CUP3 CUP4 CUP1 CUP2 CUP5)))

(check-expect (create-binning-helper CUPS "brown" string=? cup-color)
              (list (make-cup 10 "brown" "wood") (make-cup 8 "brown" "ceramic")))
(check-expect (create-binning-helper CUPS 10 = cup-oz)
              (list (make-cup 10 "brown" "wood") (make-cup 10 "red" "plastic")))
(check-expect (create-binning-helper CUPS "wood" string=? cup-material)
              (list (make-cup 10 "brown" "wood") (make-cup 8 "blue" "wood")))
(check-expect (create-binning-helper CUPS "plastic" string=? cup-material)
              (list (make-cup 10 "red" "plastic") (make-cup 6 "clear" "plastic")))

(define (create-binning-helper lox binner equiv? extract)
  (cond
    [(empty? lox) empty]
    [(cons? lox)
     (if (equiv? binner (extract (first lox)))
         (cons (first lox) (create-binning-helper (rest lox) binner equiv? extract))
         (create-binning-helper (rest lox) binner equiv? extract))]))
