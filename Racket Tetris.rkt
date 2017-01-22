;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Racket Tetris|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A Nat is a Number [0, +inf).
;; A NonNegativeInteger is a Number [1, +inf).

; list-to-n-1 : Nat -> [List-of Nat]
; Forms a list counting natural numbers from zero to n-1
(check-expect (list-to-n-1 5) '(0 1 2 3 4))
(check-expect (list-to-n-1 15) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(check-expect (list-to-n-1 1) '(0))
(check-expect (list-to-n-1 0) '())
(check-expect (list-to-n-1 10) '(0 1 2 3 4 5 6 7 8 9))

(define (list-to-n-1 n)
  (build-list n +))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; list-to-n : Nat -> [List-of Nat]
; Forms a list counting positive integers from 1 up to and including n
(check-expect (list-to-n 1) '(1))
(check-expect (list-to-n 0) '())
(check-expect (list-to-n 8) '(1 2 3 4 5 6 7 8))
(check-expect (list-to-n 4) '(1 2 3 4))

(define (list-to-n n)
  (build-list n add1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; inv-10-to-the-x : Nat -> [List-of Number]
; Forms a list with n terms of 1/10^x, starting
; at x=0 and increasing by 1 for every term
(check-expect (inv-10-to-the-x 0) '())
(check-expect (inv-10-to-the-x 1) '(1))
(check-expect (inv-10-to-the-x 2) '(1 1/10))
(check-expect (inv-10-to-the-x 3) '(1 1/10 1/100))
(check-expect (inv-10-to-the-x 6) '(1 1/10 1/100 1/1000 1/10000 1/100000))

(define (inv-10-to-the-x n)
  (build-list n (λ (x) (/ 1 (expt 10 x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; count-evens : Nat -> [List-of Nat]
; Forms a list of the first n evens, counting up and starting with 0.
(check-expect (count-evens 0) '())
(check-expect (count-evens 1) '(0))
(check-expect (count-evens 2) '(0 2))
(check-expect (count-evens 8) '(0 2 4 6 8 10 12 14))

(define (count-evens n)
  (build-list n (λ (x) (* x 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; diagonal-arr : Nat -> [List-of [List-of Nat]]
; creates a diagonal of 1s in a number grid of n x n size,
; in the format of a list of lists.
(check-expect (diagonal-arr 0) '())
(check-expect (diagonal-arr 1) '((1)))
(check-expect (diagonal-arr 2) '((1 0)
                                 (0 1)))
(check-expect (diagonal-arr 5) '((1 0 0 0 0)
                                 (0 1 0 0 0)
                                 (0 0 1 0 0)
                                 (0 0 0 1 0)
                                 (0 0 0 0 1)))

(define (diagonal-arr n)
  (build-list n (λ (x) (build-list n (λ (y) (if (= x y)
                                                1
                                                0))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tabulate : Nat [Number -> Number] -> [List-of Number]
; Tabulates a [Number -> Number] operation between n
; and 0 (inclusive) in a list.
(check-expect (tabulate 5 add1) '(6 5 4 3 2 1))
(check-expect (tabulate 6 sub1) '(5 4 3 2 1 0 -1))
(check-expect (tabulate 0 cos) '(1))
(check-within (tabulate 7 sqrt) '(2.6 2.4 2.2 2 1.7 1.4 1 0) .1)      

#;(define (tabulate-old n op)
  (cond
    [(= n 0) (list (op 0))]
    [else
     (cons (op n)
           (tabulate-old (sub1 n) op))]))

(define (tabulate n op)
  (build-list (+ n 1) (λ (x) (op (- n x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; <10? : Number -> Boolean
; Returns true if number is less than 10
(check-expect (<10? 3) true)
(check-expect (<10? 232) false)

(define <10?
  (λ (x) (< x 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; product-to-str : Number Number -> String
; Multiplies numbers x y and converts product to a string
(check-expect (product-to-str 4 3) "12")
(check-expect (product-to-str 0 2) "0")

(define product-to-str
  (λ (x y) (number->string (* x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An IR (Inventory Record) is a (make-ir String Number)
(define-struct ir (name price))

; cheaper? : IR IR -> Boolean
; Checks whether ir1 is cheaper than ir2
(check-expect (cheaper? (make-ir "abc" 10)
                        (make-ir "def" 15)) true)
(check-expect (cheaper? (make-ir "abc" 20)
                        (make-ir "def" 8)) false)

(define cheaper?
  (λ (ir1 ir2) (< (ir-price ir1)
                  (ir-price ir2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; checker : Number -> Number
; Returns 0 if n is even, returns 1 if n is odd.
(check-expect (checker 104) 0)
(check-expect (checker 7) 1)

(define checker
  (λ (n) (if (even? n)
             0
             1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; place-dot : Posn Image -> Image
; Places a red 3-px red dot on position p of the Image rect.
; Constaints: rect must be a rectangular image.
(check-expect (place-dot (make-posn 4 10)
                         (rectangle 20 40 "outline" "red"))
              (place-image (circle 3 "solid" "red")
                           4 10
                           (rectangle 20 40 "outline" "red")))

(define place-dot
  (λ (p rect) (place-image (circle 3 "solid" "red")
                           (posn-x p) (posn-y p)
                           rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEM 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; append-from-foldr : [List-of Any] [List-of Any] -> [List-of Any]
; Attaches list ys to the end of xs.
(check-expect (append-from-foldr '(1 2 3 4) '(5 6 7 8))
              '(1 2 3 4 5 6 7 8))
(check-expect (append-from-foldr '("this" "is" "a") '("complete" "sentence"))
              '("this" "is" "a" "complete" "sentence"))
(check-expect (append-from-foldr '() '()) '())

(define (append-from-foldr xs ys)
  (foldr (λ (x restx) (cons x restx)) ys xs))


;; When foldl is used the first list is read RIGHT TO LEFT
;; instead of LEFT TO RIGHT:

; append-from-foldl : [List-of Any] [List-of Any] -> [List-of Any]
; Rearranges list xs from right to left and attaches list ys at the end.
(check-expect (append-from-foldl '(1 2 3 4) '(5 6 7 8))
              '(4 3 2 1 5 6 7 8))
(check-expect (append-from-foldl '("a" "not" "is" "this")
                                 '("scrambled" "sentence"))
              '("this" "is" "not" "a" "scrambled" "sentence"))
(check-expect (append-from-foldl '() '()) '())

(define (append-from-foldl xs ys)
  (foldl (λ (x restx) (cons x restx)) ys xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sum : [List-of Number] -> Number
; sums the numbers in the list
(check-expect (sum '(2 4 6)) 12)
(check-expect (sum '(10 12 14)) 36)
(check-expect (sum '(0)) 0)
(check-expect (sum '()) 0)

(define (sum lon)
  (foldr + 0 lon))

; product : [List-of Number] -> Number
; multiplies the numbers in the list together
(check-expect (product '(2 4 6)) 48)
(check-expect (sum '(1)) 1)
(check-expect (sum '(0)) 0)
(check-expect (sum '()) 0)

(define (product lon)
  (foldr * 1 lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; horizontal : [List-of Image] -> Image
; Takes a list of images and draws the images beside each other.

(check-expect (horizontal (list (circle 10 "solid" "black")
                          (circle 10 "solid" "green")))
              (beside (circle 10 "solid" "black")
                      (circle 10 "solid" "green")))

(define (horizontal x)
  (foldr beside empty-image x))

; vertical : [List-of Image] -> Image
; Takes a list of images and draws them on top of each other.

(check-expect (vertical (list (circle 10 "solid" "black")
                          (circle 10 "solid" "green")))
              (above (circle 10 "solid" "black")
                      (circle 10 "solid" "green")))                    

(define (vertical x)
  (foldr above empty-image x))

;;> You should have included the tests, but ok

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEM 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| COMPLETED TETRIS WITH LOOP FUNCTIONS |#

;; The check-expects from the last version of tetris were left out as
;; they made the code really bulky.
;; We added check-expects for all the new row-clearing functions though!

;; TO START THE GAME, PUT THE FOLLOWING IN THE INTERACTIONS WINDOW:

#;(main START)

;; Thank you! :)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS
 
; A Block is a (make-block Number Number Color)
(define-struct block (x y color))
 
; A Tetra is a (make-tetra Posn BSet)
; The center point is the point around which the tetra rotates
; when it spins.
(define-struct tetra (center blocks))
 
; A Set of Blocks (BSet) is one of:
; - empty
; - (cons Block BSet)
; Order does not matter.
 
; A World is a (make-world Tetra [List-of BSet])
; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

;; GRID 

(define BLOCKH 30)
(define BLOCKW 30)
(define ROWS 20)
(define COLUMNS 10)
(define GRIDH (* ROWS BLOCKH))
(define GRIDW (* COLUMNS BLOCKW))

(define GRID (empty-scene GRIDW GRIDH))


;; TETRAS

(define O (make-tetra (make-posn (* BLOCKW 5) BLOCKW)
                      (list (make-block (* BLOCKW 4) 0 "green")
                            (make-block (* BLOCKW 5) 0 "green")
                            (make-block (* BLOCKW 4) BLOCKW "green")
                            (make-block (* BLOCKW 5) BLOCKW "green"))))

(define I (make-tetra (make-posn (* BLOCKW 5) 0)
                      (list (make-block (* BLOCKW 3) 0 "blue")
                            (make-block (* BLOCKW 4) 0 "blue")
                            (make-block (* BLOCKW 5) 0 "blue")
                            (make-block (* BLOCKW 6) 0 "blue"))))

(define L (make-tetra (make-posn (* BLOCKW 5) BLOCKW)
                      (list (make-block (* BLOCKW 4) BLOCKW "purple")
                            (make-block (* BLOCKW 5) BLOCKW "purple")
                            (make-block (* BLOCKW 6) BLOCKW "purple")
                            (make-block (* BLOCKW 6) 0 "purple"))))

(define J (make-tetra (make-posn (* BLOCKW 5) BLOCKW)
                      (list (make-block (* BLOCKW 4) 0 "cyan")
                            (make-block (* BLOCKW 4) BLOCKW "cyan")
                            (make-block (* BLOCKW 5) BLOCKW "cyan")
                            (make-block (* BLOCKW 6) BLOCKW "cyan"))))

(define T (make-tetra (make-posn (* BLOCKW 5) BLOCKW)
                      (list (make-block (* BLOCKW 4) BLOCKW "orange")
                            (make-block (* BLOCKW 5) BLOCKW "orange")
                            (make-block (* BLOCKW 6) BLOCKW "orange")
                            (make-block (* BLOCKW 5) 0 "orange"))))

(define Z (make-tetra (make-posn (* BLOCKW 5) 0)
                      (list (make-block (* BLOCKW 4) 0 "pink")
                            (make-block (* BLOCKW 5) 0 "pink")
                            (make-block (* BLOCKW 5) BLOCKW "pink")
                            (make-block (* BLOCKW 6) BLOCKW "pink"))))

(define S (make-tetra (make-posn (* BLOCKW 5) 0)
                      (list (make-block (* BLOCKW 4) BLOCKW "red")
                            (make-block (* BLOCKW 5) BLOCKW "red")
                            (make-block (* BLOCKW 5) 0 "red")
                            (make-block (* BLOCKW 6) 0 "red"))))

(define TESTWORLD (make-world O (list (make-block 30 570 "red")
                                      (make-block 60 570 "red")
                                      (make-block 90 570 "red")
                                      (make-block 120 570 "red")
                                      (make-block 150 570 "red")
                                      (make-block 180 570 "red")
                                      (make-block 210 570 "red")
                                      (make-block 240 570 "red")
                                      (make-block 270 570 "red")
                                      (make-block 240 540 "cyan"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TO-DRAW FUNCTIONS

; draw: World -> Image
; Draws the pile followed by the current tetra.

(define (draw w)
  (foldr (λ (x restx) (place-image (square BLOCKW
                                           "solid"
                                           (block-color x))
                                   (block-x x)
                                   (block-y x) restx))
         (draw-tetra (tetra-blocks (world-tetra w)))
         (world-pile w)))

; draw-tetra: BSet -> Image
; Turns the given tetra into an image.

(define (draw-tetra abset)
  (foldr (λ (x restx) (place-image (square BLOCKW
                                           "solid"
                                           (block-color x))
                                   (block-x x)
                                   (block-y x) restx))
         GRID
         abset))
                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ON-TICK FUNCTIONS

; move-world: World -> World
; Moves tetra one grid space down, while testing for full rows and collision.

(define (move-world w)
  (if (or (hit-bottom? (tetra-blocks (world-tetra w)))
          (hit-pile? (tetra-blocks (world-tetra w)) w))
      (add-to-pile w)
      (if (full-row? (total-rows w))
          (make-world (drop-tetra (world-tetra w))
                      (after-row-remove w))
          (make-world (drop-tetra (world-tetra w))
                      (world-pile w)))))

; drop-tetra: Tetra -> Tetra
; Moves whole tetra one grid space down.

(define (drop-tetra t)
  (make-tetra (make-posn
               (posn-x (tetra-center t))
               (+ BLOCKH (posn-y (tetra-center t))))
              (drop-tetra-blocks (tetra-blocks t))))

; drop-tetra-blocks: BSet -> BSet
; Lowers each block of the tetra one grid space down.

(define (drop-tetra-blocks abset)
  (map (λ (x) (make-block (block-x x)
                          (+ BLOCKH (block-y x))
                          (block-color x))) abset))

; add-to-pile: World -> BSet
; Adds the current tetra to the pile and generates a new tetra.

(define (add-to-pile w)
  (make-world (random-tetra (random 7))
              (append (tetra-blocks (world-tetra w))
                      (world-pile w))))

; after-row-remove : World -> BSet
; Drops all blocks above cleared row one grid space down.
(check-expect (after-row-remove TESTWORLD)
              (list (make-block 240 570 "cyan")))
              
(define (after-row-remove w)
  (map (λ (x) (if (< (block-y x) (row-y-value w))
                  (make-block (block-x x)
                              (+ BLOCKH (block-y x))
                              (block-color x))
                  x)) (remove-row w)))

; remove-row : World -> BSet
; Removes the blocks that are in the full row from the pile.
(check-expect (remove-row TESTWORLD)
              (list (make-block 240 540 "cyan")))

(define (remove-row w)
  (filter (λ (b) (not (= (row-y-value w) (block-y b)))) (world-pile w)))

; row-y-value: World -> Number
; Returns the y position of the full row.
(check-expect (row-y-value TESTWORLD) 570)

(define (row-y-value w)
  (* BLOCKH (count-row (total-rows w))))


; count-row: [List-of Number] -> Number
; Determines which number row is the full one.
(check-expect (count-row '(0 0 0 8 2 3 9 0 0 0)) 6)

(define (count-row lon)
  (foldr (λ (x y) (if (= (- COLUMNS 1) x)
                      0
                      (+ 1 y))) 0 lon))

; full-row?: [List-of Number] -> Boolean
; Is there a full row in the pile?
(check-expect (full-row? '(0 0 8 2 4 5 2 8)) false)
(check-expect (full-row? '(9 0 0 0 2 3 1)) true)
(check-expect (full-row? '()) false)

(define (full-row? lon)
  (ormap (λ (x) (<= (- COLUMNS 1) x)) lon))

; total-rows: World -> [List-of Number]
; Returns a list of how many blocks are in each row.
(check-expect (total-rows TESTWORLD) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 9))

(define (total-rows w)
  (map (λ (x) (calc-row-blocks (world-pile w) x))
       (build-list ROWS (λ (x) (* BLOCKH x)))))
    
; calc-row-blocks: BSet Number -> Number
; Calculates how many blocks are in a given row.
(check-expect (calc-row-blocks (world-pile TESTWORLD) 570) 9)

(define (calc-row-blocks bset curr-row)
  (foldr (λ (x y) (if (= (block-y x) curr-row)
                      (+ 1 y)
                      (+ 0 y))) 0 bset))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; COLLISIONS

; hit-bottom?: BSet -> Boolean
; Are any of the blocks hitting the bottom of the grid?

(define (hit-bottom? bset)
  (ormap (λ (x) (= (+ BLOCKH (block-y x)) GRIDH)) bset))

; hit-pile?: BSet World -> Boolean
; Is any part of the tetra hitting a block in a pile?

(define (hit-pile? bset w)
  (ormap (λ (tb) (ormap (λ (pb) (and (= (block-x tb) (block-x pb))
                                     (= (+ (block-y tb) BLOCKH) (block-y pb))))
                        (world-pile w))) bset))

; hit-top?: World -> Boolean
; Are any blocks in the pile hitting the top of the grid?

(define (hit-top? w)
  (ormap (λ (x) (= (block-y x) 0)) (world-pile w)))

; hit-left?: BSet -> Boolean
; Are any of the blocks hitting the left border of the grid?

(define (hit-left? abset)
  (ormap (λ (x) (= BLOCKW (block-x x))) abset))

; hit-right?: BSet -> Boolean
; Are any of the blocks hitting the right border of the grid?

(define (hit-right? abset)
   (ormap (λ (x)  (= GRIDW (+ (block-x x) BLOCKW))) abset))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RANDOMIZERS

; random-tetra: Number -> Tetra
; Selects a random tetra based on a number [0, 6].

(define (random-tetra n)
  (cond [(= n 0) O]
        [(= n 1) I]
        [(= n 2) L]
        [(= n 3) J]
        [(= n 4) T]
        [(= n 5) Z]
        [(= n 6) S]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KEY EVENTS

; key-world: World Key-Event -> World
; Creates the new world with the key press's effect.

(define (key-world w ke)
  (make-world (key-tetra (world-tetra w) ke)
              (world-pile w)))

; key-tetra: Tetra Key-Event -> Tetra
; Moves tetra respectively with arrow keys,
; rotates the tetra counterclockwise with "a",
; and rotates the tetra clockwise "s".

(define (key-tetra t ke)
  (cond [(and (key=? ke "left")
              (not (hit-left? (tetra-blocks t))))
         (make-tetra (make-posn (- (posn-x (tetra-center t)) BLOCKW)
                                (posn-y (tetra-center t)))
                     (addntoall (tetra-blocks t) (- BLOCKW)))]
        [(and (key=? ke "right")
              (not (hit-right? (tetra-blocks t))))
         (make-tetra (make-posn (+ (posn-x (tetra-center t)) BLOCKW)
                                (posn-y (tetra-center t)))
                     (addntoall (tetra-blocks t) BLOCKW))]
        [(and (key=? ke "down")
              (not (hit-bottom? (tetra-blocks t))))
         (drop-tetra t)]
        [(key=? ke "a") (make-tetra (tetra-center t)
                                    (rotate-all-blocks t block-rotate-ccw))]
        [(key=? ke "s") (make-tetra (tetra-center t)
                                    (rotate-all-blocks t block-rotate-cw))]
        [else (make-tetra (tetra-center t) (tetra-blocks t))]))
       

; addntoall: BSet Number -> BSet
; Adds n to every block in the list.

(define (addntoall abset n)
  (map (λ (x) (make-block (+ (block-x x) n)
                          (block-y x)
                          (block-color x))) abset))


; block-rotate-ccw: Posn Block -> Block
; Rotate the block 90 degrees counterclockwise around the posn.
(define (block-rotate-ccw c b)
  (make-block (+ (posn-x c)
                 (- (posn-y c)
                    (block-y b)))
              (+ (posn-y c)
                 (- (block-x b)
                    (posn-x c)))
              (block-color b)))

; block-rotate-ccw: Posn Block -> Block
; Rotate the block 90 degrees clockwise around the posn.


(define (block-rotate-cw c b)
  (block-rotate-ccw c (block-rotate-ccw c (block-rotate-ccw c b))))


; A Rotation is one of:
; - block-rotate-ccw
; - block-rotate-cw

; rotate-all-blocks: Tetra Rotation -> BSet
; Rotates every block in the tetra with the given Rotation.


(define (rotate-all-blocks t r)
  (map (λ (x) (r (tetra-center t) x)) (tetra-blocks t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BIG-BANG

(define (main w)
  (big-bang w
            [to-draw draw]
            [on-tick move-world .53]
            [on-key key-world]
            [stop-when hit-top?]))

(define START (make-world (random-tetra (random 7)) empty))
               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  