#lang typed/racket
(require "../include/uchicago151.rkt")
(require typed/test-engine/racket-tests)
(require typed/2htdp/image)


(define-type (Optional a)
  (U 'none (Some a)))

(define-struct (Some a)
  ([x : a]))

(define-type Player 
  (U 'black 'white))

(define-struct Loc
  ([row : Integer]   ;; an integer on the interval [0,5]
   [col : Integer])) ;; an integer on the interval [0,5]

(define-type Quadrant
  (U 'NW 'NE 'SW 'SE))

(define-type Direction
  (U 'clockwise 'counterclockwise))

(define-struct Board
  ([NW : (Listof (Optional Player))] ;; these are all lists of length 9
   [NE : (Listof (Optional Player))]
   [SW : (Listof (Optional Player))]
   [SE : (Listof (Optional Player))]))

(define-struct Game
  ([board : Board]
   [next-player : Player]
   [next-action : (U 'place 'twist)]))

(define-type Outcome
  (U Player 'tie))

(: empty-board : Board)
(define empty-board
  (Board (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)
         (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)
         (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)
         (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)))

(define first-player
  (list 'black 'white))

(: new-game : Game)
;; starting position in new game
(define new-game
  (Game empty-board
        (list-ref first-player (random 2))
        'place))

(: next : Player -> Player)
;; switches player
(define (next p)
  (if (eq? p 'black) 'white 'black))

(: quad-ref : Loc -> Quadrant)
;; returns the quadrant given loc
(define (quad-ref loc)
  (match loc
    [(Loc r c)
     (cond
       [(and (> 3 r) (> 3 c))
        'NW]
       [(and (> 3 r) (<= 3 c))
        'NE]
       [(and (<= 3 r) (> 3 c))
        'SW]
       [(and (<= 3 r) (<= 3 c))
        'SE]
       [else (error "out of bounds")])]))

(: board-ref : Board Loc -> (Optional Player))
;; returns the contents of given square
(define (board-ref b loc)
  (match loc
    [(Loc r c)
     (cond
       [(eq? (quad-ref loc) 'NW)
        (list-ref (Board-NW b) (+ c (* 3 r)))]
       [(eq? (quad-ref loc) 'NE)
        (list-ref (Board-NE b) (+ (- c 3) (* 3 r)))]
       [(eq? (quad-ref loc) 'SW)
        (list-ref (Board-SW b) (+ c (* 3 (- r 3))))]
       [(eq? (quad-ref loc) 'SE)
        (list-ref (Board-SE b) (+ (- c 3) (* 3 (- r 3))))]
       [else (error "out of bounds")])]))

(: switch : All (a) (Listof a) a Integer -> (Listof a))
;; switches elements of a list to given one given ref
(define (switch xs x n)
  (cond
    [(= n 0) (cons x (rest xs))]
    [else (append (list (first xs))
                  (switch (rest xs) x (sub1 n)))]))

(: quad-place : (Listof (Optional Player)) (Optional Player) Loc -> (Listof (Optional Player)))
;; places marble in the quadrant
(define (quad-place q p loc)
  (match loc
    [(Loc r c)
     (switch q p (+ c (* 3 r)))]))

(: board-place : Board (Optional Player) Loc -> Board)
;; places a marble or none given board, player or none and square
(define (board-place b p loc)
  (match b
    [(Board nw ne sw se)
     (match loc
       [(Loc r c)
        (cond
          [(and (> 3 r) (> 3 c))
           (Board (quad-place nw p loc) ne sw se)]
          [(and (> 3 r) (<= 3 c))
           (Board nw (quad-place ne p (Loc r (- c 3))) sw se)]
          [(and (<= 3 r) (> 3 c))
           (Board nw ne (quad-place sw p (Loc (- r 3) c)) se)]
          [(and (<= 3 r) (<= 3 c))
           (Board nw ne sw (quad-place se p (Loc (- r 3) (- c 3))))]
          [else (error "out of bounds")])])]))

(: place-marble : Game Player Loc -> Game)
;; places a marble given game, player and square
;; square must be given by (row, column) integers between 0 and 5
(define (place-marble g p loc)
  (match g
    [(Game b n-p n-a)
     (cond
       [(not (eq? p n-p)) (error "Not your turn")]
       [(eq? p 'none) (error "invalid player")]
       [(not (eq? n-a 'place)) (error "Must twist")]
       [(or (not (<= 0 (Loc-row loc) 5))
            (not (<= 0 (Loc-col loc) 5)))
        (error "Row and column must be between 0 and 5")]
       [(not (eq? (board-ref b loc) 'none))
        (error "The place is not empty")]
       [else (Game (board-place b (Some p) loc) n-p 'twist)])]))

(: twist : (Listof (Optional Player)) Direction -> (Listof (Optional Player)))
;; twists a list in given direction
(define (twist xs d)
  (match d
    ['clockwise
     (list (list-ref xs 6)
           (list-ref xs 3)
           (list-ref xs 0)
           (list-ref xs 7)
           (list-ref xs 4)
           (list-ref xs 1)
           (list-ref xs 8)
           (list-ref xs 5)
           (list-ref xs 2))]
    ['counterclockwise
     (list (list-ref xs 2)
           (list-ref xs 5)
           (list-ref xs 8)
           (list-ref xs 1)
           (list-ref xs 4)
           (list-ref xs 7)
           (list-ref xs 0)
           (list-ref xs 3)
           (list-ref xs 6))]
    [else (error "Invalid direction")]))

(: twist-quadrant : Game Quadrant Direction -> Game)
;; twists given quadrant in given direction
(define (twist-quadrant g q d)
  (match g
    [(Game b n-p n-a)
     (cond
       [(not (eq? n-a 'twist)) (error "Must place")]
       [else (match b
               [(Board nw ne sw se)
                (match q
                  ['NW (Game (Board (twist nw d) ne sw se)
                             (next n-p)
                             'place)]
                  ['NE (Game (Board nw (twist ne d) sw se)
                             (next n-p)
                             'place)]
                  ['SW (Game (Board nw ne (twist sw d) se)
                             (next n-p)
                             'place)]
                  ['SE (Game (Board nw ne sw (twist se d))
                             (next n-p)
                             'place)]
                  [else (error "Invalid quadrant")])])])]))

(: board-full? : Board -> Boolean)
;; checks if board is full
(define (board-full? b)
  (match b
    [(Board nw ne sw se)
     (if (member 'none (append nw ne se sw)) #f #t)]))

(: same? : All (a) (U 'none (Some a)) (U 'none (Some a)) -> Boolean)
;; checks if two contents are same
(define (same? x y)
  (match x
    [(Some m )
     (match y
       [(Some n)
        (eq? n m)]
       [_ #f])]
     [_ #f]))

(: row-done? : Board Integer -> (Optional Player))
;; checks if board has five marbles horizontally given row
;; returns type of marble
(define (row-done? b n)
  (if (and (same? (board-ref b (Loc n 1))
                  (board-ref b (Loc n 2)))
           (same? (board-ref b (Loc n 1))
                  (board-ref b (Loc n 3)))
           (same? (board-ref b (Loc n 1))
                  (board-ref b (Loc n 4)))
           (or (same? (board-ref b (Loc n 1))
                      (board-ref b (Loc n 0)))
               (same? (board-ref b (Loc n 1))
                      (board-ref b (Loc n 5)))))
      (board-ref b (Loc n 3))
      'none))
     

(: col-done? : Board Integer -> (Optional Player))
;; checks if column has five marbles vertically
(define (col-done? b n)
  (if (and (same? (board-ref b (Loc 1 n))
                  (board-ref b (Loc 2 n)))
           (same? (board-ref b (Loc 1 n))
                  (board-ref b (Loc 3 n)))
           (same? (board-ref b (Loc 1 n))
                  (board-ref b (Loc 4 n)))
           (or (same? (board-ref b (Loc 1 n))
                      (board-ref b (Loc 0 n)))
               (same? (board-ref b (Loc 1 n))
                      (board-ref b (Loc 5 n)))))
      (board-ref b (Loc 3 n))
      'none))


(: check-all : All (a) Board (Board Integer -> a) -> (Listof a))
;; checks if a function is satisfied on the board in all rows/columns
(define (check-all b f)
  (list (f b 0) (f b 1) (f b 2) (f b 3) (f b 4) (f b 5)))
    

(: loc-se : Loc Integer -> Loc)
;; move n places se of given
(define (loc-se loc n)
  (match loc
    [(Loc r c) (Loc (+ n r) (+ n c))]))

(: loc-ne : Loc Integer -> Loc)
;; move n places ne of given
(define (loc-ne loc n)
  (match loc
    [(Loc r c) (Loc (- r n) (+ n c))]))

(: diag? : Board Loc -> (Optional Player))
;; checks if there are 5 in a row in a diagonal given loc
(define (diag? b loc)
  (cond
    [(eq? (board-ref b loc) 'none) 'none]
    [(and (eq? (quad-ref loc) 'NW)
          (same? (board-ref b loc)
                 (board-ref b (loc-se loc 1)))
          (same? (board-ref b loc)
                 (board-ref b (loc-se loc 2)))
          (same? (board-ref b loc)
                 (board-ref b (loc-se loc 3)))
          (same? (board-ref b loc)
                 (board-ref b (loc-se loc 4))))
     (board-ref b loc)]
    [(and (eq? (quad-ref loc) 'SW)
          (same? (board-ref b loc)
                 (board-ref b (loc-ne loc 1)))
          (same? (board-ref b loc)
                 (board-ref b (loc-ne loc 2)))
          (same? (board-ref b loc)
                 (board-ref b (loc-ne loc 3)))
          (same? (board-ref b loc)
                 (board-ref b (loc-ne loc 4))))
     (board-ref b loc)]
    [else 'none]))

(: diag-done? : Board -> (Listof (Optional Player)))
;; checks if there are 5 in a row diagonally on board
(define (diag-done? b)
  (list (diag? b (Loc 0 0))
        (diag? b (Loc 1 0))
        (diag? b (Loc 0 1))
        (diag? b (Loc 1 1))
        (diag? b (Loc 4 0))
        (diag? b (Loc 5 0))
        (diag? b (Loc 4 1))
        (diag? b (Loc 5 1))))

(: contains? : (Optional Player) (Listof (Optional Player)) -> Boolean)
(define (contains? x xs)
  (cond
    [(empty? xs) #f]
    [(cons? xs) (or (same? x (first xs))
                    (contains? x (rest xs)))]))

(: five-white? : Board -> Boolean)
;; checks if board has 5 in a row vertically, horizontally of diagonally
(define (five-white? b)
  (or (contains? (Some 'white) (diag-done? b))
      (contains? (Some 'white) (check-all b row-done?))
      (contains? (Some 'white) (check-all b col-done?))))

(: five-black? : Board -> Boolean)
;; checks if board has 5 in a row vertically, horizontally of diagonally
(define (five-black? b)
  (or (contains? (Some 'black) (diag-done? b))
      (contains? (Some 'black) (check-all b row-done?))
      (contains? (Some 'black) (check-all b col-done?))))

(define test-board
  (Board (list (Some 'white) (Some 'black) 'none (Some 'white) 'none (Some 'black)  (Some 'white) 'none 'none )
         (list 'none 'none 'none 'none 'none 'none (Some 'black) 'none 'none)
         (list (Some 'white) 'none (Some 'black)  (Some 'white) 'none (Some 'black)  (Some 'white) 'none 'none)
         (list (Some 'black) (Some 'black) 'none 'none (Some 'black) (Some 'black) 'none 'none (Some 'black))))
           
(: game-over? : Game -> Boolean)
;; checks if game is over
(define (game-over? g)
  (match g
    [(Game b n-p n-a)
     (or (five-white? b)
         (five-black? b)
         (board-full? b))]))

(: outcome : Game -> Outcome)
; returns outcome of the game
(define (outcome g)
  (match g
    [(Game b n-p n-a)
     (cond
       [(and (five-white? b)
             (five-black? b))
        'tie]
       [(five-white? b) 'white]
       [(five-black? b) 'black]
       [(board-full? b) 'tie]
       [else (error "Game not over")])]))

;; === Visualizations ===

(: num-img : Integer Integer -> Image)
(define (num-img n s)
  (if (<= s 0) (error "negative size")
  (underlay
   (square s 0 'white)
   (scale (/ s 20) (text  (number->string n) 10 'ivory)))))

(: nums-image : Integer (-> Image Image Image * Image) -> Image)
(define (nums-image n f)
  (f (num-img 0 n)
     (num-img 1 n)
     (num-img 2 n)
     (num-img 3 n)
     (num-img 4 n)
     (num-img 5 n)))

(: board-bg : Integer -> Image)
(define (board-bg n)
  (if (< n 0) (error "negative dimensions")
  (underlay/align "left" "middle"
                  (underlay/align "middle" "top"
                                  (square n "solid" 'olive)
                                  (nums-image (quotient n 8) beside))
                  (nums-image (quotient n 8) above))))

(: grid-img : Integer -> Image)
(define (grid-img n)
  (if (< n 0) (error "negative dimensions")
      (scene+line
       (scene+line
        (scene+line
         (scene+line (square n 0 'white)
                     (quotient n 3) 0
                     (quotient n 3) n
                     'black)
         (* 2 (quotient n 3)) 0
         (* 2 (quotient n 3)) n
         'black)
        0 (quotient n 3)
        n (quotient n 3)
        'black)
       0 (* 2 (quotient n 3))
       n (* 2 (quotient n 3))
       'black)))

(: quad-img : Integer Image-Color (Listof (Optional Player)) -> Image)
(define (quad-img n c xs)
  (if (<= n 0) (error "negative dimensions")
      (underlay
       (underlay (frame (square (quotient (* 3 n) 8) "solid" c))
                 (grid-img (quotient (* 3 n) 8)))
       (pieces-img (pieces->image (quotient (* 3 n) 8) xs)))))

(: piece : Integer Image-Color -> Image)
(define (piece n c)
  (if (< n 0) (error "negative dimensions")
      (underlay
       (square (quotient n 3) 0 'cyan)
       (match c
         ['tan (scale 9/10 (circle (quotient n 6) 0 c))]
         [_
          (scale 4/5 (circle (quotient n 6) "solid" c))]))))


(: pieces->image : Integer (Listof (Optional Player)) -> (Listof Image))
(define (pieces->image n xs)
  (match xs
    ['() (list empty-image)]
    [(cons hd tl)
     (cons
      (match hd
        ['none (piece n 'tan)]
        [(Some 'black) (piece n 'black)]
        [(Some 'white) (piece n 'white)])
        (pieces->image n tl))]))
  

(: pieces-img : (Listof Image) -> Image)
(define (pieces-img xs)
  (above
   (beside (list-ref xs 0)
           (list-ref xs 1)
           (list-ref xs 2))
   (beside (list-ref xs 3)
           (list-ref xs 4)
           (list-ref xs 5))
   (beside (list-ref xs 6)
           (list-ref xs 7)
           (list-ref xs 8))))

(: 4-quad : Integer Board -> Image)
(define (4-quad n b)
  (match b
    [(Board nw ne sw se)
     (above
      (beside (quad-img n 'crimson nw)
              (quad-img n 'indianred ne))
      (beside (quad-img n 'indianred sw)
              (quad-img n 'crimson se)))]))
   
(: board-image : Board Integer -> Image)
;; produces an image of the board of width n
(define (board-image b n)
  (underlay
   (board-bg n)
   (4-quad n b)))

(: player->string : Player -> String)
(define (player->string p)
  (match p
    ['black "Black"]
    ['white "White"]))

(: comment-img : Player (U 'place 'twist) Integer -> Image)
(define (comment-img p a n)
  (if (<= n 0) (error "negative dimensions")
  (scale (/ n 160)
         (text (string-append (player->string p)
                              "'s turn to "
                              (symbol->string a))
                              10 'ivory))))

(: game-image : Game Integer -> Image)
;; produces an image of current game state
(define (game-image g n)
  (match g
    [(Game b n-p n-a)
     (underlay/align "middle" "bottom"
                     (board-image b n)
                     (comment-img n-p n-a n))]))
               
                  