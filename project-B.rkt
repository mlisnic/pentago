#lang typed/racket
(require "../include/uchicago151.rkt")
(require typed/test-engine/racket-tests)
(require typed/2htdp/image)
(require typed/2htdp/universe)


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
   [next-action : (U 'place 'twist)]
   [player1 : (U Human Bot)]
   [player2 : (U Human Bot)]))

(define-type Outcome
  (U (U Human Bot) 'tie))

(: empty-board : Board)
(define empty-board
  (Board (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)
         (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)
         (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)
         (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)))

(define first-player
  (list 'black 'white))

;; === Game Model ===

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
    [(Game b n-p n-a p1 p2)
     (cond
       [(not (eq? p n-p)) (error "Not your turn")]
       [(eq? p 'none) (error "invalid player")]
       [(not (eq? n-a 'place)) (error "Must twist")]
       [(or (not (<= 0 (Loc-row loc) 5))
            (not (<= 0 (Loc-col loc) 5)))
        (error "Row and column must be between 0 and 5")]
       [(not (eq? (board-ref b loc) 'none))
       ;; (error "The place is not empty")]
        g]
       [else (Game (board-place b (Some p) loc) n-p 'twist p1 p2)])]))

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
    [(Game b n-p n-a p1 p2)
     (cond
       [(not (eq? n-a 'twist)) (error "Must place")]
       [else (match b
               [(Board nw ne sw se)
                (match q
                  ['NW (Game (Board (twist nw d) ne sw se)
                             (next n-p)
                             'place p1 p2)]
                  ['NE (Game (Board nw (twist ne d) sw se)
                             (next n-p)
                             'place p1 p2)]
                  ['SW (Game (Board nw ne (twist sw d) se)
                             (next n-p)
                             'place p1 p2)]
                  ['SE (Game (Board nw ne sw (twist se d))
                             (next n-p)
                             'place p1 p2)]
                  [else (error "Invalid quadrant")])])])]))

(: board-full? : Board -> Boolean)
;; checks if board is full
(define (board-full? b)
  (match b
    [(Board nw ne sw se)
     (if (member 'none (append nw ne se sw)) #f #t)]))

(: blank-board? : Board -> Boolean)
;; checks if board is blank
(define (blank-board? b)
  (match b
    [(Board nw ne sw se)
     (if (or (member (Some 'black) (append nw ne se sw))
             (member (Some 'white) (append nw ne se sw))) #f #t)]))

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
;; checks if list of optional contains some
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

;; a test board with some marbles
(define test-board
  (Board (list (Some 'white) (Some 'black) 'none (Some 'white) 'none (Some 'black)  (Some 'white) 'none 'none )
         (list 'none 'none 'none 'none 'none 'none (Some 'black) 'none 'none)
         (list (Some 'white) 'none (Some 'black)  (Some 'white) 'none (Some 'black)  (Some 'white) 'none 'none)
         (list (Some 'black) (Some 'black) 'none 'none (Some 'black) (Some 'black) 'none 'none (Some 'black))))
           
(: game-over? : Game -> Boolean)
;; checks if game is over
(define (game-over? g)
  (match g
    [(Game b n-p n-a p1 p2)
     (or (five-white? b)
         (five-black? b)
         (board-full? b))]))

(: outcome : Game -> Outcome)
; returns outcome of the game
(define (outcome g)
  (match g
    [(Game b n-p n-a p1 p2)
     (cond
       [(and (five-white? b)
             (five-black? b))
        'tie]
       [(five-white? b) p2]
       [(five-black? b) p1]
       [(board-full? b) 'tie]
       [else (error "Game not over")])]))

;; === Visualizations ===

(: num-img : Integer Integer -> Image)
;; image of number (for rows & columns)
(define (num-img n s)
  (if (<= s 0) (error "negative size")
  (underlay
   (square s 0 'white)
   (scale (/ s 20) (text  (number->string n) 10 'ivory)))))

(: nums-image : Integer (-> Image Image Image * Image) -> Image)
;; applies f (above or beside will be used) to number images
(define (nums-image n f)
  (f (num-img 0 n)
     (num-img 1 n)
     (num-img 2 n)
     (num-img 3 n)
     (num-img 4 n)
     (num-img 5 n)))

(: board-bg : Integer -> Image)
;; image of background of the board
(define (board-bg n)
  (if (< n 0) (error "negative dimensions")
  (underlay/align "left" "middle"
                  (underlay/align "middle" "top"
                                  (square n "solid" 'olive)
                                  (nums-image (quotient n 8) beside))
                  (nums-image (quotient n 8) above))))

(: grid-img : Integer -> Image)
;; image of grid
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
;; image of a quad
(define (quad-img n c xs)
  (if (<= n 0) (error "negative dimensions")
      (underlay
       (underlay (frame (square (quotient (* 3 n) 8) "solid" c))
                 (grid-img (quotient (* 3 n) 8)))
       (pieces-img (pieces->image (quotient (* 3 n) 8) xs)))))

(: piece : Integer Image-Color -> Image)
;; image of a piece
;; transparent 'tan used for 'none
(define (piece n c)
  (if (< n 0) (error "negative dimensions")
      (underlay
       (square (quotient n 3) 0 'cyan)
       (match c
         ['tan (scale 9/10 (circle (quotient n 6) 0 c))]
         [_
          (scale 4/5 (circle (quotient n 6) "solid" c))]))))


(: pieces->image : Integer (Listof (Optional Player)) -> (Listof Image))
;; converts list of pieces to list of corresponding images
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
;; lays pieces in correct grid to be place over board
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
;; aboves and besides the 4 quadrants on board
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

(: human->string : (U Human Bot) -> String)
;; converts human or bot-name into a string
(define (human->string p)
  (match p
    [(Bot name _)
     (if (symbol? name)
         (symbol->string name)
         name)]
    [Human
     (cond
       [(symbol? p) (symbol->string p)]
       [(string? p) p]
       [else (error "idk")])]))
    

(: comment-img : Game Integer -> Image)
;; shows next player and suggests next action
(define (comment-img g n)
  (match g
    [(Game b n-p n-a p1 p2)
  (if (<= n 0) (error "negative dimensions")
  (scale (/ n 160)
         (text (string-append (match n-p
                                ['black (human->string p1)]
                                ['white (human->string p2)])
                              "'s turn to "
                              (symbol->string n-a))
                              10 'ivory)))]))

(: game-image : Game Integer -> Image)
;; produces an image of current game state
(define (game-image g n)
  (match g
    [(Game b n-p n-a p1 p2)
     (underlay/align "middle" "bottom"
                     (board-image b n)
                     (comment-img g n))]))

;; === Building GUI ===

(define-struct World
  ([game : Game]
   [state : States]))

(define-type States
  (U 'play 'twist-NW 'twist-NE 'twist-SW 'twist-SE 'bot-move 'end))

(define-struct Move
  ([loc : Loc]
   [q   : Quadrant]
   [dir : Direction]))

(define-type Human
  (U String Symbol))

(define-struct Bot
  ([name : (U String Symbol)]
   [mind : (Game -> (Optional Move))]))

(: mouse-place : Game Player Integer Integer -> Game)
;; converts cursor's x and y to loc
(define (mouse-place g n-p x y)
  (place-marble g n-p (Loc (- (quotient y 50) 1)
                           (- (quotient x 50) 1))))

(: pre-twist : Game Player Integer Integer -> World)
;; choose quadrant before choosing direction
(define (pre-twist g n-p x y)
  (local
    {(: xy-quad : Integer Integer -> States)
     (define (xy-quad x y)
       (cond
         [(<= 50 x 200)
          (if (<= 50 y 200) 'twist-NW 'twist-SW)]
         [else
          (if (<= 50 y 200) 'twist-NE 'twist-SE)]))}
    (World g (if (game-over? g)
                 'end
                 (xy-quad x y)))))


(: after-click : Game -> States)
;; checks status of game
(define (after-click g)
  (match g
    [(Game b n-p n-a p1 p2)
     (cond
       [(game-over? g) 'end]
       [(or (and (eq? n-p 'black) (Bot? p1))
            (and (eq? n-p 'white) (Bot? p2))) 'bot-move]
       [else 'play])]))

(: handle-mouse : World Integer Integer Mouse-Event -> World)
;; places marble on clicked square or chooses quad to twist
(define (handle-mouse w x y e)
  (match w
    [(World g st)
     (match g
       [(Game b n-p n-a p1 p2)
        (match n-a
          ['place
           (if (and (<= 50 x 350) (<= 50 y 350))
           (match e
             ["button-down"
              (World (mouse-place g n-p x y) (after-click (mouse-place g n-p x y)))]
             [_ w]) w)]
          ['twist
           (if (and (<= 50 x 350) (<= 50 y 350))
           (match e
             ["button-down"
              (if (game-over? g)
                  (World g 'end)
              (pre-twist g n-p x y))]
             [_ w]) w)])])]))

(: handle-key : World String -> World)
;; picks direction for twist
(define (handle-key w s)
  (match w
    [(World g st)
     (match st
       ['twist-NW
        (match s
          ["j" (World (twist-quadrant g 'NW 'counterclockwise)
                      (after-click (twist-quadrant g 'NW 'counterclockwise)))]
          ["k" (World (twist-quadrant g 'NW 'clockwise)
                      (after-click (twist-quadrant g 'NW 'clockwise)))]
          [_ w])]
       ['twist-NE
        (match s
          ["j" (World (twist-quadrant g 'NE 'counterclockwise)
                      (after-click (twist-quadrant g 'NE 'counterclockwise)))]
          ["k" (World (twist-quadrant g 'NE 'clockwise)
                      (after-click (twist-quadrant g 'NE 'clockwise)))]
          [_ w])]
       ['twist-SW
        (match s
          ["j" (World (twist-quadrant g 'SW 'counterclockwise)
                      (after-click (twist-quadrant g 'SW 'counterclockwise)))]
          ["k" (World (twist-quadrant g 'SW 'clockwise)
                      (after-click (twist-quadrant g 'SW 'clockwise)))]
          [_ w])]
       ['twist-SE
        (match s
          ["j" (World (twist-quadrant g 'SE 'counterclockwise)
                      (after-click (twist-quadrant g 'SE 'counterclockwise)))]
          ["k" (World (twist-quadrant g 'SE 'clockwise)
                      (after-click (twist-quadrant g 'SE 'clockwise)))]
          [_ w])]
       [_ w])]))

(: handle-key2 : World String -> World)
;; if played with a bot, returns call to bot's move after player twists
(define (handle-key2 w s)
  (check-bot (handle-key w s)))

(: check-bot : World -> World)
;; checks if next player is a bot
(define (check-bot w)
  (match w
    [(World g st)
     (match g
       [(Game b n-p n-a p1 p2)
     (match st
       ['bot-move
             (local
       {(define thebot
         (cond
           [(and (eq? n-p 'black) (Bot? p1)) p1]
           [(and (eq? n-p 'white) (Bot? p2)) p2]
           [else (error "no bot")]))}
        (World (bot-move g thebot)
               (if (game-over? g) 'end
                   (if (2bot? g) 'bot-move 'play))))]
       [else w])])]))

(: draw : World -> Image)
;; produces image of current state of world
(define (draw w)
  (underlay (game-image (World-game w) 400)
            (match (World-state w)
              ['twist-NW (twist-img 'NW)]
              ['twist-NE (twist-img 'NE)]
              ['twist-SW (twist-img 'SW)]
              ['twist-SE (twist-img 'SE)]
              ['end (win-img (outcome (World-game w)))]
              [_ empty-image])))

(: twist-img : Quadrant -> Image)
;; image prompting twist direction, translucent over the quad chosen
(define (twist-img q)
  (local
    {(: q-x : Quadrant -> Integer)
     (define (q-x d)
       (match d
         ['NW 0]
         ['NE 150]
         ['SW 0]
         ['SE 150]))
     (: q-y : Quadrant -> Integer)
     (define (q-y d)
       (match d
         ['NW 0]
         ['NE 0]
         ['SW 150]
         ['SE 150]))}
  (underlay/xy
   (square 300 0 'white)
   (q-x q) (q-y q)
   (underlay (square 150 100 'aliceblue)
             (above (text "J for counterclockwise" 15 'black)
                    (square 20 0 'white)
                    (text "K for clockwise" 15 'black))))))

(: win-img : Outcome -> Image)
;; image with ending message, overlayed translucently on board
(define (win-img o)
  (local
    {(: o->string : Outcome -> String)
     (define (o->string out)
       (match out
         ['tie "It's a tie!"]
         [_ (string-append (human->string out) " won!")]))}
  (underlay
   (square 300 200 'aliceblue)
   (text (o->string o) 40 'black)) ))

;; === AI ===

(: first-available : Game -> (Optional Move))
;; a bot's mind function
;; places marble on first available column & row
;; twists the NW quadrant clockwise
(define (first-available g)
  (match g
    [(Game b n-p n-a p1 p2)
        (if (board-full? b) 'none 
           (Some (Move (botloc g) 'NW 'clockwise)))]))

(: botloc : Game -> Loc)
;; finds first available loc for bot's piece
(define (botloc g)
  (local
    {(: lp : Integer -> Loc)
     (define (lp x)
       (cond
         [(<= 0 x 5) (Loc 0 x)]
         [(<= 6 x 11) (Loc 1 (- x 6))]
         [(<= 12 x 17) (Loc 2 (- x 12))]
         [(<= 18 x 23) (Loc 3 (- x 18))]
         [(<= 24 x 29) (Loc 4 (- x 24))]
         [else (Loc 5 (- x 30))]))
     (: check : Board Integer -> Loc)
     (define (check b y)
       (match (board-ref b (lp y))
         ['none (lp y)]
         [_ (check b (add1 y))]))}
    (check (Game-board g) 0)))

(: bot-move : Game Bot -> Game)
;; makes bot move according to its mind function
(define (bot-move g bot)
  (match g
    [(Game b n-p n-a p1 p2)
     (match bot
       [(Bot name mind)
        (match (mind g)
          ['none g]
          [(Some (Move loc q dir))
           (twist-quadrant
            (place-marble g n-p loc)
            q
            dir)])])]))

(: handle-tick : World -> World)
;; for a game with 2 bots
;; makes a move each tick
(define (handle-tick w)
  (match w
    [(World g st)
     (match g
       [(Game b n-p n-a p1 p2)
        (local
          {(define thebot
             (cond
               [(and (eq? n-p 'black) (Bot? p1)) p1]
               [(and (eq? n-p 'white) (Bot? p2)) p2]
               [else (error "no bots")]))}
          (if (game-over? g) w
          (World (bot-move g thebot) (after-click (bot-move g thebot)))))])]))

(: 2bot? : Game -> Boolean)
;; checks if bot players in game are bots
(define (2bot? g)
  (match g
    [(Game b n-p n-a p1 p2)
     (cond
       [(and (Bot? p1) (Bot? p2)) #t]
       [else #f])]))

    
;; === PLAYING ===

(: pentago : (U Human Bot) (U Human Bot) -> World)
;; function that calls the game
(define (pentago p1 p2)
  (cond
    [(and (Bot? p1) (not (Bot? p2)))
     (big-bang (World (bot-move (Game empty-board 'black 'place p1 p2) p1) 'play) : World
               [to-draw draw]
               [on-key handle-key2]
               [on-mouse handle-mouse])]
    [(and (Bot? p1) (Bot? p2))
     (big-bang (World (Game empty-board 'black 'place p1 p2) 'bot-move) : World
               [to-draw draw]
               [on-tick handle-tick 1])]
    [else 
     (big-bang (World (Game empty-board 'black 'place p1 p2) 'play) : World
               [to-draw draw]
               [on-key handle-key2]
               [on-mouse handle-mouse])]))
   
   
