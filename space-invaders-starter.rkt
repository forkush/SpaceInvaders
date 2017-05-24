;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders Game

;; =================

;;
;; Constants:

(define BLANK (square 0 "solid" "white"))

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

;; =================
;; Data definitions:

;; Game is the state of the invaders, missiles, and tank in the game.

(define-struct game (invaders missiles t))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-t s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of Invaders
(define LOI1 empty)
(define LOI2 (cons (make-invader 1 2 3) empty))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi) (rest loi))]))



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; - empty
;; - (cons missile ListOfMissile)
;; interp. a list of missiles
(define MS1 (make-missile 0 0))
(define MS2 (make-missile 50 50))
#;
(define (fn-for-lom lom) 
  (cond [(empty? lom) (...)]
        {else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))}))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; =================
;; Functions:

;; Game -> Game
;; start the world with ...
;; 
(define (main g)
  (big-bang g                           ; Game
            (on-tick   next-game)       ; Game -> Game
            (to-draw   render)          ; Game -> Image
            (on-key    handle-key)))    ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game
(check-expect (next-game (make-game empty empty (make-tank 50 1)))  (make-game empty empty (make-tank (+ 50 (*  1 TANK-SPEED))  1)))
(check-expect (next-game (make-game empty empty (make-tank 50 -1))) (make-game empty empty (make-tank (+ 50 (* -1 TANK-SPEED)) -1)))
(check-expect (next-game (make-game (cons (make-invader 100 200 3) empty) empty (make-tank 50 1)))
              (make-game (cons (make-invader (+ 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 3) empty) empty
                         (make-tank (+ 50 (* 1 TANK-SPEED)) 1)))

(check-expect (next-game (make-game (cons (make-invader 100 200 3) (cons (make-invader 200 300 6) empty)) 
                                    (cons (make-missile 20 200) empty)
                                    (make-tank 50 1)))
              (make-game (cons (make-invader (+ 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 3)
                               (cons (make-invader (+ 200 INVADER-X-SPEED) (+ 300 INVADER-Y-SPEED) 6) empty))
                         (cons (make-missile 20 (- 200 MISSILE-SPEED)) empty) 
                         (make-tank (+ 50 (* 1 TANK-SPEED)) 1)))   

;(define (next-game g) g)      ;stub

(define (next-game s)
  (make-game (move-invaders (game-invaders s))
             (move-missiles (game-missiles s))
             (move-tank (game-t s))))

;; ListOfInvader -> ListOfInvader
;; create an updated list of invaders with correct x and y screen coordinates
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (cons (make-invader 10 20 5) empty))
              (cons (make-invader (+ 10 INVADER-X-SPEED) (+ 20 INVADER-Y-SPEED) 5) empty))
(check-expect (move-invaders (cons (make-invader 30 40 5) (cons (make-invader 10 20 5) empty)))
              (cons (make-invader (+ 30 INVADER-X-SPEED) (+ 40 INVADER-Y-SPEED) 5)
                    (cons (make-invader (+ 10 INVADER-X-SPEED) (+ 20 INVADER-Y-SPEED) 5) empty)))

;(define (move-invaders loi) empty)    ;stub

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-one-invader (first loi)) 
               (move-invaders (rest loi)))]))

;; Invader -> Invader
;; update the x and y coordinate of one invader
(check-expect (move-one-invader (make-invader 0 0 5)) (make-invader (+ 0 INVADER-X-SPEED) (+ 0 INVADER-Y-SPEED) 5))
;(define (move-one-invader i) (make-invader 0 0 0))

(define (move-one-invader invader)
  (make-invader (+ (invader-x invader) INVADER-X-SPEED)
                (+ (invader-y invader) INVADER-Y-SPEED)
                (invader-dx invader)))

;; ListOfMissile -> ListOfMissile
;; create an updated list of missiles with correct x and y screen coordinates
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (cons (make-missile 100 200) empty)) (cons (make-missile 100 (- 200 MISSILE-SPEED)) empty))
(check-expect (move-missiles (cons (make-missile 200 300) (cons (make-missile 100 200) empty)))
              (cons (make-missile 200 (- 300 MISSILE-SPEED)) (cons (make-missile 100 (- 200 MISSILE-SPEED)) empty))) 
;(define (move-missiles lom) empty)   ;empty

(define (move-missiles lom) 
  (cond [(empty? lom) empty]
        {else
         (cons (move-one-missile (first lom)) (move-missiles (rest lom)))}))

;; Missile -> Missile
;; Update the x and y coordinates of a missile
(check-expect (move-one-missile (make-missile 100 200)) (make-missile 100 (- 200 MISSILE-SPEED)))
;(define (move-one-missile m) (make-missile 0 0))

(define (move-one-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; Tank -> Tank
;; produce tank with updated x coordinate
(check-expect (move-tank (make-tank 50 1))  (make-tank (+ (* TANK-SPEED  1) 50)  1))
(check-expect (move-tank (make-tank 50 -1)) (make-tank (+ (* TANK-SPEED -1) 50) -1))
;(define (move-tank m) (make-tank 0 1))

(define (move-tank t)
  (make-tank (+ (* TANK-SPEED (tank-dir t)) (tank-x t)) (tank-dir t)))



;; Game -> Image
;; render the invaders, missiles, and tank on the screen 
(check-expect (render (make-game empty empty (make-tank 50 1)))
              (place-image TANK (tank-x (make-tank 50 1)) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(check-expect (render (make-game (cons (make-invader 100 200 3) empty) 
                                 (cons (make-missile 20 200) empty)
                                 (make-tank 50 1)))
              (place-image INVADER (invader-x (make-invader 100 200 3)) (invader-y (make-invader 100 200 3))
                           (place-image MISSILE (missile-x (make-missile 20 200)) (missile-y (make-missile 20 200))
                                        (place-image TANK (tank-x (make-tank 50 1)) (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))) 

(check-expect (render (make-game (cons (make-invader 100 200 3) (cons (make-invader 200 300 3) empty))  
                                 (cons (make-missile 20 200) empty)
                                 (make-tank 50 1)))
              (place-image INVADER (invader-x (make-invader 100 200 3)) (invader-y (make-invader 100 200 3))
                           (place-image INVADER (invader-x (make-invader 200 300 3)) (invader-y (make-invader 200 300 3))
                                        (place-image MISSILE (missile-x (make-missile 20 200)) (missile-y (make-missile 20 200))
                                                     (place-image TANK (tank-x (make-tank 50 1)) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))) 

;(define (render g) BACKGROUND)     ;stub

(define (render s)
       (each-invader (game-invaders s)
       (fn-for-lom (game-missiles s))
       (get-tank (game-t s))))

;; ListOfInvader -> Image
;; produces each invader in the list
;; !!!
(define (each-invader loi) (make-invader 0 0 0))

(define (each-invader loi)
  (cond [(empty? loi) BLANK]
        [else
         (create-image (first loi)
              (each-invader (rest loi)))]))

;; Invader Image -> Image
;;

;(define (create-image i img) BLANK)    ;stub

(define (create-image invader)
  (place-image INVADER (invader-x i) (invader-y i) img))

;; ListOfMissile -> Image
;; produces each missile in the list
;; !!!
(define (each-missile lom) (make-missle 0 0))















        
 









;; Game KeyEvent -> Game
;; change the direction of the tank component of Game 
;; !!!
(define (handle-key g ke) g)






