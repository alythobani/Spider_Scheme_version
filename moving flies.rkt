;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |moving flies|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Spider dodging fireballs and eating flies
;; Start game with (main G10)

;; Rules of game:
;; - bring spider to top of screen with space bar
;; - move spider left and right with arrow keys
;; - avoid fireballs and lava at bottom of screen
;; - gain 3 points for every fireball that reaches edge of screen
;; - gain 100 points for every fly caught
;; - aim for highest score!

;; Constants to adjust to alter difficulty:

(define FIREBALL-SPEED 5)        ;the lower the easier (changes speed of fireballs)
(define FIREBALL-SEPARATION 75)  ;the higher the easier (changes # of fireballs)
(define Y-SPEED 5)               ;the lower the easier (changes speed of falling spider)


;; =================
;; Other constants:
(define WIDTH 500)
(define HEIGHT WIDTH)

(define X-POS (/ WIDTH 2))

(define SPIDER (place-image (beside (circle 2 "solid" "red")
                                    (rectangle 4 0 "solid" "black")
                                    (circle 2 "solid" "red"))
                            20
                            20
                            (radial-star 8 20 10 "solid" "black")))

(define X-SPEED 10)
(define ROT-SPEED 15)

(define FLY (beside (circle 2 "solid" "black")
                    (overlay (circle 5 "outline" "black")
                             (circle 5 "solid" "silver"))
                    (circle 2 "solid" "black")))
(define FLY-ROT-SPEED 5)
(define FLY-SPEED 3)

(define FIREBALL (scale 0.8 (overlay (circle 3 "solid" "white")
                                     (circle 5 "solid" "yellow")
                                     (circle 7 "solid" "gold")
                                     (circle 10 "solid" "orange")
                                     (circle 12 "solid" "tomato")
                                     (circle 14 "solid" "red"))))

(define EXTRA-LIFE-BALL (scale 0.8 (overlay (circle 3 "solid" "white")
                                            (circle 5 "solid" "pink")
                                            (circle 7 "solid" "lightcoral")
                                            (circle 10 "solid" "deeppink")
                                            (circle 12 "solid" "crimson")
                                            (circle 14 "solid" "pink"))))

(define LAVA-TOP (local [(define sub (overlay (triangle 14 "solid" "orange red")
                                              (triangle 14 "outline" "red")))]
                   (beside sub sub sub sub sub sub sub sub sub sub 
                           sub sub sub sub sub sub sub sub sub sub 
                           sub sub sub sub sub sub sub sub sub sub 
                           sub sub sub sub sub sub sub sub)))
(define LAVA-TOP-ORANGE (local [(define sub (overlay (triangle 14 "solid" "orange")
                                                     (triangle 14 "outline" "red")))]
                          (beside sub sub sub sub sub sub sub sub sub sub 
                                  sub sub sub sub sub sub sub sub sub sub 
                                  sub sub sub sub sub sub sub sub sub sub 
                                  sub sub sub sub sub sub sub sub sub sub
                                  sub sub sub sub)))

(define LAVA-BODY (rectangle WIDTH 20 "solid" "darkorange"))


(define SPIDER-LIFE (scale 0.7 SPIDER))
(define 5-LIVES (beside SPIDER-LIFE
                        (rectangle 10 0 "solid" "gray")
                        SPIDER-LIFE
                        (rectangle 10 0 "solid" "gray")
                        SPIDER-LIFE
                        (rectangle 10 0 "solid" "gray")
                        SPIDER-LIFE
                        (rectangle 10 0 "solid" "gray")
                        SPIDER-LIFE))
(define 4-LIVES (beside SPIDER-LIFE
                        (rectangle 10 0 "solid" "gray")
                        SPIDER-LIFE
                        (rectangle 10 0 "solid" "gray")
                        SPIDER-LIFE
                        (rectangle 10 0 "solid" "gray")
                        SPIDER-LIFE))
(define 3-LIVES (beside SPIDER-LIFE
                        (rectangle 10 0 "solid" "gray")
                        SPIDER-LIFE
                        (rectangle 10 0 "solid" "gray")
                        SPIDER-LIFE))
(define 2-LIVES (beside SPIDER-LIFE
                        (rectangle 10 0 "solid" "gray")
                        SPIDER-LIFE))
(define 1-LIVES SPIDER-LIFE)

(define EXTRA-LIFE (scale 0.8 (overlay (circle 3 "solid" "white")
                                       (circle 5 "solid" "pink")
                                       (circle 7 "solid" "lightcoral")
                                       (circle 10 "solid" "deeppink")
                                       (circle 12 "solid" "crimson")
                                       (circle 14 "solid" "pink"))))

(define TEXT-SIZE 20)
(define TEXT-COLOUR "black")

;(define MTS (empty-scene WIDTH HEIGHT))
(define MTS (underlay/align/offset "left" "top"
                                   (above (rectangle WIDTH 350 "solid" "dark gray")
                                          (rectangle WIDTH 1 "solid" "black")
                                          (place-image LAVA-TOP
                                                       (/ WIDTH 2)
                                                       145
                                                       (rectangle WIDTH 150 "solid" "dim gray"))
                                          LAVA-BODY)
                                   10 10
                                   (text "LIVES: " TEXT-SIZE TEXT-COLOUR)))

(define GAMEOVER (overlay (rectangle 250 50 "outline" "red")
                          (rectangle 255 55 "outline" "black")
                          (rectangle 260 60 "outline" "red")
                          (text "GAME OVER" 40 "red")))
(define GAMEOVER-SPEED 3)

(define RETRY-BUTTON (overlay (text "RETRY?" 30 "black")
                              (rectangle 200 40 "solid" "gray")
                              (rectangle 200 40 "outline" "black")))

(define RETRY (above (text "You ran out of lives!" 24 "black")
                     (rectangle 0 30 "solid" "white")
                     RETRY-BUTTON))



;; =================
;; Data definitions:

(define-struct spider (x y rot))
;; Spider is (make-spider Number[0,WIDTH] Number[0,HEIGHT] Number[-360,360])
;; interp. spider's x-position, y-position, and amount it has rotated
(define S0 (make-spider X-POS 0 0))                             ;top of screen
(define S1 (make-spider X-POS 10 10))                           ;top(ish)
(define S2 (make-spider X-POS (/ HEIGHT 2) ROT-SPEED))          ;middle
(define S3 (make-spider X-POS (- HEIGHT 10) (* ROT-SPEED -1)))  ;bottom(ish)
(define S4 (make-spider X-POS HEIGHT 0))                        ;bottom
#;
(define (fn-for-spider s)
  (... (spider-x s)
       (spider-y s)
       (spider-rot s)))

(define-struct web (x y length))
;; Web is (make-web Natural[0,WIDTH] Natural[0,HEIGHT] Natural)
;; interp. x-position and y-position of strand of web (of bottom of strand), and web's length
(define W0 (make-web X-POS 0 0))
(define W1 (make-web X-POS 10 10))
(define W4 (make-web X-POS HEIGHT HEIGHT))
#;
(define (fn-for-web w)
  (... (web-x w)
       (web-y w)
       (web-length w)))

;; ListOfWeb is one of:
;; - empty
;; - (cons Web ListOfWeb)
;; interp. the webs on the screen
(define LOW0 (list W0))
(define LOW1 (list W1))
#;
(define (fn-for-low low)
  (cond [(empty? low) (...)]
        [else
         (... (fn-for-web (first low))
              (fn-for-low (rest low)))]))

(define-struct fly (x y rot spd))
;; Fly is (make-fly Number[0,WIDTH] Number[0,HEIGHT] Number[-360,360] Number)
;; interp. x- and y-position of fly, amount it has rotated, and speed
(define F1 (make-fly (* WIDTH (/ 3 4)) (/ HEIGHT 2) 0 0))
(define F2 (make-fly (/ WIDTH 3) (* HEIGHT (/ 2 3)) 0 0))
#;
(define (fn-for-fly f)
  (... (fly-x f)
       (fly-y f)
       (fly-rot f)
       (fly-spd f)))

;; ListOfFly is one of:
;; - empty
;; - (cons Fly ListOfFly)
;; interp. a list of flies
(define LOF0 empty)
(define LOF1 (list F1))
(define LOF2 (list F1 F2))
(define LOF10 (list (make-fly (* WIDTH 3/4) (/ HEIGHT 2) 0 3)
                    (make-fly (/ WIDTH 3) (* HEIGHT 2/3) 0 -5)))
#;
(define (fn-for-lof lof)
  (cond [(empty? lof) (...)]
        [else
         (... (fn-for-fly (first lof))
              (fn-for-lof (rest lof)))]))

(define-struct fireball (x y))
;; Fireball is (make-fireball Number[0,WIDTH] Number[0,HEIGHT])
;; interp. a fireball's x- and y- position
(define FB1 (make-fireball 0 0))
(define FB2 (make-fireball 250 250))
#;
(define (fn-for-fireball fb)
  (... (fireball-x fb)
       (fireball-y fb)))

;; ListOfFireball is one of:
;; - empty
;; - (cons Fireball ListOfFireball)
;; interp. a list of fireballs
(define LOFB0 empty)
(define LOFB1 (list FB1))
(define LOFB2 (list FB1 FB2))
(define LOFB3 (list (make-fireball 498 250)))
#;
(define (fn-for-lofb lofb)
  (cond [(empty? lofb) (...)]
        [else
         (... (fn-for-fireball (first lofb))
              (fn-for-lofb (rest lofb)))]))

(define-struct extra-life-ball (x y))
;; ExtraLifeBall is (make-extra-life-ball Number[0,WIDTH] Number[0,HEIGHT])
;; interp. an extra life ball's x- and y- position
#;
(define (fn-for-extra-life-ball b)
  (... (extra-life-ball-x b)
       (extra-life-ball-y b)))

(define-struct game (spider webs flies lives level score fireballs extra-life-ball lava gameover retry paused))
;; Game is (make-game Spider ListOfWeb ListOfFly Natural[0,3] Natural Natural ListOfFireball (false or ExtraLifeBall) Number (false or Number) Boolean Boolean)
;; interp. spider, its strands of web, flies on the screen, remaining lives, current level, user score, fireballs on screen,
;;         optional extra life ball, flowing lava, placing of gameover sign, whether retry button is there or not
(define L0 0)
(define G0 (make-game S0 LOW0 LOF0 3 0 0 LOFB0 false L0 false false false))
(define G1 (make-game S1 LOW1 LOF1 2 0 0 LOFB0 false L0 false false false))
(define G2 (make-game S2 LOW1 (list (make-fly (+ X-POS 5) (- (/ HEIGHT 2) 2) 0 0)) 3 0 0 LOFB0 false L0 false false false))
(define G3 (make-game S2 LOW1 (list F1 (make-fly (+ X-POS 5) (- (/ HEIGHT 2) 2) 0 0)) 3 0 0 LOFB0 false L0 false false false))
(define G4 (make-game S2 (list W4) LOF1 3 0 0 LOFB0 false L0 false false false))
(define G5 (make-game S0 LOW0 LOF0 3 0 0 LOFB2 false L0 false false false))
(define G7 (make-game S0 LOW0 LOF0 3 0 0 LOFB3 false L0 false false false))
(define G8 (make-game S1 LOW1 LOF1 2 0 0 LOFB3 false L0 false false false))
(define G10 (make-game S0 LOW0 LOF10 3 0 0 LOFB0 false L0 false false false))
#;
(define (fn-for-game g)
  (... (fn-for-spider (game-spider g))
       (fn-for-low (game-webs g))
       (fn-for-lof (game-flies g))
       (game-lives g)
       (game-level g)
       (game-score g)
       (fn-for-lofb (game-fireballs g))
       (fn-for-extra-life-ball (game-extra-life-ball g))
       (game-lava g)
       (game-gameover g)
       (game-retry g)
       (game-paused g)))


;; =================
;; Functions:

;; Game -> Game
;; start the world with (main G10)
;; 
(define (main g)
  (big-bang g                                         ; Game
    (on-tick       tock)                      ; Game -> Game
    (to-draw       render)                    ; Game -> Image
    (on-mouse      handle-mouse)              ; Game Integer Integer MouseEvent -> Game
    (on-key        handle-key)))              ; Game KeyEvent -> Game
;           (stop-with     esc-pressed?)              ; Game -> Boolean

;; Game -> Game
;; produce the next game state
(check-expect (tock G8) (make-game (make-spider X-POS (+ 10 Y-SPEED) (* ROT-SPEED -1))
                                   (list (make-web X-POS (+ 10 Y-SPEED) (+ 10 Y-SPEED)))
                                   (map move-fly LOF1)
                                   2 0 0 (tock-fireballs G8) false (tock-lava (game-lava G8))
                                   false false false))
(define (tock g)
  (if (game-paused g)
      (make-game (game-spider g)
                 (game-webs g)
                 (game-flies g)
                 (game-lives g)
                 (game-level g)
                 (game-score g)
                 (game-fireballs g)
                 (game-extra-life-ball g)
                 (tock-lava (game-lava g))
                 (game-gameover g)
                 (game-retry g)
                 (game-paused g))
      (if (gameover? g)
          (make-game (game-spider g)
                     (game-webs g)
                     (game-flies g)
                     (game-lives g)
                     (game-level g)
                     (game-score g)
                     (tock-fireballs g)
                     (tock-extra-life-ball g (game-score g))
                     (tock-lava (game-lava g))
                     (tock-gameover (game-gameover g))
                     (tock-retry g)
                     (game-paused g))
          (local [(define new-score (tock-score g))]
            (make-game (tock-spider g)
                       (tock-webs g)
                       (tock-flies g)
                       (tock-lives g)
                       (tock-level g)
                       new-score
                       (tock-fireballs g)
                       (tock-extra-life-ball g new-score)
                       (tock-lava (game-lava g))
                       false false false)))))

;; Game -> Spider
;; produce the next spider
#;
(check-expect (tock-spider S0)
              (make-spider X-POS Y-SPEED ROT-SPEED))
#;
(check-expect (tock-spider S1)
              (make-spider X-POS (+ 10 Y-SPEED) (* ROT-SPEED -1)))
#;
(check-expect (tock-spider S4)
              (make-spider X-POS 0 ROT-SPEED))
(define (tock-spider g)
  (cond [(>= (spider-y (game-spider g)) HEIGHT)
         (make-spider (spider-x (game-spider g))
                      0
                      (rotate-spider (spider-rot (game-spider g))))]
        [else
         (make-spider (spider-x (game-spider g))
                      (+ (spider-y (game-spider g)) (+ Y-SPEED (game-level g)))
                      (rotate-spider (spider-rot (game-spider g))))]))

;; Number[-360,360] -> Number[-360,360]
;; rotate spider
(check-expect (rotate-spider 0) ROT-SPEED)
(check-expect (rotate-spider (* ROT-SPEED -1)) ROT-SPEED)
(check-expect (rotate-spider ROT-SPEED) (* ROT-SPEED -1))
;(define (rotate-spider rot) rot)   ;stub
(define (rotate-spider rot)
  (cond [(<= rot 0) ROT-SPEED]
        [(> rot 0) (* ROT-SPEED -1)]))

;; Game -> ListOfWeb
;; produce the next list of webs
#;
(check-expect (tock-webs G0)
              (list (make-web X-POS Y-SPEED Y-SPEED)))
#;
(check-expect (tock-webs G1)
              (list (make-web X-POS (+ 10 Y-SPEED) (+ 10 Y-SPEED))))
#;
(check-expect (tock-webs G4)
              (list (make-web X-POS 0 0)))
(define (tock-webs g)
  (cond [(>= (web-y (first (game-webs g))) HEIGHT)
         (list (make-web (web-x (first (game-webs g)))
                         0 0))]
        [else
         (local [(define (tock-web w)
                   (make-web (web-x w)
                             (+ (web-y w) (+ Y-SPEED (game-level g)))
                             (+ (web-length w) (+ Y-SPEED (game-level g)))))]
           (cons (tock-web (first (game-webs g)))
                 (rest (game-webs g))))]))


;; Game -> ListOfFly
;; produce the next set of flies
(define (tock-flies g)
  (local [(define (un-eaten? f) (not (eaten? f)))
          (define (eaten? f)
            (and
             (<= (- (spider-x (game-spider g)) 19)
                 (fly-x f)
                 (+ (spider-x (game-spider g)) 19))
             (<= (- (spider-y (game-spider g)) 19)
                 (fly-y f)
                 (+ (spider-y (game-spider g)) 19))))
          (define (random-speed n)
            (check (random n)))
          (define (check r)
            (if (= 0 r)
                (random-speed 7)
                r))]
    (cond [(empty? (game-flies g))
           (map move-fly (list (make-fly (random WIDTH) (random (- HEIGHT 20)) 0 (random-speed 7))
                               (make-fly (random WIDTH) (random (- HEIGHT 20)) 0 (random-speed 7))))]
          [(ormap eaten? (game-flies g))
           (map move-fly (cons (make-fly (random WIDTH) (random (- HEIGHT 20)) 0 (random-speed 7))
                               (filter un-eaten? (game-flies g))))]
          [else
           (map move-fly (game-flies g))])))

;; Fly -> Fly
;; move and rotate the fly and change speed if necessary
(define (move-fly f)
  (local [(define (rotate-fly rot)
            (cond [(<= rot 0) FLY-ROT-SPEED]
                  [(> rot 0) (* FLY-ROT-SPEED -1)]))]
    (cond [(or (and (<= (fly-x f) 0) (< (fly-spd f) 0))
               (and (>= (fly-x f) WIDTH) (> (fly-spd f) 0)))
           (make-fly (- (fly-x f) (fly-spd f))
                     (fly-y f)
                     (rotate-fly (fly-rot f))
                     (* (fly-spd f) -1))]
          [else (make-fly (+ (fly-x f) (fly-spd f))
                          (fly-y f)
                          (rotate-fly (fly-rot f))
                          (fly-spd f))])))


;; Game -> Natural[0,4]
;; produce number of lives remaining
(define (tock-lives g)
  (local [(define (hit? ballx bally)
            (and
             (<= (- (spider-x (game-spider g)) 19)
                 ballx
                 (+ (spider-x (game-spider g)) 19))
             (<= (- (spider-y (game-spider g)) 19)
                 bally
                 (+ (spider-y (game-spider g)) 19))))
          (define (hit-fireball? fb) (hit? (fireball-x fb) (fireball-y fb)))
          (define (hit-extra-life-ball? b) (hit? (extra-life-ball-x b) (extra-life-ball-y b)))]
    (cond [(>= (spider-y (game-spider g)) HEIGHT)
           (sub1 (game-lives g))]
          [(ormap hit-fireball? (game-fireballs g))
           (sub1 (game-lives g))]
          [(and (not (false? (game-extra-life-ball g)))
                (hit-extra-life-ball? (game-extra-life-ball g)))
           (add1 (game-lives g))]
          [else (game-lives g)])))

;; Game -> Natural
;; produce current level
(define (tock-level g)
  (floor (/ (game-score g) 1000)))

;; Game -> Natural
;; produce current user score
(check-expect (tock-score G0) 0)
(check-expect (tock-score G1) 0)
(check-expect (tock-score G2) 100)
(define (tock-score g)
  (local [(define (eaten? f)
            (and
             (<= (- (spider-x (game-spider g)) 19)
                 (fly-x f)
                 (+ (spider-x (game-spider g)) 19))
             (<= (- (spider-y (game-spider g)) 19)
                 (fly-y f)
                 (+ (spider-y (game-spider g)) 19))))
          (define (addscore f)
            (if (eaten? f)
                100
                0))
          (define (addscores lof)
            (map addscore lof))
          (define (outscreen? fb)
            (<= (fireball-x fb) 0))
          (define (fbscore fb)
            (if (outscreen? fb)
                3 0))
          (define (fbscores lofb)
            (map fbscore lofb))]
    (+ (foldr + 0 (addscores (game-flies g)))
       (game-score g)
       (foldr + 0 (fbscores (game-fireballs g))))))

;; Game -> ListOfFireball
;; create new fireball if width - first's x > 75
(define (tock-fireballs g)
  (cond [(empty? (game-fireballs g))
         (cons (make-fireball WIDTH (random HEIGHT))
               (movefbs g))]
        [(> (- WIDTH (fireball-x (first (game-fireballs g)))) FIREBALL-SEPARATION)
         (cons (make-fireball WIDTH (random HEIGHT))
               (movefbs g))]
        [else (movefbs g)]))

;; Game -> ListOfFireball
;; move fireballs unless at left edge already
(define (movefbs g)
  (local [(define (movefb fb)
            (make-fireball (- (fireball-x fb) (+ FIREBALL-SPEED (game-level g)))
                           (fireball-y fb)))
          (define (inscreen? fb)
            (> (fireball-x fb) 0))
          (define (unhit? fb) (not (hit? fb)))
          (define (hit? fb)
            (and
             (<= (- (spider-x (game-spider g)) 19)
                 (fireball-x fb)
                 (+ (spider-x (game-spider g)) 19))
             (<= (- (spider-y (game-spider g)) 19)
                 (fireball-y fb)
                 (+ (spider-y (game-spider g)) 19))))]
    (map movefb (filter inscreen? (filter unhit? (game-fireballs g))))))

;; Game -> Number -> ExtraLifeBall
;; move extra life ball (if it exists) unless at left edge already, or create extra life ball if user has reached new level (every 1000 points)
(define (tock-extra-life-ball g new-score)
  (local [(define hit-new-level?
            (= (add1 (floor (/ (game-score g) 1000)))
               (floor (/ new-score 1000))))]
    (if (false? (game-extra-life-ball g))
        (if hit-new-level?
            (make-extra-life-ball WIDTH (random HEIGHT))
            false)
        (local [(define new-x (- (extra-life-ball-x (game-extra-life-ball g))
                                 (+ FIREBALL-SPEED (game-level g))))
                (define (hit? ball)
                  (and
                   (<= (- (spider-x (game-spider g)) 19)
                       (extra-life-ball-x ball)
                       (+ (spider-x (game-spider g)) 19))
                   (<= (- (spider-y (game-spider g)) 19)
                       (extra-life-ball-y ball)
                       (+ (spider-y (game-spider g)) 19))))]
          (if (or (<= new-x 0) (hit? (game-extra-life-ball g)))
              false
              (make-extra-life-ball new-x
                                    (extra-life-ball-y (game-extra-life-ball g))))))))


;; Number -> Number
;; move lava right
(check-expect (tock-lava L0) (+ 0 2))
(define (tock-lava l) (modulo (+ l 2) 14))

;; Number -> Number
;; move GAMEOVER sign up screen till 224
(check-expect (tock-gameover 551)
              (- 551 GAMEOVER-SPEED))
(check-expect (tock-gameover 210) 210)
(check-expect (tock-gameover 300) (- 300 GAMEOVER-SPEED))
(define (tock-gameover go)
  (cond [(false? go) 551]
        [(<= go 224) go]
        [else
         (- go GAMEOVER-SPEED)]))

;; Game -> Boolean
;; see if retry sign should be placed on screen
(check-expect (tock-retry G0) false)
(check-expect (tock-retry (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 400 false false)) false)
(check-expect (tock-retry (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 200 false false)) true)
(check-expect (tock-retry (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 600 false false)) false)
(define (tock-retry g)
  (if (and (gameover? g) (not (false? (game-gameover g))))
      (<= (game-gameover g) 224)
      false))



;; Game -> Image
;; render image of game state onto MTS
#;
(check-expect (render G0)
              (place-image/align (rectangle 1 0 "solid" "gray")
                                 X-POS
                                 0
                                 "middle"
                                 "bottom"
                                 (place-image SPIDER
                                              X-POS
                                              0
                                              (underlay/align/offset "left" "top"
                                                                     (underlay/align/offset "right" "top"
                                                                                            (render-lava L0)
                                                                                            -10 10
                                                                                            (txt-to-image "0"))
                                                                     80 5
                                                                     3-LIVES))))
#;
(check-expect (render G1)
              (place-image/align (rectangle 1 10 "solid" "gray")
                                 X-POS
                                 10
                                 "middle"
                                 "bottom"
                                 (place-image (rotate 10 SPIDER)
                                              X-POS
                                              10
                                              (underlay/align/offset "left" "top"
                                                                     (underlay/align/offset "right" "top"
                                                                                            (place-image FLY
                                                                                                         (* WIDTH (/ 3 4))
                                                                                                         (/ HEIGHT 2)
                                                                                                         (render-lava L0))
                                                                                            -10 10
                                                                                            (txt-to-image "0"))
                                                                     80 5
                                                                     2-LIVES))))
#;
(check-expect (render G5)
              (place-image/align (rectangle 1 0 "solid" "gray")
                                 X-POS
                                 0
                                 "middle"
                                 "bottom"
                                 (place-image SPIDER
                                              X-POS
                                              0
                                              (underlay/align/offset "left" "top"
                                                                     (underlay/align/offset "right" "top"
                                                                                            (place-image FIREBALL
                                                                                                         0
                                                                                                         0
                                                                                                         (place-image FIREBALL
                                                                                                                      250
                                                                                                                      250
                                                                                                                      (render-lava L0)))
                                                                                            -10 10
                                                                                            (txt-to-image "0"))
                                                                     80 5
                                                                     3-LIVES))))

;(define (render g) MTS)   ;stub
(define (render g)
  (local [(define (render-gameon g)
            (render-webs (game-webs g)
                         (render-spider (game-spider g)
                                        (render-lives (game-lives g)
                                                      (render-level (game-level g)
                                                                    (render-score (game-score g)
                                                                                  (render-fireballs (game-fireballs g)
                                                                                                    (render-extra-life-ball (game-extra-life-ball g)
                                                                                                                            (render-flies (game-flies g)
                                                                                                                                          (render-paused (game-paused g)
                                                                                                                                                         (render-lava (game-lava g))))))))))))]
    (if (gameover? g)
        (if (false? (game-gameover g))
            (place-image GAMEOVER
                         (/ WIDTH 2)
                         551
                         (render-retry (game-retry g)
                                       (render-gameon g)))
            (place-image GAMEOVER
                         (/ WIDTH 2)
                         (game-gameover g)
                         (render-retry (game-retry g)
                                       (render-gameon g))))
        (render-gameon g))))

;; ListOfWeb Image -> Image
;; render webs onto image
(check-expect (render-webs LOW0 MTS)
              (place-image/align (rectangle 1 0 "solid" "gray")
                                 X-POS
                                 0
                                 "middle"
                                 "bottom"
                                 MTS))
(check-expect (render-webs LOW1 MTS)
              (place-image/align (rectangle 1 10 "solid" "gray")
                                 X-POS
                                 10
                                 "middle"
                                 "bottom"
                                 MTS))
(define (render-webs low i)
  (cond [(empty? low) i]
        [else
         (render-web (first low)
                     (render-webs (rest low) i))]))

;; Web Image -> Image
;; render image of web onto image
(check-expect (render-web W0 MTS)
              (place-image/align (rectangle 1 0 "solid" "gray")
                                 X-POS
                                 0
                                 "middle"
                                 "bottom"
                                 MTS))
(check-expect (render-web W1 MTS)
              (place-image/align (rectangle 1 10 "solid" "gray")
                                 X-POS
                                 10
                                 "middle"
                                 "bottom"
                                 MTS))
(define (render-web w i)
  (place-image/align (rectangle 1 (web-length w) "solid" "gray")
                     (web-x w)
                     (web-y w)
                     "middle"
                     "bottom"
                     i))

;; Spider Image -> Image
;; render image of spider onto image
(check-expect (render-spider S0 MTS)
              (place-image SPIDER
                           X-POS
                           0
                           MTS))
(check-expect (render-spider S1 MTS)
              (place-image (rotate 10 SPIDER)
                           X-POS
                           10
                           MTS))
(define (render-spider s i)
  (place-image (rotate (spider-rot s) SPIDER)
               (spider-x s)
               (spider-y s)
               i))

;; Natural[0,4] Image -> Image
;; render image of lives onto image
(check-expect (render-lives 0 MTS) MTS)
(check-expect (render-lives 5 MTS)
              (underlay/align/offset "left" "top"
                                     MTS
                                     80 5
                                     5-LIVES))
(check-expect (render-lives 4 MTS)
              (underlay/align/offset "left" "top"
                                     MTS
                                     80 5
                                     4-LIVES))
(check-expect (render-lives 3 MTS)
              (underlay/align/offset "left" "top"
                                     MTS
                                     80 5
                                     3-LIVES))
(check-expect (render-lives 2 MTS)
              (underlay/align/offset "left" "top"
                                     MTS
                                     80 5
                                     2-LIVES))
(check-expect (render-lives 1 MTS)
              (underlay/align/offset "left" "top"
                                     MTS
                                     80 5
                                     1-LIVES))
(define (render-lives l i)
  (local [(define (render-lives-image-onto-image lives-image)
            (underlay/align/offset "left" "top"
                                   i
                                   80 5
                                   lives-image))]
    (cond [(> l 5)
           (render-lives-image-onto-image (beside SPIDER-LIFE
                                                  (rectangle 5 0 "solid" "gray")
                                                  (txt-to-image (string-append "X " (number->string l)))))]
          [(= l 5)
           (render-lives-image-onto-image 5-LIVES)]
          [(= l 4)
           (render-lives-image-onto-image 4-LIVES)]
          [(= l 3)
           (render-lives-image-onto-image 3-LIVES)]
          [(= l 2)
           (render-lives-image-onto-image 2-LIVES)]
          [(= l 1)
           (render-lives-image-onto-image 1-LIVES)]
          [(= l 0) i])))

;; Natural Image -> Image
;; render image of current level onto image
(define (render-level l i)
  (underlay/align/offset "left" "top"
                         i
                         10 35
                         (txt-to-image (string-append "LEVEL: " (number->string l)))))

;; Natural Image -> Image
;; render image of score onto image
(check-expect (render-score 0 MTS)
              (underlay/align/offset "right" "top"
                                     MTS
                                     -10 10
                                     (txt-to-image "0")))
(check-expect (render-score 400 MTS)
              (underlay/align/offset "right" "top"
                                     MTS
                                     -10 10
                                     (txt-to-image "400")))
(define (render-score s i)
  (underlay/align/offset "right" "top"
                         i
                         -10 10
                         (txt-to-image (number->string s))))

;; ListOfFireball Image -> Image
;; render fireballs onto image
(check-expect (render-fireballs LOFB0 MTS) MTS)
(check-expect (render-fireballs LOFB1 MTS)
              (place-image FIREBALL
                           0
                           0
                           MTS))
(check-expect (render-fireballs LOFB2 MTS)
              (place-image FIREBALL
                           0
                           0
                           (place-image FIREBALL
                                        250
                                        250
                                        MTS)))
(define (render-fireballs lofb i)
  (cond [(empty? lofb) i]
        [else
         (local [(define (render-fb fb i)
                   (place-image FIREBALL
                                (fireball-x fb)
                                (fireball-y fb)
                                i))]
           (render-fb (first lofb)
                      (render-fireballs (rest lofb) i)))]))

;; (false or ExtraLifeBall) Image -> Image
(define (render-extra-life-ball b i)
  (if (false? b)
      i
      (place-image EXTRA-LIFE-BALL
                   (extra-life-ball-x b)
                   (extra-life-ball-y b)
                   i)))

;; Fly Image -> Image
;; render image of fly onto image
(check-expect (render-fly F1 MTS)
              (place-image FLY
                           (* WIDTH (/ 3 4))
                           (/ HEIGHT 2)
                           MTS))
(check-expect (render-fly F2 MTS)
              (place-image FLY
                           (/ WIDTH 3)
                           (* HEIGHT (/ 2 3))
                           MTS))
;(define (render-fly f i) MTS)   ;stub
(define (render-fly f i)
  (place-image (rotate (modulo (fly-rot f) 360) FLY)
               (fly-x f)
               (fly-y f)
               i))

;; ListOfFly Image -> Image
;; render flies onto MTS
(check-expect (render-flies LOF0 MTS) MTS)
(check-expect (render-flies LOF1 MTS)
              (place-image FLY
                           (* WIDTH (/ 3 4))
                           (/ HEIGHT 2)
                           MTS))
(check-expect (render-flies LOF2 MTS)
              (place-image FLY
                           (* WIDTH (/ 3 4))
                           (/ HEIGHT 2)
                           (place-image FLY
                                        (/ WIDTH 3)
                                        (* HEIGHT (/ 2 3))
                                        MTS)))
(define (render-flies lof i)
  (cond [(empty? lof) i]
        [else
         (render-fly (first lof)
                     (render-flies (rest lof) i))]))


;; Number -> Image
;; render lava onto MTS
(check-expect (render-lava L0)
              (place-image LAVA-TOP-ORANGE
                           (/ WIDTH 2)
                           497
                           MTS))
(define (render-lava l)
  (place-image LAVA-TOP-ORANGE
               (+ (/ WIDTH 2) l)
               497
               MTS))

;; Boolean Image -> Image
;; render retry button onto scene
(check-expect (render-retry false MTS) MTS)
(check-expect (render-retry true MTS)
              (place-image RETRY
                           (/ WIDTH 2)
                           347
                           MTS))
(define (render-retry b i)
  (if (false? b)
      i
      (place-image RETRY
                   (/ WIDTH 2)
                   347
                   i)))

;; Boolean Image -> Image
;; render paused message onto scene
(define (render-paused p i)
  (if (false? p)
      i
      (place-image (txt-to-image "PAUSED")
                   (/ WIDTH 2)
                   250
                   i)))


;; Game KeyEvent -> Game
;; if space bar is pressed bring spider to top of screen and right 10 pixels
(check-expect (handle-key G0 "\b") G0)
(check-expect (handle-key G0 " ")
              (make-game (make-spider X-POS
                                      0
                                      (spider-rot (game-spider G0)))
                         (list (make-web X-POS 0 0))
                         (game-flies G0)
                         3 0 0 LOFB0 false L0
                         false false false))
(check-expect (handle-key G0 "left")
              (make-game (make-spider (- X-POS (+ X-SPEED (game-level G0)))
                                      (spider-y (game-spider G0))
                                      (spider-rot (game-spider G0)))
                         (cons (make-web (- X-POS (+ X-SPEED (game-level G0)))
                                         (spider-y (game-spider G0))
                                         0)
                               (game-webs G0))
                         (game-flies G0)
                         3 0 0 LOFB0 false L0
                         false false false))
(check-expect (handle-key G0 "right")
              (make-game (make-spider (+ X-POS (+ X-SPEED (game-level G0)))
                                      (spider-y (game-spider G0))
                                      (spider-rot (game-spider G0)))
                         (cons (make-web (+ X-POS (+ X-SPEED (game-level G0)))
                                         (spider-y (game-spider G0))
                                         0)
                               (game-webs G0))
                         (game-flies G0)
                         3 0 0 LOFB0 false L0
                         false false false))
;(define (handle-key g ke) g)   ;stub

(define (handle-key g ke)
  (if (game-paused g)
      (if (key=? ke "p")
          (make-game (game-spider g)
                     (game-webs g)
                     (game-flies g)
                     (game-lives g)
                     (game-level g)
                     (game-score g)
                     (game-fireballs g)
                     (game-extra-life-ball g)
                     (game-lava g)
                     (game-gameover g)
                     (game-retry g)
                     false)
          g)
      (cond [(and (key=? ke " ")
                  (not (false? (game-retry g))))
             (make-game (make-spider (spider-x (game-spider g))
                                     0
                                     (spider-rot (game-spider g)))
                        (list (make-web (spider-x (game-spider g))
                                        0 0))
                        (game-flies g)
                        3
                        0
                        0
                        (game-fireballs g)
                        false
                        (game-lava g)
                        false
                        false
                        false)]
            [(and (key=? ke "up") (gameover? g))
             (make-game (make-spider (spider-x (game-spider g))
                                     (modulo (- (spider-y (game-spider g)) X-SPEED) HEIGHT)
                                     (spider-rot (game-spider g)))
                        (game-webs g)
                        (game-flies g)
                        (game-lives g)
                        (game-level g)
                        (game-score g)
                        (game-fireballs g)
                        (game-extra-life-ball g)
                        (game-lava g)
                        (game-gameover g)
                        (game-retry g)
                        (game-paused g))]
            [(and (key=? ke "down") (gameover? g))
             (make-game (make-spider (spider-x (game-spider g))
                                     (modulo (+ (spider-y (game-spider g)) X-SPEED) HEIGHT)
                                     (spider-rot (game-spider g)))
                        (game-webs g)
                        (game-flies g)
                        (game-lives g)
                        (game-level g)
                        (game-score g)
                        (game-fireballs g)
                        (game-extra-life-ball g)
                        (game-lava g)
                        (game-gameover g)
                        (game-retry g)
                        (game-paused g))]
            [(and (key=? ke " ") (not (gameover? g)))
             (make-game (make-spider (spider-x (game-spider g))
                                     0
                                     (spider-rot (game-spider g)))
                        (list (make-web (spider-x (game-spider g))
                                        0
                                        0))
                        (game-flies g)
                        (game-lives g)
                        (game-level g)
                        (game-score g)
                        (game-fireballs g)
                        (game-extra-life-ball g)
                        (game-lava g)
                        (game-gameover g)
                        (game-retry g)
                        (game-paused g))]
            [(key=? ke "left")
             (make-game (make-spider (modulo (- (spider-x (game-spider g)) (+ X-SPEED (game-level g))) WIDTH)
                                     (spider-y (game-spider g))
                                     (spider-rot (game-spider g)))
                        (cons (make-web (modulo (- (spider-x (game-spider g)) (+ X-SPEED (game-level g))) WIDTH)
                                        (spider-y (game-spider g))
                                        0)
                              (game-webs g))
                        (game-flies g)
                        (game-lives g)
                        (game-level g)
                        (game-score g)
                        (game-fireballs g)
                        (game-extra-life-ball g)
                        (game-lava g)
                        (game-gameover g)
                        (game-retry g)
                        (game-paused g))]
            [(key=? ke "right")
             (make-game (make-spider (modulo (+ (spider-x (game-spider g)) (+ X-SPEED (game-level g))) WIDTH)
                                     (spider-y (game-spider g))
                                     (spider-rot (game-spider g)))
                        (cons (make-web (modulo (+ (spider-x (game-spider g)) (+ X-SPEED (game-level g))) WIDTH)
                                        (spider-y (game-spider g))
                                        0)
                              (game-webs g))
                        (game-flies g)
                        (game-lives g)
                        (game-level g)
                        (game-score g)
                        (game-fireballs g)
                        (game-extra-life-ball g)
                        (game-lava g)
                        (game-gameover g)
                        (game-retry g)
                        (game-paused g))]
            [(key=? ke "p")
             (make-game (game-spider g)
                        (game-webs g)
                        (game-flies g)
                        (game-lives g)
                        (game-level g)
                        (game-score g)
                        (game-fireballs g)
                        (game-extra-life-ball g)
                        (game-lava g)
                        (game-gameover g)
                        (game-retry g)
                        true)]
                    
            [else g])))

;; Game Integer Integer MouseEvent -> Game
;; restart game when user clicks retry button
(check-expect (handle-mouse G0 120 120 "drag") G0)
(check-expect (handle-mouse G10 240 375 "button-down") G10)
(check-expect (handle-mouse G10 200 300 "button-down") G10)
(check-expect (handle-mouse G10 240 375 "drag") G10)
(check-expect (handle-mouse (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 300 false false) 240 375 "button-down")
              (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 300 false false))
(check-expect (handle-mouse (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 200 false false) 240 375 "button-down")
              (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 200 false false))
(check-expect (handle-mouse (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 200 true false) 200 300 "button-down")
              (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 200 true false))
(check-expect (handle-mouse (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 200 true false) 240 375 "button-down")
              (make-game (make-spider 250 0 10) LOW0 LOF0 3 0 0 LOFB0 false L0 false false false))
;(define (handle-mouse g x y me) g)   ;stub

(define (handle-mouse g x y me)
  (if (game-paused g)
      g
      (cond [(and (mouse=? me "button-down")
                  (not (false? (game-retry g)))
                  (< (- (/ WIDTH 2) 100) x (+ (/ WIDTH 2) 100))
                  (< (- 378 40) y (+ 378 40)))
             (make-game (make-spider (spider-x (game-spider g))
                                     0
                                     (spider-rot (game-spider g)))
                        (list (make-web (spider-x (game-spider g))
                                        0 0))
                        (game-flies g)
                        3
                        0
                        0
                        (game-fireballs g)
                        false
                        (game-lava g)
                        false
                        false
                        false)]
            [else g])))


;; String -> Image
;; make string into image
(check-expect (txt-to-image "test")
              (text "test" TEXT-SIZE TEXT-COLOUR))
(define (txt-to-image s)
  (text s TEXT-SIZE TEXT-COLOUR))

;; Game -> Boolean
(define G6 (make-game S1 LOW1 LOF0 0 0 300 LOFB0 false L0 false false false))
(check-expect (gameover? G0) false)
(check-expect (gameover? G1) false)
(check-expect (gameover? G6) true)
(define (gameover? g) (<= (game-lives g) 0))

(main G10)