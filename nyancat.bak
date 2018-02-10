;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname nyancat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH 500)
(define HEIGHT 500)
(define BG-COLOUR "blue")
(define X-POS (/ WIDTH 2))
(define Y-POS (/ HEIGHT 2))

(define RAINBOW-WIDTH 10)
(define RAINBOW-HEIGHT 6)
(define RAINBOW-IMAGE (above
                       (rectangle RAINBOW-WIDTH RAINBOW-HEIGHT "solid" "red")
                       (rectangle RAINBOW-WIDTH RAINBOW-HEIGHT "solid" "orange")
                       (rectangle RAINBOW-WIDTH RAINBOW-HEIGHT "solid" "yellow")
                       (rectangle RAINBOW-WIDTH RAINBOW-HEIGHT "solid" "green")
                       (rectangle RAINBOW-WIDTH RAINBOW-HEIGHT "solid" "blue")
                       (rectangle RAINBOW-WIDTH RAINBOW-HEIGHT "solid" "purple")))



(define BACKGROUND (rectangle WIDTH HEIGHT "solid" BG-COLOUR))


  

;; NyanFrame is Image

;; (listof NyanFrame)
;; interp. a series of frames that should be cycled through

;; Nyan is (make-nyan Natural Integer Integer (listof NyanFrame))
;; interp. a cat with an animation frame and x and y coordinates
(define-struct nyan (frame x y lonf))


;; Rainbow is (make-rainbow Number Natural Natural)
;; interp. the x and y position of a piece, and
;; delta from the nyan's position that this piece is
(define-struct rainbow (delta x y))

;; Star is (make-star Image Natural Natural)
;; one of many possible star choices
(define-struct pixel-star (img x y))

;; Prize is (make-prize Image Natural Natural Natural Natural)
;; interp an image, its pos, its size (box), and point value
(define-struct prize (img x y size pts))


;; Background is (make-background (listof Stars) (listof Prize))
(define-struct background (los lop))

;; NyanState is (make-nyanstate Nyan (listof Rainbow) Background Natural Rsound)
;; interp. a game state with nyan cat, rainbow following, background, score, and sound
(define-struct nyanstate (nyan lor bg score sound))




(define NYAN1 (square 20 "solid" "red"))
(define NYAN2 (square 20 "solid" "orange"))
(define NYAN3 (square 20 "solid" "yellow"))
(define NYAN4 (square 20 "solid" "green"))

(define NYANLIST (list NYAN1 NYAN2 NYAN3 NYAN4))


              
(define POPTART
  (overlay (rectangle 70 50 "solid" "pink")
           (rectangle 55 40 "solid" "tan")))
(define HEAD
  (ellipse 40 35 "solid" "gray"))
(define NYAN_BASIC
  (overlay/offset HEAD -20 -10 POPTART))

(define NYANLIST_BASIC (list NYAN_BASIC NYAN_BASIC))

(define DEFAULT-NYAN (make-nyan 0 X-POS Y-POS NYANLIST_BASIC))

(define BORING-BG (make-background empty empty))

(define (generate-rainbows w)
  (map
   (lambda (n)
     (make-rainbow (+ (modulo n 3) (/ 5 RAINBOW-WIDTH))
                   (- X-POS n)
                   Y-POS))
  (build-list (/ w RAINBOW-WIDTH)
  (lambda (n) (* n RAINBOW-WIDTH)))))

(define start
  (make-nyanstate
     DEFAULT-NYAN
     (generate-rainbows WIDTH)
     BORING-BG
     0
     0))

;;;;;;=====

(define (main ns)
  (big-bang ns
            (on-tick tock)
            (to-draw render)
            (on-key handle-key)))



;; NyanState -> NyanState
;; tick the state of the world forward

(define (tock ns)
  (make-nyanstate
   (tock-nyan (nyanstate-nyan ns))
   (tock-rainbow (nyanstate-lor ns))
   (tock-background (nyanstate-bg ns))
   (nyanstate-score ns)
   (replay-sound (nyanstate-sound ns))))


(define (tock-nyan nyan)
  (make-nyan (modulo
              (add1 (nyan-frame nyan))
              (sub1 (length (nyan-lonf nyan))))
             (nyan-x nyan)
             (nyan-y nyan)
             (nyan-lonf nyan)))

(define (tock-rainbow rb)
  (if (empty? rb)
      empty
      (cons (tock-rainbow--one (first rb))
            (tock-rainbow (rest rb)))))

;; !!! do some shit to make this a sine wave
(define (tock-rainbow--one rb)
  (make-rainbow
   (sin (* 2 pi )) ;; need each piece to keep track
   (rainbow-x rb)
   (rainbow-y rb))
  )

(define (tock-background bg)
  bg) ;; !!! later on, add random star generations

(define (replay-sound s)
  s)



(define (render ns)
  (render-nyan (nyanstate-nyan ns)
               (render-rainbow (nyanstate-lor ns)
                               (render-bg (nyanstate-bg ns)))))

(define (render-nyan nyan bg)
  (place-image (get-nyanframe nyan)
               (nyan-x nyan)
               (nyan-y nyan)
               bg))

(define (get-nyanframe nyan)
  (list-ref (nyan-lonf nyan)
            (nyan-frame nyan)))

(define (render-rainbow lor bg)
  (if (empty? lor)
      bg
      (place-rainbow (first lor)
                     (render-rainbow (rest lor) bg))))

(define (place-rainbow r bg)
  (place-image RAINBOW-IMAGE
               (rainbow-x r)
               (+ (rainbow-y r)
                  (rainbow-delta r))
               bg))

(define (render-bg bg)
  BACKGROUND)

(define (handle-key ns keyev)
  ns)
  
                  