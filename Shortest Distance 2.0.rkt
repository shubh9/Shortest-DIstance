;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Shortest Distance - add-line|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;(require htdp/image)
(require 2htdp/universe)
(require spd/tags)           

;;==========================================
;; Constants

(define WIDTH 400)
(define HEIGHT 600)

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define BKG-MAP (rectangle WIDTH HEIGHT "solid" "yellow"))
  
(define MARKER (overlay/align "center" "center"
                              (square 10 "solid" "yellow")
                              (square 15 "solid" "black")))

(define obstacle1 (place-image (rectangle 50 50 "solid" "green") CTR-X CTR-Y BKG-MAP))

;;=================================================
;; Data Definitions:

(@HtDD Position)
(define-struct position (xpos ypos))
;; Postition is (make-position Natural Natural)
;; interp. the x, y position of an position on the screen

(define P1 (make-position 100 50))
(define P2 (make-position 200 300))
(define P3 (make-position 200 100))
(define P4 (make-position 350 25))
(define P5 (make-position 350 550))

#;
(define (fn-for-position p)
  (... (position-xpos)
       (position-ypos)))

(@HtDD ListOfPosition)
;; ListOfEgg is one of:
;; - empty
;; - (cons Position ListOfPosition)
;; interp. a list of positions
(define LOP0 empty)
(define LOP1 (cons P1 empty))
(define LOP2 (list P1 P2 P3 P4 P5))

(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-position (first lop))
              (fn-for-lop (rest lop)))]))

;;===================================================
;; Functions:

(@HtDF main)
(@signature ListOfPosition -> ListOfPosition)
;; start the world with (main empty)
;; no tests for main
(@template htdw-main)
(define (main lop) 
  (big-bang lop                ;ListOfPosition
    (to-draw render-markers)   ;ListOfPosition -> Image
    (on-mouse mark-pos)))      ;ListOfPosition Integer Integer MouseEvent -> Image


(@HtDF render-markers)
(@signature ListOfPosition -> Image)
;; place markers for each position

(@template ListOfPosition)
(define (render-markers lop)
  (cond [(empty? lop) BKG-MAP]
        [(empty? (rest lop))(place-marker (first lop) BKG-MAP)]
        [else (add-line (place-marker (first lop)
                                      (render-markers (rest lop)))
                        (position-xpos (first lop))
                        (position-ypos (first lop))
                        (position-xpos (second lop))
                        (position-ypos (second lop))
                        "blue")]))

(@HtDF place-marker)
(@signature Position Image -> Image)
;; place MARKER at x and y

(@template Position add-param)
(define (place-marker p1 img)
  (place-image MARKER (position-xpos p1) (position-ypos p1) img))  

(@HtDF mark-pos)
(@signature ListOfPosition Integer Integer MouseEvent -> ListOfPosition)
;; add a marker at x, y when the mouse is clicked

(@template MouseEvent add-param)
(define (mark-pos lop x y me)
  (cond [(and (mouse=? me "button-down")
              (= (count lop) 0)) (list (make-position x y))]
        [(and (mouse=? me "button-down")
              (= (count lop) 1)) (cons (make-position x y) lop)]
        [(and (mouse=? me "button-down")
              (= (count lop) 2)) empty]
        [else lop]))

(define (count lop)
  (cond [(empty? lop) 0]
        [else (add1 (count (rest lop)))]))


 (define (collision? p1 p2)
   (local[
          (define (collision p1 p2)
            (local [(define (slope p1 p2)
                      (if (= (position-xpos p2) (position-xpos p1))
                          0
                          (/ (- (position-ypos p2) (position-ypos p1))
                             (- (position-xpos p2) (position-xpos p1)))))
                    (define pix (square 1 "black" "solid"))
                    (define XADD 1)]
              (cons (place-image pix
                                 (position-xpos p1)
                                 (position-ypos p1)
                                 BKG-MAP)
                    (if (at-position p1 p2)
                        empty
                        (collision (+ XADD (position-xpos p1)) (- (slope p1 p2) (position-ypos p1)))))))]
     (andmap (inside-image? (collision p1 p2)))))

(define (at-position p1 p2)
  (and (= (position-xpos p1)(position-xpos p2))
                             (= (position-ypos p1)(position-ypos p2))))

   (define (inside-image? i1 i2)
     (image-inside? i1 obstacle1))

   

