(uiop:define-package motion-estimation
  (:use #:cl))
(in-package #:motion-estimation)

;; Imago is a library for image processing/manipulation in common-lisp
;; See: https://github.com/tokenrove/imago
(ql:quickload "imago")

;; FIXME: some variables used for testing, frame and resources
;; path will be given as input to program
(defvar data-path "../data/")
(defvar F1 (imago:read-jpg (concatenate 'string data-path "LF000.jpg")))

;; Compute H component of HSL representation
(defun H (max r g b delta)
  (cond ((= delta 0) 0)
        ((= max r) (* 60 (mod (/ (- g b) delta) 6)))
        ((= max g) (* 60 (+ (/ (- b r) delta) 2)))
        (t (* 60 (+ (/ (- r g) delta) 4)))))

;; Compute S component of HSL representation
(defun S (delta l)
  (if (= delta 0)
      0
      (/ delta (- 1 (abs (- (* 2 l) 1))))))

(defun L (max min)
  (/ (+ max min) 2))

;; TODO: use values to reduce function size and lisibility
(defun rgb-to-hsl (rgb)
  (let*
      ((r (/ (imago:color-red rgb) 255))
       (g (/ (imago:color-green rgb) 255))
       (b (/ (imago:color-blue rgb) 255))
       (min (min r g b))
       (max (max r g b))
       (l (L max min))
       (h (H max r g b (- max min)))
       (s (S (- max min) l)))
    (setq h )

    (setq l (* 100 l))
    (setq s (* 100 s))
  ))

;; Imago macro to iterate over all image pixels
(imago:do-image-pixels (F1 color x y)
  (rgb-to-hsl color))
