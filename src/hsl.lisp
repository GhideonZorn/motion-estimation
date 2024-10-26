(uiop:define-package motion-estimation
  (:use #:cl))
(in-package #:motion-estimation)

;; Imago is a library for image processing/manipulation in common-lisp
;; See: https://github.com/tokenrove/imago
(ql:quickload "imago")

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

;; Compute L component of HSL representation
(defun L (max min)
  (/ (+ max min) 2))

(defun rgb-to-hsl (rgb)
  (let*
      ((r (/ (imago:color-red rgb) 255))
       (g (/ (imago:color-green rgb) 255))
       (b (/ (imago:color-blue rgb) 255))
       (min (min r g b))
       (max (max r g b))
       (l (L max min)))
    (values (H max r g b (- max min))
            (* 100 (S (- max min) l))
            (* 100 l))))
