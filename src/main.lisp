(uiop:define-package motion-estimation
  (:use #:cl))
(in-package #:motion-estimation)

(load "pixel-wise.lisp")
(load "display.lisp")
;;(load "bma.lisp")

;; FIXME: some variables used for testing, frame and resources
;; path will be given as input to program
(defvar data-path "../data/")
(defvar F1 (imago:read-jpg (concatenate 'string data-path "LF000.jpg")))
(defvar F2 (imago:read-jpg (concatenate 'string data-path "LF001.jpg")))

(defparameter u ())
(defparameter v ())
(setf (values u v) (pixel-wise-motion-vector F1 F2))

(display u v 5 (imago:image-width F1) (imago:image-height F1))
