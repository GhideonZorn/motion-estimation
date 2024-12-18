(uiop:define-package motion-estimation
  (:use #:cl))
(in-package #:motion-estimation)

(load "pixel-wise.lisp")
(load "display.lisp")
(load "bma.lisp")

;; FIXME: some variables used for testing, frame and resources
;; path will be given as input to program
(defvar data-path "../data/")
(defvar F1 (imago:read-jpg (concatenate 'string data-path "LF000.jpg")))
(defvar F2 (imago:read-jpg (concatenate 'string data-path "LF001.jpg")))

;; Size of the search for corresponding pixel
;; from frame A to frame B
(defvar +ray+ 5)

;; A block is representend by a center, and a size
(defvar +block-size+ 10)
(defvar +block-search-ray+ 10)

(setq +block-size+ 10)
(setq +block-search-ray+ 10)

(defparameter u ())
(defparameter v ())
(setf (values u v) (pixel-wise-motion-vector F1 F2))

(defparameter u_block ())
(defparameter v_block ())
(defparameter x_block ())
(defparameter y_block ())
(setf (values u_block v_block x_block y_block)
      (decompose (bma-motion-vector F1 F2 10 10 +block-size+ +block-search-ray+)))

(defun within-borns (size v)
  (if (and (> v 0) (< v size))
    t
    nil))

;; FIXME: merge the two compute-neighbors in one using function
;; passed as parameter
(defun compute-neighbors(list idx size width alpha)
  (let* ((neighbors (list -1 1 (- width) width))
         (sum (nth idx list))
         (count 1))
    (loop for n in neighbors
          do (when (within-borns size (+ n idx))
               (setf count (1+ count))
               (setf sum (+ sum (nth (+ n idx) list)))))
    (floor (+ (* alpha (nth idx list)) (* (- 1 alpha) (floor sum count))))))

(defun neighbors (list size width alpha)
  (mapcar #'(lambda (pair) (compute-neighbors list (cdr pair) size width alpha))
        (mapcar #'(lambda (elm idx) (cons elm idx)) list
                (loop for i from 0 to size collect i))))

(setf (values u v) (pixel-wise-motion-vector F1 F2))

;; Calls to change u and v using smoothness of motion vectors
;; and a alpha argument
(setf u (neighbors u (length u) (imago:image-width F1) 0.0))
(setf v (neighbors v (length v) (imago:image-width F1) 0.0))

;; Calls to display the resulfs for pixel wise and bma
(display u v 5 (imago:image-width F1) (imago:image-height F1))
(display-block u_block v_block x_block y_block (imago:image-width F1) (imago:image-height F1))
