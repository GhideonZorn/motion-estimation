(uiop:define-package motion-estimation
  (:use #:cl))
(in-package #:motion-estimation)

(load "pixel-wise.lisp")
(load "display.lisp")
(load "bma.lisp")

(defun pixel-wise-and-display (f1_path f2_path ray)
  (let ((f1 (imago:read-jpg f1_path))
        (f2 (imago:read-jpg f2_path))
        (u ()) (v ()))
    (setf (values u v) (pixel-wise-motion-vector f1 f2 ray))
    (display-pixel-wise u v ray (imago:image-width f1) (imago:image-height f2) "")))

(defun pixel-wise-and-save (f1_path f2_path ray savepath)
  (let ((f1 (imago:read-jpg f1_path))
        (f2 (imago:read-jpg f2_path))
        (u ()) (v ()))
    (setf (values u v) (pixel-wise-motion-vector f1 f2 ray))
    (display-pixel-wise u v ray (imago:image-width f1) (imago:image-height f2) savepath)))

(pixel-wise-and-display "../data/LF015.jpg" "../data/LF016.jpg" 5)
(pixel-wise-and-save "../data/LF015.jpg" "../data/LF016.jpg" 5 "../save.png")

;; A block is representend by a center, and a size
(defparameter +block-size+ 10)
(defparameter +block-search-ray+ 10)

(setq +block-size+ 10)
(setq +block-search-ray+ 10)

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
(display-block u_block v_block x_block y_block (imago:image-width F1) (imago:image-height F1))
