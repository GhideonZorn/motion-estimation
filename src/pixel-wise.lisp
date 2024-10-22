(uiop:define-package motion-estimation
  (:use #:cl))
(in-package #:motion-estimation)

(load "hsl.lisp")

;; Size of the search for corresponding pixel
;; from frame A to frame B
(defvar +ray+ 5)

(defun pixel-wise-motion-vector (F1 F2)
  (let ((u nil) (v nil) (tmp nil))
  ;; Imago macro to iterate over all image pixels
    (imago:do-image-pixels (F1 color x y)
      (let ((h 0) (s 0) (l 0))
        (setf (values h s l) (rgb-to-hsl color))
        (setf tmp (get-pixel-new-pos F2 x y l))
        (setf u (cons (first tmp) u))
        (setf v (cons (second tmp) v))))
    (values (reverse u) (reverse v))))

;; Return the vector going from
(defun vector-between (x1 y1 x2 y2)
  (list (- x1 x2) (- y1 y2)))

;; Search for a pixel with the nearest luminosity
;; to l in the ray of size +ray+ and return the
;; vector between the two pixels
(defun get-pixel-new-pos(F2 x y l)
  (cond ((or (< x +ray+) (>= x (- (imago:image-width F2) +ray+))) '(0 0))
        ((or (< y +ray+) (>= y (- (imago:image-height F2) +ray+))) '(0 0))
        (t (iter-over-area F2 x y l))))

;; FIXME: Find a better way to handling first call than
;; to return a big value when encountering -1
(defun distance (x1 y1 x2 y2)
  (if (= x2 -1)
      10000
      (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(defun iter-over-area (F2 x y l)
  (let ((l_saved 0)
        (l_temp 0)
        (new_x -1)
        (new_y -1))
    (dotimes (i (+ (* +ray+ 2) 1))
      (dotimes (j (+ (* +ray+ 2) 1))
        (setf l_temp (check-pixel-at-pos F2 l l_saved
                        (- (+ x i) 5) (- (+ y j) 5)))
        (when (or (/= l_temp l_saved)
                   (< (distance x y (- (+ x i) 5) (- (+ y j) 5))
                      (distance x y new_x new_y)))
            (setf l_saved l_temp)
            (setf new_x (- (+ x i) 5))
            (setf new_y (- (+ y j) 5))
            )))
    (vector-between x y new_x new_y)))

;; Get the luminosity at x y of frame F2 and compare
;; it with l_saved to get the nearest to l
(defun check-pixel-at-pos (F2 l l_saved x y)
  (let ((h_found 0) (s_found 0) (l_found 0))
    (setf (values h_found s_found l_found)
          (rgb-to-hsl (imago:image-pixel F2 x y)))
    (get-nearest-luminosity l l_saved l_found)))

;; Compare the actual saved luminosity with the found one
;; Return the nearest to the luminosity ref from frame F2
(defun get-nearest-luminosity(l_ref l_saved l_found)
  (if (< (abs (- l_ref l_found)) (abs (- l_ref l_saved)))
    l_found
    l_saved))
