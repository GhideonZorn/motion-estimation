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
(defvar F2 (imago:read-jpg (concatenate 'string data-path "LF001.jpg")))

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

;; Size of the search for corresponding pixel
;; from frame A to frame B
(defvar +ray+ 5)

;; Considering
(defun motion-vector (F1 F2)
  ;; Imago macro to iterate over all image pixels
  (imago:do-image-pixels (F1 color x y)
    (let ((h 0) (s 0) (l 0))
      (setf (values h s l) (rgb-to-hsl color))
      (get-pixel-new-pos F2 x y l))))

;; Return the vector going from
(defun vector-between (x1 y1 x2 y2)
  (list (- x1 x2) (- y1 y2)))

;; Search for a pixel with the nearest luminosity
;; to l in the ray of size +ray+ and return the
;; vector between the two pixels
(defun get-pixel-new-pos(F2 x y l)
  (cond ((or (< x +ray+) (>= x (- (imago:image-width F2) +ray+))) l)
        ((or (< y +ray+) (>= y (- (imago:image-height F2) +ray+))) l)
        (t (iter-over-area F2 x y l))))

(defun iter-over-area (F2 x y l)
  (let ((l_saved 0)
        (l_temp 0)
        (new_x x)
        (new_y y))
    (dotimes (i (+ (* +ray+ 2) 1))
      (dotimes (j (+ (* +ray+ 2) 1))
        (setf l_temp (check-pixel-at-pos F2 l l_saved
                        (- (+ x i) 5) (- (+ y j) 5)))
        (when (/= l_temp l_saved)
            (setf l_saved l_temp)
            (setf new_x (- (+ x i) 5))
            (setf new_y (- (+ y j) 5)))))
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

(motion-vector F1 F2)
