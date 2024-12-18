(uiop:define-package motion-estimation
  (:use #:cl))
(in-package #:motion-estimation)

(load "hsl.lisp")

(defun luminosity (F x y)
  (let ((h 0) (s 0) (l 0))
    (setf (values h s l) (rgb-to-hsl (imago:image-pixel F x y)))
  l))

(defun block-is-within-frame (F x y block-size)
  (if (and (>= (- x (floor block-size 2)) 0)
           (< (+ x (floor block-size 2)) (- (imago:image-width F) (floor block-size 2)))
           (>= (- y (floor block-size 2)) 0)
           (< (+ y (floor block-size 2)) (- (imago:image-height F) (floor block-size 2))))
      t
      nil))

(defun block-luminosity (F x y block-size)
  (if (block-is-within-frame F x y block-size)
      (let ((sum 0))
        (dotimes (i (+ block-size 1))
          (dotimes (j (+ block-size 1))
            (setf sum (+ sum (luminosity F (- (+ x i) (floor block-size 2))
                                (- (+ y j) (floor block-size 2)))))))
        sum)
        -1000))

(defun check-block-at-pos (F l l_saved bx by block-size)
  (let ((l_found (block-luminosity F bx by block-size)))
    (get-nearest-luminosity l l_saved l_found)))

(defun search-for-matching-blocks (F1 F2 bx by block-size block-search-ray)
  (let ((l (block-luminosity F1 bx by block-size))
        (l_saved 0)
        (l_temp 0)
        (new_bx -1000)
        (new_by -1000))
    (dotimes (i (+ (* block-search-ray 2) 1))
      (dotimes (j (+ (* block-search-ray 2) 1))
        (setf l_temp (check-block-at-pos F2 l l_saved (- (+ bx i) block-search-ray)
                                      (- (+ by j) block-search-ray) block-size))
        (when (or (/= l_temp l_saved)
                  (< (distance bx by (- (+ bx i) block-search-ray) (- (+ by j) block-search-ray))
                     (distance bx by new_bx new_by)))
          (setf l_saved l_temp)
          (setf new_bx (- (+ bx i) block-search-ray))
          (setf new_by (- (+ by j) block-search-ray)))))
    (list (vector-between bx by new_bx new_by) (list new_bx new_by))))

;; Take a list of vectors and decompose it in two
;; lists containing x and y seperately
(defun decompose (list)
  (let ((u (mapcar #'(lambda (elm) (first (first elm))) list))
        (v (mapcar #'(lambda (elm) (second (first elm))) list))
        (x (mapcar #'(lambda (elm) (first (second elm))) list))
        (y (mapcar #'(lambda (elm) (second (second elm))) list)))
    (values u v x y)))

(defun bma-motion-vector (F1 F2 x y block-size block-search-ray)
  (when (> x (imago:image-width F1))
    (setf x 0)
    (setf y (+ y block-size)))
  (when (< y (imago:image-height F1))
    (cons (search-for-matching-blocks F1 F2 x y block-size block-search-ray)
          (bma-motion-vector F1 F2 (+ x block-size) y
                                   block-size block-search-ray))))
