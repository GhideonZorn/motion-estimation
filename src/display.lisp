(uiop:define-package motion-estimation
  (:use #:cl))
(in-package #:motion-estimation)

;; Imago is a library for image processing/manipulation in common-lisp
;; See: https://github.com/tokenrove/imago
(ql:quickload "imago")

;; Using py4cl to use matplotlib and quiver to print obtained
;; vectors
(ql:quickload :py4cl)
(setf py4cl:*python-command* "python3")

(py4cl:import-module "numpy" :as "np")
(py4cl:import-module "matplotlib.pyplot" :as "plt")

;; Reduce by keeping element every index + spacing
(defun reduce-by-spacing (list spacing width)
  (mapcar #'(lambda (pair) (car pair))
      (remove-if #'(lambda (pair)
                     (if (and (eq (mod (cdr pair) spacing) 0)
                              (eq (mod (floor (cdr pair) width) spacing) 0)) nil t))
          (mapcar #'(lambda (elm idx) (cons elm idx)) list
              (loop for i from 0 to (length list) collect i)))))

(defun get-points-arrays (spacing width height)
  (let ((x (np:arange 0 width)) (y (np:arange 0 height))
        (bigx nil) (bigy nil) (tmp nil))
    (setf tmp (np:meshgrid x y))
    (setf bigx (aref tmp 0))
    (setf bigy (aref tmp 1))
    (let ((i_x (np:arange 0 (floor (np:size bigx) spacing) spacing))
        (i_y (np:arange 0 height spacing)))
    (setf i_y (np:clip i_y 0 (- height 1)))
    (setf bigx (np:take bigx i_y 0))
    (setf bigy (np:take bigy i_y 0))
    (setf i_x (np:clip i_x 0 (- (np:size bigx) 1)))
    (setf bigx (py4cl:python-call "np.ndarray.flatten" bigx))
    (setf bigy (py4cl:python-call "np.ndarray.flatten" bigy))
    (setf bigx (np:take bigx i_x))
    (setf bigy (np:take bigy i_x)))
    (values bigx bigy)
  ))

(defun display (u v spacing width height)
  (setf u (reduce-by-spacing u spacing width))
  (setf v (reduce-by-spacing v spacing width))
  (plt:subplots :figsize '(10 10))
  (let ((x nil) (y nil))
    (setf (values x y) (get-points-arrays spacing width height))
    (print (np:size x))
    (print (np:size y))
    (plt:quiver x y u v :color "r" :angles "xy" :scale 1
                              :scale_units "xy" :linewidth 0.5))
  (plt:xlim 0 width)
  (plt:ylim height 0)
  (plt:show))
