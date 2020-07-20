(in-package #:sdl-gui-utils)

;;;;; LINE 
(defclass line ()
  ((p1 :initarg :p1 :type 'sdl:point)
   (p2 :initarg :p2 :type 'sdl:point)
   (tolerance :initarg :tolerance :initform 5)
   (color :initarg :color :initform sdl:*black*)))

(defmethod mouse-over-p ((w line) mouse)
  (with-slots (p1 p2 tolerance) w
    (multiple-value-bind (s c) (sin-cos (p- p2 p1))
      (let* ((x1 (sdl:x p1)) (y1 (sdl:y p1))
	     (intercept-cos (- (* y1 c) (* x1 s)))
	     (d (+ (* s (sdl:x mouse)) (* c -1 (sdl:y mouse)) intercept-cos)))
	(<= (abs d) tolerance)))))

;;
;;; External Commands
;; 
(defmethod draw ((w line) &key)
  (with-slots (p1 p2 color) w
    (sdl:draw-line p1 p2 :color color :aa t)))

(defmethod hover ((w line) mouse)
  (setf (slot-value w 'color) sdl:*blue*))

(defmethod unhover ((w line) mouse)
  (setf (slot-value w 'color) sdl:*black*))


;;;;; POINT
(defclass point ()
  ((p :initarg :p :type 'sdl:point)
   (color :initform sdl:*black*)))

(defmethod mouse-over-p ((w point) mouse)
  (with-slots (p) w
    (let ((tolerance 5))
      (<= (p-dot-p (p- p mouse)) (expt tolerance 2)))))

;;
;;; External Commands
;;
(defmethod draw ((w point) &key)
  (with-slots (p color) w 
    (sdl:draw-circle p 3 :color color)))

(defmethod hover ((w point) mouse)
  (declare (ignore mouse))
  (setf (slot-value w 'color) sdl:*blue*))

(defmethod unhover ((w point) mouse)
  (declare (ignore mouse))
  (setf (slot-value w 'color) sdl:*black*))
