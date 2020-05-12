(in-package #:sdl-gui-utils)

(defclass dragndropnscale ()
  ((callback :initform nil :initarg :callback
 		   :documentation "A function f(coord, scale) that wil be called on position and scale updates")
   (drag-state :type boolean :initform nil)
   (origin :type complex :initform #C(0 0) :initarg :origin)
   (scale :type float :initform 1 :initarg :scale)
   (scale-factor :type float :initform 1.2 :initarg scale-factor)
   (scale-at-point :type boolean :initform t :initarg :scale-at-point?)
   (hold-position :type complex :initform nil)
   (translation :type complex :initform #C(0 0) :initarg :translation)
   (continuous-update :type boolean :initform t :initarg :continuous-update?)))

(defmethod click ((w dragndropnscale) x y)
  (with-slots (drag-state hold-position) w
	(setf drag-state t)
	(setf hold-position (complex x y))))

(defmethod unclick ((w dragndropnscale) mouse-x mouse-y)
  (with-slots (drag-state hold-position scale callback translation) w
	(setf drag-state nil)
	(incf translation (- (complex mouse-x mouse-y) hold-position))
	(when callback
	  (funcall callback translation scale))))

(defmethod scroll ((w dragndropnscale) sf x y)
  (with-slots (translation scale callback scale-at-point origin) w
	(setf scale (* scale sf))
	(when scale-at-point
	  (setf translation (- (* sf translation) (* (1- sf) (complex x y)))))
	(funcall callback translation scale)))

;; External Inputs

(defmethod mouse-motion ((w dragndropnscale) mouse-x mouse-y)
  (when (slot-value w 'drag-state)
	(with-slots (continuous-update hold-position scale callback translation) w
	  (when (and callback continuous-update)
		(funcall callback
				 (+ translation (- (complex mouse-x mouse-y) hold-position))
				 scale)))))

(defmethod mouse-button-down ((w dragndropnscale) button x y)
  (cond 
	((eql button sdl:mouse-left) (click w x y))
	((eql button sdl:mouse-wheel-up) (scroll w (slot-value w 'scale-factor) x y))
	((eql button sdl:mouse-wheel-down) (scroll w (/ (slot-value w 'scale-factor)) x y))))

(defmethod mouse-button-up ((w dragndropnscale) button x y)
  (cond 
	((eql button sdl:mouse-left) (unclick w x y))))
