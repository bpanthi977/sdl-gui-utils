(in-package #:sdl-gui-utils)

(defclass dragnscale ()
  ((callback :initform nil :initarg :callback
	     :documentation "A function f(coord, scale) that wil be called on position and scale updates")
   (drag-state :type boolean :initform nil)
   (scale :type float :initform 1.0 :initarg :scale)
   (scale-factor :type float :initform 1.2 :initarg scale-factor)
   (scale-at-point :type boolean :initform t :initarg :scale-at-point?)
   (hold-position :type sdl:point :initform nil)
   (translation :type sdl:point :initform (sdl:point :x 0 :y 0) :initarg :translation)
   (continuous-update :type boolean :initform t :initarg :continuous-update?)))

(defmethod click ((w dragnscale) mouse)
  (with-slots (drag-state hold-position) w
    (setf drag-state t)
    (setf hold-position mouse)))

(defmethod unclick ((w dragnscale) mouse)
  (with-slots (drag-state hold-position scale callback translation) w
    (setf drag-state nil)
    (when hold-position
      (pincf translation (p- mouse hold-position))
      (when callback
	(funcall callback translation scale)))))

(defmethod scroll ((w dragnscale) sf mouse)
  (with-slots (translation scale callback scale-at-point) w
    (setf scale (* scale sf))
    (when scale-at-point
      (setf translation (p- (p* sf translation) (p* (1- sf) mouse))))
    (funcall callback translation scale)))

;; External Inputs

(defmethod mouse-motion ((w dragnscale) mouse)
  (when (slot-value w 'drag-state)
    (with-slots (continuous-update hold-position scale callback translation) w
      (when (and callback continuous-update)
	(funcall callback
		 (p+ translation (p- mouse hold-position))
		 scale)))))

(defmethod mouse-button-down ((w dragnscale) button mouse)
  (cond 
    ((eql button sdl:mouse-left) (click w mouse) :activate)
    ((eql button sdl:mouse-right) :deactivate)
    ((eql button sdl:mouse-wheel-up) (scroll w (slot-value w 'scale-factor) mouse))
    ((eql button sdl:mouse-wheel-down) (scroll w (/ (slot-value w 'scale-factor)) mouse))))

(defmethod mouse-button-up ((w dragnscale) button mouse)
  (cond 
    ((eql button sdl:mouse-left) (unclick w mouse))))

