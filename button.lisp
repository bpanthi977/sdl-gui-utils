(in-package :sdl-gui-utils)

(defclass button (widget)
  ((text :type string :initform "OK" :initarg :text)
   (state :type boolean :initform nil :initarg :state)
   (callback :type function :initarg :callback)))

(defmethod mouse-button-down ((w button) button mouse)
  (cond
    ((not (mouse-over-p w mouse))
     :deactivate)
    ((eql button sdl:mouse-left)
     (setf (slot-value w 'state) (not (slot-value w 'state)))
     (callback w))))

(defmethod draw ((w button) &key)
  (with-slots (text state width height x y) w
    (let ((color (if state
		     sdl:*blue*
		     sdl:*black*)))
      (sdl:draw-rectangle-* x y width height :color color)
      (sdl:draw-string-solid-* text x y :color color))))
