(in-package :sdl-gui-utils)

(defclass slider(widget)
  ((value :type float :initarg :value :initform 0.0 :accessor value)
   (dynamic-update :type boolean :initform nil :initarg :dynamic-update)
   (slider-width :type integer :initform 50 :initarg :slider-width)
   (height :type integer :initform 5 :initarg :height)
   (range :type (vector float 2) :initarg :range :initform #(0 10))
   (click-state :type boolean :initform nil)
   (callback :type function :initarg :callback)))

(defmethod initialize-instance :after ((w slider) &key)
  (setf (slot-value w 'width) (slot-value w 'slider-width)))

;;
;;; External Inputs
;;
(defun percent (slider)
  (with-slots (range value) slider
    (/ (- value (aref range 0))
       (- (aref range 1) (aref range 0)))))

(defmethod draw ((w slider) &key)
  (with-slots (range slider-width x y value height click-state) w
    (let ((y (+ y (truncate height 2))))
      (sdl:draw-line-* x y (+ x slider-width) y)
      (sdl:draw-circle-* (+ x (truncate (* (percent w) slider-width)))
			 y
			 (truncate height 2)
			 :color (if click-state
				    sdl:*blue*
				    sdl:*black*)))))

(defmethod mouse-motion ((w slider) mouse)
  (when (slot-value w 'click-state)
    (with-slots (value range x y slider-width) w
      (let* ((x-del (- (sdl:x mouse) x))
	     (percent (/ x-del slider-width)))
	(when (<= 0 percent 1)
	  (setf value (+ (aref range 0) (* percent (- (aref range 1) (aref range 0)))))
	  (when (slot-value w 'dynamic-update)
	    (callback w))))))
  :pass)

(defmethod mouse-button-down ((w slider) button mouse)
  (cond
    ((or (not (eql button sdl:mouse-left))
	 (not (mouse-over-p w mouse)))
     :pass)
    (t 
     (setf (slot-value w 'click-state) t)
     (mouse-motion w mouse)
     :activate)))

(defmethod mouse-button-up ((w slider) button mouse)
  (cond
    ((eql button sdl:mouse-left)
     (setf (slot-value w 'click-state) nil)
     (callback w))))

;;
;;; Slider with number entry
;; 

(defclass slider-with-entry (slider)
  ((entry :type number-entry)))

(defmethod initialize-instance :after ((w slider-with-entry) &key)
  (with-slots (x y slider-width) w
    (setf (slot-value w 'entry)
	  (make-instance 'number-entry
			 :value (slot-value w 'value)
			 :x (+ x slider-width 10)
			 :y y))
    (setf (slot-value w 'width)
	  (+ slider-width (slot-value (slot-value w 'entry) 'width)))))


(defmethod key-down ((w slider-with-entry) &key key char)
  (with-slots (value entry) w
    (cond ((or (eql key :sdl-key-return)
	       (eql key :sdl-key-kp-enter))
	   (setf value (slot-value entry 'value))
	   (callback w))
	  (t
	   (key-down entry :key key :char char)))))

(defmethod draw :after ((w slider-with-entry) &key)
  (draw (slot-value w 'entry)))

(defmethod mouse-motion :after ((w slider-with-entry) mouse)
  (setf (value (slot-value w 'entry)) (coerce (slot-value w 'value) 'float)))

(defmethod (setf value) (new-value (w slider-with-entry))
  (setf (slot-value w 'value) new-value)
  (setf (value (slot-value w 'entry)) new-value))


;;
;;; Slider with name
;;
(defclass slider-with-name (slider)
  ((name :type string :initarg :name)))

(defmethod draw ((w slider-with-name) &key)
  (with-slots (x y name height) w
    (sdl:draw-string-solid-* name (- x (truncate height 2)) (+ y (truncate height 2) -4) :justify :right)
    (call-next-method)))

;;
;;; Slider with entry&name

(defclass slider-with-entry&name (slider-with-name slider-with-entry) nil)
