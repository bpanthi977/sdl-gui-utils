(in-package :sdl-gui-utils)

(defclass plotter (widget dragnscale)
  ((data :initform nil :initarg :data :accessor data)
					;   (dx :type float)
					;   (dy :type float)
   (origin-x :type float :initarg :origin-x :initform 0.0)
   (origin-y :type float :initarg :origin-y :initform 0.0)
					;   (xbound :type (vector float 2))
					;   (ybound :type (vector float 2))
   (scalex :type float)
   (scaley :type float)
   (clipped-data :initform nil)))

(defmethod (setf data) (new-value (w plotter))
  (setf (slot-value w 'data) new-value)
  (calculate-clipped-data w)
  (with-slots (x y height translation) w 
    (setf translation
	  (sdl:point :x x
		     :y (+ y height)))))

(defmethod initialize-instance :after ((w plotter) &key)
  (when (slot-value w 'data)
    ;;	(calculate-stat w)
    (calculate-clipped-data w))
  (with-slots (x y height translation scalex scaley) w 
    (setf translation
	  (sdl:point :x x
		     :y (+ y height)))
    (setf (slot-value w 'callback)
	  (lambda (translation scale)
	    (setf (slot-value w 'origin-x) (- (/ (- (sdl:x translation) x) (* scalex scale))))
	    (setf (slot-value w 'origin-y) (/ (- (sdl:y translation) y height) (* scaley scale)))
	    (calculate-clipped-data w (* scale scalex) (* scale scaley))))))

(defun calculate-clipped-data (plotter &optional (scalex nil) (scaley nil))
  (with-slots (clipped-data origin-x origin-y data width height) plotter
    (unless origin-y
      (setf origin-y (reduce #'min data :key #'second)))
    (cond ((eql scalex nil)
	   (setf clipped-data (remove-if-not (lambda (point)
					       (and (>= (first point) origin-x)
						    (>= (second point) origin-y)))
					     data))
	   (let ((maxx (reduce #'max clipped-data :key #'first))
		 (maxy (reduce #'max clipped-data :key #'second)))
	     (with-slots (scalex scaley scale) plotter 
	       (setf scalex (/ width (- maxx origin-x))
		     scaley (/ height (- maxy origin-y))
		     scale 1))))
	  (t
	   (let ((maxx (+ origin-x (/ width scalex)))
		 (maxy (+ origin-y (/ height scaley))))
	     (setf clipped-data (remove-if-not (lambda (point)
						 (and (>= maxx (first point) origin-x)
						      (>= maxy (second point) origin-y)))
					       data)))))
    (setf clipped-data (sort clipped-data #'< :key #'first))))


(defmethod draw (plotter &key)
  (with-slots (x y width height clipped-data scale scalex scaley origin-x origin-y) plotter
    (when (second clipped-data)
      (loop
	 for (x2 y2) in (rest clipped-data)
	 with x11 = (truncate (* (- (first (first clipped-data)) origin-x) scalex scale))
	 with y11 = (truncate (* (- (second (first clipped-data)) origin-y) scaley scale))
	 for x22 = (truncate (* (- x2 origin-x) scalex scale))
	 for y22 = (truncate (* (- y2 origin-y) scaley scale)) do 
	   (sdl:draw-line-* (+ x x11)
			    (+ y height (- y11))
			    (+ x x22) (+ y height (- y22)))
	   (setf x11 x22
		 y11 y22)))

    (sdl:draw-rectangle-* x y width height
			  :color sdl:*blue*)
    
    (sdl:draw-string-solid-* (format nil "(~,3f,~,3f)" origin-x origin-y)
			     x (+ y height))
    (sdl:draw-string-solid-* (format nil "~,3f" (+ origin-x (/ width scalex scale)))
			     (+ x width) (+ y height))
    (sdl:draw-string-solid-* (format nil "~,3f" (+ origin-y (/ height scaley scale)))
			     x y)))

(defmethod mouse-button-down ((w plotter) button mouse)
  (if (mouse-over-p w mouse)
      (call-next-method)
      :deactivate))


;;
;;; Animating Plotter 
;;

(defclass animation-plotter (plotter)
  ((fulldata :initform nil :initarg :fulldata)
   (index :type fixnum :initform 0)))

(defmethod initialize-instance :after ((w animation-plotter) &key)
  (if (slot-value w 'fulldata)
      (progn 
	(setf (slot-value w 'data) (second (nth (slot-value w 'index)
						(slot-value w 'fulldata))))
	(calculate-clipped-data w))
      (setf (slot-value w 'scalex) nil
	    (slot-value w 'scaley) nil)))

(defmethod next-frame ((w animation-plotter))
  (with-slots (data fulldata index scalex scaley) w
    (setf index (mod (1+ index) (length fulldata))
	  data (second (nth index fulldata)))
    (calculate-clipped-data w scalex scaley)
    (first (nth index fulldata))))

(defmethod frame ((w animation-plotter) i)
  (with-slots (data fulldata index scalex scaley) w
    (setf index i
	  data (second (nth i fulldata)))
    (calculate-clipped-data w scalex scaley)))

(defmethod add-frame ((w animation-plotter) id data)
  (with-slots (fulldata index scalex scaley) w 
    (setf fulldata (merge 'list fulldata (list (list id data)) #'< :key #'first)
	  (slot-value w 'data) data
	  index (position id fulldata :key #'first))
    (calculate-clipped-data wo scalex scaley)))

