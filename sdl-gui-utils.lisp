;;;; sdl-gui-utils.lisp

(in-package #:sdl-gui-utils)

(defclass widget ()
  ((x :initarg :x :type 'integer :accessor x :initform (sdl:mouse-x))
   (y :initarg :y :type 'integer :accessor y :initform (sdl:mouse-y))
   (width :initarg :width :type 'single-float :accessor width :initform 50)
   (height :initarg :height :type 'single-float :accessor height :initform 20))
  (:documentation "A Widget (Things that are visible are widgets)")
  (:default-initargs :x 0 :y 0))

(defun callback (w)
  (when (slot-boundp w 'callback)
    (funcall (slot-value w 'callback) w)))

(defmethod key-down (o &key key char)
  (declare (ignore char))
  (format t "~a din't use key-down event" o)
  (cond
    ((eql key :sdl-key-escape) :deactivate)))

(defmethod mouse-button-down (i button mouse)
  :activate)

(defmethod mouse-over-p ((w widget) mouse)
  (with-slots (x y width height) w
    (and (<= x (sdl:x mouse) (+ x width))
	 (<= y (sdl:y mouse) (+ y height)))))


(defmethod mouse-button-up (i button mouse)
  t)

(defmethod mouse-motion (i mouse)
  :pass)

(defmethod hover (i mouse)
  (format t "~a doesn't handle hover" i))

(defmethod unhover (i mouse)
  (format t "~a doesn't handle unhover" i))

(defmethod mouse-over-p (i mouse)
  (format t "~a doesn't handle mouse-over-p" i))



;;
;;; EXAMPLE
;;

(defparameter *translation* #C(0 0))
(defparameter *scale* 1)

(defun example ()
  (setf *translation* (sdl:point :x 0 :y 0)
	*scale* 1)
  (let* ((drag-scale (make-instance 'dragndropnscale
				    :scale *scale* :translation *translation* :origin (sdl:point :x 0 :y 0)
				    :callback (lambda (tr s)
						(setf *translation* tr
						      *scale* s))))
	 (commandbox (make-instance 'command-box :x 50 :y 40))
	 (evh (make-instance 'events-handler))
	 (point-selector (make-instance 'point-selector)))
    (flet ((add-command2 (command interface-function)
	     (add-command commandbox command
			  #'(lambda ()
			      (funcall interface-function evh)))))
      ;; add commands 
      (add-command commandbox "drag"
		   (lambda ()
		     (push-interface evh drag-scale :hoverable? nil :activable? t :drawable? nil)))
      (add-command2 "line" #'line-entry-interface)
      (add-command2 "point" #'point-entry-interface)
      (add-command2 "points"
		    (lambda (event-handler)
		      (push-interface
		       event-handler
		       (make-instance 'point-entry
				      :callback (lambda (w)
						  (push-interface event-handler (slot-value w 'point)
								  :activable? nil :hoverable? t)
						  (push (slot-value w 'point) (slot-value point-selector 'points))
						  (setf (slot-value event-handler 'active-object) w))))))
      (add-command2 "select-point"
		    (lambda (event-handler)
		      (push-interface event-handler point-selector)))

      ;; send inputs i.e. activate commandbox
      (push-interface evh commandbox)
      
      ;; Initialize sdl 
      (sdl:with-init ()
	(sdl:window 1000 1000)
	(sdl:enable-unicode)
	(sdl:initialise-default-font)
	(setf sdl:*default-color* sdl:*black*)
	(sdl:with-events ()
	  (:quit-event () t)
	  (:key-down-event (:key key :unicode code)
			   (print key)
			   (case key
			     (:sdl-key-q (sdl:push-quit-event))
			     (t (key-down evh :key key :char (code-char code)))))
	  (:mouse-motion-event
	   (:x x :y y)
	   (mouse-motion evh (sdl:point :x x :y y)))
	  (:mouse-button-down-event
	   (:button button :x x :y y)
	   (mouse-button-down evh button (sdl:point :x x :y y)))
	  (:mouse-button-up-event
	   (:button button :x x :y y)
	   (mouse-button-up evh button (sdl:point :x x :y y)))
	  (:idle ()
		 (sdl:clear-display sdl:*white*)
		 ;; Draw sth
		 (sdl:draw-line *translation*
				(p+ *translation* (p* *scale* (sdl:point :x 100 :y 100)))
				:color sdl:*blue* :aa t)
		 ;; widgets
		 ;; (if scale-widget
		 ;; 	   (draw scale-widget))
		 (draw evh)
		 (sdl:update-display)))))))

(defmacro loop-with-events-to ((evh) &body body)
  (alexandria:once-only (evh)
			`(sdl:with-init ()
			   (sdl:window 1000 1000)
			   (sdl:enable-unicode)
			   (sdl:initialise-default-font)
			   (setf sdl:*default-color* sdl:*black*)
			   (sdl:with-events ()
			     (:quit-event () t)
			     (:key-down-event (:key key :unicode code)
					      (case key
						(:sdl-key-q (sdl:push-quit-event))
						(t (key-down ,evh :key key :char (code-char code)))))
			     (:mouse-motion-event
			      (:x x :y y)
			      (mouse-motion ,evh (sdl:point :x x :y y)))
			     (:mouse-button-down-event
			      (:button button :x x :y y)
			      (mouse-button-down ,evh button (sdl:point :x x :y y)))
			     (:mouse-button-up-event
			      (:button button :x x :y y)
			      (mouse-button-up ,evh button (sdl:point :x x :y y)))
			     (:idle ()
				    (sdl:clear-display sdl:*white*)
				    ,@body
				    (draw ,evh)
				    (sdl:update-display))))))


