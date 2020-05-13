;;;; sdl-gui-utils.lisp

(in-package #:sdl-gui-utils)

(defclass widget ()
  ((x :initarg :x :type 'integer :accessor x :initform (sdl:mouse-x))
   (y :initarg :y :type 'integer :accessor y :initform (sdl:mouse-y))
   (width :initarg :width :type 'single-float :accessor width :initform 50)
   (height :initarg :height :type 'single-float :accessor height :initform 20))
  (:documentation "A Widget (Things that are visible are widgets)")
  (:default-initargs :x 0 :y 0))


;;
;;; EXAMPLE
;;

(defparameter *translation* #C(0 0))
(defparameter *scale* 1)

(defun example ()
  (setf *translation* (sdl:point :x 0 :y 0)
		*scale* 1)
  (let* ((dds (make-instance 'dragndropnscale
							 :scale *scale* :translation *translation* :origin (sdl:point :x 0 :y 0)
							 :callback (lambda (tr s)
										 (setf *translation* tr
											   *scale* s))))
		 
		 scale-widget
		 command-widget)
	(declare (ignorable scale-widget))
	;; Initialize sdl 
	(sdl:with-init ()
	  (sdl:window 1000 1000)
	  (sdl:enable-unicode)
	  (sdl:initialise-default-font)
	  (setf sdl:*default-color* sdl:*black*)
	  (sdl:with-events ()
		(:quit-event () t)
		(:key-down-event (:key key :unicode code)
						 (case key
						   (:sdl-key-q (sdl:push-quit-event))
						   (t (if (not command-widget)
								  (setf command-widget
										(make-instance 'command-box 
													   :commands-map (alexandria:alist-hash-table
																	  (list (cons "scale"  (lambda ()
																							  (print "skelse")))
																			(cons "fun"  (lambda ()
																							(print "i am funny")))
																			(cons "fu"  (lambda ()
																						  (print "fubuki"))))
																	  :test #'equal)
													   :x 50
													   :y 40)))
							(key-down command-widget :key key :char (code-char code)))))
						   ;; (t (if (not scale-widget)
						   ;; 		  (setf scale-widget (make-instance 'number-entry
						   ;; 											:value *scale*
						   ;; 											:x (sdl:mouse-x)
						   ;; 											:y (sdl:mouse-y)
						   ;; 											:selected-range t))																	
						   ;; 		  (progn
						   ;; 			(let ((char (code-char code)))
						   ;; 			  (if (or (char= char #\Return) (char= char #\Esc))
						   ;; 				  (progn (setf *scale* (value scale-widget))
						   ;; 						 (setf scale-widget nil))
						   ;; 				  (key-down scale-widget :char char))))))))
		(:mouse-motion-event
		 (:x x :y y)
		 (mouse-motion dds x y))
		(:mouse-button-down-event
		 (:button button :x x :y y)
		 (mouse-button-down dds button x y))
		(:mouse-button-up-event
		 (:button button :x x :y y)
		 (mouse-button-up dds button x y))
		(:idle ()
			   (sdl:clear-display sdl:*white*)
			   ;; Draw sth
			   (sdl:draw-line *translation*
							  (p+ *translation* (p* *scale* (sdl:point :x 100 :y 100)))
							  :color sdl:*blue* :aa t)
			   ;; widgets
			   ;; (if scale-widget
			   ;; 	   (draw scale-widget))
			   (if command-widget
			   	   (draw command-widget))
			   (sdl:update-display))))))

