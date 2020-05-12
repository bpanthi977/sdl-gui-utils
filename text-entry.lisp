(in-package #:sdl-gui-utils)

;;
;;; Text Entry
;;

(defclass text-style ()
  ((font :initarg :font)
   (character-width :initarg :character-width :initform 8))
  (:documentation "Text Style"))

(defclass text-entry (widget)
  ((text :initarg :text :type 'string :initform "" :accessor text)
   (style :initarg :style :type 'text-style)
   (cursor-position :type integer)
   (editable :type boolean :initform t :initarg :editable :accessor editable)
   (clipped-text :type string
				 :documentation "The text that wiil be displayed due to width limitation ")) 
  (:documentation "Widget for single line text entry"))

(defparameter *default-text-style* (make-instance 'text-style :character-width 8))
(defgeneric calculate-clipped-text (widget))

(defmethod initialize-instance :after ((w text-entry) &key)
  "Set cursor position and display text"
  (setf (slot-value w 'cursor-position)
		(length (text w)))
  (unless (slot-boundp w 'style)
	(setf (slot-value w 'style) *default-text-style*))
  (setf (slot-value w 'clipped-text)
		(calculate-clipped-text w)))

(defmethod calculate-clipped-text ((w text-entry))
  "Find the string to display in text box"
  (with-slots (text style width cursor-position) w
	(let* ((l (length text))
		   (cwidth (slot-value style 'character-width))
		   (permitted (floor width cwidth))
		   (p/2 (floor permitted 2)))
	  (if (> l permitted)
		  (if (> cursor-position p/2)
			  (let ((e (min l (+ cursor-position p/2))))
				(subseq text (- e permitted) e))
			  (let ((s (max 0 (- cursor-position p/2))))
				(subseq text s (+ s permitted))))
		  text))))

;; Callable Commands

(defmethod draw ((w text-entry) &key)
  "Draw Text Entry"
  (with-slots (x y width height clipped-text cursor-position style editable) w
	(sdl:draw-rectangle-* x y width height
						  :color (if editable sdl:*blue* sdl:*blue*))
	(sdl:draw-string-solid-* clipped-text x y)))

;; External Inputs

(defmethod key-down ((w text-entry) &key char)
  "Handle Keypress for a Text Entry
char = (code-char unicode)
remember to (sdl:enable-unicode)"
  (with-slots (text clipped-text cursor-position editable) w
	(when editable 
	  (cond ((eql char #\Backspace)
			 (let ((l (length text)))
			   (when (> l 0)
				 (setf text (subseq text 0 (- l 1)))

				 (decf cursor-position)
				 (setf clipped-text (calculate-clipped-text w)))))
			
			(char 
			 (setf text (concatenate 'string text (list char)))
			 (incf cursor-position)
			 (setf clipped-text (calculate-clipped-text w)))))))
