(in-package #:sdl-gui-utils)

;;
;;; Text Entry
;;

(defclass text-style ()
  ((font :initarg :font)
   (character-width :initarg :character-width :initform 8))
  (:documentation "Text Style"))

(defclass text-entry (widget)
  ((text :initarg :text :type string :initform "" :accessor text)
   (style :initarg :style :type 'text-style)
   (cursor-position :type integer)
   (editable :type boolean :initform t :initarg :editable :accessor editable)
   (clipped-text :type string
		 :documentation "The text that wiil be displayed due to width limitation ")
   (selected-range :type (or t null (vector integer 2)) :initarg :selected-range :initform nil))
  (:documentation "Widget for single line text entry"))

(defparameter *default-text-style* (make-instance 'text-style :character-width 8))
(defgeneric calculate-clipped-text (widget))

(defmethod initialize-instance :after ((w text-entry) &key)
  "Set cursor position and display text"
  ;; put cursor at end of text
  (setf (slot-value w 'cursor-position)
	(length (text w)))
  ;; set default style 
  (unless (slot-boundp w 'style)
    (setf (slot-value w 'style) *default-text-style*))
  (setf (slot-value w 'clipped-text)
	(calculate-clipped-text w))
  (unless (slot-boundp w 'selected-range)
    (setf (slot-value w 'selected-range) (vector 0 (length (slot-value w 'text))))))

(defmethod calculate-clipped-text%start-end ((w text-entry))
  (with-slots (text style width cursor-position) w
    (let* ((l (length text))
	   (cwidth (slot-value style 'character-width))
	   (permitted (floor width cwidth))
	   (p/2 (floor permitted 2)))
      (if (> l permitted)
	  (if (> cursor-position p/2)
	      (let ((e (min l (+ cursor-position p/2))))
		(values (- e permitted) e))
	      (let ((s (max 0 (- cursor-position p/2))))
		(values s (+ s permitted))))
	  (values 0 l)))))

(defmethod calculate-clipped-text ((w text-entry))
  "Find the string to display in text box"
  (multiple-value-bind (start end) (calculate-clipped-text%start-end w)
    (subseq (slot-value w 'text) start end)))

;; Callable Commands

(defmethod draw ((w text-entry) &key)
  "Draw Text Entry"
  (with-slots (x y width height clipped-text cursor-position style editable selected-range) w
    ;; fill a rectangle below selected text
    (if selected-range
	(let (s wid (cw (slot-value (slot-value w 'style) 'character-width)))
	  (cond 
	    ((eql selected-range t)
	     (setf s x wid width))
	    (t (multiple-value-bind (start end) (calculate-clipped-text%start-end w)
		 (setf s (max start (aref selected-range 0))
		       wid (- (min end (aref selected-range 1)) s)
		       s (+ x (* cw (- s start)))
		       wid (+ x (* cw wid))))))
	  (sdl:draw-filled-polygon (list (sdl:point :x s :y y)
					 (sdl:point :x (+ s wid) :y y)
					 (sdl:point :x (+ s wid) :y (+ y height))
					 (sdl:point :x s :y (+ y height)))
				   :color sdl:*blue*)))
    ;; draw the text box
    (sdl:draw-rectangle-* x y width height
			  :color (if editable sdl:*blue* sdl:*blue*))
    ;; write the text
    (sdl:draw-string-solid-* clipped-text x y)))

(defmethod (setf text) (new (w text-entry))
  (with-slots (text selected-range clipped-text cursor-position) w
    (setf text new
	  selected-range nil
	  cursor-position (length new)
	  clipped-text (calculate-clipped-text w))))

(defmethod mouse-button-down ((w text-entry) button mouse)
  :activate)

(defmethod mouse-button-up ((w text-entry) button mouse)
  t)

(defmethod mouse-motion ((w text-entry) mouse)
  :pass)

(defmethod hover ((w text-entry) mouse)
  nil)

(defmethod unhover ((w text-entry) mouse)
  nil)

;; External Inputs

(defmethod key-down ((w text-entry) &key key char)
  "Handle Keypress for a Text Entry
char = (code-char unicode)
remember to (sdl:enable-unicode)"
  (with-slots (text clipped-text cursor-position editable selected-range) w
    (when editable
      (if selected-range
	  ;; if some text is selected, replace that text
	  (progn  
	    (cond
	      ((eql selected-range t)
	       (if (or (eql key :sdl-key-delete) (eql char #\Backspace))
		   (setf text ""
			 clipped-text ""
			 cursor-position 0)
		   (setf text (string char)
			 cursor-position 1
			 clipped-text (calculate-clipped-text w))))					
	      ((eql char #\Backspace)
	       (setf text (concatenate 'string
				       ;; if cursor is at begining of selection, then delete the preceeding character too
				       (subseq text 0 (if (= cursor-position (aref selected-range 0))
							  (max 0 (1- (aref selected-range 0)))
							  (aref selected-range 0)))
				       (subseq text (aref selected-range 1)))
		     cursor-position (if (= cursor-position (aref selected-range 0))
					 (max 0 (1- (aref selected-range 0)))
					 (aref selected-range 0))
		     clipped-text (calculate-clipped-text w)))
	      ((eql key :sdl-key-delete)
	       (setf text (concatenate 'string
				       (subseq text 0 (aref selected-range 0))
				       ;; if cursor is at end delete the following character too
				       (subseq text (if (= cursor-position (aref selected-range 1))
							(min (length text) (1+ (aref selected-range 0)))
							(aref selected-range 1))))
		     cursor-position (aref selected-range 0)
		     clipped-text (calculate-clipped-text w)))
	      
	      (char
	       (setf text (concatenate 'string
				       (subseq text 0 (aref selected-range 0))
				       (unless (eql char #\Nul) (string char))
				       (subseq text (aref selected-range 1)))
		     cursor-position (1+ (aref selected-range 0))
		     clipped-text (calculate-clipped-text w))))
	    (setf selected-range nil))
	  ;; else either insert a character or delete a character  
	  (cond ((eql char #\Backspace)
		 (let ((l (length text)))
		   (when (> l 0)
		     (setf text (subseq text 0 (- l 1)))

		     (decf cursor-position)
		     (setf clipped-text (calculate-clipped-text w)))))				
		((not (eql char #\Nul))
		 (setf text (concatenate 'string text (list char)))
		 (incf cursor-position)
		 (setf clipped-text (calculate-clipped-text w))))))))
