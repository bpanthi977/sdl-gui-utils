(in-package #:sdl-gui-utils)
;;
;;; TODO: Remove all activating, and deactivating mechanisms
;;

(defclass events-handler ()
  ((active-object :initarg :active-object :initform nil
		  :documentation "Currently active object that will recieve events")
   (command-sequence :initarg :command-sequence :initform nil
		     :documentation "List of commnads that will be executed turn by turn")
   (hovered :initform nil
	    :documentation "Objects that were hovered in last mouse motion")
   (activable :initarg :activable :initform nil
	      :documentation "Objects that can be clicked or made active otherwise")
   (drawable :initform nil
	     :documentation "Objects that can be drawn")
   (hoverable :initform nil
	      :documentation "Objects that can be hovered over")))

(defmethod deactivate-active ((w events-handler) active-object)
  "Active object has to be passed because in code when :deactivate return values are used the command-list might have been 
modified before the :deactivate is processed"
  (print "deactivating")
  (print active-object)
  (with-slots (active-object command-sequence) w
    (setf command-sequence (remove active-object command-sequence)
	  active-object (first command-sequence))
    ))

(defmethod push-interface ((evh events-handler) i &key (drawable? t) (activable? t) (hoverable? t))
  (with-slots (active-object command-sequence activable drawable hoverable) evh
    (if drawable?
	(push i drawable))
    (when activable?
      (setf active-object i)
      (push i command-sequence)
      (push i activable))
    (if hoverable?
	(push i hoverable))
    i))

(defmethod delete-interface ((w events-handler) i)
  (with-slots (active-object drawable activable hoverable command-sequence) w
    (if (eql active-object i)
	(deactivate-active w i)
	(setf command-sequence (remove i command-sequence)))
    (setf drawable (remove i drawable))
    (setf hoverable (remove i hoverable))
    (setf activable (remove i activable))))

(defmethod draw ((w events-handler) &key)
  (loop for o in (slot-value w 'drawable) do
       (draw o)))

(defmethod mouse-motion ((w events-handler) mouse)
  "Send the mouse movement to the active object, but if it passes check for hover"
  (with-slots (active-object hoverable hovered) w
    (case (if active-object
	      (mouse-motion active-object mouse)
	      :pass)
      (:deactivate (deactivate-active w active-object))
      (:pass (loop for c in hoverable
		with new-hovered = nil do
		  (when (mouse-over-p c mouse)
		    (hover c mouse)
		    (push c new-hovered))
		finally (progn
			  (loop for c in (set-difference hovered new-hovered) do
			       (unhover c mouse))
			  (setf hovered new-hovered)))))))

(defmethod mouse-button-down ((w events-handler) button mouse)
  "Let active object handle the button down, but it passes check for activation"
  (with-slots (active-object activable) w
    (case (if active-object
	      (mouse-button-down active-object button mouse)
	      :pass)
      (:deactivate
       (deactivate-active w active-object)
       (loop for c in activable do
	    (if (mouse-over-p c mouse)
		(when (eql :activate (mouse-button-down c button mouse))
		  (setf active-object c)
		  (return)))))
      (:pass 
       (loop for c in activable do
	    (if (mouse-over-p c mouse)
		(when (eql :activate (mouse-button-down c button mouse))
		  (setf active-object c)
		  (return))))))))

(defmethod mouse-button-up ((w events-handler) button mouse)
  (with-slots (active-object activable) w
    (if active-object
	(if (eql :deactivate (mouse-button-up active-object button mouse))
	    (deactivate-active w active-object)))))

(defmethod key-down ((w events-handler) &key key char)
  (with-slots (active-object command-sequence) w
    (print (list active-object command-sequence))
    (if active-object
	(when (eql :deactivate (key-down active-object :key key :char char))
	  (deactivate-active w active-object)))))


;; (defmethod click ((w events-handler) mouse)
;;   (with-slots (active-object activable) w
;; 	(if active-object
;; 		(click active-object mouse)
;; (loop for c in activable do
;;   (if (mouse-over-p c mouse)
;; 	  (click c mouse))))))

;; (defmethod unclick ((w events-hander) mouse)
;;   (with-slots (active-object) w
;; 	(if active-object
;; 		(unclick active-object mouse ))))

;; (defmethod scroll ((w events-hander) sf mouse)
;;   (with-slots (active-object) w
;; 	(if active-object
;; 		(scroll active-object sf mouse))))
