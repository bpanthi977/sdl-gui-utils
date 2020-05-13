(in-package :sdl-gui-utils)

(defclass command-box (listbox)
  ((commands-map :type hash-table :initarg :commands-map)))

(defun fillEntries (command-box)
  (with-slots (entries commands-map) command-box
	(let ((e (make-array (hash-table-size commands-map)
						 :element-type 'string
						 :initial-element ""
						 :fill-pointer 0)))
	  (loop for k being the hash-key of commands-map do
		(vector-push k e))
	  (setf entries e))))

(defmethod initialize-instance :after ((w command-box) &key)
  (fillentries w))

;; External Input

(defmethod key-down :before (w &key key)
  (if (eql key :sdl-key-return)
	  (let ((f (gethash  (slot-value w 'text) (slot-value w 'commands-map))))
		(if f
			(funcall f)))))
	  
