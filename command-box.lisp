(in-package :sdl-gui-utils)

(defclass command-box (listbox)
  ((commands-map :type hash-table :initarg :commands-map
		 :documentation "Hash map from string to functions"
		 :initform (make-hash-table :test #'equal))))

(defun fillEntries (command-box)
  (with-slots (entries commands-map) command-box
    ;; (let ((e (make-array (hash-table-size commands-map)
    ;; 					 :element-type 'string
    ;; 					 :initial-element ""
    ;; 					 :fill-pointer 0)))
    (let (e)
      (loop for k being the hash-key of commands-map do
	   (push k e))
      (setf entries (sort e #'< :key #'length)))))

(defmethod initialize-instance :after ((w command-box) &key)
  (fillentries w))

(defmethod add-command ((w command-box) string function)
  (with-slots (commands-map entries) w
    (unless (gethash string commands-map)
      (setf entries (merge 'list (list string) entries #'< :key #'length)))
    (setf (gethash string commands-map) function)))


;; External Input

(defmethod key-down ((w command-box) &key key)
  (if (or (eql key :sdl-key-return) (eql key :sdl-key-kp-enter))
      (let ((f (gethash  (slot-value w 'text) (slot-value w 'commands-map))))
	(when f
	  (funcall f)
	  (setf (text w) "")))
      (call-next-method)))

