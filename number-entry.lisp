(in-package #:sdl-gui-utils)

(defclass number-entry (text-entry)
  ((type :type (or :integer :float) :initform :float :initarg :type)
   (value :initform 0 :initarg :value :accessor value)))

(defmethod initialize-instance :after ((w number-entry) &key)
  (setf (slot-value w 'text)
	(format nil "~a" (or (slot-value w 'value) ""))
	(slot-value w 'clipped-text) (calculate-clipped-text w)))


;; Exteranl Commands
;; (draw number-entry)

;; External Input 

(defmethod key-down :after ((w number-entry) &key)
  (with-slots (text type value clipped-text) w
    (case type
      (:integer
       (multiple-value-bind (v p) (parse-integer text :junk-allowed t)
	 (setf value (or v 0)
	       text (subseq text 0 p))))
      (:float
       (multiple-value-bind (v p) (parse-float:parse-float (print text) :junk-allowed t)
	 (setf value (or v 0.0)
	       text (subseq text 0 p)))))
    (setf clipped-text (calculate-clipped-text w))))

(defmethod (setf value) (new-value (w number-entry))
  (setf (slot-value w 'value) new-value
	(slot-value w 'text) (format nil "~a" new-value)
	(slot-value w 'clipped-text) (calculate-clipped-text w)))


