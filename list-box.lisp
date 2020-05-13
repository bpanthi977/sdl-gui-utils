(in-package #:sdl-gui-utils)

(defclass listbox (text-entry)
  ((entries :type (or null vector)
			:initarg :entries :initform nil)
   (filter :type (or null (function string))
		   :initarg :filter :initform nil)))

;; External Input 

(defmethod key-down :after ((w listbox) &key)
  (with-slots (text entries filter selected-range clipped-text cursor-position) w
	(let ((found (cond (entries
						(find-if (lambda (e)
								   (alexandria:starts-with-subseq text e))
								 entries))
					   (filter
						(funcall filter text))
					   (t (error "Either entires or filter must be non-nil")))))
	  (when (and found
				 (not (string= found ""))
				 (not (string= found text)))
		(setf selected-range (vector cursor-position (length found))
			  text found
			  clipped-text (calculate-clipped-text w))))))
		  
