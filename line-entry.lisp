(in-package :sdl-gui-utils)

;;;; LINE ENTRY 

(defclass line-entry ()
  ((line :type line)
   (click-state :type int :initform 0 :documentation "0 = no point, 1 = 1 point selected, 2 = 2 points selected")
   (callback :type function :initarg :callback)))

;;
;;; External Inputs
;;

(defmethod draw ((w line-entry) &key)
  (let ((mouse (sdl:point :x (sdl:mouse-x)
			  :y (sdl:mouse-y)))
	(hl (sdl:point :x 5 :y 0))
	(vl (sdl:point :x 0 :y 5)))
    (sdl:draw-line (p- mouse hl) (p+ mouse hl))
    (sdl:draw-line (p- mouse vl) (p+ mouse vl)))
  (unless (= 0 (slot-value w 'click-state))
    (draw (slot-value w 'line))))

(defmethod mouse-motion ((w line-entry) mouse)
  (with-slots (click-state) w
    (case click-state
      (0 nil)
      (1
       (setf (slot-value (slot-value w 'line) 'p2) mouse)
       (draw (slot-value w 'line)))
      (2 (draw (slot-value w 'line))))))

(defmethod mouse-button-down ((w line-entry) button mouse)
  (cond
    ((eql button sdl:mouse-left)
     (case (slot-value w 'click-state)
       (0 (setf (slot-value w 'line) (make-instance 'line :p1 mouse :p2 mouse)
		(slot-value w 'click-state) 1))
       (1 (setf (slot-value (slot-value w 'line) 'p2) mouse
		(slot-value w 'click-state) 2)
	  (callback w))
       (2 (print "this shouldn't have happened"))))))


(defmethod mouse-button-up ((w line-entry) button mouse)
  nil)

(defun line-entry-interface (event-handler)
  (push-interface event-handler 
		  (make-instance 'line-entry
				 :callback
				 (lambda (w)
				   (push-interface event-handler (slot-value w 'line)
						   :activable? nil :hoverable? t)
				   (delete-interface event-handler w)))
		  :activable? t
		  :hoverable? nil))


;;;; POINT ENTRY

(defclass point-entry ()
  ((point :type point)
   (callback :type function :initarg :callback)))

;;
;;; External Inputs
;;

(defmethod draw ((w point-entry) &key)
  (let ((mouse (sdl:point :x (sdl:mouse-x)
			  :y (sdl:mouse-y)))
	(hl (sdl:point :x 5 :y 0))
	(vl (sdl:point :x 0 :y 5)))
    (sdl:draw-line (p- mouse hl) (p+ mouse hl))
    (sdl:draw-line (p- mouse vl) (p+ mouse vl))))  

(defmethod mouse-button-down ((w point-entry) button mouse)
  (setf (slot-value w 'point) (make-instance 'point :p mouse))
  (callback w))

(defun point-entry-interface (event-handler)
  (push-interface event-handler 
		  (make-instance 'point-entry
				 :callback (lambda (w)
					     (push-interface event-handler (slot-value w 'point)
							     :activable? nil :hoverable? t)
					     (delete-interface event-handler w)))))

(defun point-entry-multiple-interface (event-handler)
  (push-interface event-handler
		  (make-instance 'point-entry
				 :callback (lambda (w)
					     (push-interface event-handler (slot-value w 'point)
							     :activable? nil :hoverable? t)
					     (setf (slot-value event-handler 'active-object) w)))))


;;;; POINT SELECTOR

(defclass point-selector ()
  ((points :initform nil :initarg :points)
   (lines :initform nil :initarg :lines)
   (found-point :type sdl:point :initform nil)
   (callback :initarg :callback)))

(defun find-nearest-point (w pos tolerance)
  (with-slots (points lines) w
    (let (found found-line (dist tolerance))
      (loop for point in points
	 for d = (pdist2 pos (slot-value point 'p)) do
	   (when (<  d dist)
	     (setf found point
		   dist d
		   found-line nil)))
      (loop for line in lines
	 for p1 = (slot-value line 'p1)
	 for p2 = (slot-value line 'p2)
	 for d1 = (pdist2 p1 pos)
	 for d2 = (pdist2 p2 pos) do
	   (when (< d1 dist)
	     (setf dist d1
		   found p1
		   found-line line))
	   (when (< d2 dist)
	     (setf dist d2
		   found p2
		   found-line line)))
      (cond (found-line
	     (values found found-line))
	    (found (values (slot-value found 'p) found))))))

(defmethod mouse-motion ((w point-selector) mouse)
  (multiple-value-bind (point object) (find-nearest-point w mouse 50)
    (declare (ignore object))
    (setf (slot-value w 'found-point) point)))

(defmethod draw ((w point-selector) &key)
  (with-slots (found-point) w
    (when found-point
      (sdl:draw-circle found-point 5 :color sdl:*red*))))
