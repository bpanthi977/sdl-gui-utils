(in-package :sdl-gui-utils)
;;
;;; Utils for Working with sdl:point 
;;


(defun p+ (p1 p2)
  (sdl:point :x (+ (sdl:x p1)
		   (sdl:x p2))
	     :y (+ (sdl:y p1)
		   (sdl:y p2))))

(defun p- (p1 p2)
  (sdl:point :x (- (sdl:x p1)
		   (sdl:x p2))
	     :y (- (sdl:y p1)
		   (sdl:y p2))))

(defun pincf (p1 p2)
  (incf (sdl:x p1) (sdl:x p2))
  (incf (sdl:y p1) (sdl:y p2))
  p1)

(defun p* (s p)
  (sdl:point :x (* s (sdl:x p))
	     :y (* s (sdl:y p))))

(defun sin-cos (p)
  "Sin(\theta) and Cos(\theta) of a vector"
  (let* ((x (sdl:x p))
	 (y (sdl:y p))
	 (l (sqrt (+ (expt x 2) (expt y 2)))))
    (values (/ y l) (/ x l))))

(defun p-dot-p (p)
  (+ (expt (sdl:x p) 2)
     (expt (sdl:y p) 2)))

(defun pdot (p1 p2)
  (+ (* (sdl:x p1) (sdl:x p2))
     (* (sdl:y p1) (sdl:y p2))))

(defun pdist2 (p1 p2)
  (+ (expt (- (sdl:x p1) (sdl:x p2)) 2)
     (expt (- (sdl:y p1) (sdl:y p2)) 2)))

(defun in-bounding-box (p tl br)
  (and (<= (sdl:x tl) (sdl:x p) (sdl:x br))
       (<= (sdl:y tl) (sdl:y p) (sdl:y br))))
