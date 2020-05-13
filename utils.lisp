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
