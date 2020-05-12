;;;; sdl-gui-utils.lisp

(in-package #:sdl-gui-utils)

(defclass widget ()
  ((x :initarg :x :type 'integer :accessor x)
   (y :initarg :y :type 'integer :accessor y)
   (width :initarg :width :type 'single-float :accessor width :initform 50)
   (height :initarg :height :type 'single-float :accessor height :initform 20))
  (:documentation "A Widget (Things that are visible are widgets)")
  (:default-initargs :x 0 :y 0))
  
