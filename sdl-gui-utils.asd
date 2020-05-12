;;;; sdl-gui-utils.asd

(asdf:defsystem #:sdl-gui-utils
  :description "Describe sdl-gui-utils here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl #:parse-float) ;; parse-float is for number-entry
  :components ((:file "package")
               (:file "sdl-gui-utils")
			   (:file "dragndropnscale")
			   (:file "text-entry")
			   (:file "number-entry")))
