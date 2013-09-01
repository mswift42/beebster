;;;; beebster.asd

(asdf:defsystem #:beebster
  :serial t
  :description "Describe beebster here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:cl-who
               #:cl-ppcre
	       #:drakma
	       #:cxml-stp
	       #:closure-html
	       #:parenscript
	       #:inferior-shell
	       #:split-sequence
	       #:fiveam)
  :components ((:file "package")
               (:file "beebster")
	       (:file "tests")))

