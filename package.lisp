;;;; package.lisp

(defpackage #:beebster
  (:use #:cl :cl-ppcre :cl-who :hunchentoot  :cxml-stp :inferior-shell
	:split-sequence :fiveam )
  (:shadow :! :run))

