;;;; package.lisp

(defpackage #:pathology
  (:use #:cl #:pathology.route #:pathology.posix)
  (:export :relative-p
	   :incomplete-p
	   :push-token
	   :pop-token
	   :join-routes
	   :split-route))
