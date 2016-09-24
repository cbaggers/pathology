;;;; package.lisp

(defpackage #:pathology
  (:use #:cl #:pathology.route #:pathology.route.posix)
  (:export :deserialize-prefix
           :deserialize-token
           :relative-p
	   :incomplete-p
	   :push-token
	   :pop-token
	   :join-routes
	   :split-route))
