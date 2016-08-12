;;;; package.lisp

(defpackage #:pathology
  (:use #:cl #:pathology.route #:pathology.route.unix)
  (:export :serialize-route
	   :deserialize-route
	   :relative-p
	   :incomplete-p
	   :push-token
	   :pop-token
	   :join-routes
	   :split-route))
