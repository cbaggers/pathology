;;;; package.lisp

(defpackage #:pathology
  (:use #:cl #:pathology.route #:pathology.route.unix)
  (:export :serialize-route
	   :deserialize-route
	   :is-relative
	   :incomplete-p
	   :push-token
	   :pop-token
	   :join-routes
	   :split-route))
