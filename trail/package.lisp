;;;; package.lisp

(defpackage #:trail
  (:use #:cl)
  (:export :route
	   :tokens
	   :is-relative
	   :push-token
	   :pop-token
	   :join-routes
	   :split-route
	   :incomplete-p
	   :incomplete-token
	   :def-route-flavor
	   :serialize-route
	   :deserialize-route))
