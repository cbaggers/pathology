;;;; package.lisp

(defpackage #:pathology.route
  (:use #:cl #:named-readtables)
  (:export :route
           :route*
           :tokens
	   :terminates-p
           :relative-p
           :push-token
           :pop-token
           :join-routes
           :split-route
           :incomplete-p
	   :incomplete
           :incomplete-token
	   :parts
           :def-route-flavor
           :serialize-route
           :deserialize-route
	   :validate-token))
