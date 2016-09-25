;;;; package.lisp

(defpackage #:pathology
  (:use #:cl #:pathology.route #:pathology.posix)
  (:export :relative
           :absolute
           :absolute-p
           :relative-p
           :terminates-p
           :incomplete-p
           :parts
           :tokens
           :push-token
           :pop-token
           :join-routes
           :split-route
           :def-path-kind
           :serialize-path
           :deserialize-path
           :posix))
