;;;; package.lisp

(uiop:define-package #:pathology.route
    (:use #:cl #:named-readtables)
  (:export :route
           :relative
           :absolute
           :relative*
           :absolute*
           :absolute-p
           :relative-p
           :make-relative
           :terminates-p
           :incomplete-p
           :parts
           :tokens
           :push-token
           :pop-token
           :join-routes
           :split-route
           :subseq-route
           :def-path-kind
           :serialize-path
           :deserialize-path
           :route-from-pathname
           :promote-route-to
           :path-syntax
           :from-pathname
           ;; extras
           :probe-path
           :path-write-date))
