(in-package #:trail.unix)

(def-route-flavor unix-path)

(defmethod serialize-route ((route unix-path) &optional stream)
  (format stream "~a~{~a~^/~}"
	  (if (is-relative route) "" "/")
	  (reverse (tokens route))))
