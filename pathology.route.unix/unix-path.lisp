(in-package #:pathology.route.unix)

(def-route-flavor unix-path "/")

(defmethod serialize-route ((route unix-path) &optional stream)
  (format stream "~a~{~a~^/~}~@[/~]"
	  (if (relative-p route) "" "/")
	  (reverse (tokens route))
	  (not (terminates route))))

(defmethod deserialize-route (kind string (as (eql 'unix-path)))
  (error "Pathology: Can only deserialize strings to unix-paths~%Received:~s"
	 string))

(defmethod deserialize-route (kind (string string) (as (eql 'unix-path)))
  (let* ((split (uiop:split-string string :separator '(#\/)))
	 (first-raw (first split))
	 (absolute (and (stringp first-raw) (uiop:emptyp first-raw)))
	 (tokens (remove-if #'uiop:emptyp split)))
    (values tokens (not absolute) (eq kind :file))))

(defmethod validate-token ((token string) (flavor (eql 'unix-path)))
  (null (find #\/ token)))
