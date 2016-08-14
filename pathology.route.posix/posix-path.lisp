(in-package #:pathology.route.posix)
(named-readtables:in-readtable pathology.route::path-syntax)

(def-route-flavor posix-path "/")

(defmethod serialize-route ((route posix-path) &optional stream)
  (format stream "~a~{~a~^/~}~@[/~]"
	  (if (relative-p route) "" "/")
	  (mapcar #'posix-escape (reverse (tokens route)))
	  (not (terminates route))))

(defun posix-escape (token)
  token)

(defmethod deserialize-route (kind string (as (eql 'posix-path)))
  (error "Pathology: Can only deserialize strings to posix-paths~%Received:~s"
	 string))

(defmethod deserialize-route (kind (string string) (as (eql 'posix-path)))
  (let* ((split (uiop:split-string string :separator '(#\/)))
	 (first-raw (first split))
	 (absolute (and (stringp first-raw) (uiop:emptyp first-raw)))
	 (tokens (remove-if #'uiop:emptyp split)))
    (values tokens (not absolute) (eq kind :file))))

(defmethod validate-token ((token string) (flavor (eql 'posix-path)))
  (null (find #\/ token)))

;; (defun meh ()
;;   (print #>posix-path>(:file "/hi/there")))
