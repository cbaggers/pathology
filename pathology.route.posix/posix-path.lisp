(in-package #:pathology.route.posix)
(named-readtables:in-readtable pathology.route::path-syntax)

(def-route-flavor posix-path "/")

(defmethod serialize-route ((route posix-path) &optional stream (escape t))
  (let ((tokens (reverse (tokens route))))
    (format stream "~a~{~a~^/~}~@[/~]"
	    (if (relative-p route) "" "/")
	    (if escape
		(mapcar #'posix-escape tokens)
		tokens)
	    (not (terminates-p route)))))

(defun posix-escape (token)
  (let ((unsafe " []*?"))
    (labels ((fix (c) (if (find c unsafe) (format nil "\\~a" c) c)))
      (format nil "~{~a~}" (map 'list #'fix token)))))

(defmethod deserialize-route (kind string (as (eql 'posix-path)))
  (error "Pathology: Can only deserialize strings to posix-paths~%Received:~s"
	 string))

(defmethod deserialize-route (kind (string string) (as (eql 'posix-path)))
  (let* ((split (uiop:split-string string :separator '(#\/)))
	 (first-raw (first split))
	 (absolute (and (stringp first-raw) (uiop:emptyp first-raw)))
	 (tokens (remove-if #'uiop:emptyp split)))
    (when (and (eq kind :file) (char= (aref string (1- (length string))) #\/))
      (error "The posix path for a file cannot end in /"))
    (values tokens (not absolute) (eq kind :file))))

(defmethod validate-token ((token string) (flavor (eql 'posix-path)))
  (null (find #\/ token)))

;; (defun meh ()
;;   (print #>posix-path>(:file "/hi/there")))
