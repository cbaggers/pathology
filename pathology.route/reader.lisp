(in-package #:pathology.route)

(defgeneric %make-path (path-form path-kind-name))

(defun path-syntax-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))

  (let* ((path-kind-string
	  (concatenate 'string
		       (loop :for c := (read-char stream) :until (char= c #\>)
			  :collect c)))
	 (path-kind-name (intern (string-upcase path-kind-string) :keyword)))
    (let ((path-form (read stream t nil nil)))
      (%make-path path-form path-kind-name))))

(defreadtable path-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\> #'path-syntax-reader))
