(in-package #:pathology.route)

(defgeneric %make-path (path-form path-kind-name))

(defun path-syntax-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((path-kind-string
          (string-upcase
           (concatenate 'string
                        (loop :for c := (read-char stream) :until (char= c #\>)
                           :collect c))))
         (path-form (read stream t nil nil)))
    (destructuring-bind (name &optional package)
        (reverse (uiop:split-string path-kind-string :separator '(#\:)))
      (let ((name (string-upcase name))
            (package (when package (string-upcase package))))
        (cond
          ((equal name "RELATIVE") (apply #'relative path-form))
          ((equal name "ABSOLUTE") (apply #'absolute path-form))
          (t (let ((path-kind-name (intern name (or package *package*))))
               (%make-path path-form path-kind-name))))))))

(defreadtable :pathology.reader
  (:merge :standard)
  (:dispatch-macro-char #\# #\> #'path-syntax-reader))
