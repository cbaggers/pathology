(in-package #:pathology.route)

(defgeneric %make-path (path-form path-kind-name))

(defun path-syntax-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))

  (let* ((path-kind-string
          (string-upcase
           (concatenate 'string
                        (loop :for c := (read-char stream) :until (char= c #\>)
                           :collect c)))))
    (destructuring-bind (name &optional package)
        (reverse (uiop:split-string path-kind-string :separator '(#\:)))
      (let* ((path-kind-name (intern name (or package *package*)))
             (path-form (read stream t nil nil)))
        (%make-path path-form path-kind-name)))))

(defreadtable path-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\> #'path-syntax-reader))
