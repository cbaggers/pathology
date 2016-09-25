(in-package #:pathology.route.posix)
(named-readtables:in-readtable pathology.route::path-syntax)

;;------------------------------------------------------------

(def-route-flavor posix-path #\/ nil "[]*?"
    #'validate-posix-token
    #'serialize-posix-prefix
    #'deserialize-posix-prefix)


(defun validate-posix-token (token)
  (null (find #\/ token)))

(defun serialize-posix-prefix (path)
  (declare (ignore path))
  "/")

(defun deserialize-posix-prefix (string)
  (let ((absolute (char= #\/ (aref string 0))))
    (values (not absolute) (subseq string (if absolute 1 0)))))
