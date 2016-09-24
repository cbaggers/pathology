(in-package #:pathology.route.posix)
(named-readtables:in-readtable pathology.route::path-syntax)

;;------------------------------------------------------------

(def-route-flavor posix-path "/" #\/ nil "[]*?")

;;------------------------------------------------------------

(defmethod deserialize-prefix (string (as (eql 'posix-path)))
  (let ((absolute (char= #\/ (aref string 0))))
    (values (not absolute) (subseq string (if absolute 1 0)))))

(defmethod deserialize-token (string (as (eql 'posix-path)))
  (remove #\\ string :test #'char=))

;;------------------------------------------------------------

(defmethod validate-token ((token string) (flavor (eql 'posix-path)))
  (null (find #\/ token)))

(defmethod validate-token ((token incomplete-token) (flavor (eql 'posix-path)))
  (every (lambda (x) (validate-token x flavor))
	 (remove-if-not #'stringp (parts token))))

;;------------------------------------------------------------

(defmethod serialize-prefix ((path posix-path))
  "/")

;;------------------------------------------------------------

;; (defun meh ()
;;   (print #>posix-path>(:file "/hi/there")))
