(in-package :pathology.route)

;;----------------------------------------------------------------------

(defmethod probe-path ((path path))
  (probe-file (serialize-path path)))

(defmethod path-write-date ((path path))
  (assert (terminates-p path))
  (let ((pf (serialize-path path)))
    (assert (probe-file pf) ()
            "Pathology: Cannot get write date of file that doesnt exist:~%~s"
            path)
    (file-write-date pf)))

;;----------------------------------------------------------------------
