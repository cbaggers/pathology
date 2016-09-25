(in-package #:pathology.windows)
(named-readtables:in-readtable :pathology.reader)

;;------------------------------------------------------------

(def-path-kind (win :constructor win-path) #\\ nil ""
    #'validate-win-token
    #'serialize-win-prefix
    #'deserialize-win-prefix
  (drive "C"))

(defun validate-win-token (token)
  (null (find #\\ token)))

(defun serialize-win-prefix (path)
  (format nil "~c:\\" (drive path)))

(defun deserialize-win-prefix (string)
  (if (equal (subseq string 1 3) ":\\")
      (values nil (subseq string 3) :drive (aref string 0))
      (values t string)))

;;------------------------------------------------------------
