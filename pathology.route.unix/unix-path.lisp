(in-package #:pathology.route.unix)

(def-route-flavor unix-path "/")

(defmethod serialize-route ((route unix-path) &optional stream)
  (format stream "~a~{~a~^/~}"
	  (if (is-relative route) "" "/")
	  (reverse (tokens route))))

(defmethod deserialize-route (string (as (eql 'unix-path)))
  (error "Pathology: Can only deserialize strings to unix-paths~%Received:~a"
	 string))

;; (defmethod deserialize-route ((string string) (as (eql 'unix-path)))
;;   (loop :for c :across string :do
;;      ())
;;   )
