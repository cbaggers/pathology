(in-package :trail)

(defclass route-flavor ()
  ((route :initarg :route)
   (token-validator :initform #'identity :initarg :token-validator)))

(defmacro def-route-flavor (name &body additional-fields)
  (destructuring-bind (name &key constructor) (if (listp name) name (list name))
    (assert (symbolp name))
    (assert (symbolp constructor))
    (assert (every #'symbolp additional-fields))
    (let ((constructor (or constructor name))
	  (fields (mapcar (lambda (f)
			    (destructuring-bind (name initform)
				(if (listp f) f (list f))
			      (list name initform
				    (intern (symbol-name name) :keyword))))
			  additional-fields)))
      (labels ((emit-new (new-route-val old-data-val)
		 (assert (and (symbolp new-route-val)
			      (symbolp old-data-val)))
		 `(make-instance
		   ',name
		   :route ,new-route-val
		   ,@(loop :for (name initform arg) :in fields :collect
			`(,arg (,name ,old-data-val))))))
	`(progn
	   (defclass ,name (route-flavor)
	     ,(loop :for (name initform arg) :in fields :collect
		 `(,name :initform ,initform
			 :initarg ,(intern (symbol-name name) :keyword)
			 :reader ,name)))

	   (defmethod ,constructor (relative? &rest tokens)
	     (make-instance ',name :route (apply #'route relative? tokens)))

	   (defmethod print-object ((obj ,name) stream)
	     (format stream ,(format nil "#>~a>~~s" (string-downcase name))
		     (serialize-route obj)))

	   (defmethod push-token ((route ,name) token &optional terminates)
	     (with-slots ((inner route) token-validator) route
	       (let* ((token (funcall token-validator token))
		      (new-route (push-token inner token terminates)))
		 ,(emit-new 'new-route 'route))))

	   (defmethod pop-token ((route ,name))
	     (with-slots ((inner route) token-validator) route
	       (multiple-value-bind (new-route token) (pop-token inner)
		 (values
		  ,(emit-new 'new-route 'route)
		  token))))

	   (defmethod join-routes ((first ,name) (second ,name) &rest rest)
	     (let ((new-route (apply #'join-routes
				     (slot-value first 'route)
				     (slot-value second 'route)
				     rest)))
	       ,(emit-new 'new-route 'route)))

	   (defmethod split-route ((route ,name) (at integer))
	     (destructuring-bind (l r) (split-route route at)
	       (list ,(emit-new 'l 'route)
		     ,(emit-new 'r 'route))))

	   (defmethod terminates ((route ,name))
	     (terminates (slot-value route 'route)))

	   (defmethod is-relative ((route ,name))
	     (is-relative (slot-value route 'route)))

	   (defmethod incomplete-p ((route ,name))
	     (incomplete-p (slot-value route 'route)))

	   (defmethod tokens ((route ,name))
	     (tokens (slot-value route 'route))))))))
