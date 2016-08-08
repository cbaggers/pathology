(in-package :pathology.route)

;;----------------------------------------------------------------------

(defclass route-flavor ()
  ((route :initarg :route)
   (token-validator :initform #'identity :initarg :token-validator)))

(defmethod terminates ((route route-flavor))
  (with-slots (route) route
    (when route
      (terminates route))))

(defmethod is-relative ((route route-flavor))
  (with-slots (route) route
    (if route
	(is-relative route)
	t)))

(defmethod incomplete-p ((route route-flavor))
  (with-slots (route) route
    (when route
      (is-relative route))))

(defmethod tokens ((route route-flavor))
  (with-slots (route) route
    (when route
      (tokens route))))

(defmethod print-object ((obj route-flavor) stream)
  (format stream "#>~a>~s"
	  (string-downcase (type-of obj))
	  (serialize-route obj)))

;;----------------------------------------------------------------------

(defmacro def-route-flavor (name default-path &body additional-fields)
  (destructuring-bind (name &key constructor) (if (listp name) name (list name))
    (assert (symbolp name))
    (assert (symbolp constructor))
    (assert (every #'symbolp additional-fields))
    (assert (stringp default-path))
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
			`(,arg (,name ,old-data-val)))))
	       (emit-rel (new-route-val old-data-val)
		 (assert (and (symbolp new-route-val)
			      (symbolp old-data-val)))
		 `(make-instance ',name :route ,new-route-val)))
	`(progn
	   (defclass ,name (route-flavor)
	     ,(loop :for (name initform arg) :in fields :collect
		 `(,name :initform ,initform
			 :initarg ,(intern (symbol-name name) :keyword)
			 :reader ,name)))

	   (defmethod ,constructor (&optional (path-string ,default-path))
	     (deserialize-route path-string ',name))

	   (defmethod push-token ((route ,name) token &optional terminates)
	     (with-slots ((inner route) token-validator) route
	       (let* ((token (funcall token-validator token))
		      (new-route (push-token inner token terminates)))
		 ,(emit-new 'new-route 'route))))

	   (defmethod pop-token ((route ,name))
	     (with-slots ((inner route) token-validator) route
	       (when inner
		 (multiple-value-bind (new-route token) (pop-token inner)
		   (values
		    ,(emit-new 'new-route 'route)
		    token)))))

	   (defmethod join-routes ((first ,name) (second ,name) &rest rest)
	     (let ((new-route (apply #'join-routes
				     (slot-value first 'route)
				     (slot-value second 'route)
				     rest)))
	       ,(emit-new 'new-route 'route)))

	   (defmethod split-route ((route ,name) (at integer))
	     (destructuring-bind (l r) (split-route route at)
	       (list ,(emit-new 'l 'route)
		     ,(emit-rel 'r 'route)))))))))
