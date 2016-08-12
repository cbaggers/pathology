(in-package :pathology.route)

;;----------------------------------------------------------------------

(defclass route-flavor ()
  ((route :initarg :route)))

(defmethod terminates ((route route-flavor))
  (with-slots (route) route
    (when route
      (terminates route))))

(defmethod relative-p ((route route-flavor))
  (with-slots (route) route
    (if route
        (relative-p route)
        t)))

(defmethod incomplete-p ((route route-flavor))
  (with-slots (route) route
    (when route
      (relative-p route))))

(defmethod tokens ((route route-flavor))
  (with-slots (route) route
    (when route
      (tokens route))))

(defmethod print-object ((obj route-flavor) stream)
  (format stream "#>~a>~s"
          (string-downcase (type-of obj))
          (serialize-route obj)))

(defgeneric validate-token (token flavor))

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

           (defmethod ,constructor (kind &optional (path-string ,default-path))
	     (assert (find kind '(:file :dir)))
	     (multiple-value-bind (tokens relative terminated key-vals)
		 (deserialize-route kind path-string ',name)
	       (declare (ignorable key-vals))
	       (,@(if fields
		      `(destructuring-bind ,(mapcar (lambda (x) (subseq x 0 2))
						    fields)
			   key-vals)
		      `(progn))
		 (let ((route (route* relative terminated tokens)))
		   (make-instance ',name :route route)))))

	   (defmethod initialize-instance :after
	     ((path ,name) &key ,@(mapcar #'first fields))
	     (when (relative-p path)
	       (when (or ,@(mapcar #'first fields))
		 (error "No special fields allowed in relative paths"))))

           (defmethod push-token ((route ,name) token &optional terminates)
             (with-slots ((inner route)) route
               (let* ((token (validate-token token ',name))
                      (new-route (push-token inner token terminates)))
                 ,(emit-new 'new-route 'route))))

           (defmethod pop-token ((route ,name))
             (with-slots ((inner route)) route
               (when inner
                 (multiple-value-bind (new-route token) (pop-token inner)
		   (unless (and (null new-route) (relative-p route))
		     (values
		      ,(emit-new 'new-route 'route)
		      token))))))

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
