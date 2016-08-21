(in-package :pathology.route)

;;----------------------------------------------------------------------

(defclass route-flavor ()
  ((route :initarg :route)))

(defgeneric %clone (original &optional new-inner-route))
(defgeneric %clone-to-rel (original &optional new-inner-route))

(defmethod terminates-p ((route route-flavor))
  (with-slots (route) route
    (when route
      (terminates-p route))))

(defmethod relative-p ((route route-flavor))
  (with-slots (route) route
    (if route
        (relative-p route)
        t)))

(defmethod incomplete-p ((route route-flavor))
  (with-slots (route) route
    (when route
      (incomplete-p route))))

(defmethod tokens ((route route-flavor))
  (with-slots (route) route
    (when route
      (tokens route))))

(defmethod print-object ((obj route-flavor) stream)
  (format stream "#>~a>(~a ~s)"
          (string-downcase (type-of obj))
	  (if (terminates-p obj) ":file" ":dir")
          (serialize-route obj nil nil)))

(defgeneric validate-token (token flavor))

(defmethod validate-token :around ((token string) flavor)
  (call-next-method (copy-seq token) flavor))

(defmethod push-token ((route route-flavor) token &optional terminates-p)
  (with-slots ((inner route)) route
    (unless (validate-token token (type-of route))
      (error "Invalid token ~s pushed" token))
    (let* ((new-route (push-token inner token terminates-p)))
      (%clone route new-route))))

(defmethod pop-token ((route route-flavor))
  (with-slots ((inner route)) route
    (when inner
      (multiple-value-bind (new-route token) (pop-token inner)
        (unless (and (null new-route) (relative-p route))
          (values
           (%clone route new-route)
           token))))))

(defmethod join-routes ((first route-flavor) (second route-flavor) &rest rest)
  (let ((new-route (apply #'join-routes
                          (slot-value first 'route)
                          (slot-value second 'route)
                          rest)))
    (%clone first new-route)))

(defmethod split-route ((route route-flavor) (at integer))
  (with-slots ((inner route)) route
    (destructuring-bind (l r) (split-route inner at)
      (list (%clone route l)
            (%clone-to-rel route r)))))

(defmethod serialize-route ((route route-flavor) &optional stream escape)
  (let* ((tokens (reverse (tokens route)))
         (seperator (token-seperator route))
         (template (format nil "~~a~~a~~{~~a~~^~c~~}~~@[~c~~]"
                           seperator seperator)))
    (format stream template
            (if (terminates-p route)
                (serialize-path-prefix route)
                "")
            (if (relative-p route) "" (string seperator))
            (mapcar (lambda (x)
                      (if (typep x 'incomplete-token)
                          (serialize-incomplete-token x stream escape)
                          (serialize-token x stream escape (type-of route))))
                    tokens)
            (not (terminates-p route)))))

(defmethod serialize-path-prefix ((route route-flavor))
  (declare (ignore route))
  "")

;;----------------------------------------------------------------------

(defmacro def-route-flavor (name default-path seperator &body additional-fields)
  (destructuring-bind (name &key constructor) (if (listp name) name (list name))
    (assert (symbolp name))
    (assert (symbolp constructor))
    (assert (every #'symbolp additional-fields))
    (assert (stringp default-path))
    (assert (characterp seperator))
    (let ((constructor (or constructor name))
	  (name-kwd (intern (symbol-name name) :keyword))
	  (fields (mapcar (lambda (f)
			    (destructuring-bind (name initform)
				(if (listp f) f (list f))
			      (list name initform
				    (intern (symbol-name name) :keyword))))
			  additional-fields)))
      `(progn
         (defclass ,name (route-flavor)
           ,(cons
             `(seperator :initform ,seperator :reader token-seperator)
             (loop :for (name initform arg) :in fields :collect
                `(,name :initform ,initform
                        :initarg ,(intern (symbol-name name) :keyword)
                        :reader ,name))))

         (defmethod %clone ((route ,name) &optional new-inner-route)
           (make-instance
            ',name
            :route new-inner-route
            ,@(loop :for (name initform arg) :in fields :collect
                 `(,arg (,name route)))))

         (defmethod %clone-to-rel ((route ,name) &optional new-inner-route)
           (make-instance ',name :route new-inner-route))

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
                (loop :for token :in tokens :do (validate-token token ',name))
                (let ((route (route* relative terminated tokens)))
                  (make-instance ',name :route route)))))

         (defmethod pathology.route::%make-path
             (path-form (path-kind-name (eql ,name-kwd)))
           (apply #',constructor path-form ))

         (defmethod make-load-form ((path ,name) &optional environment)
           (declare (ignore environment))
           (list ',constructor (if (terminates-p path) :file :dir)
                 (serialize-route path)))

         (defmethod initialize-instance :after
           ((path ,name) &key ,@(mapcar #'first fields))
           (when (relative-p path)
             (when (or ,@(mapcar #'first fields))
               (error "No special fields allowed in relative paths"))))))))

;;----------------------------------------------------------------------
