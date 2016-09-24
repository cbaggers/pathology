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

(defmethod %serialize-route ((route route-flavor) &optional stream escape)
  (let* ((tokens (reverse (tokens route)))
         (seperator (token-seperator route))
         (template (format nil "~~a~~{~~a~~^~c~~}~~@[~c~~]"
                           seperator seperator)))
    (format stream template
            (if (relative-p route) "" (serialize-prefix route))
            (mapcar (lambda (x)
                      (if (typep x 'incomplete-token)
                          (serialize-incomplete-token x stream escape)
                          (serialize-token x stream escape (type-of route))))
                    tokens)
            (not (terminates-p route)))))


(defun %deserialize-tokens (string escape seperator)
  (assert (stringp string))
  (assert (functionp escape))
  (assert (characterp seperator))
  (let (results)
    (labels ((new () (push (make-array 0 :element-type 'character
                                       :fill-pointer 0 :adjustable t)
                           results))
             (add (c) (vector-push-extend c (first results))))
      (new)
      (with-input-from-string (s string)
        (loop :for c := (read-char s nil) :while c :do
           (cond
             ((funcall escape c) (add c) (add (read-char s)))
             ((char= c seperator) (new))
             (t (add c)))))
      (nreverse results))))

(defun %deserialize-route (kind string as seperator escape wild-chars)
  (let ((escape (cond ((functionp escape) escape)
                      ((characterp escape) (lambda (c) (char= c escape)))
                      (t (lambda (c) (declare (ignore c)) nil)))))
    (destructuring-bind (relative string &rest key-vals)
        (multiple-value-list (deserialize-prefix string as))
      ;;
      (let ((result (when (not (uiop:emptyp string))
                      (%deserialize-tokens string escape seperator))))
        (when (and (eq kind :file) (equal (car (last result)) ""))
          (error "The ~s path for a file cannot end in ~c"
                 (string-downcase (symbol-name as)) seperator))
        ;;
        (labels ((wild-p (c) (member c wild-chars)))
          (values (mapcar (lambda (x)
                            (let ((token (deserialize-token x as)))
                              (if (some #'wild-p token)
                                  (incomplete token wild-chars)
                                  token)))
                          result)
                  relative
                  key-vals))))))

(defmethod deserialize-prefix (string as)
  (error "No 'deserialize-prefix' method found for path type '~a'.
All path types must have this method implemented"
         as))

(defmethod serialize-prefix ((route route-flavor))
  (error "No 'serialize-prefix' method found for path type '~a'.
All path types must have this method implemented"
         (type-of route)))

;;----------------------------------------------------------------------

(defmacro def-route-flavor (name default-path seperator escape wild-chars
                            &body additional-fields)
  (destructuring-bind (name &key constructor) (if (listp name) name (list name))
    (assert (symbolp name))
    (assert (symbolp constructor))
    (assert (every #'symbolp additional-fields))
    (assert (stringp default-path))
    (assert (characterp seperator))
    (assert (stringp wild-chars))
    (assert (or (null escape)
                (characterp escape)
                (and (listp escape)
                     (= (length escape) 2)
                     (eq 'function (first escape))
                     (symbolp (second escape)))))
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

         (defmethod ,constructor (kind &optional (path-string ,default-path)
                                         (escape ,escape))
           (assert (find kind '(:file :dir)))
           (assert (stringp path-string))
           (assert (or (null escape) (functionp escape) (characterp escape)))
           (multiple-value-bind (tokens relative key-vals)
               (%deserialize-route kind path-string ',name ,seperator ,escape
                                   ',(map 'list #'identity wild-chars))
             (declare (ignorable key-vals))
             (let ((terminated (eq kind :file)))
               (,@(if fields
                      `(destructuring-bind ,(mapcar (lambda (x) (subseq x 0 2))
                                                    fields)
                           key-vals)
                      `(progn))
                  (loop :for token :in tokens :do (validate-token token ',name))
                  (let ((route (route* relative terminated tokens)))
                    (make-instance ',name :route route))))))

         (defmethod deserialize-token (string (as (eql ',name)))
           (declare (ignore as))
           string)

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
