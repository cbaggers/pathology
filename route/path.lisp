(in-package :pathology.route)

;;----------------------------------------------------------------------

(defclass path ()
  ((route
    :initarg :route :initform (error "path must be created with route"))
   (validator
    :initarg :validator :initform nil)
   (prefix-serializor
    :initarg :prefix-serializor
    :initform (error "path kind must be created with a prefix-serializor"))
   (prefix-deserializor
    :initarg :prefix-deserializor
    :initform (error "path kind must be created with a prefix-deserializor"))))

(defgeneric %clone (original &optional new-inner-route))
(defgeneric %clone-to-rel (original &optional new-inner-route))

(defmethod terminates-p ((route path))
  (with-slots (route) route
    (when route
      (terminates-p route))))

(defmethod relative-p ((route path))
  (with-slots (route) route
    (if route
        (relative-p route)
        t)))

(defmethod incomplete-p ((route path))
  (with-slots (route) route
    (when route
      (incomplete-p route))))

(defmethod tokens ((route path))
  (with-slots (route) route
    (when route
      (tokens route))))

(defmethod print-object ((obj path) stream)
  (format stream "#>~a>(~a ~s)"
          (string-downcase (type-of obj))
	  (if (terminates-p obj) ":file" ":dir")
          (serialize-path obj nil)))


(defun validate-token (token route)
  (with-slots (validator) route
    (%validate-token token validator)))

(defun %validate-token (token validator)
  (if (typep token 'incomplete-token)
      (validate-incomplete-token token validator)
      (funcall validator (copy-seq token))))

(defun validate-incomplete-token (token validator)
  (every (lambda (x) (%validate-token x validator))
	 (remove-if-not #'stringp (parts token))))

(defmethod push-token ((route path) token &optional terminates-p)
  (with-slots ((inner route)) route
    (unless (validate-token token route)
      (error "Invalid token ~s pushed" token))
    (let* ((new-route (push-token inner token terminates-p)))
      (%clone route new-route))))

(defmethod pop-token ((route path))
  (with-slots ((inner route)) route
    (when inner
      (multiple-value-bind (new-route token) (pop-token inner)
        (unless (and (null new-route) (relative-p route))
          (values
           (%clone route new-route)
           token))))))

(defmethod join-routes ((first path) (second path) &rest rest)
  (let ((new-route (apply #'join-routes
                          (slot-value first 'route)
                          (slot-value second 'route)
                          rest)))
    (%clone first new-route)))

(defmethod split-route ((route path) (at integer))
  (with-slots ((inner route)) route
    (destructuring-bind (l r) (split-route inner at)
      (list (%clone route l)
            (%clone-to-rel route r)))))

(defun serialize-path (route &optional stream)
  (let* ((tokens (reverse (tokens route)))
         (seperator (token-seperator route))
         (template (format nil "~~a~~{~~a~~^~c~~}~~@[~c~~]"
                           seperator seperator)))
    (format stream template
            (if (relative-p route) "" (serialize-prefix route))
            (mapcar (lambda (token)
                      (if (typep token 'incomplete-token)
                          (serialize-incomplete-token token nil)
                          token))
                    tokens)
            (not (terminates-p route)))))


(defun %deserialize-tokens (string escape seperator wild-chars)
  (assert (stringp string))
  (assert (functionp escape))
  (assert (characterp seperator))
  (let (results pending)
    (labels ((wild-p (c)
               (member c wild-chars))
             (ugly-str ()
               (make-array 0 :element-type 'character
                           :fill-pointer 0 :adjustable t))
             (new ()
               (when pending
                 (push (reverse pending) results))
               (setf pending (list (ugly-str))))
             (add (c)
               (vector-push-extend c (first pending)))
             (wild (c)
               (push c pending)
               (push (ugly-str) pending)))
      (new)
      (with-input-from-string (s string)
        (loop :for c := (read-char s nil) :while c :do
           (cond
             ((funcall escape c) (add c) (add (read-char s)))
             ((char= c seperator) (new))
             ((wild-p c) (wild c))
             (t (add c)))))
      (when pending
        (push pending results))
      (nreverse (mapcar (lambda (token)
                          (let ((l (length token)))
                            (cond ((= 0 l) (error "Token Bug:~%~s~%~s"
                                                  results pending))
                                  ((= 1 l) (first token))
                                  (t (make-instance 'incomplete-token
                                                    :parts token)))))
                        results)))))

(defun %deserialize-path (kind string as seperator escape wild-chars)
  (let ((escape (cond ((functionp escape) escape)
                      ((characterp escape) (lambda (c) (char= c escape)))
                      (t (lambda (c) (declare (ignore c)) nil)))))
    (destructuring-bind (relative string &rest key-vals)
        (multiple-value-list (deserialize-prefix string as))
      ;;
      (let ((result
             (when (not (equal string ""))
               (%deserialize-tokens string escape seperator wild-chars))))
        (when (and (eq kind :file) (equal (car (last result)) ""))
          (error "The ~s path for a file cannot end in ~c"
                 (string-downcase (symbol-name as)) seperator))
        ;;
        (values result
                relative
                key-vals)))))

(defun serialize-prefix (route)
  (with-slots (prefix-serializor) route
    (funcall prefix-serializor route)))

;;----------------------------------------------------------------------

(defun func-arg-p (x &key (nullable t))
  (or (when nullable (null x))
      (and (listp x)
           (= (length x) 2)
           (eq 'function (first x))
           (symbolp (second x)))))

(defmacro def-path-kind (name seperator escape wild-chars
                            validator prefix-serializor prefix-deserializor
                            &body additional-fields)
  (destructuring-bind (name &key constructor) (if (listp name) name (list name))
    (assert (symbolp name))
    (assert (symbolp constructor))
    (assert (every #'symbolp additional-fields))
    (assert (characterp seperator))
    (assert (stringp wild-chars))
    (assert (or (null escape)
                (characterp escape)
                (func-arg-p escape)))
    (assert (func-arg-p validator))
    (assert (func-arg-p prefix-serializor :nullable nil))
    (assert (func-arg-p prefix-deserializor :nullable nil))
    (let ((constructor (or constructor name))
          (validator (or validator (lambda (x) x)))
	  (fields (mapcar (lambda (f)
			    (destructuring-bind (name initform)
				(if (listp f) f (list f))
			      (list name initform
				    (intern (symbol-name name) :keyword))))
			  additional-fields)))
      `(progn
         (defclass ,name (path)
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

         (defmethod ,constructor (kind path-string)
           (assert (find kind '(:file :dir)))
           (assert (stringp path-string))
           (multiple-value-bind (tokens relative key-vals)
               (%deserialize-path kind path-string ',name ,seperator ,escape
                                   ',(map 'list #'identity wild-chars))
             (declare (ignorable key-vals))
             (let ((terminated (eq kind :file)))
               (,@(if fields
                      `(destructuring-bind ,(mapcar (lambda (x) (subseq x 0 2))
                                                    fields)
                           key-vals)
                      `(progn))
                  (loop :for token :in tokens :do
                     (%validate-token token ,validator))
                  (let ((route (route* relative terminated tokens)))
                    (make-instance
                     ',name :route route :validator ,validator
                     :prefix-serializor ,prefix-serializor
                     :prefix-deserializor ,prefix-deserializor))))))

         (defmethod deserialize-path ((as (eql ',name)) kind string)
           (declare (ignore as))
           (,constructor kind string))

         (defmethod deserialize-prefix (route (as (eql ',name)))
           (funcall ,prefix-deserializor route))

         (defmethod pathology.route::%make-path
             (path-form (path-kind-name (eql ',name)))
           (apply #',constructor path-form ))

         (defmethod make-load-form ((path ,name) &optional environment)
           (declare (ignore environment))
           (list ',constructor (if (terminates-p path) :file :dir)
                 (serialize-path path)))

         (defmethod initialize-instance :after
           ((path ,name) &key ,@(mapcar #'first fields))
           (when (relative-p path)
             (when (or ,@(mapcar #'first fields))
               (error "No special fields allowed in relative paths"))))))))

;;----------------------------------------------------------------------
