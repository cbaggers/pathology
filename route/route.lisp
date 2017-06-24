(in-package #:pathology.route)

;;----------------------------------------------------------------------

(defclass route ()
  ((tokens :initform nil :initarg :tokens :reader tokens)
   (relative-p :initform nil :initarg :relative-p :reader relative-p)
   (terminates-p :initform nil :initarg :terminates-p :reader terminates-p)
   (incomplete-token-count :initform nil :initarg :incomplete-token-count
                           :reader incomplete-token-count)))

(defmethod print-object ((route route) stream)
  (let* ((tokens (reverse (tokens route)))
         (name (if (relative-p route) "relative" "absolute"))
         (head (if (terminates-p route) ":leaf" ":branch")))
    (format stream "#>~a>(~a ~{~s~^ ~})" name head tokens)))

(defgeneric incomplete-p (route))

(defmethod incomplete-p ((route route))
  (> (incomplete-token-count route) 0))

(defgeneric absolute-p (route))

(defmethod absolute-p ((route route))
  (not (relative-p route)))

;;----------------------------------------------------------------------

(defgeneric route (obj))

(defmethod route ((obj route))
  obj)

;; these can only be expanded whilst route is being actualized which,
;; naturally, can only happen on the relevant system

(defclass incomplete-token ()
  ((parts :initform nil :initarg :parts :reader parts)))

(defmethod print-object ((itok incomplete-token) stream)
  (format stream "#<ITOKEN (~{~s~^ ~})>" (parts itok)))

(defgeneric incomplete-token-p (token))

(defmethod incomplete-token-p ((token incomplete-token))
  t)

(defmethod incomplete-token-p (token)
  nil)

(defun serialize-incomplete-token (token stream)
  (format stream "~{~a~}" (mapcar #'string (parts token))))

(defmethod make-instance :after ((i-tok incomplete-token) &key)
  (assert (every (lambda (x) (or (stringp x) (keywordp x)))
                 (parts i-tok))))

(defun intersperse (list with)
  (let ((list list))
    (loop :for (elem . rest) := list
       :collect elem
       :when rest :collect with
       :while rest
       :do (setf list (cdr list)))))

;;----------------------------------------------------------------------


(defun can-be-encoded-by-utf8 (token)
  (declare (ignore token))
  t)

(deftype token ()
  `(or incomplete-token
       (satisfies up-token-p)
       (and string (satisfies can-be-encoded-by-utf8))))

;;----------------------------------------------------------------------

(defun up-token-p (token)
  (eq token :up))

(defun resolve-up-tokens (relative reverse-token-list)
  (let* (up
         (new (loop :for token :in reverse-token-list
                 :for up? := (up-token-p token)
                 :if (or up up?)
                 :do (if up? (push token up) (pop up))
                 :else
                 :collect token)))
    (unless relative
      (when up
        (error ":up token found at start of absolute path")))
    (append new up)))

;;----------------------------------------------------------------------

(defun relative (kind &rest tokens)
  (assert (find kind '(:leaf :file :branch :dir)))
  (route* t (or (eq kind :leaf) (eq kind :file)) tokens))

(defun absolute (kind &rest tokens)
  (assert (find kind '(:leaf :file :branch :dir)))
  (route* nil (or (eq kind :leaf) (eq kind :file)) tokens))

(defun relative* (kind tokens)
  (assert (find kind '(:leaf :file :branch :dir)))
  (route* t (or (eq kind :leaf) (eq kind :file)) tokens))

(defun absolute* (kind tokens)
  (assert (find kind '(:leaf :file :branch :dir)))
  (route* nil (or (eq kind :leaf) (eq kind :file)) tokens))

(defgeneric make-relative (route))

(defmethod make-relative ((route route))
  (if (relative-p route)
      route
      (route* t (terminates-p route) (reverse (tokens route)))))

(defgeneric route* (relative? terminated? tokens))

(defmethod route* (relative? terminated? tokens)
  (assert (every (lambda (x) (typep x 'token))
                 tokens))
  (let* ((tc (count-if (lambda (x) (typep x 'incomplete-token)) tokens))
         (tokens (resolve-up-tokens relative? (reverse tokens))))
    (when (and terminated? (= (length tokens) 0))
      (error "invalid termination"))
    (when tokens
      (make-instance
       'route
       :tokens tokens
       :relative-p (not (null relative?))
       :terminates-p (not (null terminated?))
       :incomplete-token-count tc))))

(defgeneric push-token (route token &optional terminates-p?))

(defmethod push-token ((route route) token &optional terminates-p?)
  (assert (typep token 'token))
  (assert (not (terminates-p route)))
  (make-instance
   'route
   :tokens (resolve-up-tokens (relative-p route) (cons token (tokens route)))
   :terminates-p terminates-p?
   :relative-p (relative-p route)
   :incomplete-token-count (if (typep token 'incomplete-token)
                               (1+ (incomplete-token-count route))
                               (incomplete-token-count route))))

(defgeneric pop-token (route))

(defmethod pop-token ((route route))
  (let ((focus (first (tokens route)))
        (tc (incomplete-token-count route))
        (tokens (cdr (tokens route))))
    (when tokens
      (values
       (make-instance
        'route
        :tokens tokens
        :terminates-p nil
        :relative-p (relative-p route)
        :incomplete-token-count (if (typep focus 'incomplete-token)
                                    (1- tc)
                                    tc))
       focus))))

(defmethod pop-token ((route null))
  nil)

;;----------------------------------------------------------------------

(defgeneric join-routes (first second &rest rest))

(defmethod join-routes ((first route) (second route) &rest rest)
  (if rest
      (reduce #'%join-routes (cons second rest) :initial-value first)
      (%join-routes first second)))

(defmethod join-routes ((first route) (second null) &rest rest)
  (if rest
      (apply #'join-routes first (first rest) (rest rest))
      first))

(defmethod join-routes ((first null) (second route) &rest rest)
  (if rest
      (apply #'join-routes second (first rest) (rest rest))
      second))

(defmethod join-routes ((first null) (second null) &rest rest)
  (when rest
    (apply #'join-routes (first rest) (second rest) (cddr rest))))


(defgeneric %join-routes (first second))

(defmethod %join-routes ((first route) (second route))
  (assert (not (terminates-p first)))
  (assert (relative-p second))
  (make-instance
   'route
   :tokens (append (tokens second) (tokens first))
   :terminates-p (terminates-p second)
   :relative-p (relative-p first)
   :incomplete-token-count (+ (incomplete-token-count first)
                              (incomplete-token-count second))))

;;----------------------------------------------------------------------
(defgeneric split-route (route at))
(defmethod split-route ((route route) (at integer))
  (assert (>= at 0))
  (let* ((tokens (tokens route))
         (at (- (length tokens) at))
         (left-tokens (subseq tokens at))
         (right-tokens (subseq tokens 0 at)))
    (list
     (when left-tokens
       (make-instance
        'route
        :tokens left-tokens
        :terminates-p nil
        :relative-p (relative-p route)
        :incomplete-token-count (count-if #'incomplete-token-p left-tokens)))
     (when right-tokens
       (make-instance
        'route
        :tokens right-tokens
        :terminates-p (terminates-p route)
        :relative-p t
        :incomplete-token-count (count-if #'incomplete-token-p right-tokens))))))

;;----------------------------------------------------------------------

(defgeneric subseq-route (route start &optional end))

(defmethod subseq-route (route start &optional end)
  (assert (integerp start))
  (assert (or (and (integerp end) (>= end start))
              (null end)))
  (let ((len (length (tokens route))))
    (assert (and (<= start len) (or (null end) (<= end len))) nil
            "Pathology: subseq-route out of bounds~%start:~s~%end:~s~%route:~s~%length:~s"
            start end route len)
    (cond
      ((equal start end) nil)
      ((and (= start 0) (null end)) route)
      ((= start 0) (first (split-route route end)))
      ((null end) (second (split-route route start)))
      (t (first (split-route (second (split-route route start)) (- end start)))))))

;;----------------------------------------------------------------------

(defgeneric route-from-pathname (pathname)
  (:method (pathname)
    (let* ((path (uiop:ensure-pathname pathname))
           (absolute (uiop:absolute-pathname-p path))
           (is-file (uiop:file-pathname-p path))
           (kind (if is-file :file :dir))
           (name (if (pathname-type path)
                     (format nil "~a.~a"
                             (pathname-name path)
                             (pathname-type path))
                     (pathname-name path)))
           (tokens (append (rest (pathname-directory path))
                           (when name (list name)))))
      (if absolute
          (absolute* kind tokens)
          (relative* kind tokens)))))
