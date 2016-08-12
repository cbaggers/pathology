(in-package #:pathology.route)

;;----------------------------------------------------------------------

(defclass route ()
  ((tokens :initform nil :initarg :tokens :reader tokens)
   (relative-p :initform nil :initarg :relative-p :reader relative-p)
   (terminates :initform nil :initarg :terminates :reader terminates)
   (incomplete-token-count :initform nil :initarg :incomplete-token-count
                           :reader incomplete-token-count)))

(defmethod print-object ((route route) stream)
  (let* ((tokens (reverse (tokens route)))
         (tokens (if (terminates route)
                     (append (butlast tokens)
                             (cons :> (last tokens)))
                     tokens)))
    (format stream "#>>~s" tokens)))


(defmethod incomplete-p ((route route))
  (> (incomplete-token-count route) 0))

;;----------------------------------------------------------------------


;; these can only be expanded whilst route is being actualized which,
;; naturally, can only happen on the relevant system

(defclass incomplete-token ()
  ((token :initform nil :initarg :token :reader token)))

(defmethod incomplete-token-p ((token incomplete-token))
  t)

(defmethod incomplete-token-p (token)
  nil)

;;----------------------------------------------------------------------


(defun terminate-markerp (x)
  (and (symbolp x) (string-equal x ">")))

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


(defmethod route (relative? &rest tokens)
  (assert (every (lambda (x) (or (typep x 'token)
                                 (terminate-markerp x)))
                 tokens))
  (let* ((terminates? (member-if #'terminate-markerp tokens))
         (tokens (remove-if #'terminate-markerp tokens)))
    (route* relative? terminates? tokens)))

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
       :terminates (not (null terminated?))
       :incomplete-token-count tc))))

(defmethod push-token ((route route) token &optional terminates?)
  (assert (typep token 'token))
  (assert (not (terminates route)))
  (make-instance
   'route
   :tokens (resolve-up-tokens (relative-p route) (cons token (tokens route)))
   :terminates terminates?
   :relative-p (relative-p route)
   :incomplete-token-count (if (typep token 'incomplete-token)
                               (1+ (incomplete-token-count route))
                               (incomplete-token-count route))))

(defmethod pop-token ((route route))
  (let ((focus (first (tokens route)))
        (tc (incomplete-token-count route))
        (tokens (cdr (tokens route))))
    (when tokens
      (values
       (make-instance
        'route
        :tokens tokens
        :terminates nil
        :relative-p (relative-p route)
        :incomplete-token-count (if (typep focus 'incomplete-token)
                                    (1- tc)
                                    tc))
       focus))))

(defmethod pop-token ((route null))
  nil)

;;----------------------------------------------------------------------

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

(defmethod %join-routes ((first route) (second route))
  (assert (not (terminates first)))
  (assert (relative-p second))
  (make-instance
   'route
   :tokens (append (tokens second) (tokens first))
   :terminates (terminates second)
   :relative-p (relative-p first)
   :incomplete-token-count (+ (incomplete-token-count first)
                              (incomplete-token-count second))))

;;----------------------------------------------------------------------

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
        :terminates nil
        :relative-p (relative-p route)
        :incomplete-token-count (count-if #'incomplete-token-p left-tokens)))
     (when right-tokens
       (make-instance
        'route
        :tokens right-tokens
        :terminates (terminates route)
        :relative-p t
        :incomplete-token-count (count-if #'incomplete-token-p right-tokens))))))

;;----------------------------------------------------------------------

(defmethod serialize-route ((route route) &optional stream)
  (declare (ignore stream))
  (error "Basic route types have no serializable form"))

(defmethod deserialize-route (kind string (as (eql 'route)))
  (error "Basic route types have no serializable form and as such you cannot
deserialize this string to one.
:string ~s
:route-type route"
         string))
