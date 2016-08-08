(in-package #:pathology.route)

;;----------------------------------------------------------------------

(defclass route ()
  ((tokens :initform nil :initarg :tokens :reader tokens)
   (is-relative :initform nil :initarg :is-relative :reader is-relative)
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
       (and string (satisfies can-be-encoded-by-utf8))))

;;----------------------------------------------------------------------


(defmethod route (relative? &rest tokens)
  (assert (every (lambda (x) (or (typep x 'token)
				 (terminate-markerp x)))
		 tokens))
  (let* ((tc (count-if (lambda (x) (typep x 'incomplete-token))
		       tokens))
	 (terminate (member-if #'terminate-markerp tokens))
	 (tokens (remove-if #'terminate-markerp (reverse tokens))))
    (unless (or (null terminate) (= (length terminate) 2))
      (error "invalid termination"))
    (when tokens
      (make-instance
       'route
       :tokens tokens
       :is-relative (not (null relative?))
       :terminates (not (null terminate))
       :incomplete-token-count tc))))

(defmethod push-token ((route route) token &optional terminates?)
  (assert (typep token 'token))
  (assert (not (terminates route)))
  (make-instance
   'route
   :tokens (cons token (tokens route))
   :terminates terminates?
   :is-relative (is-relative route)
   :incomplete-token-count (if (typep token 'incomplete-token)
			       (1+ (incomplete-token-count route))
			       (incomplete-token-count route))))

(defmethod pop-token ((route route))
  (assert (not (terminates route)))
  (let ((focus (first (tokens route)))
	(tc (incomplete-token-count route))
	(tokens (cdr (tokens route))))
    (when tokens
      (values
       (make-instance
	'route
	:tokens tokens
	:terminates nil
	:is-relative (is-relative route)
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
  (assert (is-relative second))
  (make-instance
   'route
   :tokens (append (tokens second) (tokens first))
   :terminates (terminates second)
   :is-relative (is-relative first)
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
	:is-relative (is-relative route)
	:incomplete-token-count (count-if #'incomplete-token-p left-tokens)))
     (when right-tokens
       (make-instance
	'route
	:tokens right-tokens
	:terminates (terminates route)
	:is-relative t
	:incomplete-token-count (count-if #'incomplete-token-p right-tokens))))))

;;----------------------------------------------------------------------

(defmethod serialize-route ((route route) &optional stream)
  (declare (ignore stream))
  (error "Basic route types have no serializable form"))

(defmethod deserialize-route (string (as (eql 'route)))
  (error "Basic route types have no serializable form and as such you cannot
deserialize this string to one.
:string ~s
:route-type route"
	 string))
