;; stuff to do
;;
;; - rename the platform specific routes to paths? (work out and do it)
;;
;; - maybe take control of deserialize-route and just have deserialize-prefix
;;   and deserialize-token?
;;
;; - add ntfs spec
;;
;; - work out if swapping route token store from list to extendable vector
;;   would make sense
;;
;; - go on a performance sweep
;;
;; - path types are global...ehh


;;----------------------------------------------------------------------

;; Notes on escaping
;;   This one is a problem as escaping is specific to the shell
;;   Ah I think I was confused. Escaping is different for each shell, however
;;   pathnames/filenames are specified by the posix standard including the
;;   wildcard pattern stuff, so this should be fine.
;;   http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_266
;;   http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_13_02
;;   ^^^^ SCRATCH MOST OF THIS :p ^^^^^
;;   Escaping is too shell specific so we will take a token escaping function as
;;   an arg to serialize. We will provide a default escaping func for each path
;;   kind though.
;;   .. Doesnt this mean that we also need unescaping func for deserialize too?


;; Note on deserializer
;; - make different classes for absolute and relative paths, use the provided
;;   path name for the base class
;; - update any function that calls make-instance on some kind of path
;; - deserialize takes a whole string and returns tokens
;; - deserialize should remove escaping
;; - deserialize returns the following values:
;;   - a list of tokens
;;   - relative flag
;;   - terminated flag
;;   - key pair args for the special fields
;; - The results of deserialize are this way so that higher level functions
;;   can be in change of some universal logic (like how relative paths
;;   can't have special fields


;; Note on validator
;;
;; We are going to keep the name validator, along with some details
;; - validator takes a token
;; - validator can return t or nil and optionals a failure reason for the
;;   nil case using #'values
;; - validator should NOT escape chars, that is the job of the serializer
;;   It must return t if the token CAN be serialized into a valid path
;;   component
;; - serializer takes a single token
;; - by having serialize work on a single token, we can cache it in a
;;   smarter way. The user also doesnt have to worry about escaping in
;;   their tokens.
;;   They can use spaces, odd chars, whatever, if it passes the validator
;;   it's kosher
