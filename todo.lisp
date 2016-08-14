;; Note on deserializer
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


;; stuff to do
;; - incomplete-token should contain a list of parts
;;   each part can be a string or keyword. We don't care what keywords they use.
;;   the idea is that the strings hold the constant parts of the token and the
;;   keywords indicate the kind of wildcard/special-char
;; - change #'terminates to #'terminates-p
;; - this #>posix-path>(:file "/hi/there/") is too forgiving
;; - rename the platform specific routes to paths? (work out and do it)
;; - call the validator in the correct funcs (do this in route?)

;; - add escaping to the posix serialize
;;   This one is a problem as escaping is specific to the shell
;;   Ah I think I was confused. Escaping is different for each shell, however
;;   pathnames/filenames are specified by the posix standard including the
;;   wildcard pattern stuff, so this should be fine.
;;   http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_266
;;   http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_13_02


;; - add ntfs spec

;; - swap route token store from list to extendable vector
;; - generally go on a performance sweep

;; - path types are global...ehh
