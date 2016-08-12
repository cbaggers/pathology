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


;; Shit to do
;; - change #'terminates to #'terminates-p
;; - this #>unix-path>(:file "/hi/there/") is too forgiving
;; - rename the platform specific routes to paths? (work out and do it)
;; - call the validator in the correct funcs (do this in route?)

;; - add escaping to the unix serialize
;;   This one is a problem as escaping is specific to the shell

;; - add ntfs spec

;; - swap route token store from list to extendable vector
;; - generally go on a performance sweep

;; - path types are global...ehh
