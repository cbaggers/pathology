
;; Note on validator
;;
;; We are going to keep the name validator, along with some details
;; - validator takes a token
;; - validator can return t or nil and optionals a failure reason for the nil
;;   case using #'values
;; - validator should NOT escape chars, that is the job of the serializer
;;   It must return t if the token CAN be serialized into a valid path component
;; - serializer takes a single token
;; - deserialize takes a whole string and returns tokens
;; - deserialize should remove escaping
;; - by having serialize work on a single token, we can cache it in a smarter
;;   way. The user also doesnt have to worry about escaping in their tokens.
;;   They can use spaces, odd chars, whatever, if it passes the validator it's
;;   kosher

;; Shit to do
;; - Add #'route* which lets you specify terminates by value
;; - Add support for the :up token
;; - only allow flavor special fields on the absolute paths
;; - Add incomplete-token class
;; - Fix valid token checks to allow incomplete-token
;; - Add function to make incomplete-tokens

;; - rename the platform specific routes to paths? (work out and do it)
;; - call the validator in the correct funcs (do this in route?)
;; - Use validator :before to only pass copy of token to method so can't
;;   do anything but validate
;; -
;; - Modify path constructors to use (:file "") and (:dir "") syntax for
;;   defining whether they terminate or not

;; - add unix token validator (escape chars etc etc)

;; - add escaping to the unix serialize

;; - make the reader macro (uses >this> to dispatch to deserializer)

;; - add ntfs spec

;; - swap route token store from list to extendable vector
;; - generally go on a performance sweep
