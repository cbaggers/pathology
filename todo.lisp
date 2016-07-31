;; - make the reader macro (uses >this> to dispatch to deserializer)
;; - only allow flavor special fields on the absolute paths
;; - add escaping to the unix serialize
;; - add unix token validator (escape chars etc etc)
;; - establish how incomplete tokens are created
;; - if escaping changes and making incomplete-tokens is the business
;;   of the 'validator' then it needs a better name
;; - add ntfs spec
