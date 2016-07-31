;;;; trail.asd

(asdf:defsystem #:trail
  :description "Provides a route type which is a series of utf8 tokens"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :components ((:file "trail/package")
               (:file "trail/route")
	       (:file "trail/flavor")))
