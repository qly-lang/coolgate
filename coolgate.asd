;;;; coolgate.asd

(asdf:defsystem #:coolgate
  :description "Describe coolgate here"
  :author "Bo Yao <icerove@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:named-readtables
               #:trivial-escapes)
  :components ((:file "package")
               (:module "cpu"
                :serial t
                :components
                ((:file "arm64")))
               (:file "coolgate")))
