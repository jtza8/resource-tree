(asdf:defsystem "resource-tree"
  :description "A file-system resource management system."
  :version "0.1"
  :author "Jens Thiede"
  :license "BSD-Style License"
  :depends-on ("cl-fad")
  :components ((:file "package")
               (:file "tree")))