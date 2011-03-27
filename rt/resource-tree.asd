(asdf:defsystem "resource-tree"
  :description "A file resource management system."
  :version "1.0.1"
  :author "Jens Thiede"
  :license "BSD-Style License"
  :depends-on ("cl-fad")
  :components ((:file "package")
               (:file "resource-tree" :depends-on ("package"))))
