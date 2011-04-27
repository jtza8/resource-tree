(asdf:defsystem "resource-tree"
  :description "A file resource management system."
  :version "0.1.3"
  :author "Jens Thiede"
  :license "BSD-Style License"
  :depends-on ("cl-fad")
  :components ((:file "package")
               (:file "resource-tree" :depends-on ("package"))))
