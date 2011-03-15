(asdf:defsystem "resource-tree-tests"
  :description "A file-system resource management system."
  :version "0.1"
  :author "Jens Thiede"
  :license "BSD-Style License"
  :depends-on ("resource-tree" "xlunit")
  :components ((:file "package")
               (:file "resource-tree-test" :depends-on ("package"))))