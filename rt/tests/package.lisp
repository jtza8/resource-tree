; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :resource-tree)

(use-package :xlunit)

(defparameter *test-tree-path* 
  (asdf:system-relative-pathname :resource-tree-tests "test-tree/"))
