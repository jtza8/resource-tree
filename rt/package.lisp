; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage #:resource-tree
  (:use #:cl #:cl-fad)
  (:nicknames #:rt)
  (:export #:free
           #:free-node
           #:invalid-node
           #:invalid-path
           #:load-path
           #:node
           #:node-of
           #:path-keyword
           #:remove-node
           #:resource-tree
           #:valid-path
           #:with-nodes
           #:with-resource-tree))

(in-package :resource-tree)
(defparameter *nothing* (gensym "nothing-"))