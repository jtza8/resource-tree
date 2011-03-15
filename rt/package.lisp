; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage #:resource-tree
  (:use #:cl #:cl-fad)
  (:nicknames #:rt)
  (:export #:invalid-node
           #:invalid-path
           #:valid-path
           #:resource-tree
           #:node
           #:parse-keyword
           #:build-tree))

(in-package :resource-tree)
(defconstant +nothing+ (gensym "nothing-"))
