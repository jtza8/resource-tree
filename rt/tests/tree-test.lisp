; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :resource-tree)

(defclass tree-test (test-case)
  ())

(def-test-method test-node ((test tree-test))
  (let ((tree (make-instance 'resource-tree :tree '(:a 1 :b (:c 3 :d 4) :e 5))))
    (assert-equal 1 (node tree :a))
    (assert-equal '(:c 3 :d 4) (node tree :b))
    (assert-equal 3 (node tree :b :c))
    (assert-equal 4 (node tree :b :d))
    (assert-equal 5 (node tree :e))))

(def-test-method test-setf-node ((test tree-test))
  (let ((tree (make-instance 'resource-tree)))
    (setf (node tree) '(:a 1 :b 2))
    (assert-equal 2 (node tree :b))
    (assert-condition 'simple-type-error (setf (node tree) 'blah))
    (assert-equal 1 (node tree :a))
    (setf (node tree :b) 3)
    (assert-equal 3 (node tree :b))
    (setf (node tree :c) 4)
    (assert-equal 4 (node tree :c))))