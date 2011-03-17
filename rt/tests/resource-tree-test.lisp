; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :resource-tree)

(defclass tree-test (test-case)
  ())

(defparameter *test-path-result-basic*
  '(:HELLO "Hello World!"))
(defparameter *test-path-result-recursive*
  '(:HELLO "Hello World!" :PORTAL (:CAKE "The cake is a lie.")))

(def-test-method test-node-of ((test tree-test))
  (let ((tree '(:a 1 :b (:c 3 :d 4) :e 5 :f ())))
    (assert-equal 1 (node-of tree :a))
    (assert-equal '(:c 3 :d 4) (node-of tree :b))
    (assert-equal 3 (node-of tree :b :c))
    (assert-equal 4 (node-of tree :b :d))
    (assert-equal 5 (node-of tree :e))
    (assert-equal '() (node-of tree :f))
    (assert-condition 'invalid-node (node-of tree :f :g :h))
    (assert-condition 'invalid-node (node-of tree :b :x))
    (assert-condition 'invalid-node (node-of tree :b :c :z))))

(def-test-method test-setf-node-of ((test tree-test))
  (let (tree)
    (assert-equal '(:a 1 :b 2) (setf (node-of tree) '(:a 1 :b 2)))
    (assert-equal 2 (node-of tree :b))
    (assert-condition 'simple-type-error (setf (node-of tree) 'blah))
    (assert-equal 1 (node-of tree :a))
    (assert-equal 3 (setf (node-of tree :b) 3))
    (assert-equal 3 (node-of tree :b))
    (assert-equal 4 (setf (node-of tree :c) 4))
    (assert-equal 4 (node-of tree :c))
    (assert-condition 'invalid-node (setf (node-of tree :c :d) 10))
    (assert-equal (setf (node-of tree) nil) nil)
    (assert-equal 3 (setf (node-of tree :foo) 3))
    (assert-equal 3 (node-of tree :foo))
    (setf (node-of tree :blah) '()
          (node-of tree :blah :foo) 5)
    (assert-equal 5 (node-of tree :blah :foo))))

(defun process-str-file (file-path)
  (unless (string= (pathname-type file-path) "str")
    (return-from process-str-file '()))
  (with-open-file (file file-path)
    (list (path-keyword file-path)
          (loop with eof = (gensym) and line = ""
                if (eq (setf line (read-line file nil eof)) eof) do
                   (return (format nil "~{~&~a~}" strings))
                else collect line into strings))))

(def-test-method test-build-tree ((test tree-test))
  (let ((string-tree (make-instance 'resource-tree 
                                    :file-loader #'process-str-file))
        (hello-path (merge-pathnames "hello.str" *test-tree-path*)))
    (assert-equal *test-path-result-basic* (build-tree string-tree hello-path))
    (assert-equal *test-path-result-recursive*
                  (build-tree string-tree *test-tree-path*))
    (assert-equal *test-path-result-basic*
                  (build-tree string-tree *test-tree-path* :recursive nil))))

(def-test-method test-load-path ((test tree-test))
  (let ((string-tree (make-instance 'resource-tree
                                    :file-loader #'process-str-file)))
    (load-path string-tree *test-tree-path* :recursive nil)
    (assert-equal "Hello World!" (node string-tree :hello))
    (setf (node string-tree :foo) '())
    (load-path string-tree *test-tree-path* 
               :recursive nil
               :parent-node-path '(:foo))
    (assert-equal "Hello World!" (node string-tree :foo :hello))))