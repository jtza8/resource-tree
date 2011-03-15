; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :resource-tree)

(defclass tree-test (test-case)
  ())

(defconstant +test-path-result-basic+ 
  '(:HELLO "Hello World!"))
(defconstant +test-path-result-recursive+
  '(:HELLO "Hello World!" :PORTAL (:CAKE "The cake is a lie.")))

(def-test-method test-node ((test tree-test))
  (let ((tree (make-instance 'resource-tree 
                             :tree '(:a 1 :b (:c 3 :d 4) :e 5 :f ())
                             :file-loader nil)))
    (assert-equal 1 (node tree :a))
    (assert-equal '(:c 3 :d 4) (node tree :b))
    (assert-equal 3 (node tree :b :c))
    (assert-equal 4 (node tree :b :d))
    (assert-equal 5 (node tree :e))
    (assert-equal '() (node tree :f))
    (assert-condition 'invalid-node (node tree :f :g :h))
    (assert-condition 'invalid-node (node tree :b :x))
    (assert-condition 'invalid-node (node tree :b :c :z))))

(def-test-method test-setf-node ((test tree-test))
  (let ((tree (make-instance 'resource-tree :file-loader nil)))
    (setf (node tree) '(:a 1 :b 2))
    (assert-equal 2 (node tree :b))
    (assert-condition 'simple-type-error (setf (node tree) 'blah))
    (assert-equal 1 (node tree :a))
    (setf (node tree :b) 3)
    (assert-equal 3 (node tree :b))
    (setf (node tree :c) 4)
    (assert-equal 4 (node tree :c))
    (assert-condition 'invalid-node (setf (node tree :c :d :e) 10))))

(defun process-str-file (file-path)
  (unless (string= (pathname-type file-path) "str")
    (return-from process-str-file '()))
  (with-open-file (file file-path)
    (list (parse-keyword (file-namestring file-path))
          (loop with eof = (gensym) and line = ""
                if (eq (setf line (read-line file nil eof)) eof) do
                   (return (format nil "~{~&~a~}" strings))
                else collect line into strings))))

(def-test-method test-build-tree ((test tree-test))
  (let ((string-tree (make-instance 'resource-tree 
                                    :file-loader #'process-str-file))
        (hello-path (merge-pathnames "hello.str" *test-tree-path*)))
    (assert-equal +test-path-result-basic+ (build-tree string-tree hello-path))
    (assert-equal +test-path-result-recursive+
                  (build-tree string-tree *test-tree-path*))
    (assert-equal +test-path-result-basic+
                  (build-tree string-tree *test-tree-path* :recursive nil))))

(def-test-method test-load-path ((test tree-test))
  (let ((string-tree (make-instance 'resource-tree
                                    :file-loader #'process-str-file)))
    (load-path string-tree *test-tree-path* :recursive nil)
    (setf (node string-tree :foo) '(:1 ))
    (load-path string-tree *test-tree-path* 
               :recursive nil
               :parent-node-path '(:foo))
    (assert-equal "Hello World!" (node string-tree :foo :hello))))