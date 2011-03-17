; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :resource-tree)

(defclass tree-test (test-case)
  ())

(def-test-method test-node-of ((test tree-test))
  (let ((tree (make-hash-table)))
    (setf (gethash :a tree) 1
          (gethash :b tree) (make-hash-table)
          (gethash :c (gethash :b tree)) 2)
    (assert-equal 1 (node-of tree :a))
    (assert-equal 2 (node-of tree :b :c))
    (assert-condition 'invalid-node (node-of tree :c))
    (assert-condition 'invalid-node (node-of tree :a :d))))

(def-test-method test-setf-node-of ((test tree-test))
  (let ((tree (make-hash-table))
        (sub-tree (make-hash-table)))
    (setf (gethash :c sub-tree) 3
          (gethash :d sub-tree) 4)
    (assert-condition 'invalid-node (setf (node-of tree) sub-tree))
    (setf (node-of tree :a) 1)
    (assert-equal 1 (node-of tree :a))
    (assert-condition 'invalid-node (setf (node-of tree :a :b) sub-tree))
    (setf (node-of tree :b) sub-tree)
    (assert-equal 4 (node-of tree :b :d))))

(defun process-str-file (file-path)
  (unless (string= (pathname-type file-path) "str")
    (return-from process-str-file '()))
  (with-open-file (file file-path)
    (loop with eof = (gensym) and line = ""
          if (eq (setf line (read-line file nil eof)) eof) do
             (return (format nil "~{~&~a~}" strings))
          else collect line into strings)))

(def-test-method test-build-tree ((test tree-test))
  (let ((hello-path (merge-pathnames "hello.str" *test-tree-path*))
        string-tree branch)
    (flet ((reset-string-tree ()
             (setf string-tree 
                   (make-instance 'resource-tree 
                                  :file-loader #'process-str-file))))
      (reset-string-tree)
      (setf branch (build-tree string-tree hello-path))
      (assert-equal "Hello World!" (node-of branch :hello))
      (reset-string-tree)
      (setf branch (build-tree string-tree *test-tree-path*))
      (assert-equal "The cake is a lie." (node-of branch :portal :cake))
      (reset-string-tree)
      (setf branch (build-tree string-tree *test-tree-path* :recursive nil))
      (assert-condition 'invalid-node (node-of branch :portal)))))

;; (def-test-method test-load-path ((test tree-test))
;;   (let ((string-tree (make-instance 'resource-tree
;;                                     :file-loader #'process-str-file)))
;;     (load-path string-tree *test-tree-path* :recursive nil)
;;     (assert-equal "Hello World!" (node string-tree :hello))
;;     (setf (node string-tree :foo) '())
;;     (load-path string-tree *test-tree-path* 
;;                :recursive nil
;;                :parent-node-path '(:foo))
;;     (assert-equal "Hello World!" (node string-tree :foo :hello))))