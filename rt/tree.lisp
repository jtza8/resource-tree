; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :resource-tree)

(define-condition invalid-node (error)
  ((invalid-path :initform (error "must specify invalid-path")
                 :initarg :invalid-path
                 :reader invalid-path)
   (valid-path :initform +nothing+
               :initarg :valid-path
               :reader valid-path))
  (:report (lambda (condition stream)
             (with-slots (invalid-path valid-path) condition
               (format stream "invalid path ~s~:[ but ~s is valid.~;~]"
                       invalid-path (eq valid-path +nothing+) valid-path)))))

(defclass resource-tree ()
  ((tree :initform '()
         :initarg :tree)))

(defmethod node ((rtree resource-tree) &rest path)
  (with-slots (tree) rtree
    (loop with pointer = tree
          for keyword in path
          do (progn 
               (assert (listp pointer) () 'invalid-node
                       :invalid-path path
                       :valid-path (reverse valid-path))
               (setf pointer (getf pointer keyword +nothing+)))
          collect keyword into valid-path
          finally (return pointer))))

(defmethod extend-tree ((rtree resource-tree) &rest path)
  ())

;; (defmethod (setf node) (value (rtree resource-tree) &rest path)
;;   (with-slots (tree) rtree
;;     (if (null path)
;;         (progn (check-type value list)
;;                (return-from node (setf tree value)))
;;         (loop with pointer = tree
;;               for