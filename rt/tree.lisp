; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :resource-tree)

(define-condition invalid-node (error)
  ((node :initform (error "must specify node")
         :initarg :node))
  (:report (lambda (condition stream)
             (with-slots (node) condition
               (format stream "invalid node: ~a" node)))))

(defclass resource-tree ()
  ((tree :initform '()
         :initarg :tree)))

(defmethod node ((rtree resource-tree) &rest path)
  (with-slots (tree) rtree
    (loop with pointer = tree
          for keyword in path
          do (setf pointer (getf pointer keyword))
          finally (progn (assert (or (null tree)
                                     (not (null pointer)))
                                 () 'invalid-node
                                 :node path)
                         (return pointer)))))

(defmethod (setf node) (value (rtree resource-tree) &rest path)
  (with-slots (tree) rtree
    (if (null path)
        (progn (check-type value list)
               (return-from node (setf tree value)))
        (loop with current = tree
              for keyword in path
              for remaining from (1- (length path)) downto 0
              for next = (getf current keyword)
              if (null next)
              do (if (= remaining 0) 
                     (setf (getf current keyword) value)
                     (setf (getf current keyword) next
                           current next))
              else
              do (if (= remaining 0)
                     (setf (getf current keyword) value)
                     (setf current next))
              end
              finally (return tree)))))
