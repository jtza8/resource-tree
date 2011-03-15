; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :resource-tree)

(define-condition invalid-node (error)
  ((invalid-path :initform (error "must specify invalid-path")
                 :initarg :invalid-path
                 :reader invalid-path)
   (valid-path :initform *nothing*
               :initarg :valid-path
               :reader valid-path))
  (:report (lambda (condition stream)
             (with-slots (invalid-path valid-path) condition
               (format stream "invalid path ~s~:[ but ~s is valid.~;~]"
                       invalid-path (eq valid-path *nothing*) valid-path)))))

(defclass resource-tree ()
  ((tree :initform '()
         :initarg :tree)
   (file-loader :initform (error "must specify file-loader")
                :initarg :file-loader)))

(defmethod initialize-instance :after ((rtree resource-tree) &key)
  (with-slots (file-loader) rtree
    (when (null file-loader)
      (setf file-loader (lambda (file)
                          (declare (ignore file))
                          (error (format nil "file-loader needed but ~
                                              explicitly not specified")))))))

(defmethod node ((rtree resource-tree) &rest path)
  (with-slots (tree) rtree
    (loop with pointer = tree
          for keyword in path
          do (let (next)
               (assert (and 
                        (listp pointer) 
                        (not (eq (setf next (getf pointer keyword *nothing*))
                                 *nothing*)))
                       () 'invalid-node
                       :invalid-path path
                       :valid-path (reverse valid-path))
               (setf pointer next))
          collect keyword into valid-path
          finally (return pointer))))

(defmethod (setf node) (value (rtree resource-tree) &rest path)
  (with-slots (tree) rtree
    (loop initially (when (null path)
                      (check-type value list)
                      (return (setf tree value)))
          with now = tree and past = tree
          for remaining from (length path) downto 1
          for keyword in path
          for next = (getf now keyword *nothing*)
          if (= remaining 1)
            return (let ((pair (list keyword value))) ; Messy
                     (if (null now)
                         (if (null tree)
                             (setf tree pair)
                             (setf (getf past (car valid-path)) pair))
                         (if (eq next *nothing*)
                             (nconc now pair)
                             (setf (getf now keyword) value)))
                     value)
          else
            do (progn (assert (and (not (or (eq next *nothing*)))
                                   (listp next))
                              () 'invalid-node
                              :invalid-path path
                              :valid-path (reverse valid-path))
                      (setf past now
                            now next))
          collect keyword into valid-path)))

(defun path-keyword (path)
  (let* ((keyword (or (pathname-name path)
                      (car (last (pathname-directory path)))))
         (pos (position #\. keyword)))
    (unless (null pos)
      (setf keyword (subseq keyword 0 pos)))
    (setf keyword (nsubstitute #\- #\_ (string-upcase keyword)))
    (intern  keyword "KEYWORD")))
              
(defmethod build-tree ((rtree resource-tree) path &key (recursive t))
  (with-slots (file-loader) rtree
    (cond ((directory-exists-p path) 
           (loop for sub-path in (list-directory path)
                 if (and recursive (directory-exists-p sub-path))
                   collect (list (path-keyword sub-path)
                                 (build-tree rtree sub-path :recursive t))
                   into sub-tree
                 else when (file-exists-p sub-path)
                   collect (funcall file-loader sub-path)
                   into sub-tree
                 finally (return (apply #'nconc sub-tree))))
          ((file-exists-p path) (funcall file-loader path)))))

(defmethod load-path ((rtree resource-tree) path &key
                      parent-node-path (recursive t))
  (with-slots (tree) rtree
    (let ((branch (build-tree rtree path :recursive recursive)))
      (setf (apply #'node rtree parent-node-path) branch))))
