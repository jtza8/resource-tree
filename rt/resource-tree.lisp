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
  ((tree :initform (make-hash-table)
         :initarg :tree)
   (file-loader :initform (error "must specify file-loader")
                :initarg :file-loader)
   (free-func :initform nil
              :initarg :free-func)))

(defmethod initialize-instance :after ((rtree resource-tree) &key)
  (with-slots (file-loader) rtree
    (when (null file-loader)
      (setf file-loader (lambda (file)
                          (declare (ignore file))
                          (error (format nil "file-loader needed but ~
                                              explicitly not specified")))))))

(defun node-of (branch &rest path)
  (loop with pointer = branch
        for key in path
        for remaining from (length path) downto 1
        do (multiple-value-bind (value present)
               (gethash key pointer)
             (assert (and present
                          (if (> remaining 1) 
                              (typep value 'hash-table)
                              t))
                     () 'invalid-node
                     :invalid-path path
                     :valid-path (reverse valid-path))
             (setf pointer value))
        collect key into valid-path
        finally (return pointer)))

(defmethod node ((rtree resource-tree) &rest path)
  (with-slots (tree) rtree
    (apply #'node-of tree path)))

(defun set-node-of (branch value path)
  (let* ((parent (apply #'node-of branch (butlast path)))
         (key (car (last path))))
    (assert (and (not (null key))
                 (typep parent 'hash-table))
            () 'invalid-node
            :invalid-path path)
    (setf (gethash key parent) value)))

(defsetf node-of (branch &rest path) (value)
  `(set-node-of ,branch ,value ',path))

(defmethod (setf node) (value (rtree resource-tree) &rest path)
  (with-slots (tree) rtree
    (if (null path)
        (progn (check-type value hash-table)
               (setf tree value))
        (set-node-of tree value path))))

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
    (let ((sub-tree (make-hash-table)))
      (cond ((directory-exists-p path) 
             (loop for sub-path in (list-directory path)
                   if (and recursive (directory-exists-p sub-path))
                     do (setf (gethash (path-keyword sub-path) sub-tree)
                              (build-tree rtree sub-path :recursive t))
                   else when (and (not (directory-exists-p sub-path))
                                  (file-exists-p sub-path))
                     do (setf (gethash (path-keyword sub-path) sub-tree)
                              (funcall file-loader sub-path))
                   finally (return sub-tree)))
            ((file-exists-p path) 
             (setf (gethash (path-keyword path) sub-tree)
                   (funcall file-loader path))))
      sub-tree)))

(defmethod load-path ((rtree resource-tree) path &key
                      parent-node-path (recursive t))
  (let ((branch (build-tree rtree path :recursive recursive)))
    (setf (apply #'node rtree parent-node-path) branch)))

(defmethod free-node ((rtree resource-tree) node)
  (with-slots (free-func) rtree
    (when (null free-func)
      (return-from free-node))
    (if (typep node 'hash-table)
        (loop for sub-node being the hash-values in node
              do (free-node rtree sub-node))
        (funcall free-func node))))

(defgeneric free (resource))
(defmethod free ((rtree resource-tree))
  (with-slots (tree) rtree
    (free-node rtree tree)))

(defmacro with-nodes (resources resource-branch &body body)
  (let ((node (gensym "RESOURCE-BRANCH-")))
    `(let ((,node ,resource-branch))
       (let (,@(loop for resource in resources collect
                    (list resource
                          `(node-of ,node ,(intern (symbol-name resource)
                                                         "KEYWORD")))))
         ,@body))))
