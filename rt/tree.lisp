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
                        (not (eq (setf next (getf pointer keyword +nothing+))
                                 +nothing+)))
                       () 'invalid-node
                       :invalid-path path
                       :valid-path (reverse valid-path))
               (setf pointer next))
          collect keyword into valid-path
          finally (return pointer))))

(defun parse-keyword (string)
  (flet ((strip-postfix (string)
           (let ((pos (position #\. string)))
             (if (null pos)
                 string
                 (subseq string 0 pos)))))
    (intern (nsubstitute #\- #\_ (string-upcase (strip-postfix string)))
            "KEYWORD")))
              
(defmethod load-path ((rtree resource-tree) path &key (recursive t))
  (with-slots (file-loader) rtree
    (cond ((directory-exists-p path) 
           (loop for sub-path in (list-directory path)
                 if (and recursive (directory-exists-p sub-path))
                   collect (list (parse-keyword 
                                  (car (last (pathname-directory sub-path))))
                                 (load-path rtree sub-path :recursive t))
                   into sub-tree
                 else when (file-exists-p sub-path)
                   collect (funcall file-loader sub-path)
                   into sub-tree
                 finally (return (apply #'nconc sub-tree))))
          ((file-exists-p path) (funcall file-loader path)))))
