(in-package :sphinx-mode-cl)

(defvar *db* nil)
(defvar *sphinx* nil)

(defun ensure-db-connected ()
  (unless (and *db* (eql :open (clsql-sys:database-state *db*)))
    (setf *db* (clsql:connect '("localhost" "emacs_user" "emacs" "emacs")
                              :database-type :mysql))))

(defun ensure-sphinx-connected ()
  (unless *sphinx*
    (setf *sphinx* (make-instance 'cl-sphinx-search::sphinx-client))
    (cl-sphinx-search:set-server *sphinx* :port 9306)))
