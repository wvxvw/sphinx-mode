(defvar sphinx-mysql-connection nil)

(cl-defmacro with-sphinx-info-tfile ((tmp-file command) &body body)
  (let ((tmp-file tmp-file))
    `(let ((,tmp-file (make-temp-file "info")))
       (unwind-protect
           (progn
             (shell-command ,command)
             (with-temp-buffer
               (insert-file-contents ,tmp-file)
               ,@body))
         (delete-file ,tmp-file)))))

(defun sphinx-info-nodes ()
  (with-sphinx-info-tfile
   (tmp (format "info --subnodes -o '%s'" tmp))
   (cl-loop while (re-search-forward "^\\*\\s-*\\([^:]+\\):" nil t)
            unless (string-equal "Menu" (match-string 1))
            collect (match-string 1))))

(defun sphinx-add-info-row (node)
  (message "adding row for node: %s" node)
  ;; (sql-send-string (format "insert into info (node) value ('%s');" node))
  )

(defun sphinx-mysql-connect ()
  (message "connected to mysql")
  ;; (when sphinx-mysql-connection
  ;;   (sql-send-string "quit;"))
  ;; (setf sphinx-mysql-connection nil)
  ;; (with-current-buffer (get-buffer-create "*sphinx-mysql*")
  ;;   (let ((sql-user "emacs")
  ;;         (sql-password "emacs")
  ;;         (sql-database "emacs_user"))
  ;;     (sql-comint-mysql nil nil))
  ;;   (setf sphinx-mysql-connection (current-buffer)))
  )

(defun sphinx-info-build-node-index (root)
  (message "info::: %s" root)
  (with-sphinx-info-tfile
   (tmp (format "info %s -o '%s'" root tmp))
   (sphinx-add-info-row root)
   (forward-line 2)
   (while (re-search-forward "^\\*\\s-*\\([^:]+\\):" nil t)
     (unless (string-equal "Menu" (match-string 1))
       (sphinx-info-build-node-index
        (format "%s %s" root (match-string 1)))))))

(defun sphinx-info-build-index ()
  (interactive)
  (unless sphinx-mysql-connection
    (sphinx-mysql-connect))
  (mapc 'sphinx-info-build-node-index (sphinx-info-nodes)))
