(require 'comint)
(require 'emacsql)
(require 'emacsql-mysql)

(defvar sphinx-mysql-connection nil)
(defvar sphinx-connection nil)

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
   (cl-loop while (re-search-forward "^\\*\\s-*\\([^:*]+\\):" nil t)
            unless (string-equal "Menu" (match-string 1))
            collect (format "'%s'" (match-string 1)))))

(defun sphinx-add-info-row (node)
  (let ((contents (save-excursion
                    (forward-line 2)
                    (buffer-substring-no-properties
                     (point) (point-max)))))
    (condition-case error
        (progn
          (emacsql sphinx-mysql-connection
                   [:insert :into info [node contents]
                            :values [$s1 $s2]]
                   node contents)
          t)
      (emacsql-error nil))))

(defun sphinx-mysql-connect ()
  (when sphinx-mysql-connection
    (emacsql-close sphinx-mysql-connection)
    ;; We probably made some changes to schema, let's get rif
    ;; of the cached prepared queries
    (clrhash emacsql-prepare-cache)
    (setq sphinx-mysql-connection nil))
  (setq sphinx-mysql-connection
        (emacsql-mysql "emacs_user"
                       :user "emacs"
                       :password "emacs")))

(defun sphinx-connect ()
  (interactive)
  (when sphinx-connection
    (emacsql-close sphinx-connection)
    (clrhash emacsql-prepare-cache)
    (setq sphinx-connection nil))
  (let* ((database "emacs_user")
         (mysql (executable-find emacsql-mysql-executable))
         (command (list "--port=9306" "--protocol=tcp" "--user=emacs" "--password=emacs"
                        database "--skip-pager" "-rfBNL" mysql)))
    (let* ((process-connection-type t)
           (buffer (generate-new-buffer " *emacsql-mysql*"))
           (command (mapconcat #'shell-quote-argument (nreverse command) " "))
           (process (start-process-shell-command
                     "emacsql-mysql" buffer (concat "stty raw &&" command)))
           (connection (make-instance 'emacsql-mysql-connection
                                      :process process
                                      :dbname database)))
      (setf (process-sentinel process)
            (lambda (proc _) (kill-buffer (process-buffer proc))))
      (setq sphinx-connection (emacsql-register connection)))))

(defun sphinx-disconnect ()
  (interactive)
  (when sphinx-connection
    (emacsql-close sphinx-connection)
    (setq sphinx-connection nil)))

(defun sphinx-info-ensure-table ()
  (emacsql sphinx-mysql-connection
           [:create-table :if :not :exists info
                          ([(id integer :primary-key :auto_increment)
                            (node object)
                            (contents object)])])
  ;; TODO: I couldn't find a way to specify unique constraint inline
  (emacsql sphinx-mysql-connection
           "ALTER TABLE info 
            ADD CONSTRAINT contents UNIQUE IF NOT EXISTS
            (contents(100));"))

(defun sphinx-info-build-node-index (root)
  (with-sphinx-info-tfile
   (tmp (format "info %s -o '%s'" root tmp))
   (when (sphinx-add-info-row root)
     (forward-line 2)
     (while (re-search-forward "^\\*\\s-*\\([^:*]+\\):" nil t)
       (let ((node (match-string 1)))
         (unless (string-equal "Menu" node)
           (sphinx-info-build-node-index
            (format "%s '%s'" root node))))))))

(defun sphinx-info-build-index ()
  (interactive)
  (unless sphinx-mysql-connection
    (sphinx-mysql-connect))
  (sphinx-info-ensure-table)
  (mapc 'sphinx-info-build-node-index (sphinx-info-nodes)))

(defun sphinx ()
  (interactive)
  (make-comint "Sphinx" sql-mysql-program nil
               "--port=9306"
               "--protocol=tcp"
               "--prompt=Sphinx> "
               "--user=emacs"
               "--password=emacs" "emacs_user")
  (pop-to-buffer (get-buffer "*Sphinx*")))

(defun sphinx-info-apropos (query &optional ranker)
  (interactive
   (list
    (read-string "Ask Sphinx anything: ")
    (let ((arg (prefix-numeric-value current-prefix-arg)))
      (if (and (called-interactively-p 'any) (> arg 1))
          (read-string "Rank results by: ") ""))))
  (let ((composite-query
         (format "SELECT node, WEIGHT() as rank FROM info 
                  WHERE MATCH('%s') ORDER BY WEIGHT() DESC, id ASC" query)))
    (setq composite-query
          (if (string-equal ranker "")
              (format "%s;" composite-query)
            (format "%s OPTION ranker = expr('%s');" composite-query ranker)))
    (unless sphinx-connection (sphinx-connect))
    (let ((buffer "*sphinx-apropos*")
          (results (emacsql sphinx-connection composite-query)))
      (with-current-buffer (get-buffer-create buffer)
        (erase-buffer)
        (insert (replace-regexp-in-string "^\\s-*" "> " composite-query))
        (insert "\n\n")
        (cl-loop for (node rank) in results do
                 (insert (format "%d. %s\n" rank node)))
        (pop-to-buffer buffer)))))
      
