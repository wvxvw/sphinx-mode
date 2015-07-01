;; sphinx-mode.el --- an interface to Sphinx, database search indexer.
;;
;; Copyright (C) 2015  Oleg Sivokon <olegsivokon@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;;; Conventions
;;

;;* Eclim Project

(require 'comint)
(require 'emacsql)
(require 'emacsql-mysql)

(defvar sphinx-mysql-connection nil)
(defvar sphinx-connection nil)
(defvar sphinx-mode-hook nil)
(defvar sphinx-mode-map (make-sparse-keymap))
(defvar sphinx-query nil)
(defvar sphinx-rank nil)

(defcustom sphinx-searchd-program "searchd"
  "Sphinx daemon program")

(defcustom sphinx-indxer-program "indexer"
  "Sphinx indexer program")

(defcustom sphinx-dir "~/.emacs.d/sphinx/"
  "Directory where Sphinx settings and indices will be stored")

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

(defun sphinx-parse-node (raw)
  (let ((pos 0) result)
    (while (string-match "'\\([^']+\\)'" raw pos)
      (push (substring raw (match-beginning 1) (match-end 1))
            result)
      (setq pos (match-end 0)))
    (nreverse result)))

;;;###autoload
(defun sphinx ()
  (interactive)
  (make-comint "Sphinx" sql-mysql-program nil
               "--port=9306"
               "--protocol=tcp"
               "--prompt=Sphinx> "
               "--user=emacs"
               "--password=emacs" "emacs_user")
  (pop-to-buffer (get-buffer "*Sphinx*")))

;;;###autoload
(defun sphinx-info-apropos (query &optional ranker)
  (interactive
   (list
    (read-string "Ask Sphinx anything: ")
    (let ((arg (prefix-numeric-value current-prefix-arg)))
      (if (> arg 1) (read-string "Rank results by: ") ""))))
  (let ((composite-query
         (format "SELECT node, WEIGHT() as rank FROM info 
                  WHERE MATCH('%s') ORDER BY WEIGHT() DESC, id ASC" query)))
    (setq composite-query
          (if (string-equal ranker "")
              (format "%s;" composite-query)
            (format "%s OPTION ranker = expr('%s');" composite-query ranker)))
    (unless sphinx-connection (sphinx-connect))

    (let ((buffer "*sphinx-apropos*")
          (results
           (condition-case error
               (emacsql sphinx-connection composite-query)
             ;; "MySQL server has gone away"
             (emacsql-error
              (when (string-equal (cl-second error)
                                  "MySQL server has gone away")
                (sphinx-connect)
                (emacsql sphinx-connection composite-query))))))
      (with-current-buffer (get-buffer-create buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "-*- mode: sphinx; sphinx-query: %S; sphinx-rank: %S -*-\n\n"
                          query ranker))
          (insert "SQL:\n")
          (insert (replace-regexp-in-string "^\\s-*" "> " composite-query))
          (insert "\n\n")
          (put-text-property (point-min) (point) 'face 'font-lock-comment-face)
          (cl-loop for (node rank) in results do
                   (let ((beg (point)))
                     (insert (format "%d. " rank))
                     (put-text-property beg (point) 'face 'default)
                     (insert-button node
                                    'action 'sphinx-goto
                                    'help-echo "Open Info browser")
                     (insert "\n")))
          (sphinx-mode)
          (pop-to-buffer buffer))))))

(define-key sphinx-mode-map (kbd "RET") 'sphinx-goto)
(define-key sphinx-mode-map (kbd "SPC") 'sphinx-goto)
(define-key sphinx-mode-map (kbd "C-c C-f g") 'sphinx-goto)

(defun sphinx-goto (&optional node)
  (interactive)
  (when (overlayp node)
    (setq node (sphinx-parse-node (button-label node))))
  (info "(dir)Top")
  (let ((nodes node))
    (while nodes
      (Info-menu (car nodes))
      (setq nodes (cdr nodes)))))

;;;###autoload
(defun sphinx-mode ()
  "Displays Sphinx search results.

\\{sphinx-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'sphinx-mode
        mode-name "sphinx"
        mode-line-process ""
        buffer-read-only t)
  (hl-line-mode t)
  (use-local-map sphinx-mode-map)
  (beginning-of-buffer)
  (run-mode-hooks 'sphinx-mode-hook))

(provide 'sphinx-mode)
