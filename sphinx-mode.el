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

;;* Sphinx Mode

(require 'comint)
(require 'emacsql)
(require 'emacsql-mysql)

(defvar sphinx-mysql-connection nil)
(defvar sphinx-connection nil)
(defvar sphinx-mode-hook nil)
(defvar sphinx-mode-map (make-sparse-keymap))
(defvar sphinx-query nil)
(defvar sphinx-rank nil)
(defvar sphinx-library nil)
(defvar sphinx-index-alist
  '(("\\.org$" . sphinx-org-index-file)))

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
  (sphinx-ensure-searchd)
  (let* ((database "emacs_user")
         (mysql (executable-find emacsql-mysql-executable))
         (command (list "--port=9306" "--protocol=tcp" "--user=emacs" "--password=emacs"
                        database "--skip-pager" "-rfBNL" mysql)))
    (let* ((process-connection-type t)
           (buffer (generate-new-buffer " *emacsql-sphinx*"))
           (command (mapconcat #'shell-quote-argument (nreverse command) " "))
           (process (start-process-shell-command
                     "emacsql-sphinx" buffer (concat "stty raw &&" command)))
           (connection (make-instance 'emacsql-mysql-connection
                                      :process process
                                      :dbname database)))
      (setf (process-sentinel process)
            (lambda (process _)
              ;; TODO: This needs to notify the authorities, when
              ;; things go South.
              (with-current-buffer (process-buffer process)
                (message (buffer-substring (point-min) (point-max))))
              (kill-buffer (process-buffer process))))
      (setq sphinx-connection (emacsql-register connection)))))

(defun sphinx-disconnect ()
  (interactive)
  (when sphinx-connection
    (emacsql-close sphinx-connection)
    (setq sphinx-connection nil)))

(defun sphinx-ensure-searchd ()
  (let ((present (shell-command-to-string "pgrep searchd")))
    (when (string-equal present "")
      (start-process-shell-command
       "searchd" (generate-new-buffer " *searchd*")
       (format "%s -c %s" (executable-find sphinx-searchd-program)
               (expand-file-name "./etc/sphinx.conf" sphinx-dir))))))

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

(defun sphinx-org-ensure-table ()
  (emacsql sphinx-mysql-connection
           [:create-table :if :not :exists org_kinds
                          ([(id integer :primary-key :auto_increment)
                            (kind string)])])
  (emacsql sphinx-mysql-connection
           [:create-table :if :not :exists org
                          ([(id integer :primary-key :auto_increment)
                            (file object :not :null)
                            (kind integer :not :null)
                            (pos integer :not :null)
                            (contents string)])])
  ;; TODO: I couldn't find a way to specify unique constraint inline
  ;; There are multiple issues here, the if not exists causes MySQL
  ;; connection to close, these should be both inified with the table
  ;; definition.
  (emacsql sphinx-mysql-connection
           "ALTER TABLE org
            ADD FOREIGN KEY (kind) REFERENCES org_kinds(id);")
  (emacsql sphinx-mysql-connection
           "ALTER TABLE org
            ADD FOREIGN KEY (parent) REFERENCES org(id);"))

(defun sphinx-info-build-node-index (root)
  (with-sphinx-info-tfile
   ;; TODO: This is very ineffective way of doing this, there must be a
   ;; way to collect info's output without doing I/O.
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

(defun sphinx-goto (&optional node)
  (interactive)
  (when (overlayp node)
    (setq node (sphinx-parse-node (button-label node))))
  (info "(dir)Top")
  (let ((nodes node))
    (while nodes
      (Info-menu (car nodes))
      (setq nodes (cdr nodes)))))

(define-key sphinx-mode-map (kbd "RET") 'sphinx-goto)
(define-key sphinx-mode-map (kbd "SPC") 'sphinx-goto)
(define-key sphinx-mode-map (kbd "C-c C-f g") 'sphinx-goto)

(defun sphinx-install-dir ()
  (or sphinx-library
      (file-name-directory (locate-library "sphinx-mode"))))

(defun sphinx-install-config ()
  (with-temp-file
      (expand-file-name "./etc/sphinx.conf" sphinx-dir)
    (insert-file-contents
     (expand-file-name "sphinx.conf" (sphinx-install-dir)))))

(defun sphinx-install-prerequisites ()
  (let ((dir (format "/sudo::%s" (sphinx-install-dir)))
        (current default-directory))
    (cd dir)
    (condition-case error
        (progn
          (shell "sphinx-install")
          (pop-to-buffer (current-buffer))
          (goto-char (point-max))
          (insert "./install.sh")
          (comint-send-input))
      ;; TODO: There has to be another way to do it.
      ;; Wait until the script finishes
      (with-local-quit
        (while (get-buffer-process "sphinx-install")
          (sleep-for 5)
          (accept-process-output)))
      (error
       (message "Installation failed. 
You may still try to read %s and perform the required installations manually.")))
    (cd current)))

(defun sphinx-maybe-initial-index ()
  )

(defun sphinx-add-file-default (file)
  (unless sphinx-mysql-connection
    (sphinx-mysql-connect))
  (condition-case error
      (emacsql sphinx-mysql-connection
               [:insert :into documents
                        (document) :values [$s1]]
               file)
    (emacsql-error
     (message "Couldn't index because: %s" error))))

(defun sphinx-index-file (file)
  (interactive "fIndex file: ")
  (funcall
   (cl-loop for (re . func) in sphinx-index-alist
            when (string-match-p re file) do
            (cl-return func)
            finally (cl-return 'sphinx-add-file-default))
   file))

(defun sphinx-org-add-row (record)
  (cl-destructuring-bind (kind options &rest contents) record
    (let ((file (buffer-file-name))
          (begin (plist-get options :begin))
          (cnt (if (and contents (stringp (car contents)))
                   (car contents)
                 (or (car (plist-get options :title))
                     (plist-get options :value)))))
      (when cnt (setq cnt (substring-no-properties cnt 0 (length cnt))))
      (emacsql sphinx-mysql-connection
               "INSERT INTO org (kind, file, pos, contents, parent)
                VALUES
                ((SELECT id FROM org_kinds WHERE org_kinds.kind = $s1),
                 $s2, $s3, $s4,
                 (SELECT id FROM org AS org1 WHERE org1.file = $s2 LIMIT 1))"
                  kind file begin cnt))))

(defun sphinx-org-index-file (file)
  (with-current-buffer (find-file-noselect file)
    (org-element-map (org-element-parse-buffer)
        org-element-all-elements 'sphinx-org-add-row)))

;; Dired integration
(defcustom sphinx-keep-marker-index ?i
  "Controls marking of indexed files."
  :type '(choice (const :tag "Keep" t)
                 (character :tag "Mark"))
  :group 'dired-mark)

(defun sphinx-dired-do-index (&optional arg)
  "Tell Sphinx to index the marked file(s)."
  (interactive "P")
  (unless sphinx-mysql-connection
    (sphinx-mysql-connect))
  (mapc 'sphinx-index-file (dired-get-marked-files nil nil)))

(define-key dired-mode-map (kbd "J") 'sphinx-dired-do-index)

;; Autoloads

;;;###autoload
(defun sphinx-install ()
  (interactive)
  (when (or (not (file-exists-p sphinx-dir))
            (yes-or-no-p "Previous install detected, install anyway? "))
    (make-directory sphinx-dir t)
    (cl-loop for sub-dir in '("./var/log/" "./var/data/" "./etc")
             for expanded = (expand-file-name sub-dir sphinx-dir)
             unless (file-exists-p expanded) do
             (make-directory expanded t))
    (sphinx-install-config)
    (sphinx-install-prerequisites)
    (sphinx-maybe-initial-index)))

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

(provide 'sphinx-mode)
