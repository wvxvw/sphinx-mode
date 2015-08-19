;; slime-adapter.el --- an interface to Sphinx, database search indexer.
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

;;* Sphinx Mode (SLIME Adapter)

(defvar sphinx-slime-process nil
  "This process should be the `slime-default-connection'
whenever we perform any communication with our backen.")

(defvar sphinx-cl-buffer nil
  "Buffer collecting output from `sphinx-cl-proc'")

(defvar sphinx-cl-proc nil
  "The process running sphinx-mode-cl")

(defun sphinx-slime-connect (&optional host port)
  (unless host (setq host "127.0.0.1"))
  (unless port (setq port 4055))
  (let ((lib-dir (sphinx-install-dir)))
    (when sphinx-cl-proc
      (kill-process sphinx-cl-proc))
    (setq sphinx-cl-buffer (get-buffer-create "*sphinx-cl-buffer*"))
    (with-current-buffer sphinx-cl-buffer (erase-buffer))
    (setq sphinx-cl-proc
          (start-process "sphinx-cl-proc"
                         sphinx-cl-buffer
                         (expand-file-name "sphinx-mode-cl/sphinx-mode-cl" lib-dir)
                         "-s" sphinx-searchd-program
                         "-c" (expand-file-name "sphinx.conf" lib-dir)))
    ;; we need to wait here for a while until the swank starts.
    (with-current-buffer sphinx-cl-buffer
      (with-local-quit
        (let (started)
          (while (not started)
            (accept-process-output)
            (message "buffer: %s"
                     (buffer-substring-no-properties
                      (point-min) (point-max)))
            (goto-char (point-min))
            (if (search-forward-regexp ";; Swank started at port:" nil t)
                (setf started t)
              (sleep-for 1))))))
    (setq sphinx-slime-process (slime-net-connect host port)
          slime-dispatching-connection sphinx-slime-process)
    (slime-setup-connection sphinx-slime-process)))

(defun sphinx-slime-eval (exp)
  (unless sphinx-cl-proc (sphinx-slime-connect))
  (let ((slime-default-connection sphinx-slime-process))
    (slime-eval exp)))

(defun sphinx-ping-cl ()
  (interactive)
  (message "received: %s" (sphinx-slime-eval '(sphinx-mode-cl::pong))))

(provide 'sphinx/slime-adapter)
