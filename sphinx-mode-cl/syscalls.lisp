(in-package :sphinx-mode-cl)

(defun sphinx-config ()
  (merge-pathnames #P"etc/sphinx.conf" *sphinx-dir*))

(defun ensure-searchd-started ()
  (multiple-value-bind (exited status)
      (external-program:run *searchd* (list "-c" (sphinx-config)))
    (unless (zerop status)
      (log:error "Couldn't start searchd, run as ~s -c ~s"
                 *searchd* (sphinx-config)))))
