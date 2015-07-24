(in-package :sphinx-mode-cl)

(defvar *searchd* "searchd")
(defvar *sphinx-dir* "")

(defun init ()
  (opts:define-opts
      (:name :help
             :description "Print this help text."
             :short #\h
             :long "help")
      (:name :verbose
             :description "Verbosity level."
             :short #\v
             :long "verbose"
             :arg-parser #'parse-integer)
    (:name :searchd
           :description "Sphinx searchd program location."
           :short #\s
           :long "searchd"
           :arg-parser #'identity
           :meta-var "FILE")
    (:name :configuration
           :description "Sphinx home directory (contains sphinx.conf)."
           :short #\c
           :long "configuration"
           :arg-parser #'parse-namestring
           :meta-var "DIR")))

(defun unknown-option (condition)
    (log:warn "~s option is unknown!~%" (opts:option condition))
    (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it ,@body)))

(defun config-logger (level)
  (let ((level (assoc (min level 4)
                      '((4 . :debug)
                        (3 . :info)
                        (2 . :warn)
                        (1 . :error)))))
    (when Level
      (format t "~&log level: ~s" (cdr level))
      (log:config (cdr level)))))

(defun read-arguments ()
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (config-logger 4)
          (log:error "Option ~s needs an argument!"
                     (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (config-logger 4)
          (log:error "Cannot parse ~s as argument of ~s"
                     (opts:raw-arg condition)
                     (opts:option condition))))
    ;; Here all options are checked independently, it's trivial to code any
    ;; logic to process them.
    (when-option (options :help)
      (opts:describe
       :prefix "Common Lisp interface to document indexer"
       :suffix ""))
    (when-option (options :verbose)
      (config-logger (getf options :verbose)))
    (when-option (options :searchd)
      (setf *searchd* (getf options :searchd)))
    (when-option (options :configuration)
      (setf *sphinx-dir* (getf options :configuration)))
    (log:debug "verbosity: ~a" (getf options :verbose))
    (log:debug "searchd: ~a" (getf options :searchd))
    (log:debug "configuration: ~a" (getf options :configuration))
    (log:debug "free args: ~{~a~^, ~}" free-args)))

;; sbcl --load ./sphinx-mode-cl.lisp -v 5 -c ~/.emacs.d/sphinx/ -s $(which searchd)
(defun start ()
  (init)
  (read-arguments)
  (ensure-searchd-started))

(defun start-interactive (searchd configuration &optional (verbosity 0))
  (setf *searchd* searchd
        *sphinx-dir* configuration)
  (config-logger verbosity)
  (ensure-searchd-started))
