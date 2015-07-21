(in-package :cl)
(defpackage sphinx-mode-cl-asd (:use :cl :asdf))
(in-package :sphinx-mode-cl-asd)

(defsystem sphinx-mode-cl
  :version "0.1"
  :author ("Oleg Sivokon <olegsivokon@gmail.com>"
           "Jack Zhang")
  :license "MIT"
  :depends-on (:alexandria :iterate :split-sequence :cl-containers :clsql :cl-sphinx-search)
  :serial t
  :components ((:file "package")
               (:file "db"))
  :description "Common Lisp part of Emacs sphinx-mode.")
