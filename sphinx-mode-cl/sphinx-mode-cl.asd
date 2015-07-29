(defsystem "sphinx-mode-cl"
  :version "0.1"
  :author ("Oleg Sivokon <olegsivokon@gmail.com>"
           "Jack Zhang")
  :license "MIT"
  :depends-on ("alexandria" "iterate" "split-sequence" "cl-containers" "log4cl"
                            "clsql" "cl-sphinx-search" "unix-opts" "external-program")
  :entry-point "sphinx-mode-cl:start"
  :build-operation program-op
  :serial t
  :components ((:file "package")
               (:file "db")
               (:file "syscalls")
               (:file "main"))
  :description "Common Lisp part of Emacs sphinx-mode.")
