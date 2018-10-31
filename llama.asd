;;;; llama.asd

(asdf:defsystem #:llama
  :description "Lisp Attendance and Marking Application"
  :author "AJ Armstrong <aja@nait.ca>"
  :license "BSD"
  :depends-on (#:parse-number)
  :serial t
  :components ((:file "package")
               (:file "llama")
               (:file "database")
               (:file "time")
               (:file "schemas")
               (:file "files")
               (:file "utility")
               (:file "io")
               (:file "tables")
               (:file "interface")))
