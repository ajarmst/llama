;;;; llama.asd

(asdf:defsystem #:nait.cnt.llama
  :description "Lisp Attendance and Marking Application"
  :author "AJ Armstrong <aja@nait.ca>"
  :license "BSD"
  :depends-on (#:parse-number)
  :serial t ;Load the below in order, not bothering with dep parsing
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
