;;;; llama.lisp

(in-package #:nait.cnt.llama)

(defvar *courseid* ""
  "eg: CMPE1700")
(defvar *classid* ""
  "eg: CNT12")
(defvar *date* (get-universal-time))

(defvar *workdir*
  (merge-pathnames
   (make-pathname
    :directory '(:relative "llama"))
   (user-homedir-pathname)))

(ensure-directories-exist *workdir*)
