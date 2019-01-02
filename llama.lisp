;;;; llama.lisp

(in-package #:nait.cnt.llama)

(defvar *courseid* ""
  "eg: CMPE1700")
(defvar *classid* ""
  "eg: 3D-A02")
(defvar *date* (get-universal-time))

(defvar *workdir*
  (namestring
   (merge-pathnames
    (make-pathname
     :directory '(:relative "llama"))
    (user-homedir-pathname))))

(ensure-directories-exist *workdir*)
(setf *default-pathname-defaults* (pathname *workdir*))
