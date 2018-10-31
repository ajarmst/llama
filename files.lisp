;;; Parts of this code based on code presented in
;;; Peter Seibel's "Practical Common Lisp"
;;; See top of database.lisp file for license details,
;;; which may apply here as well, although this code
;;; has been pretty heavily modified and is arguably no
;;; longer derivative.

(in-package :nait.cnt.llama)

(defun save-table-to-file (table filename)
  (with-open-file (out filename 
		       :direction :output
		       :if-exists :supersede)
		  (do-rows (row table)
			   (with-standard-io-syntax
			    (print row out)))))

(defun load-table-from-file (table filename)
  (delete-all-rows table)
  (with-open-file (in filename)
		  (with-standard-io-syntax
		   (do ((row (read in nil) (read in nil)))
		       ((eq row nil))
		     (insert-row row table)))))


