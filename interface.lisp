;;Interface functions.  These are the ones the user calls
;; Note - (load-tables) should be called before any of these
;; and (save-tables) after done.
;;Interface functions
 ;TODO: These are too hard-coded.  Need some generic
 ;      schema-driven "make-row-interactive" type stuff  

(in-package :nait.cnt.llama)

;Set globals
(defun set-class (courseid classid date)
  (if (or (null date) (equal date ""))
      (setf *courseid* courseid *classid* classid *date* (get-universal-time))
  (setf *courseid* courseid *classid* classid *date* (parse-time date))) 
  (if (null *date*)
      (error "Could not parse time string")
  (format nil "~a (~a) - ~a" *courseid* *classid* (format-time nil "%a, %d %b %Y" *date*))))

(defun set-class-interactive()
"Interactively set class info, with defaults"
(let ((courseid *courseid*) (classid *classid*) (date (get-universal-time)))
(if (string= courseid "") (setf courseid "CNT234"))
(if (string= classid "") (setf classid "NET11 - 2I"))
(set-class (prompt-read-default "Course" courseid)
	   (prompt-read-default "Class" classid)
	   (prompt-read-default "Date" (format-time nil "%d %b %Y" date)))))
  
;Add an attendance record for a student
;Depends on current globals from set-class
(defun attendance-student (studentid)
  "Interactively sets the attendance for a particular student, based on global class"
  ;Retrieve any current record
  (let  (attendance ; Attendance Note
	 note ; General note
	 new-attendance ; Input from user
	 new-note ;Input from user
	 record) ;Data from memory
  ;Going to need some tables - specs say load 'em 
  ;(load-table attendance) ; This function gets called from something else that does this
  ;Get any existing record
    (setf record (select :from *attendance* 
			 :where (matching *attendance*
					  :studentid studentid
					  :courseid *courseid*
					  :classid *classid*
					  :date *date*)))
  ;Determine the current stored attendance note (default = "Absent", "")
    (if (= 0 (table-size record))
	(progn 
	  (setf attendance "Absent")
	  (setf note ""))
      (progn
	(setf attendance (column-value (nth-row 0 record) :attendance))
	(setf note (column-value (nth-row 0 record) :note))))
  ;Get a new attendance statement from the user
    (setf new-attendance (prompt-read (concatenate 'string "Attendance (" attendance ")")))
    (if (string= new-attendance "")
      (setf new-attendance attendance)
      (if (string= new-attendance " ")
	  (setf attendance "Present")
	(setf attendance new-attendance)))
  ;Get a new note from the user
    (setf new-note (prompt-read (concatenate 'string "Note (" note  ")")))
    (if (string= new-note "")
	(setf new-note note)
      (setf note new-note))
   ;Delete old record (if exists)
    (delete-rows :from *attendance*
		 :where (matching *attendance*
				  :studentid studentid
				  :courseid *courseid*
				  :classid *classid*
				  :date *date*))
  
   ;Insert new record
    (insert-row (list :studentid studentid :courseid *courseid* :classid *classid* :date *date*
		      :attendance attendance :note note) *attendance*)
  ;(save-table attendance) ; declining to do this for efficiency reasons
    ))

					
(defun attendance-student-nonote (studentid)
  "Interactively sets the attendance for a particular student, based on global class, without modifying the note"
  (let (attendance ; Attendance Note
	note ; General note
	new-attendance; Input from user
	record) ; Database returns
  ;Get any existing record
    (setf record (select :from *attendance* 
			 :where (matching *attendance*
					  :studentid studentid
					  :courseid *courseid*
					  :classid *classid*
					  :date *date*)))
   ;Determine the current stored attendance note (default = "Absent", "")
    (if (= 0 (table-size record))
	(progn 
	  (setf attendance "Absent")
	  (setf note ""))
      (progn
	(setf attendance (column-value (nth-row 0 record) :attendance))
	(setf note (column-value (nth-row 0 record) :note))))
    ;Get a new attendance statement from the user
    (setf new-attendance (prompt-read (concatenate 'string "Attendance (" attendance ")")))
    (if (string= new-attendance "")
	(setf new-attendance attendance)
      (if (string= new-attendance " ")
	  (setf attendance "Present")
	(setf attendance new-attendance)))
				
    ;Delete old record (if exists)
    (delete-rows :from *attendance*
		 :where (matching *attendance*
				  :studentid studentid
				  :courseid *courseid*
				  :classid *classid*
				  :date *date*))
  
     ;Insert new record
    (insert-row (list :studentid studentid :courseid *courseid* :classid *classid* :date *date*
		      :attendance attendance :note note) *attendance*)))
					
(defun attendance* ()
  "Does attendance for current class"
  (let ((students (get-students *classid*)))
    (do-rows (student students)
	     (format *query-io* "~a, " (string-student student))
	     (force-output *query-io*)
	     (attendance-student-nonote (column-value student :studentid)))))
  
(defun attendance ()
  "Does attendance for a class, after interactively asking for the class info"
  (set-class-interactive) ; Set the globals
  (attendance*))

(defun event*()
  "Do an event entry for current class"
  (insert-row (list :courseid *courseid*
		    :classid *classid*
		    :date *date*
		    :coverage (prompt-read "Coverage")
		    :note (prompt-read "Note")) *events*)
  (string-event (nth-row (- (table-size *events*) 1) *events*)))

(defun event()
  "Do and event entry after interactively asking for class info"
  (set-class-interactive)
  (event*))

(defun tex-class-list* ()
  "Generate a TeX document classlist - save in local directory based on class"
  (with-open-file (out (concatenate 'string *courseid* "." *classid* ".Signin.tex")
		       :direction :output
		       :if-exists :supersede)
		  (format out "\\documentclass[10pt,letterpaper,twoside]{report}~%")                          
		  (format out "\\usepackage{aja}~%")
		  (format out "\\begin{document}~%")
		  (format out "\\course{~a}~%" *courseid*)                            
		  (format out "\\coursetitle{~a}~%" (course-name *courseid*))
		  (format out "\\doctype{Class List}~%")                                  
		  (format out "\\docnumber{1}~%")                                 
		  (format out "\\doctitle{~a}~%" *classid*)
		  (format out "\\docdate{~a}~%" (string-date-short *date*))
		  (format out "\\maketitle~%")
		  (format out "\\begin{flushright}~%")
		  (format out "\\line(1,0){225}~%")
		  (format out "\\end{flushright}~%") 
		  (format out "\\renewcommand{\\tabcolsep}{10 pt}~%")
		  (format out "\\renewcommand{\\arraystretch}{1.6}~%")
		  (format out "\\begin{center}~%")
		  (format out "\\begin{tabular*}{16 cm}[t]{|l|c||c||@{\\extracolsep{\\fill}}r|} \\hline~%")
		  (format out "{\\bf Name} & {\\bf ~~~~~~~~~~} &~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~& {\\bf Signature}\\\\ \\hline \\hline~%")
		  (let ((students (get-students *classid*)))
		    (do-rows (student students)
			     (format out "~a&&&\\\\ \\hline~%" (string-student student))))
		  (format out "\\end{tabular*}~%")
		  (format out "\\end{center}~%")
		  (format out "\\end{document}~%")
		  (finish-output out)))

(defun tex-class-list ()
  "Generate a TeX document classlist - after interactively asking for class info"
  (set-class-interactive)
  (tex-class-list*))

(defun tex-marks-list* (&optional (colno 10) (colwidth 5)); Number of marks columns, width in spaces
  "Generate a TeX document classlist - save in local directory based on class"
  (with-open-file (out (concatenate 'string *courseid* "." *classid* ".Marks.tex")
		       :direction :output
		       :if-exists :supersede)
		  (format out "\\documentclass[10pt,letterpaper,twoside]{report}~%")                          
		  (format out "\\usepackage{aja}~%")
		  (format out "\\begin{document}~%")
		  (format out "\\course{~a}~%" *courseid*)                            
		  (format out "\\coursetitle{~a}~%" (course-name *courseid*))
		  (format out "\\doctype{Marks}~%")                                  
		  (format out "\\docnumber{1}~%")                                 
		  (format out "\\doctitle{~a}~%" *classid*)
		  (format out "\\docdate{~a}~%" (string-date-short *date*))
		  (format out "\\maketitle~%")
		  (format out "\\begin{flushright}~%")
		  (format out "\\line(1,0){225}~%")
		  (format out "\\end{flushright}~%") 
		  (format out "\\renewcommand{\\tabcolsep}{10 pt}~%")
		  (format out "\\renewcommand{\\arraystretch}{1.6}~%")
		  (format out "\\begin{center}~%")
		  (format out "\\begin{tabular*}{16 cm}[t]{|l|")
		  (loop repeat (- colno 1)
			do (format out "c|"))
		  (format out "@{\\extracolsep{\\fill}}c|")
		  (format out "} \\hline~%")
		  (format out "{\\bf Name} ")
		  (loop repeat colno
			do (progn 
			     (format out "& {")
			     (loop repeat colwidth
				   do (format out "~~"))
			     (format out "} ")))
		  (format out "\\\\ \\hline \\hline~%")
		  (let ((students (get-students *classid*)))
		    (do-rows (student students)
			     (format out "~a" (string-student student))
			     (loop repeat colno
				   do (format out "&"))
			     (format out "\\\\ \\hline~%")))
		  (format out "\\end{tabular*}~%")
		  (format out "\\end{center}~%")
		  (format out "\\end{document}~%")
		  (finish-output out)))

(defun tex-marks-list ()
  "Generate a TeX document markslist - after interactively asking for class info"
  (set-class-interactive)
  (tex-marks-list*))

(defun print-students-class (classid)
  "Print a list of students for a class"
  (let (records)  
    (setf records (select :from *classes*
			:where (matching *classes*
					 :classid classid)))
    (do-rows (student records)
	     (with-column-values (studentid) student
				 (format t "~a~%" 
					 (string-student-from-id studentid))))))

(defun print-student-attendance (studentid)
  "Print a list of student attendance entries"
  (let (records)
  (setf records (select :from *attendance*
			:where (matching *attendance*
					 :studentid studentid)))
  (format *query-io* "~a:~%" (string-student-from-id studentid))
  (do-rows (record records)
	   (format *query-io* "     ~a~%" (string-attendance record)))
  (finish-output *query-io*)))

(defun print-student-attendance-not-present (studentid)
  "Print a list of student attendance entries, excluding ones with 'present'"
  (let (records)
  (format *query-io* "~a:~%" (string-student-from-id studentid))
  (setf records (select :from *attendance*
			:where (matching *attendance*
					 :studentid studentid)))
  (do-rows (record records)
	   (with-column-values (attendance) record
			       (if (not (string-equal attendance "Present"))
				   (format *query-io* "     ~a~%" (string-attendance record)))))
  (finish-output *query-io*)))

(defun print-attendance-class (courseid classid)
  "Prints non-present attendance for students in class"
  (let (records)
    (setf records (select :from *classes* 
			  :where (matching *classes*
					   :classid classid
					   :courseid courseid)))
    (format *query-io* "------~%~a~%------~%~%" classid)
    (do-rows (student records)
	     (with-column-values (studentid) student
		   (print-student-attendance-not-present studentid)))
    (format *query-io* "~%~%")
    (finish-output *query-io*)))
			  
(defun print-category-names (courseid)
  "Print out the category names corresponding to a particular course"
  (let ((categories (get-categories courseid)))
    (do-rows (category categories)
	     (format *query-io* "~a~%"
		     (column-value category :name)))
    (finish-output *query-io*)))

(defun mark-student (studentid taskid)
  "Interactively set a mark for a student, with defaults"
  (let ((mark -1)     ;Current Mark (negative = incomplete)
	new-mark      ;Input Mark
	record)       ;Current saved record
    ;Get any existing record
    (setf record (select :from *marks*
			 :where (matching *marks*
					  :studentid studentid
					  :taskid taskid)))
    ;Default already set, do we update?
    (if (not (= 0 (table-size record)))
	(setf mark (column-value (nth-row 0 record) :mark)))
    (if (< mark 0)
	(setf mark "Inc"))
    ;Get new mark from user
    (setf new-mark
	  (prompt-read (concatenate 'string (format nil "Mark (~a)" mark))))
    (if (stringp new-mark)
	(setf new-mark
	      (cond
	       ((string= new-mark "") mark)
	       ((string= new-mark "") -1)
	       ((string= new-mark "inc") -1)
	       ((string= new-mark "Inc") -1)
	       (t new-mark))))
    (if (stringp new-mark) ;Convert strings to int rep
	(progn
	;Handle special case
	  (if (or (string= new-mark "inc") (string= new-mark "Inc"))
	      (setf new-mark "-1"))
	  (setf new-mark (parse-number:parse-number new-mark))))
    ;Insert/update new mark
    (set-mark studentid taskid new-mark)))
   
(defun get-taskid* ()
  "Interactively determines a task id, or creates one for current class"
  ;This function assumes global class/date info is good
  (print-category-names *courseid*)
  (let ((categoryid nil) 
	(taskid nil)
	(index "")
	(outof nil)
	(weight 1)
	(note ""))
  ;Get a valid categoryid.  Uses loop (ewwwwww!)
    (loop while (null categoryid) do
      (setf categoryid (categoryid-from-name *courseid*
			(prompt-read "Category (from the above)"))))
    ;Get the index
    (setf index (prompt-read-default "Index" index))
    ;Does the item already exist?
    (setf taskid (taskid-from-categoryid-index categoryid index))
    (if (null taskid)
	(progn
	  (setf outof (prompt-read-number-default "Out of" 100))
	  (setf weight (prompt-read-number-default "Weight" 1))
	  (setf note (prompt-read-default "Note" ""))
	  (setf taskid (add-task categoryid index outof weight note)))
      taskid)))
    
(defun get-taskid ()
  "Interactively determines a task id, or creates one after interactively setting class info"
  (set-class-interactive)
  (get-taskid*))

(defun marks* (&optional taskid)
  "Does marks for current class"
  (let ((students (get-students *classid*)))
    ;Do I need a taskid
    (if (null taskid) 
	(setf taskid (get-taskid*)))
    (do-rows (student students)
	     (format *query-io* "~a, " (string-student student))
	     (force-output *query-io*)
	     (mark-student (column-value student :studentid) taskid))))
  
(defun marks ()
  "Does marks for a class, after interactively asking for the class info"
  (set-class-interactive) ; Set the globals
  (marks* (get-taskid*)))

(defun print-class-marks* (courseid classid)
  "Prints the current marks for the class, in columnar format, to console"
  (let ((students (get-students classid))
	(categories (get-categories courseid)))
    (do-rows (category categories)
	     (let ((tasks (get-tasks courseid (column-value category :name))))
	       (format *query-io* "~25@a" (column-value category :name))
	       (do-rows (task tasks)
			(format *query-io* "~10@a" (column-value task :index)))
	       (format *query-io* "~%")
	       (format *query-io* "-------------------------")
	       (do-rows (task tasks)
			(format *query-io* "----------"))
	       (format *query-io* "~%")
	       (do-rows (student students)
			(format *query-io* "~25@a" (string-student student))
			(do-rows (task tasks)
				 (format *query-io* "~10@a"
					 (get-mark-string* (column-value student :studentid)
						   (column-value task :taskid))))
			(format *query-io* "~%"))
	       (format *query-io* "-------------------------")
	       (do-rows (task tasks)
			(format *query-io* "----------"))
	       (format *query-io* "~%")
	       (format *query-io* "~25@a" "Out of:")
	       (do-rows (task tasks)
			(format *query-io* "~10,1F" (column-value task :outof)))
	       (format *query-io* "~%")
	       (format *query-io* "~25@a" "Average:")
	       (do-rows (task tasks)
			(format *query-io* "~10,1F" 
				(class-average classid (column-value task :taskid) t t)))
	       (format *query-io* "~%")
	       (format *query-io* "~25@a" "Average %:")
	       (do-rows (task tasks)
			(format *query-io* "~10,1F" 
				(class-average-percent classid (column-value task :taskid) t)))
	       (format *query-io* "~%")
	       (format *query-io* "-------------------------")
	       (do-rows (task tasks)
			(format *query-io* "----------"))
	       (format *query-io* "~%")
	     (format *query-io* "~%~%"))
    (finish-output *query-io*))))

(defun print-student-marks* (studentid courseid)
 "Prints a summary of marks for a student in a class to console"
 (let* ((categories (get-categories courseid))
       (classid (classid-from-studentid studentid))
       (title (format nil "~a - ~a (~a)" (string-student-from-id studentid) courseid classid)))
   (format *query-io* "~a~%~a~%~a~%~%" 
	   (string-underline title) 
	   title 
	   (string-underline title))
   (do-rows (category categories)
	    (let ((tasks (get-tasks courseid (column-value category :name))))
	      (format *query-io* "~a" (string-underline* 25))
	      (do-rows (task tasks)
		       (format *query-io* "~a" (string-underline* 10)))
	      (format *query-io* "~%")
	      (format *query-io* "~25@a" (column-value category :name))
	      (do-rows (task tasks)
		       (format *query-io* "~10@a" (column-value task :index)))
	      (format *query-io* "~%")
	      (format *query-io* "~a" (string-underline* 25))
	      (do-rows (task tasks)
		       (format *query-io* "~a" (string-underline* 10)))
	      (format *query-io* "~%")
	      (format *query-io* "~25@a" "Mark:")
	      (do-rows (task tasks)
		       (format *query-io* "~10@a"
			       (get-mark-string* studentid 
						 (column-value task :taskid))))
	      (format *query-io* "~%")
	      (format *query-io* "~25@a" "Out of:")
	      (do-rows (task tasks)
		       (format *query-io* "~10,1F" (column-value task :outof)))
	      (format *query-io* "~%")
	      (format *query-io* "~25@a" "Percent:")
	      (do-rows (task tasks)
		       (format *query-io* "~10@a" 
			       (get-mark-percent-string* studentid
						 (column-value task :taskid))))
	      (format *query-io* "~%")
	      (format *query-io* "~25@a" "Weight:")
	      (do-rows (task tasks)
		       (format *query-io* "~10,1F" (column-value task :weight)))
	      (format *query-io* "~%")
	      (format *query-io* "~a" (string-underline* 25))
	      (do-rows (task tasks)
		       (format *query-io* "~a" (string-underline* 10)))
	      (format *query-io* "~%")
	      (let* ((width (* 10 (num-tasks (column-value category :categoryid))))
		     (fstring (concatenate 'string "~25@a" "~" (format nil "~a" width) ",1F~%")))
		(format *query-io* fstring "Average:" 
			(student-category-average-percent
			 studentid courseid 
			 (column-value category :name))))				   
	      (format *query-io* "~a" (string-underline* 25))
	      (do-rows (task tasks)
		       (format *query-io* "~a" (string-underline* 10)))
	      (format *query-io* "~%~%")
	      (finish-output *query-io*)))))

(defun latex-marks-summary (courseid classid)
  "Generates a set of marks summary sheets for a class, saves as LaTeX"
  (let ((students (get-students classid)))
    (with-open-file (out (concatenate 'string courseid "." classid ".Summary.tex")
			 :direction :output
			 :if-exists :supersede)
		    (format out "\\documentclass[10pt,letterpaper,twoside]{report}~%")                          
		    (format out "\\usepackage{aja}~%")
		    (format out "\\begin{document}~%")
		    (format out "\\course{~a}~%" courseid)                            
		    (format out "\\coursetitle{~a}~%" (course-name courseid))
		    (format out "\\doctype{Mark Summaries}~%")                                  
		    (format out "\\docnumber{1}~%")                                 
		    (format out "\\doctitle{~a}~%" classid)
		    (format out "\\docdate{~a}~%" (string-date-short (get-universal-time)))
		    (format out "\\maketitle~%")
		    (format out "~%~%")
		    (format out "\\subsection*{Marks Summary Sheets}~%~%")
		    (format out "\\cleardoublepage")
		    (do-rows (student students)
			     (format out (latex-marks-student-sheet
					  (column-value student :studentid) courseid)))
		    (format out "\\end{document}~%~%")
		    (finish-output out))))

(defun latex-final-marks-summary (courseid classid)
  "Generates a set of marks summary sheets for a class, saves as LaTeX"
  (let ((students (get-students classid)))
    (with-open-file (out (concatenate 'string courseid "." classid ".Final.tex")
			 :direction :output
			 :if-exists :supersede)
		    (format out "\\documentclass[10pt,letterpaper,twoside]{report}~%")                          
		    (format out "\\usepackage{aja}~%")
		    (format out "\\begin{document}~%")
		    (format out "\\course{~a}~%" courseid)                            
		    (format out "\\coursetitle{~a}~%" (course-name courseid))
		    (format out "\\doctype{Mark Summaries}~%")                                  
		    (format out "\\docnumber{1}~%")                                 
		    (format out "\\doctitle{~a}~%" classid)
		    (format out "\\docdate{~a}~%" (string-date-short (get-universal-time)))
		    (format out "\\maketitle~%")
		    (format out "~%~%")
		    (format out "\\subsection*{Marks Summary Sheets}~%~%")
		    (format out "\\cleardoublepage")
		    (do-rows (student students)
			     (format out (latex-marks-student-final-sheet
					  (column-value student :studentid) courseid)))
		    (format out "\\end{document}~%~%")
		    (finish-output out))))
    
