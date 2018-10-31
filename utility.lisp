;;;Assorted utilities for the LLAMA system

(in-package :nait.cnt.llama)

;;Functions for generating display strings
(defun string-underline* (n)
  "Returns a string composed of n -'s"
  (let ((out ""))
    (loop for i from 1 to n
	  do(setf out (concatenate 'string out "-")))
    out))

(defun string-underline (in)
  "Returns a string composed of -'s, the same length as in"
  (let ((out ""))
    (loop for x across in
	  do(setf out (concatenate 'string out "-")))
  out)) 

(defun string-student (student)
  "Generate a string from a student record"
  (with-column-values (lastname firstname middlename nickname) student
		      (format nil "~a~a~a~a" 
			      lastname 
			      (if (eq (length firstname) 0)
				  (format nil "")
				(format nil ", ~a" firstname))
			      (if (eq (length middlename) 0)
				  (format nil "")
				(format nil " ~a" middlename))
			      (if (eq (length nickname) 0)
				  (format nil "")
				(format nil " (~a)" nickname)))))

(defun string-student-id (student)
  "Generate a string from a student record, with ID"
  (with-column-values (studentid lastname firstname middlename nickname) student
		      (format nil "~a - ~a~a~a~a"
			      studentid
			      lastname 
			      (if (eq (length firstname) 0)
				  (format nil "")
				(format nil ", ~a" firstname))
			      (if (eq (length middlename) 0)
				  (format nil "")
				(format nil " ~a" middlename))
			      (if (eq (length nickname) 0)
				  (format nil "")
				(format nil " (~a)" nickname)))))

(defun string-student-id-only (student)
  "Generate a string from a student record, with ID only"
  (with-column-values (studentid) student
		      (format nil "~a"
			      studentid)))

(defun string-student-from-id(studentid)
  "String from first student matching a studentid"
  (let (records)
  (setf records (select :from *students*
			:where (matching *students*
					 :studentid studentid)))
  (format nil "~a" (string-student-id (nth-row 0 records)))))


(defun string-student-carnage-login (student)
  "Generate a string from a student record, attempting to generate probably carnage login"
  (with-column-values (lastname firstname) student
		      (format nil "~a~a"
			      (string-downcase (if (>= (length lastname) 7) (subseq lastname 0 7) lastname))
			      (string-downcase (subseq firstname 0 1)))))

(defun string-course (course)
  "Generates a string from a course record"
  (with-column-values (courseid name) course
		      (format nil "~a~a"
			      courseid
			      (if (eq (length name) 0)
				  (format nil "")
				(format nil " - ~a" name)))))

(defun string-course-description (course)
  "Generates a multiline string from a course record, with description"
  (with-column-values (courseid name description) course
		      (format nil "~a~a~a~%"
			      courseid
			      (if (eq (length name) 0)
				  (format nil "")
				(format nil " - ~a" name))
			      (if (eq (length description) 0)
				  (format nil "")
				(format nil "~%~a" description)))))

(defun string-event (event)
  "Generates a multiline string for an event"
  (with-column-values (courseid classid date coverage note) event
		      (format nil "~a (~a) - ~a~a~a~%"
			      courseid
			      classid
			      (string-date date)
			      (if (eq (length coverage) 0)
				  (format nil "")
				(format nil "~%Coverage: ~a" coverage))
			      (if (eq (length note) 0)
				  (format nil "")
				(format nil "~%Note: ~a" note)))))

(defun string-date (date)
  "Generates a string from a date"
  (if (or (null date) (eq date 0))

      (format nil "")
    (format-time nil "%a, %d %B %Y" date)))

(defun string-date-short (date)
  "Generates a string from a date, short format"
  (if (or (null date) (eq date 0))

      (format nil "")
    (format-time nil "%d %b %Y" date)))
  

(defun string-attendance (record)
  "Generates a string from an attendance record"
  (with-column-values (courseid date attendance note) record 
		      (format nil "~a (~a): ~a~a" 
			      (string-date date)
			      courseid
			      attendance
			      (if (eq (length note) 0)
				  (format nil "")
				(format nil " - ~a" note)))))

(defun latex-attendance-student (studentid courseid classid)
  "Generates latex output for non-present attendance entries"
  (let ((records (select :from *attendance*
			 :where (matching *attendance*
					  :studentid studentid
					  :courseid courseid
					  :classid classid)))
	(output "\\begin{footnotesize}~%\\begin{enumerate}~%")
	(have-output nil))
    (setq output (concatenate 'string 
			      (format nil "\\subsubsection*{Attendance: ~a}~%~%" 
				      (string-student-from-id studentid))
			      output))
    (do-rows (record records)
	     (with-column-values (attendance) record
				 (if (not (string-equal attendance "Present"))
				     (progn
				       (setf output (concatenate 'string output
								 "\\item{"
								 (string-attendance record)
								 "}~%"))
				       (setf have-output T)))))
    (if have-output	  
	(format nil "~a\\end{enumerate}~%\\end{footnotesize}~%" output)
      "")))
				     
(defun string-mark (mark)
  "Generates a string from a numeric mark.  Typically paired with get-mark"
  (cond
      ((null mark) (format nil "Inc"))
      ((< mark 0) (format nil "Inc"))
      (t (format nil "~a" mark))))

(defun num-tasks (categoryid)
  "Return the total number of tasks for a given category"
  (let ((tasks (get-tasks* categoryid)))
    (table-size tasks)))

(defun task-maximum (taskid) ;nil if not found 
  "Return the maximum score (the 'out-of') for a task"
  ;Get Task
  (let ((tasks (select :from *tasks*
		       :where (matching *tasks*
					:taskid taskid))))
    (if (not (= 0 (table-size tasks)))
		  (column-value (nth-row 0 tasks) :outof)
      nil)))

(defun course-average (taskid &optional (float t) (count-incomplete-tasks nil)) ; Assumes 0 for incomplete
"Return the average for all students in all courses for a task"
;Get Marks
(let ((marks (select :from *marks*
		     :where (matching *marks*
				      :taskid taskid)))
      (total 0) ; Total of all marks
      (count 0) ; Number of marks
      (current 0)) ; Value of current mark
  (do-rows (mark marks)
	   (progn
	     (setf current (column-value mark :mark))
	     (if (< current 0)
		 (if (not (null count-incomplete-tasks)) (incf count))
	       (progn
		 (incf count)
		 (incf total current)))))
  (if float
      (float (/ total count))
    (/ total count))))

(defun course-average-percent (taskid &optional (count-incomplete-tasks nil))
  (float (* 100 (/ (course-average taskid count-incomplete-tasks) 
	    (task-maximum taskid)))))

(defun class-average (classid taskid &optional (count-incomplete-tasks nil) (float t))
"Return the class average for all students in a class for a task"
;Get Marks
(let ((marks (select :from *marks*
		     :where (matching *marks*
				      :taskid taskid)))
      (students (get-students classid));Students in that class
      (total 0) ; Total of all marks
      (count 0) ; Number of marks
      (current 0) ; Value of current mark
      (studentid 0)) ; Current student
  (do-rows (mark marks)
	   (progn
	     (setf current (column-value mark :mark))
	     (setf studentid (column-value mark :studentid))
	     (if (student-in-class studentid students)
		 (progn
		   (if (< current 0)
		       (if (not (null count-incomplete-tasks)) (incf count))
		     (progn
		       (incf count)
		       (incf total current)))))))
  (if (= count 0)
      nil
    (if float
	(float (/ total count))
      (/ total count)))))

(defun class-average-percent (classid taskid &optional (count-incomplete-tasks nil))
  (let ((average (class-average classid taskid count-incomplete-tasks)))
    (if average
	(float (* 100 (/ average (task-maximum taskid))))
      nil)))

(defun student-category-average (studentid courseid category)
  "Returns the weighted average for the students tasks in the category (raw fraction)"
  (let ((tasks (get-tasks courseid category))
	(total 0)
	(count 0))
    (loop for task across (rows tasks) do
	  (let ((mark (get-mark studentid (column-value task :taskid))))
	    (incf total (* (column-value task :weight)
		 (/ (if (and mark (>=  mark 0))
			(get-mark studentid (column-value task :taskid))
		      0)
		  (column-value task :outof))))
	    (incf count (column-value task :weight))))
    (if (> count 0) (/ total count) nil)))
	  
(defun student-category-average-percent (studentid courseid category)
  "Returns the above as a percentage"
  (let ((average (student-category-average studentid courseid category)))
    (if average
	(float (* 100  average))
      nil)))

(defun student-final-mark-percent (studentid courseid)
  "Returns the current percent final mark (treating unfinished as zero)"
  (let ((categories (get-categories courseid))
	(total 0)
	(count 0))
    (do-rows (category categories)
	     (let ((average 
		    (student-category-average-percent
		     studentid courseid (column-value category :name))))
	       (incf total (* (if average average 0) (column-value category :weight))))
	     (incf count (column-value category :weight)))
    (/ total count)))

(defun student-final-mark (studentid courseid)
  "Above as a fractional value"
  (/ (student-final-mark-percent studentid courseid) 100))

(defun latex-marks-category-table (studentid courseid category
				  &optional (max-width 10))
 "Generates 'tabular' summaries for a student and course"
(let* ((tables "")
      (cols (num-tasks (categoryid-from-name courseid category))))
  (if (and (> cols 0))
      (if (<= cols max-width)
	  (setf tables (concatenate 'string tables 
		       (latex-marks-category-table-range 
			studentid courseid category)))
	  (progn
	    (let ((tablecount (floor (/ cols max-width))))
	      (if (= 0 (mod cols max-width)) (decf tablecount))
	      (loop for j from 0 to tablecount do
		    (setf tables 
			  (concatenate 'string tables
				       (latex-marks-category-table-range
					studentid courseid category
					(* max-width j) 
					(+ (* max-width j) (- max-width 1))
					(= j tablecount)))))))))
  (format nil tables)))

(defun latex-marks-category-table-range (studentid courseid category
					 &optional
					 (col-start 0) ; 0 means first
					 (col-end nil) ; nil means last
					 (print-average t))
"Generates a 'tabular' entry string for a range of columns"
(let ((table "\\begin{tabular}{|")
      (cols (num-tasks (categoryid-from-name courseid category)))
      (tasks (get-tasks courseid category))
      (i 0))
  (if (and (> cols 0) (<= col-start (- cols 1)))
      (progn 
	(setf col-end (min (or col-end (- cols 1)) (- cols 1)))
	(loop for i from col-start to (+ col-end 1) do
	      (setf table (concatenate 'string table "r|")))
	(setf table (concatenate 'string table "}\\hline~%"))
	(setf table (concatenate 'string table "{\\bf " category "}"))
	(setf i 0);;FIXME: And all the others like this here
	(setf i 0)
	(do-rows (task tasks)
	  (if (and (>= i col-start) (<= i col-end))
		 (setf table (concatenate 'string table " & {\\bf "
					  (column-value task :index) "}")))
	(incf i))
	(setf table (concatenate 'string table "\\\\\\hline\\hline~%"))
	
	(setf table (concatenate 'string table "{\\bf  Mark}"))
	(setf i 0)
	(do-rows (task tasks)
	  (if (and (>= i col-start) (<= i col-end))
		 (setf table (concatenate 'string table " &  "
					  (get-mark-string* studentid
							    (column-value task :taskid)) "")))
	  (incf i))
	(setf table (concatenate 'string table "\\\\\\hline~%"))
	
	(setf table (concatenate 'string table "{\\bf  Out of}"))
	(setf i 0)
	(do-rows (task tasks)
	  (if (and (>= i col-start) (<= i col-end))
		 (setf table (concatenate 'string table " &  "
					  (format nil "~,1f" (column-value task :outof)) "")))
	  (incf i))
	(setf table (concatenate 'string table "\\\\\\hline~%"))
	
	(setf table (concatenate 'string table "{\\bf  Percent}"))
	(setf i 0) 
	(do-rows (task tasks)
	  (if (and (>= i col-start) (<= i col-end))
		 (setf table (concatenate 'string table " &  "
					  (get-mark-percent-string* studentid
								    (column-value task :taskid)) "")))
	  (incf i))
	(setf table (concatenate 'string table "\\\\\\hline~%"))
	
	(setf table (concatenate 'string table "{\\bf  Weight}"))
	(setf i 0)
	(do-rows (task tasks)
	  (if (and (>= i col-start) (<= i col-end))
		 (setf table (concatenate 'string table " &  "
					  (format nil "~,1f" (column-value task :weight)) "")))
	  (incf i))
	(setf table (concatenate 'string table "\\\\\\hline~%"))
	(if print-average
	    (progn
	      (setf table (concatenate 'string table "{\\bf  Average} & "))
	      (setf table (concatenate 'string table "\\multicolumn{"))
	      (setf table (concatenate 'string table (format nil "~a" (+ (- col-end col-start) 1))))
	      (setf table (concatenate 'string table "}{|r|}{{\\bf " 
				       (format nil "~,1f" (student-category-average-percent studentid courseid category))
				       "}} \\\\\\hline~%"))))
	(setf table (concatenate 'string table "\\end{tabular}~%"))
	(if (not print-average) (setf table (concatenate 'string table "~%")))
	(format nil table))
    "")))

(defun latex-marks-summary-table (studentid courseid 
				  &optional (max-width 10))
 "Generates 'tabular' summaries of final mark for a student and course"
(let* ((tables "")
       (categories (get-categories courseid))
       (cols (table-size categories)))
  (if (and (> cols 0))
      (if (<= cols max-width)
	  (setf tables (concatenate 'string tables
		       (latex-marks-summary-table-range 
			studentid courseid)))
	  (progn
	    (let ((tablecount (floor (/ cols max-width))))
	      (if (= 0 (mod cols max-width)) (decf tablecount))
	      (loop for j from 0 to tablecount do
		    (setf tables 
			  (concatenate 'string tables
				       (latex-marks-summary-table-range
					studentid courseid
					(* max-width j) 
					(+ (* max-width j) (- max-width 1))
					(= j tablecount)))))))))
  (format nil tables)))

(defun latex-marks-summary-table-range (studentid courseid  
					&optional
					(col-start 0) ;0 means first
					(col-end nil) ;nil means last
					(print-total t))
"Generates a 'tabular' summary of final mark for a range of columns"
(let* ((table "\\begin{tabular}{|")
       (categories (get-categories courseid))
       (cols (table-size categories))
       (i 0))
  (if (and (> cols 0) (<= col-start (- cols 1)))
      (progn
	(setf col-end (min (or col-end (- cols 1)) (- cols 1))) 
	(loop for i from col-start to (+ col-end 1) do
	      (setf table (concatenate 'string table "r|")))
	(setf table (concatenate 'string table "}\\hline~%"))
	(setf table (concatenate 'string table "{\\bf Summary}"))
	;;FIXME
	(setf i 0)
	(do-rows (category categories)
	  (if (and (>= i col-start) (<= i col-end))
	      (setf table (concatenate 'string table " & {\\bf "
				       (column-value category :name) "}")))
	  (incf i))
	(setf table (concatenate 'string table "\\\\\\hline\\hline~%"))
	(setf table (concatenate 'string table "{\\bf  Average}"))
        ;;FIXME
	(setf i 0)
	(do-rows (category categories)
	  (if (and (>= i col-start) (<= i col-end))
	      (setf table (concatenate 'string table " &  "
				       (format nil "~,1f" 
					       (student-category-average-percent 
						studentid 
						courseid 
						(column-value category :name))))))
	  (incf i)) 
	(setf table (concatenate 'string table "\\\\\\hline~%"))
	(setf table (concatenate 'string table "{\\bf  Weight}"))
        ;;FIXME
	(setf i 0)
	(do-rows (category categories)
	  (if (and (>= i col-start) (<= i col-end))
	      (setf table (concatenate 'string table " &  "
				       (format nil "~,1f" 
					       (column-value category :weight)) "")))
	  (incf i))
	(setf table (concatenate 'string table "\\\\\\hline~%"))
	(if print-total
	    (progn
	      (setf table (concatenate 'string table "{\\bf  Total} & "))
	      (setf table (concatenate 'string table "\\multicolumn{"))
	      (setf table (concatenate 'string table (format nil "~a" (+ 1 (- col-end col-start)))))
	      (setf table (concatenate 'string table "}{|r|}{{\\bf " 
				     (format nil "~,1f" (student-final-mark-percent studentid courseid))
				     "}} \\\\\\hline~%"))))
	(setf table (concatenate 'string table "\\end{tabular}~%"))
	(if (not print-total) (setf table (concatenate 'string table "~%")))
	(format nil table))
  "")))

(defun latex-marks-student-sheet (studentid courseid)
"Generates a latex two-sided page for a student's marks"
 (let* ((categories (get-categories courseid))
       (classid (classid-from-studentid studentid))
       (title (format nil "~a - ~a (~a)" 
		      (string-student-from-id studentid) courseid classid))
       (page (concatenate 'string "\\newpage~%\\section*{~~~~~~~~~~~~~~~~~~" title "}~%")))
   (do-rows (category categories)
	    (setf page (concatenate 'string page
				    (latex-marks-category-table studentid courseid
								(column-value category :name))))
	    (setf page (concatenate 'string page "~%~%")))
   (setf page (concatenate 'string page "\\cleardoublepage~%~%"))))

(defun latex-marks-student-final-sheet (studentid courseid)
"Generates a latex two-sided page for a student's marks"
 (let* ((categories (get-categories courseid))
       (classid (classid-from-studentid studentid))
       (title (format nil "~a - ~a (~a)" 
		      (string-student-from-id studentid) courseid classid))
       (page (concatenate 'string "\\newpage~%\\section*{~~~~~~~~~~~~~~~~~~" title "}~%")))
   (do-rows (category categories)
	    (setf page (concatenate 'string page
				    (latex-marks-category-table studentid courseid
								(column-value category :name))))
	    (setf page (concatenate 'string page "~%~%")))
   (setf page (concatenate 'string page (latex-marks-summary-table studentid courseid)))
   (setf page (concatenate 'string page "~%\\clearpage~%~%"))
   (setf page (concatenate 'string page "~%" (latex-attendance-student studentid courseid classid)))
   (setf page (concatenate 'string page "~%\\cleardoublepage~%~%"))))
