;;;Input/Output functions for the LLama system
;;; Functions the serve for retrieval/storage of data to the memory tables
;;; NOT Functions for reading and writing from disk files (see files.lisp)

(in-package :nait.cnt.llama)

;;;Function from Seibel's PCL book.  See database.lisp for licence authority
(defun prompt-read (prompt)
  "Sends a prompt and returns what the user types in"
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-read-default (prompt default)
  "Sends a prompt and returns what the user types in - return selects default"
  (let (temp)
    (format *query-io* "~a: (~a) " prompt default)
    (force-output *query-io*)
    (setf temp (read-line *query-io*))
    (if (string= temp "")
	default
      temp)))

(defun prompt-read-number (prompt)
  (let ((temp (prompt-read prompt)))
    (if (stringp temp)
	(parse-number:parse-number temp)
      temp)))

(defun prompt-read-number-default (prompt default)
  (let ((temp (prompt-read-default prompt default)))
    (if (stringp temp)
	(parse-number:parse-number temp)
      temp)))

(defun get-students (classid)
  "Get students for a particular class from student table"
  (select :from *students* 
	  :where (matching *students* :classid classid) 
	  :order-by :lastname))

(defun course-name (courseid)
  "Return the course name (Operating Systems) for a course id (CNT270)"
  (let ((courses (select :from *courses* :where (matching *courses* :courseid courseid))))
    (if (= 0 (table-size courses))
	""
	(column-value (nth-row 0 courses) :name))))

(defun classid-from-studentid (studentid)
  "Retrieves the classid corresponding to a studentid"
  (let ((student (select :from *students*
	  :where (matching *students* 
			   :studentid studentid))))
    (if (eq (table-size student) 0)
	nil
      (column-value (nth-row 0 student) :classid))))

(defun get-class (classid courseid)
  "Get students for a particular class/course from class table"
  (select :from *classes* :where (matching *classes* :classid classid :courseid courseid)))

(defun student-in-class (studentid class)
  "Returns a student row from a class table or nil"
  (let ((student (select :from class :where (matching class :studentid studentid))))
    (if (eq 0 (table-size student))
	nil
      (nth-row 0 student))))
  
;Add students from a class to a course
;Normally used at the beginning to populate the classes tables
(defun make-class (courseid classid)
  "If a student (defined by studentid) isn't already present in *classes*, add them"
(let (students class studentid lastname)  
  (setf students (get-students classid)) ;The students who theoretically should be in
  (setf class (get-class classid courseid)); The students who are in
  (do-rows (student students)
	   (setf studentid (column-value student :studentid))
	   (setf lastname (column-value student :lastname))
	   (if (student-in-class studentid class)
	       (format t "~a (~a) already in class ~a(~a)~%"
		       lastname studentid courseid classid)
	     (progn
	       (insert-row (list :studentid studentid
				 :courseid courseid
				 :classid classid) *classes*)
	       (format t "~a (~a) added to class ~a(~a)~%" 
		       lastname studentid courseid classid))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Routines for setting up the marks database;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Retrieve the list of all categories - (nb) sorted by categoryid
(defun get-all-categories ()
  "Get all categories"
  (select :from *categories* :order-by :categoryid))

;Retrieve the list of categories for a course
(defun get-categories (courseid)
  "Get categories for a particular course"
  (select :from *categories* :where (matching *categories* :courseid courseid) 
	  :order-by :categoryid))

;Get the next categoryid for the table (they should be unique)
(defun next-categoryid ()
  "Get next available id, 1 if none yet"
  (let ((categories (get-all-categories)))
    (if (= 0 (table-size categories))
	1 ; Default for none
      (+ ; or: one greater than the biggest
       (column-value (nth-row (- (table-size categories) 1) categories) :categoryid)
       1))))

;Add a Mark Category (Exams, Labs, etc)
(defun add-category (courseid name weight &optional (note ""))
  "Add a new category to a course"
  (let ((categoryid (next-categoryid)))
    (insert-row (list :courseid courseid
		      :categoryid categoryid
		      :name name
		      :weight weight
		      :note note) *categories*)
  categoryid)) ;Return the categoryid

;Retrieve a categoryid corresponding to a string
(defun categoryid-from-name (courseid name)
  "Retrieve a categoryid corresponding to a name"
  (let ((category
	 (select :from *categories* 
	  :where (matching *categories* :courseid courseid :name name)
	  :order-by :categoryid)))
    (if (eq 0 (table-size category))
	nil
      (column-value (nth-row 0 category) :categoryid))))

;Retrieve all tasks, sorted by taskid
(defun get-all-tasks ()
  "Get all tasks"
  (select :from *tasks* :order-by :taskid))

;Retrieve the list of tasks for a given categoryid
(defun get-tasks* (categoryid)
  "Get tasks for a particular categoryid"
  (select :from *tasks* :where (matching *tasks* :categoryid categoryid)
	  :order-by :taskid))

;Retrieve the list of tasks for a given category (as string) in a course
(defun get-tasks (courseid category)
  "Get tasks for a particular course category"
  (let ((categoryid (categoryid-from-name courseid category)))
    (if (null categoryid)
	(error "Unable to match category to course")
      (get-tasks* categoryid))))

;Get the next taskid for the table (they should be unique)
(defun next-taskid ()
  "Get next available id, 1 if none yet"
  (let ((tasks (get-all-tasks)))
    (if (= 0 (table-size tasks))
	1 ; Default for none
      (+ ; or: one greater than the biggest
       (column-value (nth-row (- (table-size tasks) 1) tasks) :taskid)
       1))))

;add a new task
(defun add-task-categoryname (courseid categoryname index outof &optional (weight 1) (note ""))
  "Add a new task to a course,category"
  (let ((categoryid (categoryid-from-name courseid categoryname)))
    (if (null categoryid)
	(error "Unable to match category to course")
      (add-task courseid categoryid index outof weight note)))) 

;add a new task
(defun add-task (categoryid index outof &optional (weight 1) (note ""))
  "Add a new task to a course,category"
  (let ((taskid (next-taskid)))
	(insert-row (list :taskid taskid
			  :categoryid categoryid
			  :index index
			  :outof outof
			  :weight weight
			  :note note) *tasks*)
	taskid)) ;Return the taskid

;get a taskid - eg. (taskid-from-categoryid-index 15 2)
(defun taskid-from-categoryid-index (categoryid index)
  "Look up a taskid based on course, category and index"
  (let ((taskid nil) (task nil) )
    (if (not (null categoryid))
	(progn
	  (setf task 
		(select :from *tasks*
			:where (matching *tasks* 
					 :index index
					 :categoryid categoryid)
			:order-by :taskid))
	  (if (not (eq 0 (table-size task)))
	      (setf taskid
		    (column-value (nth-row 0 task) :taskid)))))
    taskid)) ;return the taskid - nil if not found	      

;get a taskid - eg. (taskid-from-index "CNT270" "Assignments" 2)
(defun taskid-from-categoryname-index (courseid categoryname index)
  "Look up a taskid based on course, category and index"
  (let ((categoryid (categoryid-from-name courseid categoryname)))
    (if (not (null categoryid))
	(taskid-from-categoryid-index categoryid index)
      nil)))

;Retrieve a mark (nil if none recorded)
(defun get-mark (studentid taskid)
  "Returns the current mark for the student, nil if uncrecorded"
  (let ((mark nil)
	record) ; Database record
    (setf record (select :from *marks*
			 :where (matching *marks*
					  :studentid studentid
					  :taskid taskid)))
    (if (not (eq 0 (table-size record))) ; Read value
	  (setf mark (column-value (nth-row 0 record) :mark)))
    mark))

(defun get-mark-percent (studentid taskid)
  "Returns the above, expressed as a percentage"
  (let ((mark (get-mark studentid taskid))
	(task (select :from *tasks*
		      :where (matching *tasks*
				       :taskid taskid))))
    (if (and mark (> (table-size task) 0))
	(float (* 100 (/ mark (column-value (nth-row 0 task) :outof))))
      nil)))

(defun get-mark-string (studentid taskid)
  "Used for reports - Returns a mark as string, 'Inc' for no mark or -ve"
  (let ((mark (get-mark studentid taskid)))
    (if mark
	(if (< mark 0)
	    "Inc"
	  (format nil "~,1F"  mark))
      "Inc")))

(defun get-mark-percent-string (studentid taskid)
  "Used for reports - Returns a percentage mark as string, 'Inc' for no mark or -ve"
  (let ((mark (get-mark-percent studentid taskid)))
    (if mark
	(if (< mark 0)
	    "Inc"
	  (format nil "~,1F"  mark))
      "Inc")))

(defun get-mark-string* (studentid taskid)
  "Used for reports - Returns a mark as string, '*' for no mark or -ve"
  (let ((mark (get-mark studentid taskid)))
    (if mark
	(if (< mark 0)
	    "*"
	  (format nil "~,1F"  mark))
      "*")))

(defun get-mark-percent-string* (studentid taskid)
  "Used for reports - Returns a percentage mark as string, '*' for no mark or -ve"
  (let ((mark (get-mark-percent studentid taskid)))
    (if mark
	(if (< mark 0)
	    "*"
	  (format nil "~,1F"  mark))
      "*")))

;Add/update a mark for a student
(defun set-mark (studentid taskid mark)
  "Updates student's mark or adds a new record"
  ;Delete any old marks for this task
  (delete-rows :from *marks*
	       :where (matching *marks*
				:studentid studentid
				:taskid taskid))
  ;Insert this mark
  (insert-row (list :taskid taskid :studentid studentid :mark mark) *marks*)
  mark)

(defun get-marks (taskid)
"Returns the set of marks for a task"
(select :from *marks*
	:where (matching *marks*
			 :taskid taskid)))
