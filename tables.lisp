;Functions relating to parsing/modifying/outputing the memory tables

(in-package :nait.cnt.llama)

;Someday, I'd like to be able to use these as the set of arguments
;for the *-tables functions.  Not sure how yet.
(defvar *tables*
  (list 'students
        'courses
        'classes
        'events
        'attendance
        'categories
        'tasks
        'marks))

;I'm probably breaking a bunch of rules by using string parameters
;and parsing them out to symbols, but what the hey, lets have fun
(defmacro make-table-string (name)
  "Makes a memory table based on the table name as a string"
  `(defparameter
     ,(intern (string-upcase (concatenate 'string "*" name "*")) :nait.cnt.llama)
     (make-instance 'table :schema
                    ,(intern (string-upcase
                              (concatenate 'string "*schema." name "*"))
                             :nait.cnt.llama))))

(defmacro make-table (name)
  "Makes a memory table based on the table name as a symbol"
  `(make-table-string ,(string name)))

(defmacro save-table-string (name)
  "Saves a memory table to a file based on the table name as a string"
  `(save-table-to-file
    ,(intern (string-upcase (concatenate 'string "*" name "*")) :nait.cnt.llama)
    ,name))

(defmacro save-table (name)
  "Saves a memory table to file"
  `(save-table-string ,(string name)))

(defmacro load-table-string (name)
  "Loads a memory table from a file with the table name as a string"
  `(load-table-from-file
    ,(intern (string-upcase (concatenate 'string "*" name "*")) :nait.cnt.llama)
    ,name))

(defmacro load-table (name)
  "Loads a memory table to file"
  `(load-table-string ,(string name)))

(defun make-tables ()
  (make-table students)
  (make-table courses)
  (make-table classes)
  (make-table events)
  (make-table attendance)
  (make-table categories)
  (make-table tasks)
  (make-table marks)
  (format nil "8 Memory Tables Generated"))

(defun save-tables ()
  (save-table students)
  (save-table courses)
  (save-table classes)
  (save-table events)
  (save-table attendance)
  (save-table categories)
  (save-table tasks)
  (save-table marks)
  (format nil "8 Memory Tables Stored to Disk"))

(defun load-tables ()
  (make-tables)
  (load-table students)
  (load-table courses)
  (load-table classes)
  (load-table events)
  (load-table attendance)
  (load-table categories)
  (load-table tasks)
  (load-table marks)
  (format nil "8 Memory Tables Retrieved from Disk"))
