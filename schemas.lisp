;;;Define the Data Schemas for the LLAMA system

(in-package :nait.cnt.llama)

;;;Students
(defparameter *schema.students*
  (make-schema
   '((:studentid number 0)
     (:classid interned-string "")
     (:lastname interned-string) ; Interned, because cost can be amortized by searches
     (:firstname string "")
     (:middlename string "")
     (:nickname string ""))))

(defparameter *schema.courses*
  (make-schema
   '((:courseid interned-string) ; "CNT270"
     (:name string "") ; "Operating Systems"
     (:description string "")))) ; "This is a groovy course designed by a genius."

(defparameter *schema.classes* ;Student/Course Linkage
  (make-schema
   '((:studentid number) ; 2005988
     (:courseid interned-string) ; "CNT270" 
     (:classid interned-string)))) ; "1F" or "CNT12"

(defparameter *schema.events* ; Notes for particular classes
  (make-schema
   '((:courseid interned-string) ; "CNT270"
     (:classid interned-string) ; "1F" or "CNT12"
     (:date date) ; universal-date
     (:coverage string "") ; "What I lectured on was..."
     (:note string "")))) ; "This weird thing happened..."

(defparameter *schema.attendance* ; Attendance
  (make-schema
   '((:studentid number) ; 2005988
     (:courseid interned-string) ; "CNT270"
     (:classid interned-string) ; "1F" or "CNT12"
     (:date date) ; universal-date
     (:attendance string "Present") ; "Present", "Missed First Half", etc.
     (:note string "")))) ; "Gave extension on Lab 3"

(defparameter *schema.categories* ; Mark Categories - Exams, Labs, etc.
  (make-schema
   '((:courseid interned-string) ; "CNT270"
     (:categoryid number) ; 1, 2, 3 - index value, unique
     (:name string) ; "Labs"
     (:weight number) ; weight of that category toward final. Typically a percent, like 50
     (:note string "")))) ; note about that category - like "50% Weighted average required"

(defparameter *schema.tasks* ; Things that are marked - "Assignment 2"
  (make-schema
   '((:taskid number) ; 1, 2, 3 - index value, unique
     (:categoryid number) ; From above, includes index to course
     (:index string-insensitive) ; The number or value (1 for assignment 1, 1b, etc).
     (:outof number) ; Max score
     (:weight number) ; Weight amongst other members of category
     (:note string "")))) ; Info about task

(defparameter *schema.marks* ; Actual student marks
  (make-schema
   '((:taskid number) ; Referenced above, leads ultimately to category and course
     (:studentid number) ; 2005988
     (:mark number)))) ; actual mark (out of and weight is in task)