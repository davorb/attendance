; Attendance
; (c) 2013 Davor Babic <davor@davor.se>

(defvar *students* nil)
(defvar *attendance* nil)

(defun new-record (name attending)
  (list :name name :attending attending))

(defun add-record (record)
  (push record *attendance*))

(defun add-student (name)
  (push name *students*))

(defun save-student-db ()
  (with-open-file (out "students.db"
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
        (print *students* out))))

(defun load-student-db ()
  (with-open-file (in "students.db")
    (with-standard-io-syntax
      (setf *students* (read in)))))

(defun load-attendance (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *attendance* (read in)))))

(defun prompt-attendance (name)
  (write name)
  (add-record (new-record name
              (y-or-n-p "Attending? [y/n]: "))))

(defun take-attendance ()
  (dolist (student *students*)
    (prompt-attendance student)))

(defun present-attendance ()
  (dolist (record *attendance*)
    (format t "~{~a:~15t~a~%~}~%" record)))

(defun save-attendance (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *attendance* out))))

(defun mark-all-as-attending ()
  (dolist (student *students*)
    (add-record (new-record student t))))

(defun mark-missing (name)
  (setf *attendance* 
        (remove-if #'(lambda (x) (equal (getf x :name) name)) *attendance*))
  (add-record (new-record name nil)))
