(defvar *package-db* nil)

(defun new-package (name date saved)
  (list :name name :date date :saved saved))

(defun add-package (package)
  (push package *package-db*))

(defun dump-db ()
  (dolist (package *package-db*)
    (format t "~{~a:~10t~a~%~}~%" package)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-package ()
  (new-package
   (prompt-read "Name")
   (prompt-read "Date")
   (y-or-n-p "Saved [y/n]")))

(defun add-packages ()
  (loop (add-package (prompt-for-package))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *package-db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *package-db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *package-db*))

(defun update (selector-fn &key name date (saved nil saved-p))
  (setf *package-db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if name (setf (getf row :name) name))
               (if date (setf (getf row :date) date))
               (if saved-p (setf (getf row :saved) saved)))
             row)  *package-db*)))

(defun delete-rows (selector-fn)
  (setf *package-db* (remove-if selector-fn *package-db*)))


(defun make-comparison-expr (field value)
  `(equal (getf package ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (package) (and ,@(make-comparisons-list clauses))))
