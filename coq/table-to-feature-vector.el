(require 'cl)

(defvar arity0 nil)
(defvar arity1 
  '(("recursive-call" -1)
    ("consp" 1)
    ("integerp" 2)
    ("natp" 3)
    ("endp" 4)))



(defvar arity2  '(("append" 1) ("recursive-call" -2)))
(defvar arity3 '(("recursive-call" -3)))
(defvar arity4 '(("recursive-call" -4)))
(defvar arity5 '(("recursive-call" -5)))
(defvar n-arity0 1)
(defvar n-arity1 5)
(defvar n-arity2 2)
(defvar n-arity3 1)
(defvar n-arity4 1)
(defvar n-arity5 1)


(defun get-arity-list (i)
  (cond ((equal i 0) arity0)
	((equal i 1) arity1)
	((equal i 2) arity2)
	((equal i 3) arity3)
	((equal i 4) arity4)
	((equal i 5) arity5)
	))


(defun increase-narity (i)
  (cond ((equal i 0) (setf n-arity0 (+ 1 n-arity0)))
	((equal i 1) (setf n-arity1 (+ 1 n-arity1)))
	((equal i 2) (setf n-arity2 (+ 1 n-arity2)))
	((equal i 3) (setf n-arity3 (+ 1 n-arity3)))
	((equal i 4) (setf n-arity4 (+ 1 n-arity4)))
	((equal i 5) (setf n-arity5 (+ 1 n-arity5)))
	)  )

(defun get-narity (i)
    (cond ((equal i 0) n-arity0)
	((equal i 1) n-arity1)
	((equal i 2) n-arity2)
	((equal i 3) n-arity3)
	((equal i 4) n-arity4)
	((equal i 5) n-arity5)
	))




(defun convert_arity-1 (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) (concatenate 'string "-" temp2))
    (setf temp2 (concatenate 'string temp2 "1"))))



(defun remove-minus (string)
  (let ((minus (search "-" string)))
    (if minus
	(remove-minus (concatenate 'string (subseq string 0 minus) (subseq string (1+ minus))))
      string)))



(defun remove-minus-add-minus (string)
  (if (search "-" string)
      (concatenate 'string "-" (remove-minus string))
    string))
  


(defun convert (list i)
  (if (equal i -1)
      (string-to-number (convert_arity-1 list))
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) (string-to-number (remove-minus-add-minus temp2)))
    (if (assoc (format "%s" (car temp)) (get-arity-list i))
	(setf temp2 (concatenate 'string temp2 
				 (format "%s" (cadr (assoc (format "%s" (car temp))
						     (get-arity-list i))))))
      (progn 
        (cond 
	 ((equal i 0) (setf arity0
		   (append arity0
			   (list (list (format "%s" (car temp)) n-arity0)))))
	 ((equal i 1) (setf arity1
		   (append arity1
			   (list (list (format "%s" (car temp)) n-arity1)))))
	 ((equal i 2) (setf arity2
		   (append arity2
			   (list (list (format "%s" (car temp)) n-arity2)))))
	 ((equal i 3) (setf arity3
		   (append arity3
			   (list (list (format "%s" (car temp)) n-arity3)))))
	 ((equal i 4) (setf arity4
		   (append arity4
			   (list (list (format "%s" (car temp)) n-arity4)))))
	 ((equal i 5) (setf arity5
		   (append arity5
			   (list (list (format "%s" (car temp)) n-arity5)))))
	)
	(increase-narity i)
	(setf temp2 (concatenate 'string temp2 
				 (format "%s" (cadr (assoc (format "%s" (car temp))
						     (get-arity-list i)))))))))))



(defun populate-list (list)
  (do ((temp list (cdr temp))
       (i -1)
       (temp2 nil))
      ((endp temp) temp2)
    (if (endp (car temp))
	(progn (setf temp2 (append temp2 (list 0))) (setf i (+ i 1)))
      (progn (setf temp2 (append temp2 (list (convert (car temp) i))) )
	     (setf i (+ i 1))))))




(defun populate-table (list)
  (let ((name (car list))
	(features (cdr list)))
    (do ((temp features (cdr temp))
	 (temp2 nil))
	((endp temp) (append (list name) (list temp2)))
      (setf temp2 (append temp2 (list (populate-list (car temp))))))))





(defun convert_arity-1-sparse (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (setf temp2 (append temp2 (list -1)))))



(defun convert-sparse (list i)
  (if (equal i -1)
      (convert_arity-1-sparse list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (assoc (format "%s" (car temp)) (get-arity-list i))
	(setf temp2 (append temp2 (list (cadr (assoc (format "%s" (car temp))
						     (get-arity-list i))))))
      (progn 
        (cond 
	 ((equal i 0) (setf arity0
		   (append arity0
			   (list (list (format "%s" (car temp)) n-arity0)))))
	 ((equal i 1) (setf arity1
		   (append arity1
			   (list (list (format "%s" (car temp)) n-arity1)))))
	 ((equal i 2) (setf arity2
		   (append arity2
			   (list (list (format "%s" (car temp)) n-arity2)))))
	 ((equal i 3) (setf arity3
		   (append arity3
			   (list (list (format "%s" (car temp)) n-arity3)))))
	 ((equal i 4) (setf arity4
		   (append arity4
			   (list (list (format "%s" (car temp)) n-arity4)))))
	 ((equal i 5) (setf arity5
		   (append arity5
			   (list (list (format "%s" (car temp)) n-arity5)))))
	)
	(increase-narity i)
	(setf temp2 (append temp2 (list (cadr (assoc (format "%s" (car temp))
						     (get-arity-list i)))))))))))




(defun populate-list-sparse (list)
  (do ((temp list (cdr temp))
       (i -1)
       (temp2 nil))
      ((endp temp) temp2)
    (if (endp (car temp))
	(progn (setf temp2 (append temp2 (list (list 0)))) (setf i (+ i 1)))
      (progn (setf temp2 (append temp2 (list (convert-sparse (car temp) i))) )
	     (setf i (+ i 1))))))




(defun populate-table-sparse (list)
  (let ((name (car list))
	(features (cdr list)))
    (do ((temp features (cdr temp))
	 (temp2 nil))
	((endp temp) (append (list name) (list temp2)))
      (setf temp2 (append temp2 (list (populate-list-sparse (car temp))))))))




;(populate-table (build-table (extract-info '(defthm foo 
;		 (implies (and (consp x)
;			       (consp y))
;			  (equal (reverse (append x y))
;				 (append (reverse x) (reverse y))))))))



(defun flat (ll)
  (if (endp ll)
    nil
    (append (car ll) (flat (cdr ll)))))

(defun flatten-table (list)
  (let ((name (car list))
	(features (cadr list)))
    (append (list name) (list (flat features)))))







;(flatten-table (populate-table (build-table (extract-info '(defthm foo 
;		 (implies (and (consp x)
;			       (consp y))
;			  (equal (reverse (append x y))
;				 (append (reverse x) (reverse y)))))))))


  

(defvar defs-vectors nil)

(defun convert-recursive-several-libraries-defs ()
  (setf defs-vectors nil)
  (do ((temp tables-definitions (cdr temp)))
      ((endp temp) nil)
    (setf defs-vectors (append defs-vectors (list (flatten-table (populate-table  (car temp))))))))

(defvar thms-vectors nil)

(defun convert-recursive-several-libraries-thms ()
  (setf defs-vectors nil)
  (do ((temp tables-thms (cdr temp)))
      ((endp temp) nil)
    (setf thms-vectors (append thms-vectors (list (flatten-table (populate-table  (car temp))))))))
