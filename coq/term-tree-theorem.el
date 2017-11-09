(require 'cl)


;; Obtain definition and clean the term

(defun obtain-theorem (name)
  (proof-shell-invisible-cmd-get-result (format "Check %s" name)))



(defun replace-questionmark (term) 
  (do ((temp0 term)
       (ift (search "?" term))
       (temp2 ""))
      ((not ift) (concatenate 'string temp2 temp0))
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 0 ift) "x"))
	   (setf temp0 (subseq temp0 (+ 1 ift)))
	   (setf ift (search "?" temp0)))))



(defun clean-term-thm (term)
  (let* ((clean-term (remove-whitespaces (remove-jumps term)))
	 (arr (search "->" clean-term :from-end t))
	 (comma (search "," clean-term :from-end t))
	 (obj (cond ((and arr comma (< arr comma))  (subseq clean-term (+ 1 comma)))
	       (arr (subseq clean-term (+ 2 arr)))
		    (comma (subseq clean-term (+ 1 comma)))
		    (t (subseq clean-term (+ 1 (search ":" clean-term :from-end t))))))
	 (sfor (search "forall" clean-term))
	 (vars (if sfor 
		   (variables-fun2 (subseq clean-term (+ 7 sfor) comma))
		 nil)))
    (list (replace-questionmark (replace-quote obj)) (replace-quote vars))))



(defun next-variable (term)
  (do ((temp0 term)
       (i 1)
       (pos1 (search ")" term))
       (pos2 (search "(" term)))
      ((= i 0) temp0)
    (if pos2
	(if (< pos2 pos1)
	    (progn (setf temp0 (subseq temp0 (1+ pos2)))
		   (setf i (1+ i))
		   (setf pos1 (search ")" temp0))
		   (setf pos2 (search "(" temp0)))
	  (progn (setf temp0 (subseq temp0 (1+ pos1)))
		 (setf i (1- i))
		 (setf pos1 (search ")" temp0))
		 (setf pos2 (search "(" temp0))))
      (progn (setf temp0 (subseq temp0 (1+ pos1)))
	     (setf i (1- i))
	     (setf pos1 (search ")" temp0))
	     (setf pos2 (search "(" temp0)))
      )))

(defun variables-fun2 (term)
  (do ((posop (search "(" term))
       (temp0 term)
       (temp2 ""))
      ((not posop) (if (search ":" temp0) (subseq temp0 0 (search ":" temp0)) temp2))
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 (1+ posop) (search ":" temp0))))
	   (setf temp0 (next-variable (subseq temp0 (1+ (search ":" temp0)) )))
	   (setf posop (search "(" temp0)))))




;;; Transform the term to a list


(defun thm-to-list (term) 
  (if (search "match" term)
      (transform-match (subseq term (+ 2 (search "=>" term))))
  (string-to-list (concatenate 'string "("  term ")"))))


(defvar listofstatements nil)
(defvar listofthmvariables nil)

(defun addthm (name)
  (interactive)
  (proof-shell-invisible-cmd-get-result (format "Unset Printing All."))
  (proof-shell-invisible-cmd-get-result (format "Unset Printing Notations."))
  (let ((iftable (proof-shell-invisible-cmd-get-result (format "Print Table Printing If.")))
	(term nil))
    (if (search "None" iftable)
	nil
      (proof-shell-invisible-cmd-get-result (format "Remove Printing If %s."
						  (subseq iftable (+ 1 (search ":" iftable))))))

    (setf term (obtain-theorem name))
    (setf listofstatements (append listofstatements
				    (list (list 'theorem (make-symbol name) (thm-to-list (car (clean-term-thm term)))))))
    (setf listofthmvariables (append listofthmvariables
				  (list (list (cadr (clean-term-thm term)) )))	)	
    (if (search "None" iftable)
	nil
      (proof-shell-invisible-cmd-get-result (format "Add Printing If %s."
						  (subseq iftable (+ 1 (search ":" iftable))))))
    (proof-shell-invisible-cmd-get-result (format "Set Printing Notations."))
    (proof-shell-invisible-cmd-get-result (format "Set Printing All."))
    
    )
  )



(defvar tables-thms nil)

(defun transform-thms ()
  (setf tables-thms nil)
  (do ((temp statements-libraries (cdr temp))
       (temp2 variablesthm-libraries (cdr temp2)))
      ((endp temp) nil)
    (setf tables-thms (append tables-thms 
			      (list (build-table (extract-info (car temp) (car temp2))))))))





(defun clean-goal (goal)
  (let* ((clean-term (remove-whitespaces (remove-jumps (subseq goal (+ 28 (search "============================" goal)) 
							       (search "(dependent " goal)))))
	 (arr (search "->" clean-term :from-end t))
	 (comma (search "," clean-term :from-end t))
	 (obj (cond ((and arr comma (< arr comma))  (subseq clean-term (+ 1 comma)))
	       (arr (subseq clean-term (+ 2 arr)))
		    (comma (subseq clean-term (+ 1 comma)))
		    (t clean-term))))
    (replace-questionmark (replace-quote obj)) )
)



(defun search-vars (str)
  (do ((temp str)
       (colon (search ":" str))
       (temp2 ""))
      ((not colon) temp2)
    (let ((temp1 (subseq temp 0 (1- colon))))
      (setf temp2 (concatenate 'string temp2 " " (subseq temp1 (1+ (search " " temp1 :from-end t))) ))
      (setf temp (subseq temp (1+ colon)))
      (setf colon (search ":" temp)))))



(defun vars-goal (goal)
  (let* ((clean-vars (remove-jumps (replace-quote (subseq goal (+ 1 (search ")" goal :start2 ( + 1 (search ")" goal)))) (search "============================" goal)) ))))
    (search-vars clean-vars)	 
    ))





(defun addcurrentgoal ()
  (interactive)
  (proof-shell-invisible-cmd-get-result (format "Unset Printing All."))
  (proof-shell-invisible-cmd-get-result (format "Unset Printing Notations."))
  (let ((iftable (proof-shell-invisible-cmd-get-result (format "Print Table Printing If.")))
	(term nil))
    (if (search "None" iftable)
	nil
      (proof-shell-invisible-cmd-get-result (format "Remove Printing If %s."
						  (subseq iftable (+ 1 (search ":" iftable))))))

    (setf term (proof-shell-invisible-cmd-get-result (format "Focus")))
    (setf listofstatements (append (list (list 'theorem (make-symbol "temp") (thm-to-list (clean-goal term)))) listofstatements))
    (setf listofthmvariables (append (list (list (vars-goal term) )) listofthmvariables)	)	
    (if (search "None" iftable)
	nil
      (proof-shell-invisible-cmd-get-result (format "Add Printing If %s."
						  (subseq iftable (+ 1 (search ":" iftable))))))
    (proof-shell-invisible-cmd-get-result (format "Set Printing Notations."))
    (proof-shell-invisible-cmd-get-result (format "Set Printing All."))
    
    )
  )