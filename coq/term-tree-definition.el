(require 'cl)


;; Obtain definition and clean the term

(defun obtain-definition (name)
  (proof-shell-invisible-cmd-get-result (format "Print %s" name)))

(defun remove-jumps (string)
  (do ((temp0 string)
       (jump (search "\n" string))
       (temp2 ""))
      ((not jump) (concatenate 'string temp2 temp0)) 
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 0 jump) " "))
	   (setf temp0 (subseq temp0 (1+ jump)))
	   (setf jump (search "\n" temp0)))))

(defun remove-whitespaces (string)
  (do ((temp0 string)
       (jump (search "  " string))
       (temp2 ""))
      ((not jump) (concatenate 'string temp2 temp0)) 
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 0 jump) " "))
	   (setf temp0 (subseq temp0 (+ 2 jump)))
	   (setf jump (search "  " temp0)))))

(defun remove-argument (string)
  (subseq string 0 (search "Argument" string)))


(defun clean-term (term)
  (let* ((clean-term (remove-argument (remove-whitespaces (remove-jumps term))))
	 (obj (subseq clean-term 0 (search ":" clean-term :from-end t)))
	 (type (subseq clean-term (1+ (search ":" clean-term :from-end t)))))
    (list obj type)))




;;; Transform the term to a list


(defun add-parenthesis-match (term)
  (do ((temp0 term)
       (ift (search "match" term))
       (temp2 ""))
      ((not ift) (concatenate 'string temp2 temp0))
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 0 ift) "match ("))
	   (setf temp0 (subseq temp0 (+ 5 ift)))
	   (setf ift (search "match" temp0)))))


(defun remove-with (term)
  (do ((temp0 term)
       (ift (search " with" term))
       (temp2 ""))
      ((not ift) (concatenate 'string temp2 temp0))
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 0 ift)))
	   (setf temp0 (subseq temp0 (+ 5 ift)))
	   (setf ift (search " with" temp0)))))

(defun remove-end (term)
  (do ((temp0 term)
       (ift (search "end" term))
       (temp2 ""))
      ((not ift) (concatenate 'string temp2 temp0))
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 0 ift) ")"))
	   (setf temp0 (subseq temp0 (+ 3 ift)))
	   (setf ift (search "end" temp0)))))



(defun remove-arrow (term)
  (do ((temp0 term)
       (ift (search ">" term))
       (temp2 ""))
      ((not ift) (concatenate 'string temp2 temp0))
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 0 ift) "("))
	   (setf temp0 (subseq temp0 (+ 1 ift)))
	   (setf ift (search ">" temp0)))))

(defun remove-bar (term)
  (do ((temp0 term)
       (ift (search "|" term))
       (temp2 ""))
      ((not ift) (concatenate 'string temp2 temp0))
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 0 ift) ")"))
	   (setf temp0 (subseq temp0  (+ 1 (search "=> " temp0))))
	   (setf ift (search "|" temp0)))))

 
(defun transform-length-1 (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (consp (car temp))
	(if (equal (length (car temp)) 1)
	    (setf temp2 (append temp2 (list (caar temp))))
	  (setf temp2 (append temp2 (list (transform-length-1 (car temp))))))
      (setf temp2 (append temp2 (list (car temp))))))) 
	 


(defun replace-quote (term) 
  (do ((temp0 term)
       (ift (search "'" term))
       (temp2 ""))
      ((not ift) (concatenate 'string temp2 temp0))
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 0 ift) "quo"))
	   (setf temp0 (subseq temp0 (+ 1 ift)))
	   (setf ift (search "'" temp0)))))


(defun add-parentheses-match0 (term)
  (concatenate 'string "(" 
	       (replace-quote (remove-arrow (remove-bar (add-parenthesis-match (remove-end (remove-with term))))))
		")"))

(defun transform-match (term)
  (transform-length-1 (car (read-from-string (add-parentheses-match0 term)))))




;(transform-match "match n with | O => m | S _ => match leq (S (S (S O))) n with | true => muln (addn n m) m | false => match m with | O => S O | S _ => S (S O) end end end")


(defun string-to-list (str)
  (car (read-from-string str)))

(defun definition-to-list-aux (term) 
  (string-to-list (concatenate 'string "(" (subseq term (1+ (search "=" term))) ")")))

(defun definition-to-list-let (term) 
  (string-to-list (concatenate 'string "(nosimpl " (subseq term (+ 3 (search "in" term))) ")"))) 


(defun  definition-to-list-fix (term) 
  (transform-match (subseq term (+ 2 (search ":=" term)))))


(defun  definition-to-list-fun (term) 
  (if (search "match" term)
      (transform-match (subseq term (+ 2 (search "=>" term))))
  (string-to-list (concatenate 'string "("  (subseq term (+ 2 (search "=>" term))) ")"))))



(defun variables-fun (term)
  (do ((posop (search "(" term))
       (temp0 term)
       (temp2 ""))
      ((not posop) (if (search ":" temp0) (subseq temp0 0 (search ":" temp0)) temp2))
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 (1+ posop) (search ":" temp0))))
	   (setf temp0 (subseq temp0 (1+ (search ":" temp0)) ))
	   (setf posop (search "(" temp0)))))


(defun variables-fix (term)
  (do ((posop (search "(" term))
       (temp0 term)
       (temp2 ""))
      ((not posop) temp2)
    (progn (setf temp2 (concatenate 'string temp2 (subseq temp0 (1+ posop) (search ":" temp0))))
	   (setf temp0 (subseq temp0 (1+ (search ":" temp0)) ))
	   (setf posop (search "(" temp0)))))

(defun variables-fun-fix (term)
  (concatenate 'string (variables-fun (subseq term (+ 3 (search "fun" term)) (search "=>" term))) " "
	  (variables-fix (subseq term (+ 3 (search "fix" term))))))
	  


(defun definition-variables (term)
  (car (read-from-string (concatenate 'string "("
			       (cond ((and (search "fun " term) (search "fix " term)) (variables-fun-fix (subseq term 0 (search ":=" term))))
				     ((search "fun" term) (variables-fun (subseq term (+ 3 (search "fun" term)) (search "=>" term))))
				     ((search "fix" term) (variables-fix (subseq term (+ 3 (search "fix" term)) (search ":=" term)))))
			       ")"))))



(defun definition-to-list (term)
  (cond ((search "fix " term) (list (definition-to-list-fix term) (definition-variables term)))
	((search "let " term) (list (definition-to-list-let term) nil))
	((search "fun " term) (list (definition-to-list-fun term) (definition-variables term)))
	(t (list (definition-to-list-aux term) nil))))




    


(defvar listofdefinitions nil)
(defvar listofvariables nil)

(defun adddefinition (name)
  (interactive)
  (proof-shell-invisible-cmd-get-result (format "Unset Printing Notations."))
  (let ((iftable (proof-shell-invisible-cmd-get-result (format "Print Table Printing If.")))
	(term nil))
    (if (search "None" iftable)
	nil
      (proof-shell-invisible-cmd-get-result (format "Remove Printing If %s."
						  (subseq iftable (+ 1 (search ":" iftable))))))

    (setf term (obtain-definition name))
    (setf listofdefinitions (append listofdefinitions
				    (list (list 'definition (make-symbol name) (car (definition-to-list (car (clean-term term))))))))
    (setf listofvariables (append listofvariables
				  (list (cadr (definition-to-list (car (clean-term term)))))))		
    (if (search "None" iftable)
	nil
      (proof-shell-invisible-cmd-get-result (format "Add Printing If %s."
						  (subseq iftable (+ 1 (search ":" iftable))))))
    (proof-shell-invisible-cmd-get-result (format "Set Printing Notations."))
    
    )
  )



(defvar tables-definitions nil)

(defun transform-definitions ()
  (setf tables-definitions nil)
  (do ((temp definitions-libraries (cdr temp))
       (temp2 variables-libraries (cdr temp2)))
      ((endp temp) nil)
    (setf tables-definitions (append tables-definitions 
				     (list (build-table (extract-info (car temp) (car temp2))))))))





