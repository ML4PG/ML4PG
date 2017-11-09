(defun proof-assert-next-command-interactive3 ()
  (interactive)
  (if (get-buffer "*response*")
  (if (eq save-automatically 0)
      (proof-assert-next-command-interactive)
    (progn (with-current-buffer "*response*"
		    (beginning-of-buffer)
                    (if (zerop (buffer-size))
                      (setf temp nil)
		      (setf temp (search "No" 
				       (format "%s" (read (current-buffer)))))))
		  (if temp
		    (export-previous-lemm)
                    (proof-assert-next-command-interactive)
		      ))
    
  )
  (proof-assert-next-command-interactive)))


(defun export-previous-lemm ()
  (interactive)
  (let ((final (point))
	(result nil)
	(end nil))
    (search-backward "Proof.")
    (proof-goto-point)
    (while (< (point) final) 
      (let* ((semis (save-excursion
		      (skip-chars-backward " \t\n"
					   (proof-queue-or-locked-end))
		      (proof-segment-up-to-using-cache (point))))
	     (comment (caar semis))
	     (cmd (cadar semis))
	     (ts nil))
	(progn (setf ts (get-top-symbol))
	       (setf ng (get-number-of-goals))
	       (proof-assert-next-command-interactive)
	       (setf ng2 (get-number-of-goals))
	       (if cmd 
	       (setf result (cons (append (get-numbers cmd) (list ts) (list ng2)) result))
	       )
		    )
	  
	)	
    )
    (proof-assert-next-command-interactive)
    (setf saved-theorems (append saved-theorems 
				 (list (list (format "%s" (get-name)) 
					     (flat (reverse result))))))
    (search-forward "Qed.")

  ))


(defun get-name ()
  (search-backward "Lemma")
  (read (current-buffer))
  (read (current-buffer)))


(defun list-to-string (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) temp2)
      (setf temp2 (concat temp2 (car temp) ", "))))


(defun save-numbers ()
  (interactive)
  (progn (beginning-of-buffer)
	 (proof-goto-point)
	 (end-of-buffer)
	 (extract-feature-theorems)
	 (let* ((buf (buffer-name))
		(name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
	   (with-temp-file (concat home-dir "/definitions/" name) 
	     (insert (format "%s" listofdefinitions)))
	   (with-temp-file (concat home-dir "/variables/" name) 
	     (insert (format "%s" listofvariables)))
	   )
	 (let* ((buf (buffer-name))
		(name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
	   (with-temp-file (concat home-dir "/theorems/" name) 
	     (insert (format "%s" listofstatements)))
	   (with-temp-file (concat home-dir "/variablesthms/" name) 
	     (insert (format "%s" listofthmvariables)))
	   )	 
  (let ((d (read-string (concat "Where do you want to store this library (" (list-to-string dirs) "n (create new directory)): ")))
	    (d2 nil))
    (cond ((string-member d dirs)
	       (progn (with-temp-file 
			  (concat home-dir "libs/ssreflect/" d "/"
				  (subseq (buffer-name (current-buffer)) 0 
					  (search "." (buffer-name (current-buffer))))  
				  ".csv") (insert (extract-features-1)))
		      (with-temp-file 
			  (concat home-dir "libs/ssreflect/" d "/"
				  (subseq (buffer-name (current-buffer)) 0 
					  (search "." (buffer-name (current-buffer))))  
				  "_tactics.csv") (insert (extract-features-2 tactic-level)))
		      (with-temp-file 
			  (concat home-dir "libs/ssreflect/" d "/"
				  (subseq (buffer-name (current-buffer)) 0 
					  (search "." (buffer-name (current-buffer))))  
				  "_tree.csv") (insert (extract-features-2 proof-tree-level)))
		      (with-temp-file (concat home-dir "libs/ssreflect/" d "/"
					      (subseq (buffer-name (current-buffer)) 0 
						      (search "." (buffer-name (current-buffer))))  
					      "_names")  (insert (extract-names)))))
	      ((string= d "n")
	       (progn 
		 (setf d2 (read-string (concat "Introduce a name for the directory: ")))
		 (shell-command (concat "mkdir " home-dir "libs/ssreflect/" d2))
		 (with-temp-file 
			  (concat home-dir "libs/ssreflect/" d2 "/"
				  (subseq (buffer-name (current-buffer)) 0 
					  (search "." (buffer-name (current-buffer))))  
				  ".csv") (insert (extract-features-1)))
		 (with-temp-file 
			  (concat home-dir "libs/ssreflect/" d2 "/"
				  (subseq (buffer-name (current-buffer)) 0 
					  (search "." (buffer-name (current-buffer))))  
				  "_tree.csv") (insert (extract-features-2 proof-tree-level)))
		 (with-temp-file 
			  (concat home-dir "libs/ssreflect/" d2 "/"
				  (subseq (buffer-name (current-buffer)) 0 
					  (search "." (buffer-name (current-buffer))))  
				  "_tactics.csv") (insert (extract-features-2 tactic-level)))
		 (with-temp-file (concat home-dir "libs/ssreflect/" d2 "/"
					      (subseq (buffer-name (current-buffer)) 0 
						      (search "." (buffer-name (current-buffer))))  
					      "_names")  (insert (extract-names)))))
	      (t
	       (progn (with-temp-file 
			  (concat home-dir "libs/ssreflect/" 
				  (subseq (buffer-name (current-buffer)) 0 
					  (search "." (buffer-name (current-buffer))))  
				  ".csv") (insert (extract-features-1)))
		      (with-temp-file 
			  (concat home-dir "libs/ssreflect/" 
				  (subseq (buffer-name (current-buffer)) 0 
					  (search "." (buffer-name (current-buffer))))  
				  "_tree.csv") (insert (extract-features-2 proof-tree-level)))
		      (with-temp-file 
			  (concat home-dir "libs/ssreflect/" 
				  (subseq (buffer-name (current-buffer)) 0 
					  (search "." (buffer-name (current-buffer))))  
				  "_tactics.csv") (insert (extract-features-2 tactic-level)))
		      (with-temp-file (concat home-dir "libs/ssreflect/" 
					      (subseq (buffer-name (current-buffer)) 0 
						      (search "." (buffer-name (current-buffer))))  
					      "_names")  (insert (extract-names))))))
  )))
  



