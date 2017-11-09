;;; This is the feature vector extraction file for SSReflect

;; Different variables which are used to store information about
;; the numbers associated with tactics, rewrite rules, types, ...

(defvar hypothesis nil)

(defvar saved-theorems nil)
(defvar goal-level-temp nil)
(defvar tactic-level nil)
(defvar proof-tree-level nil)

;; Variables to store the different values associated with the tactics, the
;; types or the rewrite rules

(defvar tactic_id '(("move"    . 1)
		    ("case"      . 2)
		    ("elim"      . 3)
		    ("apply"     . 4)
		    ("apply/"    . 5)
		    ("move/"     . -5)
		    ("case/"     . 6)
		    ("rewrite"    . 7)
		    ("exists"     . 8)
		    ("[]"         . 0)
		    ("exact"         . 9)))


(defvar move nil)
(defvar case nil)
(defvar elim nil)
(defvar apply nil)
(defvar apply/ nil)
(defvar move/ nil)
(defvar case/ nil)
(defvar rewrite nil)
(defvar exists nil)
(defvar done nil)
(defvar exact nil)


(defvar types_id '(("nat"  . -2)
		   ("Prop" . -4)
		   ("bool" . -3)
		   ("T"  . -1)
		   ("seq" . -5)))

(defvar types_id_n -6)
(defvar views_id nil)
(defvar theorems_id nil)

(defvar top-symbol-id 
  '(("forall"      . 5)
    ("@eq"         . 6)
    ("and"         . 4)
    ("iff"         . 8)
    ("or"          . 3)
    ("is_true"     . 2)
    ("reflect"     . 9)
    ))

(defvar top-symbol-n 10)


(defvar add_to 0.1)
(defvar start 100)

(defvar start_view 101)
(defvar start_thm 101)


(defvar init 0)

(defvar current-level 1)
(defvar dot-level nil) 

;;; Proof tree levels

(defvar tdl1 nil) 
(defvar tdl2 nil)
(defvar tdl3 nil)
(defvar tdl4 nil)
(defvar tdl5 nil)

(defun add-info-to-level-aux (info list)
  (if (not list)
      info
    (do ((temp list (cdr temp))
	 (temp1 info (cdr temp1))
	 (temp2 nil))
	((endp temp) temp2)
	(cond ((= (car temp) 0) (setf temp2 (append temp2 (list (car temp1)))))
	      ((= (car temp1) 0) (setf temp2 (append temp2 (list (car temp)))))
	      (t (setf temp2 (append temp2 (list (string-to-number (format "%s%s" (car temp) (car temp1)))))))))))

(defun add-info-to-level (info level)
  (cond ((= level 1) (setf tdl1 (add-info-to-level-aux info tdl1)))
	((= level 2) (setf tdl2 (add-info-to-level-aux info tdl2)))
	((= level 3) (setf tdl3 (add-info-to-level-aux info tdl3)))
	((= level 4) (setf tdl4 (add-info-to-level-aux info tdl4)))
	((= level 5) (setf tdl5 (add-info-to-level-aux info tdl5)))
	(t nil)
  ))

;;; Main function of this file, it is in charge of extracting the
;;; information associated with a theorem 


(defun export-theorem ()
  (interactive)
  (progn (setf tdl1 nil 
	       tdl2 nil
	       tdl3 nil
	       tdl4 nil
	       tdl5 nil
	       move nil
	       case nil
	       elim nil
	       apply nil
	       apply/ nil
	       move/ nil
	       case/ nil
	       rewrite nil
	       exists nil
	       done nil
	       exact nil
	       current-level 1
	       dot-level nil
	       hypothesis nil
	       goal-level nil)
	 (if (equal init 0)
	     (progn (read-lemmas)
		    (read-views)
		    (setq init 1)))
	 (export-theorem-aux nil nil)
	 (proof-shell-invisible-cmd-get-result (format "Unset Printing All"))
	   ))



;; A function to obtain the type associated with an object

(defun remove-jumps-aux (string res)
  (let ((jump (search "
" string)))
    (if jump
	(remove-jumps-aux (subseq string (1+ jump)) (concatenate 'string res (subseq string 0 jump)))
      (concatenate 'string res string))))

(defun remove-jumps (string)
  (remove-jumps-aux string ""))


(defun get-type-id (object)
  (if (string= "(" (subseq object 0 1))
      -4
  (let* ((a (remove-jumps (proof-shell-invisible-cmd-get-result (concat "Check " object))))
	 (pos_jump (search "
" a :start2 (+ 2 (search " " a))))
	 (pos_space (search " " a :start2 (+ 2 (search ": " a))))
	 (type (if pos_space
		   (cdr (assoc (subseq a (+ 2 (search ": " a)) pos_space) types_id))
		 (cdr (assoc (subseq a (+ 2 (search ": " a)) pos_jump) types_id)))))
    (if type type 
      (progn (setf types_id
		   (append types_id  (list (cons  (if pos_space
						      (subseq a (+ 2 (search ": " a)) pos_space)
						    (subseq a (+ 2 (search ": " a)) pos_jump))
						  types_id_n))))
	     
	     (setf types_id_n (1- types_id_n))
	     (1+ types_id_n))
      ))))

(defun get-type-id2 (object)
  (let* ((a (proof-shell-invisible-cmd-get-result (concat "Check " object)))
	 (pos_jump (search "
" a :start2 (+ 2 (search " " a))))
	 (pos_space (search " " a :start2 (+ 2 (search ": " a))))
	 (type (if pos_space
		   (cdr (assoc (subseq a (+ 2 (search ": " a)) pos_space) types_id))
		 (cdr (assoc (subseq a (+ 2 (search ": " a)) pos_jump) types_id)))))
    (if type type 
      (progn (setf types_id
		   (append types_id  (list (cons  (if pos_space
						      (subseq a (+ 2 (search ": " a)) pos_space)
						    (subseq a (+ 2 (search ": " a)) pos_jump))
						  types_id_n))))
	     
	     (setf types_id_n (1- types_id_n))
	     (1+ types_id_n))
      )))


;; A function to obtain the value of a top symbol

(defun get-top-symbol ()
  (proof-shell-invisible-cmd-get-result (format "Set Printing All"))
  (let* ((res (proof-shell-invisible-cmd-get-result (format "Focus")))
	(res2 (subseq res (+ 32 (search "============================" res))))
	(fst-symbol (subseq res2 0 (search " " res2))))
    (cond ((search "->" res2) 7)
	  (t (let ((is (assoc fst-symbol top-symbol-id)))
	       (if is
		   (cdr is)
		 (progn (setf top-symbol-id
			      (append  top-symbol-id  (list (cons fst-symbol top-symbol-n))))
			
			(setf top-symbol-n (1+ top-symbol-n))
			(1- top-symbol-n))))))))



 
;; In some cases the intro tactic does not have parameters, the following function
;; obtain the type of the object introduced with the intro tactic in those cases
;; Sobra
(defun get-obj-intro ()
  (let* ((undo (proof-undo-last-successful-command))
	 (obj (proof-shell-invisible-cmd-get-result (format "Show Intro")))
	 (object (subseq obj 0 (search "
" obj)))
	 (dod (proof-assert-next-command-interactive))
	 (foo (setf hypothesis (append hypothesis (list object)))))
    
    (get-type-id object)
  ))

(defun extract-params (seq res)
  (let ((pos_space (search " " seq))
	(pos_jump (search "
" seq)))
    (if pos_space
	(extract-params (subseq seq (+ 1 pos_space)) (cons (subseq seq 0 pos_space) res))
      (reverse (cons (subseq seq 0 pos_jump) res)))))

(defun extract-params2 (seq res)
  (let ((pos_space (search " " seq))
	(pos_jump (search "." seq)))
    (if pos_space
	(extract-params2 (subseq seq (+ 1 pos_space)) (cons (subseq seq 0 pos_space) res))
      (reverse (cons (subseq seq 0 pos_jump) res)))))

;; Given a list of objects, it obtains the value associated with their types

(defun get-types-list (list res)
  (if (endp list)
      (* -1 res)
    (if (search "_" (car list))
	(get-types-list (cdr list) res)
    (get-types-list (cdr list) (+ (* -1 (get-type-id (car list)) (expt 10 (- (length list) 1))) res)))))


(defun get-types-list-exists (list res)
  (if (endp list)
      (* -1 res)
    (get-types-list-exists (cdr list) (+ (* -1 (get-type-id2 (car list)) (expt 10 (- (length list) 1))) res))))

;; To obtain the number of tactics applied

(defun get-number-list (list)
  (if (endp list)
      0
    (+ (expt 10 (- (length list) 1))  (get-number-list (cdr list)))))

(defun get-number-list2 (list n)
  (if (endp list)
      0
    (+ (* n (expt 10 (- (length list) 1)))  (get-number-list2 (cdr list) n))))

;; To obtain the value associated with top symbol in the case of move

(defun get-top-symbols-list (len res)
  (if (= len 0)
      res
    (let ((gs (get-top-symbol))
	  (ps (proof-shell-invisible-cmd-get-result (format "intro"))))
      (+ (get-top-symbols-list (- len 1) (+ (* gs (expt 10 (- len 1))) res))))))
  
(defun get-top-symbols-seq (seq res)
  (if (endp seq)
      res
    (let ((gs (get-top-symbol))
	  (ps (proof-shell-invisible-cmd-get-result (format (concat "intro " (car seq))))))
      (+ (get-top-symbols-seq (cdr seq) (+ (* gs (expt 10 (- (length seq) 1))) res))))))

;; To obtain the value associated with a theorem

(defun search-in-hyp (obj hyp)
  (if (endp hyp)
      nil
    (if (string= obj (car hyp))
	t
      (search-in-hyp obj (cdr hyp)))))

;;; Auxiliary functions 

(defun remove=> (string)
  (let ((d (search "=>" string)))
    (if d
	(remove=> (concatenate 'string (subseq string 0 d) (subseq string (+ 2 d))))
      string)))


(defun extract-views (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (if (and (string= (subseq (car temp) 0 1) "/") (not (string= (car temp) "//")) (not (string= (car temp) "/=")) (not (string= (car temp) "//=")))
	  (if (not (string= (subseq (car temp) 0 2) "/("))
	      (setf temp2 (append temp2 (list (subseq (car temp) 1))))
	    (setf temp2 (append temp2 (list (subseq (car temp) 2 (search " " (car temp))))))))))


(defun extract-real-params (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (if (not (or (string= (subseq (car temp) 0 1) "/") (string= (car temp) "//") (string= (car temp) "_") 
		   (search "->" (car temp)) (search "<-" (car temp)) (string= (car temp) "/=") (string= (car temp) "//=")))
	  (setf temp2 (append temp2 (list (car temp)))))))

(defun extract-rewrites (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (if (or (search "->" (car temp)) (search "<-" (car temp)))
	  (setf temp2 (append temp2 (list (car temp)))))))

(defun extract-simplifications (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (if (or (string= (car temp) "//") (string= (car temp) "/=") (string= (car temp) "//="))
	  (setf temp2 (append temp2 (list (car temp)))))))

(defun compute-value-simpl (list)
  (list 0 (length list) 0 0))


(defun extract-views-id (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) temp2)
      (if (assoc (car temp) views_id)
	  (setf temp2 (concatenate 'string temp2 (format "%s" (cdr (assoc (car temp) views_id))) ))
	(progn (setf start_view (+ start_view 1)) 
	       (save-view (car temp) start_view)
	       (setf views_id 
		     (append views_id (list (cons (car temp) start_view))))
	       (setf temp2 (concatenate 'string temp2 (format "%s" (cdr (assoc (car temp) views_id)))))))))



(defun compute-values-rewrite-tactic (list)
  (do ((temp (extract-real-params list) (cdr temp))
       (temp2 ""))
      ((endp temp) (string-to-number temp2))
      (let* ((obj1 (if (string= "-" (subseq (car temp) 0 1)) (subseq (car temp) 1) (car temp)))
	     (obj (if (string= "(" (subseq obj1 0 1)) (subseq obj1 1 (search " " obj1)) obj1)))
      (if (assoc obj theorems_id)
	  (setf temp2 (concatenate 'string temp2 (format "%s" (cdr (assoc obj theorems_id)))) )
	(progn (setf start_thm (+ start_thm 1)) 
	       (save-lemma obj start_thm)
	       (setf theorems_id 
		     (append theorems_id (list (cons obj start_thm))))
	       (setf temp2 (concatenate 'string temp2 (format "%s" (cdr (assoc obj theorems_id))))))))))


(defun compute-values-apply-tactic (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) (string-to-number temp2))
      (let ((obj (if (string= "(" (subseq (car temp) 0 1)) (subseq (car temp) 1) (car temp))))
	(if (member obj hypothesis)
	    (setf temp2 (concatenate 'string temp2 "1"))
	  (if (assoc obj theorems_id)
	      (setf temp2 (concatenate 'string temp2 (format "%s" (cdr (assoc obj theorems_id)))) )
	    (progn (setf start_thm (+ start_thm 1)) 
		   (setf theorems_id 
			 (append theorems_id (list (cons obj start_thm))))
		   (setf temp2 (concatenate 'string temp2 (format "%s" (cdr (assoc obj theorems_id)))))))))))


(defun compute-value-views-move (list)
  (list (* -1 (get-number-list2 list 5)) (length list) (* -1 (get-number-list2 list 4)) (string-to-number (extract-views-id list))))

(defun compute-value-views-apply (list)
  (list  (get-number-list2 list 5) (length list) (* -1 (get-number-list2 list 4)) (string-to-number (extract-views-id list))))

(defun compute-value-views-case (list)
  (list  (get-number-list2 list 6) (length list) (* -1 (get-number-list2 list 4)) (string-to-number (extract-views-id list))))

(defun compute-value-views-exact (list)
  (list  (get-number-list2 list 9) (length list) (* -1 (get-number-list2 list 4)) (string-to-number (extract-views-id list))))

(defun compute-value-rewrites (list)
  (list (get-number-list2 list 7) (length list) (* -1 (get-number-list2 list 4)) (get-number-list list)))



(defun remove-empties (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (if (not (string= (car temp) ""))
	  (setf temp2 (append temp2 (list (car temp)))))))



(defun occurrences (c string)
  (do ((temp string)
       (n 0))
      ((not (search c temp)) n)
      (progn (setf n (1+ n))
	     (setf temp (subseq temp (1+ (search c temp)))))))
       

(defun put-together-parenthesis (list)
  (do ((temp list (cdr temp))
       (n 0)
       (temp2 nil)
       (aux ""))
      ((endp temp) temp2)
      (cond ((search "(" (car temp)) 
	     (progn (setf n (1+ n))
		    (setf aux (concatenate 'string aux (car temp) " "))))
	    ((and (search ")" (car temp)) (not (= (- n (occurrences ")" (car temp))) 0)))
	     (progn (setf n (- n (occurrences ")" (car temp))))
		    (setf aux (concatenate 'string aux (car temp) " "))))
	    ((search ")" (car temp))
	     (progn (setf n (1- n))
		    (setf aux (concatenate 'string aux (car temp)))
		    (setf temp2 (append temp2 (list aux)))
		    (setf aux "")))
	    ((not (= n 0))
	     (progn (setf aux (concatenate 'string aux (car temp) " "))))
	    (t (setf temp2 (append temp2 (list (car temp))))
	    ))))
	    
		     
(defun remove-squared-parenthesis (string res)
  (let ((pos1 (search "[" string))
	(pos2 (search "{" string)))
    (cond ((and pos1 pos2)
	   (if (< pos1 pos2)
	       (remove-squared-parenthesis
		(subseq string (1+ (search "]" string :start2 pos1)))
		(concatenate 'string res (subseq string 0 pos1)))
	     (remove-squared-parenthesis
	      (subseq string (1+ (search "}" string :start2 pos2)))
	      (concatenate 'string res (subseq string 0 pos2)))))
	  (pos1 (remove-squared-parenthesis
		(subseq string (1+ (search "]" string :start2 pos1)))
		(concatenate 'string res (subseq string 0 pos1))))
	  (pos2 (remove-squared-parenthesis
		 (subseq string (1+ (search "}" string :start2 pos2)))
		 (concatenate 'string res (subseq string 0 pos2))))
	  (t (concatenate 'string res string)))))


(defun remove-iterations (string)
  (do ((temp string)
       (temp2 ""))
      ((= (length temp) 0) temp2)
      (if (or (string= (subseq temp 0 1) "!") (string= (subseq temp 0 1) "?"))
	  (setf temp (subseq temp 1))
	(progn (setf temp2 (concatenate 'string temp2 (subseq temp 0 1)))
	       (setf temp (subseq temp 1))
	       ))))


      
(defun remove-squared-parenthesis2 (string)
  (do ((temp string)
       (temp2 ""))
      ((= (length temp) 0) temp2)
      (if (or (string= (subseq temp 0 1) "[") (string= (subseq temp 0 1) "]") (string= (subseq temp 0 1) "|"))
	  (setf temp (subseq temp 1))
	(progn (setf temp2 (concatenate 'string temp2 (subseq temp 0 1)))
	       (setf temp (subseq temp 1))
	       ))))
		
	   
(defun extract-params3 (cmd)
 (let* ((res (extract-params2 (remove-iterations (remove-squared-parenthesis cmd "") ) nil))
	(res1 (remove-empties res)))
   (put-together-parenthesis res1)))


(defun extract-params4 (cmd)
 (let* ((res (extract-params2 (remove-squared-parenthesis2 cmd) nil))
	(res1 (remove-empties res)))
   (put-together-parenthesis res1)))



;;; The following functions provide the numbers associated with a concrete tactic


(defun numbers-move=> (cmd top level)
  (let* ((params (extract-params3 (remove=> (subseq cmd (+ 2 (search "=>" cmd)))) ))
	 (views (extract-views params))
	 (simpl (extract-simplifications params))
	 (rewrites (extract-rewrites params))
	 (rewrites-nums (compute-value-rewrites rewrites))
	 (simpl-nums (compute-value-simpl simpl))
	 (views-nums (compute-value-views-move views))
	 (real-params (extract-real-params params))
	 (foo (setf hypothesis (append hypothesis real-params)))
	 (types-params (get-types-list real-params 0))
	 (foo3 (add-info-to-level (list (get-types-list real-params 0) 0 0 0 0 0 0 0 0 0 0 0 0) level))
	 (foo2 (setf move (append move (list (list (get-types-list (if real-params (list (car real-params)) nil) 0)  (get-types-list (cdr real-params) 0)  (* -1 (get-number-list real-params)) top))))))
    (append (list (list (get-number-list2 real-params (cdr (assoc "move" tactic_id))) (length real-params) types-params (* -1 (get-number-list real-params))))
	  (if simpl (list simpl-nums) nil)
	  (if views (list views-nums) nil)
	  (if rewrites (list rewrites-nums) nil))))

(defun numbers-move/ (cmd top level)
  (let* ((params (extract-params3 (remove=> (subseq cmd 4)) ))
	 (views (extract-views params))
	 (simpl (extract-simplifications params))
	 (rewrites (extract-rewrites params))
	 (rewrites-nums (compute-value-rewrites rewrites))
	 (simpl-nums (compute-value-simpl simpl))
	 (views-nums (compute-value-views-move views))
	 (real-params (extract-real-params params))
	 (foo (setf hypothesis (append hypothesis real-params)))
	 (foo3 (add-info-to-level (list 0 0 0 0 0 (nth 2 views-nums) 0 0 0 0 0 0 0) level))
	 (foo2 (setf move/ (append move/ (list (list -4  (* -4 (get-number-list real-params))  (nth 3 views-nums) top)))))
	 (types-params (get-types-list real-params 0)))
    (append (list views-nums) 
	    (if real-params (list (list (get-number-list2 real-params (cdr (assoc "move" tactic_id))) (length real-params) types-params (* -1 (get-number-list real-params)))))
	    (if simpl (list simpl-nums) nil)
	    (if rewrites (list rewrites-nums) nil)))
)

(defun numbers-move: (cmd top level)
  (let* ((params (extract-params3 (subseq cmd (+ 1 (search ":" cmd)))) )
	 (views (extract-views params))
	 (simpl (extract-simplifications params))
	 (rewrites (extract-rewrites params))
	 (rewrites-nums (compute-value-rewrites rewrites))
	 (simpl-nums (compute-value-simpl simpl))
	 (views-nums (compute-value-views-move views))
	 (real-params (extract-real-params params))
	 (types-params (get-types-list real-params 0))
	 (foo3 (add-info-to-level (list (get-types-list real-params 0) 0 0 0 0 0 0 0 0 0 0 0 0) level))
	 (foo2 (setf move (append move (list (list (get-types-list (if real-params (list (car real-params)) nil) 0)  (get-types-list (cdr real-params) 0)  (* 1 (get-number-list real-params)) top))))))
    (append (list (list (* -1 (get-number-list2 real-params (cdr (assoc "move" tactic_id)))) (length real-params) types-params (* -1 (get-number-list real-params))))
	    (if views (list views-nums) nil)
	    (if simpl (list simpl-nums) nil)
	    (if rewrites (list rewrites-nums) nil)))

)

(defun numbers-move< (cmd top level)
  (let* ((foo (list (compute-value-rewrites (list 1))))
	 (foo3 (add-info-to-level (list 0 0 0 0 0 0 0 top 0 0 0 0 0) level))
	 (foo2 (setf rewrite (append rewrite (list (list 4 0 1 top))))))
    foo
    )
)

(defun numbers-apply: (cmd top level)
  (if (string= cmd "apply")
      (list (list (cdr (assoc "apply" tactic_id)) 1 0 0))
  (let ((moves (search "=>" cmd))
	(foo3 (add-info-to-level (list 0 0 0 100 0 0 0 0 0 0 0 0 0) level))
	(foo2 (setf apply (append apply (list (list -4  0 100 top))))))
    (if (not moves)
	(list (list (cdr (assoc "apply" tactic_id))
	      1 
	      -4
	      (compute-values-apply-tactic (extract-real-params (extract-params3 (subseq cmd (+ 1 (if (search ":" cmd) (search ":" cmd) (search " " cmd)))))))))
      (let* ((args0 (extract-params4 (subseq cmd (+ 2 moves))))
	    (simpl (extract-simplifications args0))
	    (simpl-nums (compute-value-simpl simpl))
	    (args (extract-real-params args0))
	    )
      (append (list (list (cdr (assoc "apply" tactic_id))
			  1 -4 
			  (compute-values-apply-tactic (extract-real-params (extract-params3 (subseq cmd (+ 1 (if (search ":" cmd) (search ":" cmd) (search " " cmd))) moves))))
			  ))
	      (list (list (* -1 (get-number-list2 args (cdr (assoc "move" tactic_id)))) (length args) (get-types-list args 0) (* -1 (get-number-list args))))
	      (if simpl (list simpl-nums) nil)))))
))

(defun numbers-elim (cmd top level)
  (let* ((moves (search "=>" cmd))
	 (foo3 (add-info-to-level (list 0 0 (get-types-list (list (car (extract-real-params (extract-params3 (subseq cmd (+ 1 (search ":" cmd)) moves))))) 0) 0 0 0 0 0 0 0 0 0 0) level))
	(foo2 (setf elim (append elim (list (list (get-types-list (list (car (extract-real-params (extract-params3 (subseq cmd (+ 1 (search ":" cmd)) moves))))) 0)  0 -1 top))))))
    (if (not moves)
	(list (list (cdr (assoc "elim" tactic_id))
	      1 (get-types-list (extract-real-params (extract-params3 (subseq cmd (+ 1 (search ":" cmd))))) 0) -1))
      (let* ((args0 (extract-params4 (subseq cmd (+ 2 moves))))
	    (simpl (extract-simplifications args0))
	    (simpl-nums (compute-value-simpl simpl))
	    (args (extract-real-params args0)))
      (append (list (list (cdr (assoc "elim" tactic_id))
			  1 (get-types-list (extract-real-params (extract-params3 (subseq cmd (+ 1 (search ":" cmd)) moves))) 0) -1))
	      (list (list (* -1 (get-number-list2 args (cdr (assoc "move" tactic_id)))) (length args) (get-types-list args 0) (* -1 (get-number-list args))))
	      (if simpl (list simpl-nums) nil))))))

(defun numbers-case (cmd top level)
  (if (string= cmd "case")
      (list (list (cdr (assoc "case" tactic_id)) 1 0 0))
  (let ((moves (search "=>" cmd))
	(foo3 (add-info-to-level (list 0 (if (extract-real-params (extract-params3 (if (search ":" cmd) (subseq cmd (+ 1 (search ":" cmd))) (subseq cmd (+ 1 (search " " cmd))))))
						      (get-types-list (list (car (extract-real-params (extract-params3 (if (search ":" cmd) (subseq cmd (+ 1 (search ":" cmd))) (subseq cmd (+ 1 (search " " cmd)))))))) 0) 1) 0 0 0 0 0 0 0 0 0 0 0) level))
	(foo2 (setf case (append case (list (list (if (extract-real-params (extract-params3 (if (search ":" cmd) (subseq cmd (+ 1 (search ":" cmd))) (subseq cmd (+ 1 (search " " cmd))))))
						      (get-types-list (list (car (extract-real-params (extract-params3 (if (search ":" cmd) (subseq cmd (+ 1 (search ":" cmd))) (subseq cmd (+ 1 (search " " cmd)))))))) 0) 1)
  0 -1 top))))))
    (if (not moves)
	(list (list (cdr (assoc "case" tactic_id))
	      1 (get-types-list (extract-real-params (extract-params3 (if (search ":" cmd) (subseq cmd (+ 1 (search ":" cmd))) (subseq cmd (+ 1 (search " " cmd)))))) 0) -1))
      (let* ((args0 (extract-params4 (subseq cmd (+ 2 moves))))
	    (simpl (extract-simplifications args0))
	    (simpl-nums (compute-value-simpl simpl))
	    (args (extract-real-params args0)))
	(if (extract-params3 (if (search ":" cmd) (subseq cmd (+ 1 (search ":" cmd))) (subseq cmd (+ 1 (search " " cmd)))))
	    (append (list (list (cdr (assoc "case" tactic_id))
				1 (get-types-list (extract-real-params (extract-params3 (if (search ":" cmd) (subseq cmd (+ 1 (search ":" cmd))) (subseq cmd (+ 1 (search " " cmd)))))) 0) -1))
		    (list (list (* -1 (get-number-list2 args (cdr (assoc "move" tactic_id)))) (length args) (get-types-list args 0) (* -1 (get-number-list args))))
		    (if simpl (list simpl-nums) nil))
	  (append (list (list (cdr (assoc "case" tactic_id))
				1 0 0))
		    (list (list (* -1 (get-number-list2 args (cdr (assoc "move" tactic_id)))) (length args) (get-types-list args 0) (* -1 (get-number-list args))))
		    (if simpl (list simpl-nums) nil))))))))

(defun numbers-case/ (cmd top level)
  (let* ((params (extract-params4 (separate-/ (remove=> (subseq cmd 5)) "")))
	 (views (extract-views params))
	 (simpl (extract-simplifications params))
	 (rewrites (extract-rewrites params))
	 (rewrites-nums (compute-value-rewrites rewrites))
	 (simpl-nums (compute-value-simpl simpl))
	 (views-nums (compute-value-views-case views))
	 (real-params (extract-real-params params))
	 (foo (setf hypothesis (append hypothesis real-params)))
	 (types-params (get-types-list real-params 0))
	 (foo3 (add-info-to-level (list 0 0 0 0 0 0 (nth 2 views-nums) 0 0 0 0 0 0) level))
	 (foo2 (setf case/ (append case/ (list (list -4  (/ (nth 2 views-nums) 10)  (nth 3 views-nums) top))))))
    (append (list views-nums)
	    (if real-params (list (list (get-number-list2 real-params (cdr (assoc "move" tactic_id))) (length real-params) types-params (* -1 (get-number-list real-params)))))
	    (if simpl (list simpl-nums) nil)
	    (if rewrites (list rewrites-nums) nil)))
)

(defun separate-/ (string res)
  (let ((pos (search "/" string)))
    (if (not pos)
	(concatenate 'string res string)
      (cond ((= pos 0) (separate-/ (subseq string (1+ pos)) (concatenate 'string "/" res (subseq string 0 pos))))
	    ((not (string= " " (subseq string (1- pos) pos))) 
	     (separate-/ (subseq string (1+ pos)) (concatenate 'string res (subseq string 0 pos) " /")))
	    (t (separate-/ (subseq string (1+ pos)) (concatenate 'string res (subseq string 0 pos))))))))
     

(defun numbers-apply/ (cmd top level)
  (let* ((params (extract-params4 (separate-/ (remove=> (subseq cmd 5)) "")))
	 (views (extract-views params))
	 (simpl (extract-simplifications params))
	 (rewrites (extract-rewrites params))
	 (rewrites-nums (compute-value-rewrites rewrites))
	 (simpl-nums (compute-value-simpl simpl))
	 (views-nums (compute-value-views-apply views))
	 (real-params (extract-real-params params))
	 (foo (setf hypothesis (append hypothesis real-params)))
	 (types-params (get-types-list real-params 0))
	 (foo3 (add-info-to-level (list 0 0 0 0 (nth 2 views-nums) 0 0 0 0 0 0 0 0) level))
	 (foo2 (setf apply/ (append apply/ (list (list -4  (/ (nth 2 views-nums) 10)  (nth 3 views-nums) top))))))
    (append (list views-nums)
	    (if real-params (list (list (get-number-list2 real-params (cdr (assoc "move" tactic_id))) (length real-params) types-params (* -1 (get-number-list real-params)))))
	    (if simpl (list simpl-nums) nil)
	    (if rewrites (list rewrites-nums) nil)))
)

(defun numbers-exact (cmd top level)
  (if (string= cmd "exact")
      (list (list (cdr (assoc "exact" tactic_id)) 1 0 0))
  (let* ((params (extract-params3 (cond ((search ":" cmd) (subseq cmd (+ 1 (search ":" cmd))))
					((search "/" cmd) (subseq cmd (search "/" cmd)))
					(t (subseq cmd (+ 1 (search " " cmd)))))))
	(views (extract-views params))
	(views-nums (compute-value-views-exact views))
	(foo3 (add-info-to-level (list 0 0 0 0 0 0 0 0 0 0 100 0 0) level))
	(foo2 (setf exact (append exact (list (list -4  0 100 top))))))
    (if views 
	(list views-nums)
    (list (list (cdr (assoc "exact" tactic_id))
	      1 
	      -4
	      (compute-values-apply-tactic (extract-real-params params))))))))

(defun numbers-rewrite (cmd top level)
  (let* ((params (extract-params3 (subseq cmd (+ 1 (search " " cmd)))) )
	 (views (extract-views params))
	 (simpl (extract-simplifications params))
	 (simpl-nums (compute-value-simpl simpl))
	 (views-nums (compute-value-views-move views))
	 (foo3 (add-info-to-level (list 0 0 0 0 0 0 0 (get-number-list2 (cdr params) 4) 0 0 0 0 0) level))
	 (foo2 (setf rewrite (append rewrite (list (list -4  (get-number-list2 (cdr params) 4) (compute-values-rewrite-tactic params) top))))))
    (append (list (list (get-number-list2 params (cdr (assoc "rewrite" tactic_id)))
			(length params)
			(get-number-list2 params 4)
			(compute-values-rewrite-tactic params)))
	    (if simpl (list simpl-nums) nil)
	    ))  
)

(defun numbers-exists (cmd top level)
  (let ((moves (search "=>" cmd))
	(foo3 (add-info-to-level (list 0 0 0 0 0 0 0 0 1 0 0 0 0) level))
	(foo2 (setf exists (append exists (list (list 8 0 1 top))))))
    (if (not moves)
	(let* ((params (extract-params3 (subseq cmd 7)) )
	       (types-params (get-types-list-exists params 0))
	       )
	  (list (list (cdr (assoc "exists" tactic_id)) 1 types-params 0)))
      (let* ((args0 (extract-params4 (subseq cmd (+ 2 moves))))
	    (simpl (extract-simplifications args0))
	    (simpl-nums (compute-value-simpl simpl))
	    (args (extract-real-params args0)))
      (append (list (list (cdr (assoc "exists" tactic_id))
			  1 (get-types-list-exists  (extract-params3 (subseq cmd 7 moves)) 0) -1))
	      (list (list (* -1 (get-number-list2 args (cdr (assoc "move" tactic_id)))) (length args) (get-types-list args 0) (* -1 (get-number-list args))))
	      (if simpl (list simpl-nums) nil)))))
  )
    

(defun numbers-done (cmd top level)
  (progn 
    (add-info-to-level (list 0 0 0 0 0 0 0 0 0 top 0 0 0) level)
    (setf done (append done (list (list 0 0 0 top))))
    (list (list (cdr (assoc "[]" tactic_id)) 1 0 0) ) )
)


(defun remove-multiple-spaces (string)
  (let ((d (search "  " string)))
    (if d
	(remove-multiple-spaces (concatenate 'string (subseq string 0 d) (subseq string (1+ d))))
      string)))
  


(defun compute-numbers-cmd (cmd top level)
  (let* ((cmd1 (remove-multiple-spaces cmd)))
    (cond ((search "symmetry" cmd) nil)	  
	  ((search "last by" cmd) (compute-numbers-cmd (subseq cmd (+ 3 (search "by" cmd))) top level))
	  ((search "first by" cmd) (compute-numbers-cmd (subseq cmd (+ 3 (search "by" cmd))) top level))
	  ((string= "try" (subseq cmd 0 2)) (compute-numbers-cmd (subseq cmd (+ 4 (search "try" cmd))) top level))
	  ((string= "do" (subseq cmd 0 2)) (compute-numbers-cmd (subseq cmd (cond ((search "!" cmd) (1+ (search "!" cmd)))
								    ((search "?" cmd) (1+ (search "?" cmd)))
								    (t (+ 3 (search "do" cmd))))) top level)) 
	  ((search "have" cmd) nil)
	  ((or (search "move=>" cmd1) (search "move =>" cmd1)) (numbers-move=> cmd1 top level))
	  ((or (search "move:" cmd1) (search "move :" cmd1)) (numbers-move: cmd1 top level))
	  ((or (search "move/" cmd1) (search "move /" cmd1)) (numbers-move/ cmd1 top level))
	  ((or (search "move<-" cmd1) (search "move->" cmd1) (search "move ->" cmd1) (search "move <-" cmd1)) (numbers-move< cmd1 top level))
	  ((or (search "apply/" cmd1) (search "apply /" cmd1)) (numbers-apply/ cmd1 top level))
	  ((or (search "apply:" cmd1) (search "apply :" cmd1) (search "apply" cmd1)) (numbers-apply: cmd1 top level))
	  ((or (search "elim:" cmd1) (search "elim :" cmd1)) (numbers-elim cmd1 top level))
	  ((or (search "case/" cmd1) (search "case /" cmd1)) (numbers-case/ cmd1 top level))
	  ((or (search "case:" cmd1) (search "case" cmd1)) (numbers-case cmd1 top level))
	  ((or (search "exact" cmd1) (search "exact :" cmd1)) (numbers-exact cmd1 top level))
	  ((search "rewrite" cmd1) (numbers-rewrite cmd1 top level))
	  ((search "exists" cmd1) (numbers-exists cmd1 top level))
	  ((or (search "[]" cmd1) (search "done" cmd1) (search "constructor" cmd1)) (numbers-done cmd1 top level))
	  
	  ((string= (subseq cmd1 0 4) "pose") nil)
	  ((string= (subseq cmd1 0 3) "set") nil)
	  ((string= (subseq cmd1 0 4) "left") nil)
	  ((string= (subseq cmd1 0 4) "righ") nil)
	  )
    )  
  ) 


(defun split-command (cmd result end)
  (if (or (string= " " (subseq cmd 0 1)) (string= "-" (subseq cmd 0 1)))
	  (split-command (subseq cmd 1) result end)
  (let ((is_by (string= "by" (subseq cmd 0 2))))
    (if is_by
	(split-command (subseq cmd 3) result 1)
      (let ((comma (search ";" cmd)))
	(if comma
	    (split-command (subseq cmd (1+ comma)) (append result (list (subseq cmd 0 comma))) end)
	  (list (append result (list (subseq cmd 0 (1- (length cmd))))) end)))))))


    
     
(defun add-tactics (tactics end top level)
  (do ((temp tactics (cdr temp))
       (temp2 nil))
      ((endp temp) (if (> end 0) (append temp2 (list (list 0 1 0 0))) temp2))
      (let ((res (compute-numbers-cmd (car temp) top level)))
	(if res (setf temp2 (append temp2 res))))))


;The first value is the tactic, the second one is the number of tactics,
;the third one is the argument type, the fourth one is if the
;argument is a hypothesis of a theorem, the fifth one is the top-symbol
;and the last one the number of subgoals


(defun get-numbers (cmd top level)
  (let* ((res (split-command cmd nil 0))
	 (tactics (car res))
	 (end (cadr res))
	 (nums (add-tactics tactics end top level)))
    (if nums (do ((temp (cdr nums) (cdr temp))
	 (temp2 (list (format "%s" (nth 0 (car nums))) (nth 1 (car nums))   (format "%s" (nth 2 (car nums))) (format "%s" (nth 3 (car nums))))))
	((endp temp) (list (string-to-number (nth 0 temp2)) (nth 1 temp2)  (string-to-number (nth 2 temp2)) (string-to-number (nth 3 temp2))) )
	(setf temp2 (list (if (or (< (string-to-number(nth 0 temp2)) 0) (< (nth 0 (car temp)) 0))
			      (concatenate 'string (format "-%s" (abs (string-to-number(nth 0 temp2)))) (format "%s" (abs (nth 0 (car temp)))))
			    (concatenate 'string (format "%s" (abs (string-to-number(nth 0 temp2))) ) (format "%s" (abs (nth 0 (car temp))))))
			  (+ (nth 1 temp2) (nth 1 (car temp)))
			  (if (or (< (abs (string-to-number(nth 2 temp2))) 0) (< (nth 2 (car temp)) 0))
			      (concatenate 'string (format "-%s" (abs (abs (string-to-number(nth 2 temp2))))) (format "%s" (abs (nth 2 (car temp)))))
			    (concatenate 'string (format "%s" (abs (abs (string-to-number(nth 2 temp2))))) (format "%s" (abs (nth 2 (car temp))))))
			  (if (or (< (string-to-number (nth 3 temp2)) 0) (< (nth 3 (car temp)) 0))
			      (concatenate 'string (format "-%s" (abs (string-to-number (nth 3 temp2)))) (format "%s" (abs (nth 3 (car temp)))))
			    (concatenate 'string (format "%s" (abs (string-to-number (nth 3 temp2)))) (format "%s" (abs (nth 3 (car temp))))))
			 ))
	)
    )))

;; Function to obtain the information just about the goals. 

(defun count-seq (item seq)
  (let ((is? (search item seq)))
    (if is?
	(+ 1 (count-seq item (subseq seq (+ 1 is?))))
	0)))

(defun get-number-of-goals ()
  (let ((r (proof-shell-invisible-cmd-get-result (format "Show Proof"))))
    (count-seq "?" r)))


(defun flat (ll)
  (if (endp ll)
    nil
    (append (car ll) (flat (cdr ll)))))




;; The following function computes the result of the tactic


(defun digits (n)
  (if (= (mod n 10) 0)
      0
    (1+ (digits (/ n 10)))))

(defun first-digit (n digits)
  (/ n (expt 10 (1- digits))))

(defun rest-of-digits (n digits)
  (- n (* (first-digit n digits) (expt 10 (1- digits)))))

(defun obtain-tactic-result (tactic)
  (do ((temp (cdr tactic) (cdr temp))
       (temp2 (if (endp tactic) (list 0 0 0 0 0)
		  (list (first-digit (nth 0 (car tactic)) (digits (nth 0 (car tactic))))
			(* (rest-of-digits (nth 0 (car tactic)) (digits (nth 0 (car tactic)))) (expt 10 (length (cdr tactic))))
			(* (nth 1 (car tactic)) (expt 10 (length (cdr tactic))))
			(nth 2 (car tactic))
			(nth 3 (car tactic))))))
      ((endp temp) temp2)
    (setf temp2 (list (nth 0 temp2)
		      (+ (nth 1 temp2) (* (expt 10 (length (cdr temp))) (nth 0 (car temp))))
		      (+ (nth 2 temp2) (* (expt 10 (length (cdr temp))) (nth 1 (car temp))))
		      (concat (format "%s" (nth 3 temp2)) (format "%s" (nth 2 (car temp))))
		      (+ (nth 4 temp2) (nth 3 (car temp))))
      )
  ))




(defvar useless-terms '("Defined" "Structure" "Section" "Add Ring" "Hypothesis" "Hypotheses" "Include" "Export" "Parameter" "Axiom"
"End" "Notation" "Hint" "Inductive" "Variable" "Implicit" "Import" "Canonical" "Coercion"
"Module" "Ltac" "Let" "Opaque" "Bind" "Scope" "Require" "Infix" "Record" "Fact" "Print"))

(defun is-in-search (cmd)
  (do ((temp useless-terms (cdr temp))
       (is nil))
      ((or (endp temp) is) is)
      (if (search (car temp) cmd) (setf is t))))



(defun compute-tactic-value (list)
  (if (not list) (list 0 0 0 0 0)
  (let ((len (length list))
	(arg0 (car (car list)))
	(arg1 (format "%s" (nth 1 (car list))))
	(hyp (format "%s" (nth 2 (car list))))
	(top (format "%s" (nth 3 (car list)))))
    (do ((temp (cdr list) (cdr temp)))
	((endp temp) (list arg0 (string-to-number arg1) (string-to-number hyp) (string-to-number top) len))
	(progn (setf arg1 (format "%s%s%s" arg1 (nth 0 (car temp)) (nth 1 (car temp))))
	       (setf hyp (format "%s%s" hyp (nth 2 (car temp))))
	       (setf top (format "%s%s" top (nth 3 (car temp))))
	      )))))



(defun compute-tactic-result (name)
  (append (list name) (list (append
	(compute-tactic-value move)
	(compute-tactic-value case)
	(compute-tactic-value elim)
	(compute-tactic-value apply/)
	(compute-tactic-value move/)
	(compute-tactic-value case/)
	(compute-tactic-value rewrite)
	(compute-tactic-value exists)
	(compute-tactic-value done)
	(compute-tactic-value exact)))))
	 
(defun compute-proof-tree-result (name)
  (append (list name) (list (append
	(if tdl1 tdl1 (generate-zeros 13))
	(if tdl2 tdl2 (generate-zeros 13))
	(if tdl3 tdl3 (generate-zeros 13))
	(if tdl4 tdl4 (generate-zeros 13))
	(if tdl5 tdl5 (generate-zeros 13))))))
	




(defun export-theorem-aux (result name)
  (let* ((semis (save-excursion
		 (skip-chars-backward " \t\n"
				      (proof-queue-or-locked-end))
		 (proof-segment-up-to-using-cache (point))))
	 (comment (caar semis))
	 (cmd (cadar semis))
	 (pos_dot (search "." cmd))
	 (pos_space (search " " cmd))
	 (ts nil))
    (if semis 
	(cond ((or (string= comment "comment") 
	       (is-in-search cmd))
	   (progn (proof-assert-next-command-interactive)
		  (export-theorem-aux result name)))
	      
	      ((or (search "Definition" cmd) (search "Fixpoint" cmd))
	       (progn (proof-assert-next-command-interactive)
		      (ignore-errors(adddefinition (subseq cmd (1+ (search " " cmd)) 
							   (search " " cmd :start2 (1+ (search " " cmd))))))
		      (export-theorem-aux result 
					  (subseq cmd (1+ (search " " cmd)) 
						  (search " " cmd :start2 (1+ (search " " cmd))))
							      ))
	       (proof-assert-next-command-interactive)
	       )

	      ((search "Lemma" cmd)
	   (progn (proof-assert-next-command-interactive)
		  (export-theorem-aux result 
				      (subseq cmd (1+ (search " " cmd)) 
					      (search " " cmd :start2 (1+ (search " " cmd))))
				      )))
	  ((search "Proof" cmd)
	   (progn (proof-assert-next-command-interactive)
		  (export-theorem-aux result name )))
	  ((search "Theorem" cmd)
	   (progn (proof-assert-next-command-interactive)
		  (export-theorem-aux result 
				      (subseq cmd (1+ (search " " cmd)) 
					      (search " " cmd :start2 (1+ (search " " cmd))))
				     )))
	  ((or (search "Qed." cmd) (search "Defined." cmd))
	   (progn (proof-assert-next-command-interactive)
		  ; (insert (format "\n(* %s *)\n" (reverse result)))
		  ;(setf proof-tree-level (append proof-tree-level (list (compute-proof-result)))) 
		  ;(setf tactic-level (append tactic-level (list (compute-tactic-result))))
		  (setf tactic-level (append tactic-level (list (compute-tactic-result name))))
		  (setf proof-tree-level (append proof-tree-level (list (compute-proof-tree-result name))))
		  (if name
		      (split-feature-vector name (flat (reverse result)))
		    ;  (setf saved-theorems (append saved-theorems 
	;				       (list (list name (flat (reverse result))))))
		      )
		  (ignore-errors (addthm name))
		  ))
	  (t (progn (setf ts (get-top-symbol))
		    (setf ng (get-number-of-goals))
		    (proof-assert-next-command-interactive)
		    (setf ng2 (get-number-of-goals))
		    (export-theorem-aux (cons (append (get-numbers cmd ts current-level) (list ts) (list ng2)) result)
				       name)
		    (add-info-to-level (list 0 0 0 0 0 0 0 0 0 0 0 ng2 (if (< ng2 ng) 1 0)) current-level)
		    (setf current-level (1+ current-level))
		    
		    ))))))
     



(defun split-feature-vector (name fv)
  (let ((len (1+ (floor (length fv) 30))))
    (do ((i 0 (+ i 1)))
	((equal i len) nil)
	(setf saved-theorems (append saved-theorems 
				     (list (list name (take-30-from fv i))))))
    ))


(defun take-30-from (list pos)
  (let ((j (* 30 pos)))
  (do ((i j (1+ i))
       (temp2 nil (if (nth i list) (cons (nth i list) temp2) (cons 0 temp2))))
      ((= i (+ j 30)) (reverse temp2)))))




;;; Functions to save the files

(defun save-file-conventions1 ()
  (interactive)
  (let ((file (read-file-name "Save in file (don't include the extension): ")))
    (progn (with-temp-file (concat file "_goals.csv") (insert (extract-features-1)))
	   (with-temp-file (concat file "_tactics.csv") (insert (extract-features-2 tactic-level)))
	   (with-temp-file (concat file (format "_summary.txt")) (insert (extract-names))))))

	 
(defun extract-names ()
  (do ((temp saved-theorems (cdr temp))
       (temp2 "")
       (i 1 (1+ i)))
      ((endp temp) temp2)
    (setf temp2 (concat temp2 (format "%s %s\n" i (remove_last_colon (caar temp)))) )))





(defun print-list (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) (subseq temp2 0 (1- (length temp2))))
    (cond ((equal (car temp) 1.0e+INF) (setf temp2 (concat temp2 (format "%s," 100)) ))
	  ((equal (car temp) -1.0e+INF) (setf temp2 (concat temp2 (format "%s," -100)) ))
	  (t (setf temp2 (concat temp2 (format "%s," (car temp))) )))))


(defun last-part-of-lists (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (setf temp2 (append temp2 (list (cadar temp))))))




(defun extract-features-1 ()
  (let ((fm (find-max-length)))
    (do ((temp (last-part-of-lists saved-theorems) (cdr temp))
	 (temp2 ""))
	((endp temp) temp2)
	(setf temp2 (concat temp2 
			      (format "%s\n"
				      (print-list  (take-30 (append (car temp) 
								   (generate-zeros 30))) ))))
      )
    ))






(defun extract-features-2 (list)
  (do ((temp (last-part-of-lists (cdr list)) (cdr temp))
       (temp2 ""))
      ((endp temp) temp2)
      (setf temp2 (concat temp2 (format "%s\n" (print-list (car temp)))))))
      


(defun generate-zeros (n)
  (do ((i 0 (1+ i))
       (temp nil (cons 0 temp)))
      ((= i n) temp)))

(defun find-max-length ()
  (do ((temp saved-theorems (cdr temp))
       (i 0))
      ((endp temp) i)
    (if (< i (length (cadar temp)))
	(setf i (length (cadar temp)))
      nil)))

(defun take-30 (list)
  (do ((i 0 (1+ i))
       (temp list (cdr temp))
       (temp2 nil (cons (car temp) temp2)))
      ((= i 30) (reverse temp2))))


;; Function which extract the info of a theorem up to a concrete point

(defvar tactic-temp nil)
(defvar proof-tree-temp nil)

(defun extract-info-up-to-here ()
  (interactive)
  (setf move nil
	case nil
	elim nil
	apply nil
	apply/ nil
	move/ nil
	case/ nil
	rewrite nil
	exists nil
	done nil
	exact nil
	tactic-temp nil
	tdl1 nil 
	tdl2 nil
	tdl3 nil
	tdl4 nil
	tdl5 nil
	current-level 1
	dot-level nil)
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
	       (setf result (cons (append (get-numbers cmd ts current-level) (list ts) (list ng2)) result)))
	       (add-info-to-level (list 0 0 0 0 0 0 0 0 0 0 0 ng2 (if (< ng2 ng) 1 0)) current-level)
	       (setf current-level (1+ current-level))
		    )
	  
	)	
    )
    (setf tactic-temp (cadr (compute-tactic-result "")))
    (setf proof-tree-temp (cadr (compute-proof-tree-result "")))
    (take-30 (append (flat (reverse result)) (generate-zeros 30) ))
    (proof-shell-invisible-cmd-get-result (format "Unset Printing All"))
  ))



(defun extract-features-1-bis (thm)
  (let ((fm (find-max-length)))
    (do ((temp (append (last-part-of-lists saved-theorems) (list thm)) (cdr temp))
	 (temp2 ""))
	((endp temp) temp2)
	(setf temp2 (concat temp2 
			      (format "%s\n"
				      (print-list (take-30 (append (car temp) 
								   (generate-zeros 30))) ))))
      )
    ))


(defun extract-features-2-bis (thm list)
  (let ((fm (find-max-length)))
    (do ((temp (append (last-part-of-lists (cdr list)) (list thm)) (cdr temp))
	 (temp2 ""))
	((endp temp) temp2)
	(setf temp2 (concat temp2 
			      (format "%s\n"
				      (print-list (car temp)))))
      )
    ))




 

;; Function which extract the information from all the theorems up to a point

(defun extract-feature-theorems ()
  (interactive)
  (let ((final (point))
	(current-level 1)
	(last-point -1))
    (export-theorem)
    (while (and (< (point) final) (not (= (point) last-point)))
      (progn (setq last-point (point))
	     (export-theorem)))))
    


;;; Function to normalize the results

(defun max-two-lists (list1 list2)
  (do ((temp1 (take-30 (append list1 (generate-zeros 24))) (cdr temp1))
       (temp2 (take-30 (append list2 (generate-zeros 24))) (cdr temp2))
       (temp nil))
      ((endp temp1) temp)
      (if (< (car temp1) (car temp2))
	  (setf temp (append temp (list (car temp2))))
	(setf temp (append temp (list (car temp1))))
	)))

(defun min-two-lists (list1 list2)
  (do ((temp1 (take-30 (append list1 (generate-zeros 24))) (cdr temp1))
       (temp2 (take-30 (append list2 (generate-zeros 24))) (cdr temp2))
       (temp nil))
      ((endp temp1) temp)
      (if (> (car temp1) (car temp2))
	  (setf temp (append temp (list (car temp2))))
	(setf temp (append temp (list (car temp1))))
	)))

(defun max-position (list )
  (do ((temp list (cdr temp))
       (max (generate-zeros (length (car list)))))
      ((endp temp) max)
      (setf max (max-two-lists max (car temp)))))

(defun min-position (list )
  (do ((temp list (cdr temp))
       (min (generate-zeros (length (car list)))))
      ((endp temp) min)
      (setf min (min-two-lists min (car temp)))))


(defun normalize-list (list max min)
  (do ((temp (take-30 (append list (generate-zeros 24))) (cdr temp))
       (temp-max max (cdr temp-max))
       (temp-min min (cdr temp-min))
       (temp2 nil))
      ((endp temp) temp2)
      (cond ((< 0 (car temp)) (setf temp2 (append temp2 (list (/ (+ (car temp) .0) (car temp-max))))))
	    ((= 0 (car temp)) (setf temp2 (append temp2 (list 0))))
	    (t (setf temp2 (append temp2 (list (- (/ (+ (car temp) .0) (car temp-min))))))))))

(defun normalize (list)
  
  (let ((max (max-position list))
	(min (min-position list)))
   (do ((temp list (cdr temp))
	 (temp2 nil))
	((endp temp) temp2)
	(setf temp2 (append temp2 (list (normalize-list (car temp) max min)))))))






 
    
