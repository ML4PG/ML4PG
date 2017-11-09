;;;==================================================
;;; Connection with Automaton Generator tool
;;;==================================================

(require 'image-dired)

(defun extract-theorem-proof (name)
  (let* ((b (beginning-of-buffer))
	 (l (search-forward name))
	 (beg (point))
	 (e (search-forward "Qed."))
	 (end (point)))
    (concat "Lemma " name " " (buffer-substring-no-properties beg end) "\n\n")
    ))



(defun extract-theorem-proof2 (file name)
  (with-temp-buffer 
      (insert-file-contents (concat home-dir "exported-libs/ssreflect/" file ".v"))
    (let* ((b (beginning-of-buffer))
	   (l (search-forward name))
	   (beg (point))
	   (e (search-forward "Qed."))
	   (end (point)))
      (concat "Lemma " name " " (buffer-substring-no-properties beg end) "\n\n")
      )))


(defun extract-theorem-proof-recursive (l)
  (do ((temp l (cdr temp))
       (res ""))
      ((endp temp) res)
    (if (search ":" (car temp))
	(setf res (concat res (extract-theorem-proof2 (subseq (car temp) 0 (search ":" (car temp)))
						      (subseq (car temp) (1+ (search ":" (car temp)))))))
      (setf res (concat res (extract-theorem-proof (car temp)))))
    ))



(defun save-trace (l)
  (let ((res (extract-theorem-proof-recursive l)))
    (with-temp-file 
	(concat home-dir "automaton/trace0.txt") (insert res))))


(defvar buf nil)

(defun generate-automaton (l)
  (progn 
    (with-current-buffer (buffer-name buf)
    (save-trace l))
    (with-temp-file 
	(concat home-dir "automaton/obtaintrace")
      (insert (format 
    "#!/bin/bash

java -jar %spreprocessor.jar ss $1 > %strace.txt;
java -jar %sEFSMTool012014-withdeps.jar -input %strace.txt -strategy noloops -k 1 > %strace1.txt;
B=$(expr $(wc -l %strace1.txt | cut -d\" \" -f 1) - $(grep -rne digraph %strace1.txt | cut -d: -f 1) + 1);
tail -$B %strace1.txt > %strace2.txt
E=$(grep -rne \"}\" %strace2.txt | cut -d: -f 1);
head -$E %strace2.txt > %strace.gv;
dot -Tpng %strace.gv -o %strace.png;  eog %strace.png &"
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/"))))
    (sit-for 1)
    (shell-command (concat home-dir "automaton/obtaintrace "  
			   (concat home-dir "automaton/trace0.txt")))))





(defun 30-to-5x6 (l1)
  (let ((l (append l (generate-zeros 30))))
    (list (subseq l 0 6)
	  (subseq l 6 12)
	  (subseq l 12 18)
	  (subseq l 18 24)
	  (subseq l 24 30))))

(defun print-list-patch (l)
  (let ((r (mapcar (lambda (x) (print-list x)) (30-to-5x6 l))))
    (do ((temp r (cdr temp))
	 (res ""))
	((endp temp) res)
	(setf res (concat res (car temp) "\n")))))
	



(defun generate-file-patch (i patch)
  (let ((res (print-list-patch patch)))
    (with-temp-file 
	(concat home-dir "automaton/patchtrace" (format "%s" i) ".txt") (insert res))))




(defun generate-automaton-patches (l)
  (do ((i 0 (1+ i)))
      ((equal i (length l)) (invoke-patch-automaton l))
      (generate-file-patch i (nth (1- (nth i l)) saved-theorems-libs)))
  )


(defun invoke-patch-automaton (l)
  (with-temp-file 
	(concat home-dir "automaton/obtaintrace")
      (insert (format 
    "#!/bin/bash

java -jar %spreprocessor.jar patches %s > %strace.txt;
java -jar %sEFSMTool012014-withdeps.jar -input %strace.txt -strategy noloops -k 1 > %strace1.txt;
B=$(expr $(wc -l %strace1.txt | cut -d\" \" -f 1) - $(grep -rne digraph %strace1.txt | cut -d: -f 1) + 1);
tail -$B %strace1.txt > %strace2.txt
E=$(grep -rne \"}\" %strace2.txt | cut -d: -f 1);
head -$E %strace2.txt > %strace.gv;
dot -Tpng %strace.gv -o %strace.png;  eog %strace.png &"
(concat home-dir "automaton/")
(do ((i 0 (1+ i))
     (res ""))
    ((equal i (length l)) res)
    (setf res (concat res " " (concat home-dir "automaton/patchtrace" (format "%s" i) ".txt")))) 
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/")
(concat home-dir "automaton/"))))
(sit-for 1)
    (shell-command (concat home-dir "automaton/obtaintrace" )))





(defun split-tactics (thm)
  (do ((temp thm)
       (res nil))
      ((not (search "." temp)) (reverse res))
    (progn (setf res (cons (subseq temp 0 (search "." temp)) res))
	   (setf temp (subseq temp (1+ (search "." temp))))))) 


(defun take (list n m)
  (nthcdr n (butlast list (- (length list) m))))


(defun merge (name patch tac-patches l)
  (cond ((equal patch 'l) 
	 (do ((temp tac-patches (cdr temp))
	      (i (- l 5) (1+ i))
	      (res nil))
	     ((endp temp) (reverse res))
	   (setf res (cons (list (format "%s (Step %s)" name i) (format "%s: %s" name (car temp))) res))))
	((equal patch 'u) 
	 (do ((temp tac-patches (cdr temp))
	      (i 1 (1+ i))
	      (res nil))
	     ((endp temp) (reverse res))
	   (setf res (cons (list (format "%s (Step %s)" name i) (format "%s: %s" name (car temp))(car temp)) res))))
	(t (do ((temp tac-patches (cdr temp))
	      (i (1+ (* 5 (1- patch))) (1+ i))
	      (res nil))
	     ((endp temp) (reverse res))
	   (setf res (cons (list (format "%s (Step %s)" name i) (format "%s: %s" name (car temp))) res)))
)))





(defun extract-patch-theorem (name patch)
  (let* ((thm (extract-theorem-proof name))
	 (thm1 (remove-jumps (subseq thm (+ 6 (search "Proof" thm)) (search "Qed" thm))))
	 (tactics (split-tactics thm1))
	 (tac-patches (cond ((equal patch 'l) (take tactics (- (length tactics) 5) (length tactics)))
			    ((equal patch 'u) (take tactics 0 5))
			    (t (take tactics (* 5 (1- patch)) (+ 5 (* 5 (1- patch))))))))
    (merge name patch tac-patches (length tactics))))
    




(defun which-patch2 (n m)
  (cond ((equal n 0)
	 1)
	((and (not (equal (car (nth n saved-theorems)) (car (nth (- n 1) saved-theorems))))
	      (not (equal (car (nth n saved-theorems)) (car (nth (+ n 1) saved-theorems)))))
	 'u)
	((and (equal (car (nth n saved-theorems)) (car (nth (- n 1) saved-theorems)))
	      (not (equal (car (nth n saved-theorems)) (car (nth (+ n 1) saved-theorems)))))
	 'l)
	((equal (car (nth n saved-theorems)) (car (nth (- n 1) saved-theorems))) 
	 (which-patch (1- n) (1+ m)))
	(t m)))


(defun extract-patch-theorems-recursive (l l2)
  (do ((temp l (cdr temp))
       (temp2 l2 (cdr temp2))
       (res nil))
      ((endp temp) res)
    (setf res (cons (extract-patch-theorem (car temp) (which-patch2 (car temp2) 1)) res))))



(defun generate-automaton2 (l l2)
  (with-current-buffer (buffer-name buf)
    (show-diagram (generate-diagram-from-patches (extract-patch-theorems-recursive l l2) (list-of-similarities)))))


(defun replace-i (i x lis)
  (if (= 0 i)
      (cons (cons x (car lis)) (rest lis))
    (cons (car lis) (replace-i (1- i) x (cdr lis)))))




(defun list-of-similarities ()
(let ((sim (list nil nil nil nil nil)))
  (do ((temp (why-are-similar) (cdr temp)))
      ((endp temp) nil)
    (let ((level (+ 1 (floor (car temp) 6)))
	  (inst (+ 1 (mod (car temp) 6))))
      (cond ((equal inst 1) 
	   (cond ((equal level 1) (setf sim (replace-i 0  (format " - Tactic(s) applied.") sim)))
		 ((equal level 2) (setf sim (replace-i 1  (format " - Tactic(s) applied.") sim)))
		 ((equal level 3) (setf sim (replace-i 2  (format " - Tactic(s) applied.") sim)))
		 ((equal level 4) (setf sim (replace-i 3  (format " - Tactic(s) applied.") sim)))
		 ((equal level 5) (setf sim (replace-i 4  (format " - Tactic(s) applied.") sim)))
		 ))
	    ((equal inst 2) 
	     (cond ((equal level 1) (setf sim (replace-i 0  (format " - Number of tactics.") sim)))
		   ((equal level 2) (setf sim (replace-i 1  (format " - Number of tactics.") sim)))
		   ((equal level 3) (setf sim (replace-i 2  (format " - Number of tactics.") sim)))
		   ((equal level 4) (setf sim (replace-i 3  (format " - Number of tactics.") sim)))
		   ((equal level 5) (setf sim (replace-i 4  (format " - Number of tactics.") sim)))
		   ))
	    ((equal inst 3) 
	     (cond ((equal level 1) (setf sim (replace-i 0  (format " - Type of tactic arguments.") sim)))
		   ((equal level 2) (setf sim (replace-i 0  (format " - Type of tactic arguments.") sim)))
		   ((equal level 3) (setf sim (replace-i 0  (format " - Type of tactic arguments.") sim)))
		 ((equal level 4) (setf sim (replace-i 0  (format " - Type of tactic arguments.") sim)))
		 ((equal level 5) (setf sim (replace-i 0  (format " - Type of tactic arguments.") sim)))
		 ))
	    ((equal inst 4) 
	     (cond ((equal level 1) (setf sim (replace-i 0  (format " - Applied lemma.") sim)))
		   ((equal level 2) (setf sim (replace-i 1  (format " - Applied lemma.") sim)))
		   ((equal level 3) (setf sim (replace-i 2  (format " - Applied lemma.") sim)))
		   ((equal level 4) (setf sim (replace-i 3  (format " - Applied lemma.") sim)))
		   ((equal level 5) (setf sim (replace-i 4  (format " - Applied lemma.") sim)))
		   ))
	    ((equal inst 5) 
	     (cond ((equal level 1) (setf sim (replace-i 0  (format " - Top symbol.") sim)))
		   ((equal level 2) (setf sim (replace-i 1  (format " - Top symbol.") sim)))
		   ((equal level 3) (setf sim (replace-i 2  (format " - Top symbol.") sim)))
		   ((equal level 4) (setf sim (replace-i 3  (format " - Top symbol.") sim)))
		   ((equal level 5) (setf sim (replace-i 4  (format " - Top symbol.") sim)))
		   ))
	    ((equal inst 6) 
	     (cond ((equal level 1) (setf sim (replace-i 0  (format " - Number of subgoals.") sim)))
		   ((equal level 2) (setf sim (replace-i 1  (format " - Number of subgoals.") sim)))
		   ((equal level 3) (setf sim (replace-i 2  (format " - Number of subgoals.") sim)))
		   ((equal level 4) (setf sim (replace-i 3  (format " - Number of subgoals.") sim)))
		   ((equal level 5) (setf sim (replace-i 4  (format " - Number of subgoals.") sim)))
		   ))
	    )
      
			 )
		      )
  sim))

