;;++++++++++++++++++++++++++++++++++++++++++++++++++
;; Generating graphs about clusters
;;++++++++++++++++++++++++++++++++++++++++++++++++++

;;--------------------------------------------------
;; Cluster graph definitions
;;--------------------------------------------------

(defvar clustercounter 0)

(defun clusterofone (lst)
  (do ((temp lst (cdr temp))
       (res ""))
      ((endp temp) res)
    (if (listp (car temp))
	(progn (setf clustercounter (1+ clustercounter))
	       (setf res (concat res (format "subgraph cluster%s {\n" clustercounter)
				 (clusterofone (car temp))
				 "\n}\n")))
      (if (and (cdr temp) (not (listp (cadr temp))))
	  (setf res (concat res 
			(format "%s [URL=\"./%s.html#%s\"]; %s -> %s[style=invis]\n" (car (nth (1- (car temp)) tables-definitions)) 
				(library-belong (1- (car temp)))
				(car (nth (1- (car temp)) tables-definitions))
				(car (nth (1- (car temp)) tables-definitions)) 
				(car (nth (1- (cadr temp)) tables-definitions)) )))
	(setf res (concat res 
			  (format "%s [URL=\"./%s.html\"];\n" (car (nth (1- (car temp)) tables-definitions))
				  (library-belong (1- (car temp))))))))))


(defun clusterofseveral (lol)
  (progn (setf clustercounter 0)
	 (concat "digraph {\n rankdir=LR;\n" (clusterofone lol) "\n}")))



(defun show-diagram-clusters (text)
  (with-temp-file "temp.gv"
    (insert text))
  (progn (shell-command "dot -Tcmap temp.gv -o temp.map")
	 (shell-command "dot -Tpng temp.gv -o temp.png")
	 (createwebpage)
	 (shell-command "xdg-open temp.html")))


(defun showclustergraph (lol)
  (show-diagram-clusters (clusterofseveral lol)))



(defun createwebpage ()
  (with-temp-file "temp.html"
    (insert 
     (format "<head><title>Dependency Diagram</title></head>\n<body><img src=\"temp.png\" usemap=\"#depend\"/>
<map id=\"depend\" name=\"depend\">%s</map></body>" (read-lines "temp.map")))))




;(showclustergraph '((1 2 3) ((2 5) (6 7)) 8 9))
 
(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))




(defun issubcluster (cluster1 cluster2)
  (do ((temp cluster1 (cdr temp))
       (res nil))
      ((or (endp temp) res) (not res))
    (if (not (member (car temp) cluster2))
	(setf res t))))



(defun replacecluster (cluster1 cluster2)
 (if (endp cluster2)
    (list cluster1)
  (if (listp (car cluster2))
	(if (issubcluster cluster1 (car cluster2))
	    (cons (replacecluster cluster1 (car cluster2)) 
		  (cdr cluster2))
	  (cons (car cluster2) (replacecluster cluster1 (cdr cluster2))))
     (if (member (car cluster2) cluster1)
	  (replacecluster cluster1 (cdr cluster2))
	(cons (car cluster2) (replacecluster cluster1 (cdr cluster2)))))))





;(defun replacecluster (cluster1 cluster2)
 ; (do ((temp cluster2 (cdr temp))
  ;     (flag nil)
   ;    (res nil))
    ;  ((or (endp temp) flag)
     ;  (if flag 
;	   (reverse res)
;	 (append (reverse res) (list cluster1))))
 ;   (cond ((listp (car temp))
;	   (if (issubcluster cluster1 (car temp))
;	    (progn (setf res (cons (replacecluster cluster1 (car temp)) (cdr temp)))
;		   (setf flag t))
;	    (setf res (cons (car temp) res))))
;	  (t
;	   (if (not (member (car temp) cluster1))
;	     (setf res (cons (car temp) res)))))))




(defun subclusters (cluster clusters)
 (do ((temp clusters (cdr temp))
      (flag nil)
      (flag1 nil)
      (res nil))
     ((or (endp temp) flag flag1)
      (cond (flag (append (reverse res) temp))
	    (flag1 clusters)
	    (t (append (reverse res) (list cluster)))))
   (cond ((not (issubcluster cluster (car temp)))
	  (setf res (cons (car temp) res)))
	 ((equal cluster (car temp))
	  (progn (setf res (reverse clusters))
		 (setf flag1 t)))
	 (t (progn (setf res (cons (replacecluster cluster (car temp)) res))
		   (setf flag t))))))



;(defun subclusters (cluster clusters)
 ; (if (endp clusters)
  ;    (list cluster)
   ; (if (issubcluster cluster (car clusters))
;	(if (equal cluster (car clusters))
;	    clusters
;	  (cons (replacecluster cluster (car clusters))
;		(cdr clusters)))
 ;     (cons (car clusters) (subclusters cluster (cdr clusters))))))




(defun subclustersseveral (clusters1 clusters2)
  (do ((temp clusters1 (cdr temp))
       (temp2 clusters2))
      ((endp temp) temp2)
    (setf temp2 (subclusters (car temp) temp2))))



;(showclustergraph (subclusters '(1 2) '((3 4 5) (7 8 9) (6 1 2))))

(defun dependencygraph-defs ()
  (interactive)
  (let ((clusters1 nil)
;	(clusters2 nil)
	(clusters3 nil))
    (setf granularity-level 3)
    (weka-defs)
    (sleep-for 2)
    (setf clusters1 (cdr (form-clusters (extract-clusters-from-file-defs ) (floor (length tables-definitions) 5))))
    ;(setf granularity-level 4)
    ;(weka-defs)
    ;(sleep-for 2)
    ;(setf clusters2 (cdr (form-clusters (extract-clusters-from-file-defs ) (floor (length tables-definitions) 4))))
    (setf granularity-level 5)
    (weka-defs)
    (sleep-for 2)
    (setf clusters3 (cdr (form-clusters (extract-clusters-from-file-defs ) (floor (length tables-definitions) 2))))
    (showclustergraph (subclustersseveral  clusters3 clusters1 ))))
	  



;;--------------------------------------------------
;; Cluster graph lemma statements
;;--------------------------------------------------



(defun clusterofone-statements (lst)
  (do ((temp lst (cdr temp))
       (res ""))
      ((endp temp) res)
    (if (listp (car temp))
	(progn (setf clustercounter (1+ clustercounter))
	       (setf res (concat res (format "subgraph cluster%s {\n" clustercounter)
				 (clusterofone-statements (car temp))
				 "\n}\n")))
      (if (and (cdr temp) (not (listp (cadr temp))))
	  (setf res (concat res 
			(format "%s [URL=\"./%s.html#%s\"]; %s -> %s[style=invis]\n" (car (nth (1- (car temp)) tables-thms)) 
				(library-belong-thm (1- (car temp)))
				(car (nth (1- (car temp)) tables-thms))
				(car (nth (1- (car temp)) tables-thms)) 
				(car (nth (1- (cadr temp)) tables-thms)) )))
	(setf res (concat res 
			  (format "%s [URL=\"./%s.html\"];\n" (car (nth (1- (car temp)) tables-thms))
				  (library-belong-thm (1- (car temp))))))))))


(defun clusterofseveral-statements (lol)
  (progn (setf clustercounter 0)
	 (concat "digraph {\n rankdir=LR;\n" (clusterofone-statements lol) "\n}")))



(defun showclustergraph-statements (lol)
  (show-diagram-clusters (clusterofseveral-statements lol)))


(defun dependencygraph-statements ()
  (interactive)
  (let ((clusters1 nil)
;	(clusters2 nil)
	(clusters3 nil))
    (setf granularity-level 3)
    (weka-thms)
    (sleep-for 2)
    (setf clusters1 (cdr (form-clusters (extract-clusters-from-file-defs ) (floor (length tables-thms) 5))))
    ;(setf granularity-level 4)
    ;(weka-defs)
    ;(sleep-for 2)
    ;(setf clusters2 (cdr (form-clusters (extract-clusters-from-file-defs ) (floor (length tables-definitions) 4))))
    (setf granularity-level 5)
    (weka-thms)
    (sleep-for 2)
    (setf clusters3 (cdr (form-clusters (extract-clusters-from-file-defs ) (floor (length tables-thms) 2))))
    (showclustergraph-statements (subclustersseveral  clusters3 clusters1 ) )
   ; (showclustergraph-statements clusters1 )
    ))
	  





;;--------------------------------------------------
;; Cluster graph proofs
;;--------------------------------------------------



(defun clusterofone-proof (lst)
  (do ((temp lst (cdr temp))
       (res ""))
      ((endp temp) res)
    (if (listp (car temp))
	(progn (setf clustercounter (1+ clustercounter))
	       (setf res (concat res (format "subgraph cluster%s {\n" clustercounter)
				 (clusterofone-proof (car temp))
				 "\n}\n")))
      (if (and (cdr temp) (not (listp (cadr temp))))
	  (let ((thm nil)
		(thm2 nil))
	    (progn (if (<= (car temp) (length saved-theorems))
		       (setf thm (car (nth (1- (car temp)) saved-theorems)))
		     (progn (shell-command (concat "cat "(expand-file-name "names_temp.txt") " | sed -n '" 
						     (format "%s" (- (car temp) (length saved-theorems)))
						  "p'")) 
			      (with-current-buffer "*Shell Command Output*"
				(beginning-of-buffer)
				(read (current-buffer))
				(setf thm (format "%s"  (read (current-buffer)))))))	
		   (if (<= (car temp) (length saved-theorems))
		       (setf thm2 (car (nth (1- (cadr temp)) saved-theorems)))
		     (progn (shell-command (concat "cat "(expand-file-name "names_temp.txt") " | sed -n '" 
						     (format "%s" (- (cadr temp) (length saved-theorems)))
						  "p'")) 
			      (with-current-buffer "*Shell Command Output*"
				(beginning-of-buffer)
				(read (current-buffer))
				(setf thm2 (format "%s"  (read (current-buffer)))))))
		   (setf res (concat res 
			(format "%s; %s -> %s[style=invis]\n" 
				thm 
				thm 
				thm2)))))
	(let ((thm nil))
	    (progn (if (<= (car temp) (length saved-theorems))
		       (setf thm (car (nth (1- (car temp)) saved-theorems)))
		     (progn (shell-command (concat "cat "(expand-file-name "names_temp.txt") " | sed -n '" 
						     (format "%s" (- (car temp) (length saved-theorems)))
						  "p'")) 
			      (with-current-buffer "*Shell Command Output*"
				(beginning-of-buffer)
				(read (current-buffer))
				(setf thm (format "%s"  (read (current-buffer)))))))	
		   (setf res (concat res (format "%s;\n" thm)))))))))
				  
				 


(defun clusterofseveral-proof (lol)
  (progn (setf clustercounter 0)
	 (concat "digraph {\n rankdir=LR;\n" (clusterofone-proof lol) "\n}")))



(defun showclustergraph-proof (lol)
  (show-diagram-clusters (clusterofseveral-proof lol)))


(defun dependencygraph-proof ()
  (interactive)
  (if libs-menus
	(progn (with-temp-file (expand-file-name "temp.csv")  (cond ((string= level "g") (insert (extract-features-1)))
								     ((string= level "t") (insert (extract-features-2 tactic-level)))
								     ((string= level "p") (insert (extract-features-2 proof-tree-level)))))
	       (add-libraries-temp)
	       (add-names))
      (with-temp-file (expand-file-name "temp.csv") (insert (extract-features-1))))
  (let ((clusters1 nil)
;	(clusters2 nil)
	(clusters3 nil))
    (setf granularity-level 3)
    (weka (floor (size-temp) 5))
    (sleep-for 2)
    (setf clusters1 (cdr (form-clusters (extract-clusters-from-file (floor (size-temp) 5)) (floor (size-temp) 5))))
    ;(setf granularity-level 4)
    ;(weka-defs)
    ;(sleep-for 2)
    ;(setf clusters2 (cdr (form-clusters (extract-clusters-from-file-defs ) (floor (length tables-definitions) 4))))
    (setf granularity-level 5)
    (weka (floor (size-temp)  2))
    (sleep-for 2)
    (setf clusters3 (cdr (form-clusters (extract-clusters-from-file (floor (size-temp)  2)) (floor (size-temp)  2))))
    (showclustergraph-proof (subclustersseveral (removenil (remove-if-empty clusters3)) (removenil (remove-if-empty clusters1))))))
	  



(defun remove-if-empty (lol)
  (do ((temp lol (cdr temp))
       (res nil))
      ((endp temp) res)
      (do ((temp2 (car temp) (cdr temp2))
           (res2 nil))
          ((endp temp2) (setf res (cons res2 res)))
          (if (not (string= "" (car (nth (1- (car temp2)) saved-theorems)) ))
              (setf res2 (cons (car temp2) res2))))))

