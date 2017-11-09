;;; The menu interaction

(easy-menu-define statistics-menu global-map "Statistics"
  '("Statistics" 
    ("Configuration" 
     ("ML system"
;       ["Matlab" (change-ml-system "m")
;	:selected (string= ml-system "m")
;	:style toggle
;	:help "Use Matlab as ML system"]
       ["Weka" (change-ml-system "w")
	:selected (string= ml-system "w")
	:style toggle
	:help "Use Weka as ML system"])
     ("Level"
       ["Goal level" (change-level "g")
	:selected (string= level "g")
	:style toggle
	:help "Use goal level"]
       ["Tactic level" (change-level "t")
	:selected (string= level "t")
	:style toggle
	:help "Use tactic level"]
       ["Proof tree level" (change-level "p")
	:selected (string= level "p")
	:style toggle
	:help "Use Proof tree level"])
     ("Algorithm"
       ["K-means" (change-algorithm "k")
	:selected (string= algorithm "k")
	:style toggle
	:help "Use k-means algorithm"]
       ["Gaussian" (change-algorithm "g")
	:selected (string= algorithm "g")
	:style toggle
	:active (string= ml-system "m")
	:help "Use Gaussian algorithm"]
       ["EM" (change-algorithm "e")
	:selected (string= algorithm "e")
	:style toggle
	:active (string= ml-system "w")
	:help "Use Simple EM algorithm"]
       ["FarthestFirst" (change-algorithm "f")
	:selected (string= algorithm "f")
	:style toggle
	:active (string= ml-system "w")
	:help "Use FarhestFirst algorithm"])
      ("Granularity"
       ["1"  (change-granularity 1)
	:selected (eq granularity-level 1)
	:style toggle
	:help "We will use 3 clusters"]
       ["2"  (change-granularity 2)
	:selected (eq granularity-level 2)
	:style toggle
	:help "We will use 5 clusters"]
       ["3"  (change-granularity 3)
	:selected (eq granularity-level 3)
	:style toggle
	:help "We will use 10 clusters"]
       ["4"  (change-granularity 4)
	:selected (eq granularity-level 4)
	:style toggle
	:help "We will use 15 clusters"]
       ["5"  (change-granularity 5)
	:selected (eq granularity-level 5)
	:style toggle
	:help "We will use 20 clusters"])
      ("Frequencies"
       ["1" (change-frequency 1)
	:selected (eq frequency-precision 1)
	:style toggle
	:help "The experiments will be run 100 times"]
       ["2" (change-frequency 2)
	:selected (eq frequency-precision 2)
	:style toggle
	:help "The experiments will be run 500 times"]
       ["3" (change-frequency 3)
	:selected (eq frequency-precision 3)
	:style toggle
	:help "The experiments will be run 1000 times"])
    ;  ["Iterative search of similarities" (change-iterative-search)
	;:selected iterative
	;:style toggle
	;:available (string= ml-system "m")
	;:help "With this option, the search of similarities is iterative"]
    ;["Automatically export proved theorems" (change-save)
	;:selected save-automatically
	;:style toggle
	;:help "With this option, when a theorem is proved its information is automatically saved"]
)
    ("Generate Similarity graph"
       ["Similarity graph of definitions" (dependencygraph-defs)]
       ["Similarity graph of lemma statements" (dependencygraph-statements)]
       ["Similarity graph of proofs" (dependencygraph-proof)]
       ["Term tree of a lemma statement" (showtreegraphthm)])
    ["Extract info up to point" (extract-feature-theorems)
     :keys "C-c C-SPC"]
    ["Show clusters" (show-clusters-bis)]
    ["Show clusters definitions" (cluster-definitions)]
    ["Show similar theorems" (show-clusters-of-theorem)]
    ["Export library" (save-numbers)
     :keys "C-c n"]
    ["Show cluster libraries" (exported-libraries)]
    ["Activate Icons" (activate-icons)]
))

(easy-menu-remove-item global-map '("menu-bar") "Statistics")

(easy-menu-add-item nil nil statistics-menu "help-menu") 



(defun activate-icons ()
  (interactive)
  (progn 
    (easy-menu-remove-item nil '("Statistics") "Activate Icons")
    (define-key coq-mode-map [tool-bar statistical-hint-statements]
      (list 'menu-item "Similar Theorems" 'show-similarities-statement
		  :help "Similar Theorems"
		  :image (list 'image :type 'xpm 
				:file (concat home-dir "icons/sh-hint-thm.xpm"))))
    (define-key coq-mode-map [tool-bar clustering-statements]
      (list 'menu-item "Clustering Statements" 'cluster-statements
		  :help "Clustering Statements"
		  :image (list 'image :type 'xpm 
				:file (concat home-dir "icons/clustering-thms.xpm"))))
    (define-key coq-mode-map [tool-bar statistical-hint-defs]
      (list 'menu-item "Similar Definitions" 'show-similarities-last-def
		  :help "Similar Definitions"
		  :image (list 'image :type 'xpm 
				:file (concat home-dir "icons/sh-hint-def.xpm"))))
    (define-key coq-mode-map [tool-bar clustering-defs]
      (list 'menu-item "Clustering Definitions" 'cluster-definitions
		  :help "Clustering Definitions"
		  :image (list 'image :type 'xpm 
				:file (concat home-dir "icons/clustering-defs.xpm"))))
    (define-key coq-mode-map [tool-bar statistical-hint]
      (list 'menu-item "Statistical Hint" 'show-clusters-of-theorem
		  :help "Statistical Hint"
		  :image (list 'image :type 'xpm 
				:file (concat home-dir "icons/sh-hint.xpm"))))
    (define-key coq-mode-map [tool-bar clustering]
      (list 'menu-item "Clustering" 'show-clusters-bis
		  :help "Clustering"
		  :image (list 'image :type 'xpm 
				:file (concat home-dir "icons/clustering.xpm"))))
    
    ))




(defvar ml-system "w")
(defvar algorithm "k")
(defvar granularity-level 3)
(defvar frequency-precision 1)
(defvar iterative nil)
(defvar save-automatically nil)
(defvar level "g")


(defun change-level (n)
  (setq level n))

(defun change-algorithm (s)
  (setq algorithm s))

(defun change-ml-system (s)
  (setq ml-system s)
  (setq algorithm "k")
  (cond ((string= s "w")
	 (setq iterative nil)
	 ))
  )
  
(defun change-granularity (n)
  (setq granularity-level n))

(defun change-frequency (n)
  (setq frequency-precision n))

(defun change-iterative-search ()
  (setq iterative (not iterative)))

(defun change-save ()
  (setq save-automatically (not save-automatically)))

      
;(easy-menu-add-item nil '("Statistics") statistics-menu "help-menu") 

(defun change-algorithm-interactive ()
  (interactive)
  (let ((alg (read-string 
	      "What algorithm do you want to use (k-means -> k, Gaussian -> g): ")))
    (setf algorithm (cond ((string= "g" alg) "g") 
			  ((string= "k" alg) "k")
			  (t algorithm)))))

(defun change-granularity-interactive ()
  (interactive)
  (let ((alg (read-string 
	      "Introduce the granularity level (values from 1 to 5): ")))
    (setf granularity-level (cond ((string= "1" alg) 1) 
				  ((string= "2" alg) 2)
				  ((string= "3" alg) 3)
				  ((string= "4" alg) 4)
				  ((string= "5" alg) 5)
				  (t granularity-level)))))

(defun change-frequency-interactive ()
  (interactive)
  (let ((alg (read-string 
 "Introduce the precision of the frequencies that you want to obtain (values from 1 to 3): ")))
    (setf frequency-precision (cond ((string= "1" alg) 1) 
				  ((string= "2" alg) 2)
				  ((string= "3" alg) 3)
				  (t frequency-precision)))))

(defun change-iterative-interactive ()
  (interactive)
  (let ((alg (read-string 
 "Do you want to perform iterative search? (yes -> y, no -> n): ")))
    (setf iterative (cond ((string= "y" alg) 1) 
			  ((string= "n" alg) 2)
			  (t iterative)))))



(defun exported-libraries ()
  (interactive)
  (easy-menu-remove-item nil '("Statistics") "Show cluster libraries")
  (easy-menu-add-item nil '("Statistics") 
		      (cons "Available libraries for clustering:"
			   (cons ["Current" nil
			    :selected t
			    :style toggle
			    :help "Use the current library for clustering"]
			   (select-libraries)))))


(defun select-libraries ()
  (available-libraries)
  (available-dirs)
  (append (select-libraries-aux libs nil) (libraries-dirs)))


(defun select-libraries-aux (temp temp2)
  (if (endp temp)
      temp2
    (select-libraries-aux (cdr temp) (append temp2 (list (menu-library (car temp)))))))




(defvar libs nil)

(defun available-libraries ()
  (shell-command  (concat "ls " home-dir "libs/coq | grep .csv | wc -l"))
  (let ((n nil)
	(i 0))
  (with-current-buffer "*Shell Command Output*"
    (beginning-of-buffer)
    (setq n (string-to-number (format "%s"  (read (current-buffer))))))
  (shell-command  (concat "ls " home-dir "libs/coq | grep .csv"))
  (with-current-buffer "*Shell Command Output*"
    (progn (beginning-of-buffer)
	   (while (< i n)
	     (let ((r (format "%s" (read (current-buffer)))))
	       (progn (setq i (1+ i))
		      (setq libs (append libs (list (subseq r 0 (search "." r))))))))))))



(defvar dirs nil)

(defun available-dirs ()
  (shell-command  (concat "ls -d " home-dir "libs/coq/*/ | wc -l"))
  (let ((n nil)
	(i 0))
  (with-current-buffer "*Shell Command Output*"
    (beginning-of-buffer)
    (setq n (string-to-number (format "%s"  (read (current-buffer))))))
  (shell-command  (concat "ls -d " home-dir "libs/coq/*/"))
  (with-current-buffer "*Shell Command Output*"
    (progn (beginning-of-buffer)
	   (while (< i n)
	     (let ((r (format "%s" (read (current-buffer)))))
	       (progn (setq i (1+ i))
		      (setq dirs (append dirs (list (subseq r (length (concat home-dir "libs/coq/")) (1- (length r)))))))))))
  ))




(defun libraries-dirs ()
  (do ((temp dirs (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (setf temp2 (append temp2 (list (append (list (car temp)) (libraries-dir (car temp))))))))
      


(defun libraries-dir (dir)
  (shell-command  (concat "ls " home-dir "libs/coq/" dir "/ | grep _names | wc -l"))
  (let ((n nil)
	(i 0)
	(temp nil))
  (with-current-buffer "*Shell Command Output*"
    (beginning-of-buffer)
    (setq n (string-to-number (format "%s"  (read (current-buffer))))))
  (shell-command  (concat "ls " home-dir "libs/coq/" dir "/ | grep _names"))
  (with-current-buffer "*Shell Command Output*"
    (progn (beginning-of-buffer)
	   (while (< i n)
	     (let* ((r1 (format "%s" (read (current-buffer))))
		    (r (subseq r1 0 (search "_names" r1))))
	       (progn (setq i (1+ i))
		      (setq temp (append temp (list (menu-library-dir (subseq r 0 (search "." r)) dir)))))))
))
  temp))



(defun menu-library-dir (item dir)
  (vector item (list 'change-library (concat dir "/" item)) 
    :selected (list 'string-member (concat dir "/" item) 'libs-menus)
    :style 'toggle
    :help (format "Use the %s library for clustering" item)))

(defun menu-library (item)
  (vector item (list 'change-library item) 
    :selected (list 'string-member item 'libs-menus)
    :style 'toggle
    :help (format "Use the %s library for clustering" item)))



(defvar libs-menus nil)

(defun string-member (string list)
  (do ((temp list (cdr temp))
       (is nil))
      ((or (endp temp) is) is)
      (if (string= string (car temp))
	  (setf is t))))


(defun change-library (string)
  (if (string-member string libs-menus)
      (remove-from-menus string)
    (setq libs-menus (append libs-menus (list string)))))


(defun remove-from-menus (string)
  (do ((temp libs-menus (cdr temp))
       (temp2 nil))
      ((endp temp) (setf libs-menus temp2))
      (if (not (string= string (car temp)))
	  (setf temp2 (append temp2 (list (car temp)))))))




  


	  


