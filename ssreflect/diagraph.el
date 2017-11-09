(defun concatenate-string-list (l)
  (do ((temp l (cdr temp))
       (res ""))
      ((endp temp) res)
    (setf res (concatenate 'string res (car temp)))))




(defun arrow (s0 s1 arrow n)
  (let ((gs0 (concatenate-string-list (mapcar (lambda (x) (format "%s\\n" x)) s0)))
	(gs1 (concatenate-string-list (mapcar (lambda (x) (format "%s\\n" x)) s1)))
	(ar (concatenate-string-list (mapcar (lambda (x) (format "%s\\n" x)) (remove-duplicates arrow :test #'equal)))))
(if arrow
    (format "G%s -> G%s [label=\"%s\"]
G%s [label=\"%s\"]
G%s [label=\"%s\"];" n (1+ n) ar n gs0 (1+ n) gs1))))


(defun last-arrow (s0 arrow n)
  (let ((gs0 (concatenate-string-list (mapcar (lambda (x) (format "%s\\n" x)) s0)))
	(ar (concatenate-string-list (mapcar (lambda (x) (format "%s\\n" x)) (remove-duplicates arrow :test #'equal)))))
(if arrow (format "G%s -> G%s [label=\"%s\"]
G%s [label=\"%s\"]
G%s [shape=plaintext,label=\"\"];" n (1+ n) ar n gs0 (1+ n) ))))

;;;; (last-arrow  '(c d e) '(1 2) 0)

(defun several-last-arrows (arrows)
  (do ((temp arrows (cdr temp))
       (res ""))
      ((endp temp) res)
    (setf res (concat res (last-arrow (nth 0 (car temp))
				  (nth 1 (car temp))
				  (nth 2 (car temp))) "\n"))))


;;;; (arrow '(a b c) '(c d e) '(1 2) 0)

(defun several-arrows (arrows larrows)
  (do ((temp arrows (cdr temp))
       (res "digraph G {\n graph [rankdir=LR]\n"))
      ((endp temp) (concatenate 'string res (several-last-arrows larrows)))
    (setf res (concatenate 'string res 
			   (arrow (nth 0 (car temp))
				  (nth 1 (car temp))
				  (nth 2 (car temp))
				  (nth 3 (car temp)))
			   "\n"))))

;;; (several-arrows '(((a b c) (c d e) (1 2) 0) ((a b c) (c d e) (3 4) 0) ((c d e) (f g) (5) 1)) '((f g) (5 6) 2))
 


(defun generate-arrow-parts (arrows last-arrows)
  (several-arrows arrows last-arrows))
  


(defun show-diagram (text)
  (with-temp-file "temp.gv"
    (insert text))
  (shell-command "dot -Tpng temp.gv -o temp.png; eog temp.png"))



;;; (show-diagram '(((a b c) (c d e) (1 2) 0) ((a b c) (c d e) (3 4) 0) ((c d e) (f g) (5) 1))
;;;               '(()))

(defun removenil (l)
  (do ((temp l (cdr temp))
       (res nil))
      ((endp temp) res)
    (if (car temp)
	(setf res (cons (car temp) res)))))


(defun count-nil (l)
  (if (member nil l)
      (1+ (count-nil (cdr (member nil l))))
    0))


(defun removenil-n (l n)
  (if (= n 0)
      l
    (removenil-n (removenil l) (1- n))))


(defun generate-circles (g1 g2 g3 g4)
  (let ((str ""
;"G0 [shape=circle];
;G1 [shape=circle];
;G2 [shape=circle];
;G3 [shape=circle];
;G4 [shape=circle];"
))
    (progn (if (member nil g1)
	       (setf str (concat str "\nG1 [peripheries=2];")))
	   (if (member nil (removenil-n g2 (count-nil g1)))
	       (setf str (concat str "\nG2 [peripheries=2];")))
	   (if (member nil (removenil-n g3 (count-nil (removenil-n g2 (count-nil g1)))))
	       (setf str (concat str "\nG3 [peripheries=2];")))
	   (if (member nil (removenil-n g4 (count-nil (removenil-n g3 (count-nil (removenil-n g2 (count-nil g1)))))))
	       (setf str (concat str "\nG4 [peripheries=2];"))))
    str))







(defun generate-props (props)
  (let ((str ""))
    (progn (if (nth 0 props)
	       (setf str (concat str (format "G0 -> G6[constraint=false,arrowhead=none];\nG6[shape=rectangle,label=\"%s\"];\n"
					     (concatenate-string-list (mapcar (lambda (x) (format "%s\\n" x)) (nth 0 props)))))))
	   (if (nth 1 props)
	       (setf str (concat str (format "G0 -> G7 [style=invis];
G1 -> G7[constraint=false,arrowhead=none];\nG7[shape=rectangle,label=\"%s\"];\n" 
					     (concatenate-string-list (mapcar (lambda (x) (format "%s\\n" x)) (nth 1 props)))))))
	   (if (nth 2 props)
	       (setf str (concat str (format "G1 -> G8 [style=invis];
G2 -> G8[constraint=false,arrowhead=none];\nG8[shape=rectangle,label=\"%s\"];\n" 
					     (concatenate-string-list (mapcar (lambda (x) (format "%s\\n" x)) (nth 2 props)))))))
	   (if (nth 3 props)
	       (setf str (concat str (format "G2 -> G9 [style=invis];
G3 -> G9[constraint=false,arrowhead=none];\nG9[shape=rectangle,label=\"%s\"];\n" 
					     (concatenate-string-list (mapcar (lambda (x) (format "%s\\n" x)) (nth 3 props)))))))
	   (if (nth 4 props)
	       (setf str (concat str (format "G3 -> G10 [style=invis];
G4 -> G10[constraint=false,arrowhead=none];\nG10[shape=rectangle,label=\"%s\"];\n" 
					     (concatenate-string-list (mapcar (lambda (x) (format "%s\\n" x)) (nth 4 props))))))))))


;; (setf props '((arg1 arg2) nil (arg3 arg4) nil (arg5)))
;; (generate-props props)
	     




 
	

(require 'cl)

;;; (setf patches '(((a 1) (b 2) (c 3) (d 4) (e 5)) ((f 6) (g 7) (h 8) (i 9))))

(defun generate-diagram-from-patches (patches props)
  (let* ((p0 (mapcar (lambda (x) (nth 0 x)) patches))
	 (p1 (mapcar (lambda (x) (nth 1 x)) patches))
	 (p2 (mapcar (lambda (x) (nth 2 x)) patches))
	 (p3 (mapcar (lambda (x) (nth 3 x)) patches))
	 (p4 (mapcar (lambda (x) (nth 4 x)) patches))
	 (g0 (mapcar (lambda (x) (car x)) p0))
	 (g1 (mapcar (lambda (x) (car x)) p1))
	 (g2 (mapcar (lambda (x) (car x)) p2))
	 (g3 (mapcar (lambda (x) (car x)) p3))
	 (g4 (mapcar (lambda (x) (car x)) p4))
	 (t0 (mapcar (lambda (x) (nth 1 x)) p0))
	 (t1 (mapcar (lambda (x) (nth 1 x)) p1))
	 (t2 (mapcar (lambda (x) (nth 1 x)) p2))
	 (t3 (mapcar (lambda (x) (nth 1 x)) p3))
	 (t4 (mapcar (lambda (x) (nth 1 x)) p4))
	 (arrow0 (mapcar (lambda (x) (list (removenil g0) (removenil g1) (list x) 0)) (removenil t0)))
	 (arrow1 (mapcar (lambda (x) (list (removenil g1) (removenil g2) (list x) 1)) (removenil t1)))
	 (arrow2 (mapcar (lambda (x) (list (removenil g2) (removenil g3) (list x) 2)) (removenil t2)))
	 (arrow3 (mapcar (lambda (x) (list (removenil g3) (removenil g4) (list x) 3)) (removenil t3)))
	 (arrow4 (mapcar (lambda (x) (list (removenil g4) (list x) 4)) (removenil t4))))
    (concat (generate-arrow-parts (append arrow0 arrow1 arrow2 arrow3) arrow4)
	    (generate-circles g1 g2 g3 g4)
	    (generate-props props) "}"))
    )

;;; (setf patches2 '(((a 1) (b 2) (c 3) (d 4) (e 5)) ((f 6) (g 7) (h 8) (i 9))))
;;; (setf props '((arg1 arg2) nil (arg3 arg4) nil (arg5)))
;;; (show-diagram (generate-diagram-from-patches patches2 props))
;; (insert (generate-diagram-from-patches patches2 props))	    



;(setf patches 
 ;     '((("powersum1 (step 1)"  "elim")
;	 ("powersum1 (step 2)"   "rewrite")
;	 ("powersum1 (step 3)"   "move")
;	 ("powersum1 (step 4)"   "rewrite")
;	 ("powersum1 (step 5)"   "ring"))
;	(("powersum2 (step 1)"   "elim")
;	 ("powersum2 (step 2)"   "rewrite")
;	 ("powersum2 (step 3)"   "move")
;	 ("powersum2 (step 4)"   "rewrite")
;	 )))

;
;(setf props '(nil ("type of tactic argument") nil ("type of tactic argument" "top symbol") ("tactic argument")))


;(show-diagram (generate-diagram-from-patches patches props))

