;;++++++++++++++++++++++++++++++++++++++++++++++++++
;; Drawing trees
;;++++++++++++++++++++++++++++++++++++++++++++++++++

(defvar treepos 0)

(defun treetographvizaux (parent tree p)
  (do ((temp tree (cdr temp))
       (res ""))
      ((endp temp) res)
    (if (listp (car temp))
	(progn (setf treepos (1+ treepos))
	       (setf res (concat res 
				 (format "%s[label=\"%s\"];\n" p parent)
				 (format "%s -> %s[arrowhead=none];\n" p treepos)
				 (treetographvizaux (caar temp) (cdar temp) treepos))))
      (progn (setf treepos (1+ treepos))
	     (setf res (concat res 
			       (format "%s[label=\"%s\"];\n" p parent)
			       (format "%s -> %s[arrowhead=none];\n" p treepos)
			       (format "%s[label=\"%s\"];\n" treepos (car temp))))))))



(defun treetographviz (tree)
  (progn (setf treepos 0)
	 (concat "digraph G {" (treetographvizaux (car tree) (cdr tree) 0) "\n}")))



(defun show-diagram-tree (text)
  (with-temp-file "temp.gv"
    (insert text))
  (progn (shell-command "rm temp.png")
	 (shell-command "dot -Tpng temp.gv -o temp.png; eog temp.png &")))
  


(defun showtreegraph (tree)
  (show-diagram-tree (treetographviz tree)))


;(showtreegraph '("eq   : ?16712 -> ?16712 -> Prop " ("leq   : nat -> nat -> bool " "m : nat" "n : nat") ("leq   : nat -> nat -> bool " ("double   : nat -> nat " ("half   : nat -> nat " "m : nat")) "n : nat")))