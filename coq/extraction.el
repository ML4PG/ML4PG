(require 'cl)

(defun extract-list (lis level res)
  (setf res (append res (list (list (car lis) (length (cdr lis)) level))))
  (do ((temp (cdr lis) (cdr temp)))
      ((endp temp) res)
    (if (listp (car temp))
	(setf res (append res (extract-list (car temp) (1+ level) nil)))
      (setf res (append res (list (list (car temp) 0 (+ 1 level))))))))

(defun quicksort-triple (list n)
  (if (<= (length list) 1)
      list
      (let ((pivot (nth n (car list))))
	(append	
	 (quicksort-triple (remove-if-not #'(lambda (x) (< (nth n x) pivot)) list) n) 
	 (remove-if-not #'(lambda (x) (= (nth n x) pivot)) list)
	 (quicksort-triple (remove-if-not #'(lambda (x) (> (nth n x) pivot)) list) n)
          ))))

;; Examples


;(extract-list '(reverse (append x y)) 1 nil)

;(extract-list '(implies (and (consp x)
;		(consp y))
;	   (equal (reverse (append x y))
;		  (append (reverse x) (reverse y)))) 1 nil)
  
;(quicksort-triple (extract-list '(implies (and (consp x)
;		(consp y))
;	   (equal (reverse (append x y))
;		  (append (reverse x) (reverse y)))) 1 nil))


 


(defun arity_1 (formulas vars)
  (do ((temp formulas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (and (equal (nth 1 (car temp)) 0) (member (nth 0 (car temp)) vars))
	(setf temp2 (append temp2 (list (list (nth 0 (car temp))
					      -1
					      (nth 2 (car temp))))))
      (setf temp2 (append temp2 (list (car temp)))))))
      


(defun extract-info (thm vars)
  (let ((name (cadr thm))
	(thm1 (car (cddr thm))))
    (append (list name) (arity_1 (quicksort-triple (extract-list thm1 1 nil) 2) vars))))




;(extract-info '(defthm foo 
;		 (implies (and (consp x)
;			       (consp y))
;			  (equal (reverse (append x y))
;				 (append (reverse x) (reverse y))))))

;(extract-level (cdr (extract-info '(defthm foo 
;		 (implies (and (consp x)
;			       (consp y))
;			  (equal (reverse (append x y))
;				 (append (reverse x) (reverse y))))))) 1)





(defun extract-level (formulas level)
  (do ((temp formulas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (equal (nth 2 (car temp)) level)
	(setf temp2 (append temp2 (list (car temp)))))))


(defun extract-arity (formulas arity)
  (do ((temp formulas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (equal (nth 1 (car temp)) arity)
	(setf temp2 (append temp2 (list (car temp))))))
  )




;(build-table (extract-info '(defthm foo 
;		 (implies (and (consp x)
;			       (consp y))
;			  (equal (reverse (append x y))
;				 (append (reverse x) (reverse y))))) '(x y)))



(defun build-table (list)
  (let ((name (car list))
	(formulas (cdr list)))
    (do ((i 1 (+ 1 i))
	 (temp nil))
	((equal i 8) (append (list name) temp))
      (setf temp (append temp 
			 (list (do ((j -1 (+ 1 j))
				    (temp2 nil))
				   ((equal j 6) temp2)
				 (setf temp2 (append temp2 
						     (list (do ((temp3 (extract-arity (extract-level formulas i) j) (cdr temp3))
								(temp4 nil))
							       ((endp temp3) temp4)
							     (setf temp4 (append temp4 (list (nth 0 (car temp3)))))))
						     ))))))
      )))






(defun search-for-recursive-call-step1 (list name)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) (reverse temp2))
    (if (equal (car temp) name)
	(setf temp2 (cons 'recursive-call temp2))
      (setf temp2 (cons (car temp) temp2)))))


(defun search-for-recursive-call-step2 (list name)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) (reverse temp2))
    (setf temp2 (cons (search-for-recursive-call-step1 (car temp) name) temp2))))

(defun search-for-recursive-call-step3 (list name)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) (reverse temp2))
    (setf temp2 (cons (search-for-recursive-call-step2 (car temp) name) temp2))))
	
  
(defun search-for-recursive-calls (list)
  (cons (car list) (search-for-recursive-call-step3 (cdr list) (car list))))





