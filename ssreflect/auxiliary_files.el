(defun quicksort-pair (list)
  (if (<= (length list) 1)
      list
      (let ((pivot (cadar list)))
	(append	(quicksort-pair (remove-if-not #'(lambda (x) (> (cadr x) pivot)) list))
	  (remove-if-not #'(lambda (x) (= (cadr x) pivot)) list)
          (quicksort-pair (remove-if-not #'(lambda (x) (< (cadr x) pivot)) list))))))

  
(defun zip (l1 l2)
  (do ((temp1 l1 (cdr temp1))
       (temp2 l2 (cdr temp2))
       (res nil))
      ((endp temp1) res)
      (setf res (append res (list (append (list (car temp1)) (list (car temp2))))))))

(defun unzip (l)
  (do ((temp l (cdr temp))
       (res1 nil)
       (res2 nil))
      ((endp temp) (list (reverse res1) (reverse res2)))
    (progn (setf res1 (cons (caar temp) res1))
	   (setf res2 (cons (cadr (car temp)) res2)))))
