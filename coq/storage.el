(defvar home-dir "/home/jonathan/Dropbox/emacs-matlab/")

(defun save-lemma-aux (string)
  (append-to-file string nil (concat home-dir "lemmas.txt"))  
)

(defun save-lemma (name value)
  (save-lemma-aux (format "%s&%s$" name value)))


(defun save-view-aux (string)
  (append-to-file string nil (concat home-dir "views.txt"))  
)

(defun save-view (name value)
  (save-view-aux (format "%s&%s$" name value)))


(defun read-lemmas ()
  (if (file-exists-p (concat home-dir "coq/lemmas.txt"))
      (with-temp-buffer
	(insert-file-contents (concat home-dir "coq/lemmas.txt"))
	(let ((temp (format "%s" (read (current-buffer)))))
	  (setf theorems_id (extract-info-from-files temp))
	  ))))

(defun read-views ()
  (if (file-exists-p (concat home-dir "coq/views.txt"))
      (with-temp-buffer
	(insert-file-contents (concat home-dir "coq/views.txt"))
	(let ((temp (format "%s" (read (current-buffer)))))
	  (setf views_id (extract-info-from-files temp))
	  ))))

(defun extract-info-from-files (string)
  (do ((temp string)
       (temp2 nil))
      ((not (search "$" temp)) temp2)
      (let ((dollar (search "$" temp))
	    (amper (search "&" temp)))
	(progn 
	  (setf temp2 (append temp2 (list (cons (subseq temp 0 amper)
					 (string-to-number (subseq temp (1+ amper) dollar))))))
	       (setf temp (subseq temp (1+ dollar)))))))









