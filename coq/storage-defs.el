(require 'cl)




(defun export-library-defs ()
  (interactive)
  (beginning-of-buffer)
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
  t)


(defun export-up-to-here ()
  (interactive)
  (let* ((buf (buffer-name))
	 (name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
    (with-temp-file (concat home-dir "/definitions/" name) 
      (insert (format "%s" listofdefinitions)))
    (with-temp-file (concat home-dir "/variables/" name) 
      (insert (format "%s" listofvariables)))
    )
  t)


(defun export-library-thms ()
  (interactive)
  (beginning-of-buffer)
  (proof-goto-point)
  (end-of-buffer)
  (extract-feature-theorems)
  (let* ((buf (buffer-name))
	 (name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
    (with-temp-file (concat home-dir "/theorems/" name) 
      (insert (format "%s" listofstatements)))
    (with-temp-file (concat home-dir "/variablesthms/" name) 
      (insert (format "%s" listofthmvariables)))
    )
  t)


(defun export-up-to-here-thm ()
  (interactive)
  (let* ((buf (buffer-name))
	 (name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
    (with-temp-file (concat home-dir "/theorems/" name) 
      (insert (format "%s" listofstatements)))
    (with-temp-file (concat home-dir "/variablesthms/" name) 
      (insert (format "%s" listofthmvariables)))
    )
  t)

;(defvar imported-libraries nil)


(defvar libs-defs nil)

(defun available-defs-libraries ()
  (setf libs-defs nil)
  (shell-command  (concat "ls " home-dir "definitions |  wc -l"))
  (let ((n nil)
	(i 0))
  (with-current-buffer "*Shell Command Output*"
    (beginning-of-buffer)
    (setq n (string-to-number (format "%s"  (read (current-buffer))))))
  (shell-command  (concat "ls " home-dir "definitions"))
  (with-current-buffer "*Shell Command Output*"
    (progn (beginning-of-buffer)
	   (while (< i n)
	     (let ((r (format "%s" (read (current-buffer)))))
	       (progn (setq i (1+ i))
		      (setq libs-defs (append libs-defs (list r))))))))))


(defvar libs-statements nil)

(defun available-thm-libraries ()
  (setf libs-statements nil)
  (shell-command  (concat "ls " home-dir "theorems |  wc -l"))
  (let ((n nil)
	(i 0))
  (with-current-buffer "*Shell Command Output*"
    (beginning-of-buffer)
    (setq n (string-to-number (format "%s"  (read (current-buffer))))))
  (shell-command  (concat "ls " home-dir "theorems"))
  (with-current-buffer "*Shell Command Output*"
    (progn (beginning-of-buffer)
	   (while (< i n)
	     (let ((r (format "%s" (read (current-buffer)))))
	       (progn (setq i (1+ i))
		      (setq libs-statements (append libs-statements (list r))))))))))





(defun import-definitions (name)
  (with-temp-buffer
  (insert-file-contents (concat home-dir "/definitions/" name))
  (car (read-from-string (format "%s" (read (current-buffer)))))
  ))

(defun import-variables (name)
  (with-temp-buffer
  (insert-file-contents (concat home-dir "/variables/" name))
  (car (read-from-string (format "%s" (read (current-buffer)))))
  ))


(defun import-statements (name)
  (with-temp-buffer
  (insert-file-contents (concat home-dir "/theorems/" name))
  (car (read-from-string (format "%s" (read (current-buffer)))))
  ))

(defun import-variablesthm (name)
  (with-temp-buffer
  (insert-file-contents (concat home-dir "/variablesthms/" name))
  (car (read-from-string (format "%s" (read (current-buffer)))))
  ))



(defvar definitions-libraries nil)
(defvar variables-libraries nil)
(defvar statements-libraries nil)
(defvar variablesthm-libraries nil)

(defvar number-of-defs nil)
(defvar number-of-thms nil)



(defun add-several-libraries-defs ()
  (interactive)
  (setf definitions-libraries (reverse listofdefinitions))
  (setf number-of-defs (append number-of-defs (list (list "current" (length listofdefinitions)))))
  (available-defs-libraries)
  (setf variables-libraries (reverse listofvariables))
  (do ((temp libs-defs (cdr temp)))
      ((endp temp) nil)
    (progn (setf number-of-defs (append number-of-defs (list (list (car temp) (length (import-definitions (car temp)))))))
	   (setf definitions-libraries (append definitions-libraries
					(import-definitions (car temp))))
	   (setf variables-libraries (append variables-libraries
					(import-variables (car temp)))))))



(defun add-several-libraries-thms ()
  (interactive)
  (setf statements-libraries listofstatements)
  (setf number-of-thms (append number-of-thms (list (list "current" (length listofstatements)))))
  (available-thm-libraries)
  (setf variablesthms-libraries listofthmvariables)
  (do ((temp libs-statements (cdr temp)))
      ((endp temp) nil)
    (progn (setf number-of-thms (append number-of-thms (list (list (car temp) (length (import-statements (car temp)))))))
	   (setf statements-libraries (append statements-libraries
					(import-statements (car temp))))
	   (setf variablesthm-libraries (append variablesthm-libraries
					(import-variablesthm (car temp)))))))



(defun library-belong (n)
  (do ((temp number-of-defs (cdr temp))
       (temp2 nil)
       (lib "")
       (acc 0))
      (temp2 lib)
    (if (< n (+ acc (cadr (car temp))))
	(progn (setf temp2 t)
	       (setf lib (car (car temp))))
      (setf acc (+ acc (cadr (car temp)))))))

(defun library-belong-thm (n)
  (do ((temp number-of-thms (cdr temp))
       (temp2 nil)
       (lib "")
       (acc 0))
      (temp2 lib)
    (if (< n (+ acc (cadr (car temp))))
	(progn (setf temp2 t)
	       (setf lib (car (car temp))))
      (setf acc (+ acc (cadr (car temp)))))))



