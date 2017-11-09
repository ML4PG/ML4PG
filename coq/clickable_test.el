(switch-to-buffer-other-window "*display*")
(with-current-buffer "*display*" (erase-buffer))

(with-current-buffer "*display2*" (erase-buffer))

(with-current-buffer "*display*"
    (insert-button "test" 'action (lambda (x) 
				    (progn (switch-to-buffer-other-window "*display2*")
					   (insert "hola")))
		   'face (list 'link)
		   'follow-link t))