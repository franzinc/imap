(load (compile-file-if-needed "imap"))
(load (compile-file-if-needed "smtp"))

(defun test ()
  (setq *xx* (net.post-office::make-imap-connection "tiger.franz.com"
				   :user "jkfmail"
				   :password "jkf.imap"
				   ))
  (net.post-office::select-mailbox *xx* "inbox"))

				   
(defun testp ()
  (setq *xx* (net.post-office::make-pop-connection "tiger.franz.com"
						   :user "jkfmail"
						   :password "jkf.imap"
						   )))
