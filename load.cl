(load (compile-file-if-needed "imap"))
(load (compile-file-if-needed "../smtp/smtp"))

(defun test ()
  (setq *xx* (mb::make-imap-connection "tiger.franz.com"
				   :user "jkfmail"
				   :password "jkf.imap"
				   ))
  (mb::select-mailbox *xx* "inbox"))

				   
