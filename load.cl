(load (compile-file-if-needed "imap"))
(load (compile-file-if-needed "../smtp/smtp"))

(defun test ()
  (setq *xx* (po::make-imap-connection "tiger.franz.com"
				   :user "jkfmail"
				   :password "jkf.imap"
				   ))
  (po::select-mailbox *xx* "inbox"))

				   
(defun testp ()
  (setq *xx* (po::make-pop-connection "tiger.franz.com"
				   :user "jkfmail"
				   :password "jkf.imap"
				   )))
