;; imap testing
;; requires smtp module too

(eval-when (compile load eval)
  (require :test))


(in-package :test)


(defparameter *test-machine* "tiger.franz.com")
(defparameter *test-account* "jkfmail")
(defparameter *test-password* "jkf.imap")


(defparameter *test-email* (format nil "~a@~a" *test-account* *test-machine*))


(defun test-connect ()
  ;; test connecting and disconnecting from the server
  
  (let ((mb (po:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn
    
	  (test-t (not (null mb)))  ; make sure we got a mailbox object
    
	  ; check that we've stored resonable values in the mb object
	  (test-equal "/" (po:mailbox-separator mb)) 
    
	  (test-t (po::select-mailbox mb "inbox"))
    
	  (test-t (> (po:mailbox-uidvalidity mb) 0))
	  (test-t (not (null (po:mailbox-flags mb)))))
    
      (test-t (po:close-connection mb)))))


(defun test-sends ()
  ;; test sending and reading mail
  (let ((mb (po:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn
	  (test-t (not (null mb)))  ; make sure we got a mailbox object

	  ;; go through the mailboxes and delete all letters
	  (dolist (mblist (po:mailbox-list mb :pattern "*"))
	    (if* (not (member :\\noselect (po:mailbox-list-flags mblist)))
	       then (po:select-mailbox mb (po:mailbox-list-name mblist))
		    (let ((count (po:mailbox-message-count mb)))
		      ; remove all old mail
		      (if* (> count 0)
			 then (po:alter-flags mb `(:seq 1 ,count) :add-flags :\\deleted)
			      (po:expunge-mailbox mb)
			      (test-eql 0 (po:mailbox-message-count mb)))
		      ; remove mailbox (except inbox)
		      (if* (not (equalp "inbox" (po:mailbox-list-name mblist)))
			 then (po:delete-mailbox mb (po:mailbox-list-name mblist)))
      
		      )))
      
    
	  ;; send five letters
	  (dotimes (i 5)
	    (po:send-smtp *test-machine*
			    *test-email*
			    *test-email*
			    (format nil "message number ~d" (1+ i))))
    
	  ; test to see if imap figures out that the letters are there
	  (po:select-mailbox mb "inbox")

	  ; wait a bit for the mail to be delivered
	  (dotimes (i 5) 
	    (if* (not (eql 5 (po:mailbox-message-count mb)))
	       then (sleep 1)
		    (po: noop mb)))
	      
	  (test-eql 5 (po:mailbox-message-count mb))
    
	  ; test the search facility
	  ; look for the message number we put in each message.
	  ; I hope the letters get delivered in order...
	  (dotimes (i 5)
	    (let ((mn (1+ i)))
	      (test-equal (list mn)
			  (po:search-mailbox mb 
					     `(:body ,(format nil "~d" mn))))))
	  
	  ; test getting data from mail message
	  (let ((fetch-info (po:fetch-parts mb 
					   1
					   "(envelope body[1])")))
	    (let ((envelope (po:fetch-field 1 "envelope" fetch-info))
		  (body (po:fetch-field 1 "body[1]" fetch-info)))
	      (test-equal "jkfmail" (po:address-mailbox
				     (car (po:envelope-from envelope))))
	      (test-nil (po:address-mailbox
			 (car (po:envelope-to envelope))))
	      
	      (test-equal (format nil "message number 1~c" #\newline)
			  body))))
      (test-t (po:close-connection mb)))))
    
    

(defun test-flags ()
  ;; test setting and getting flags
  ;;
  ;; assume we have 5 messages in inbox at this time
  ;;
  (let ((mb (po:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn
	  (po:select-mailbox mb "inbox")
	  
	  (let ((flags (po:fetch-field 3 
				       "flags"
				       (po:fetch-parts 
					mb 3 "flags"))))
	    (test-nil flags))
				       
	  ;; add flags
	  (let ((info (po:alter-flags mb 3 :add-flags :\\deleted)))
	    (test-equal '(:\\deleted)
			(po:fetch-field 3 "flags" info)))

	  ; good bye message
	  (test-equal '(3) (po:expunge-mailbox mb))
	  
	  (po:alter-flags mb 4 :add-flags ':\\bbbb)
	  (test-equal '(:\\bbbb)
		      (po:fetch-field 4 "flags"
				      (po:fetch-parts mb 4
						       "flags")))
	  
	  
	  )
      (test-t (po:close-connection mb)))))

(defun test-mailboxes ()
  ;; should be 4 messages now in inbox
  ;; let's create 4 mailboxes, one for each letter
  (let ((mb (po:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn 
	  (po:select-mailbox mb "inbox")
	  (dotimes (i 4)
	    (let ((mbname (format nil "temp/mb~d" i)))
	      (test-t (po:create-mailbox mb mbname))
	      (po:copy-to-mailbox mb (1+ i) mbname)))
	  
	  ; now check that each new mailbox has one message
	  (dotimes (i 4)
	    (let ((mbname (format nil "temp/mb~d" i)))
	      (po:select-mailbox mb mbname)
	      (test-eql 1 (po:mailbox-message-count mb)))))
      (test-t (po:close-connection mb)))))


(defun test-pop ()
  ;; test out the pop interface to the mailbox.
  
  (let ((pb (po:make-pop-connection *test-machine*
				    :user *test-account*
				    :password *test-password*)))
    ; still from before
    (test-eql 4 (po:mailbox-message-count pb))
    
    (po:delete-letter pb '(:seq 2 3))
    
    
    (test-eql 4 (and :second (po:mailbox-message-count pb)))
    
    (po:noop pb)
    
    (test-eql 2 (and :third (po:mailbox-message-count pb)))
    
    (po:fetch-letter pb 1)
    (test-err (po:fetch-letter pb 2))
    (test-err (po:fetch-letter pb 3))
    (po:fetch-letter pb 4)
    
    (po:close-connection pb)
    
    (setq pb (po:make-pop-connection *test-machine*
				    :user *test-account*
				    :password *test-password*))
    
    (test-eql 2 (and :fourth (po:mailbox-message-count pb)))
    
    (po:fetch-letter pb 1) ; just make sure there's no error
    
    (po:close-connection pb)))

	  
    
(defun test-imap ()
  (handler-bind ((po:po-condition 
		  #'(lambda (con)
		      (format t "Got imap condition: ~a~%" con))))
				       
    (test-connect)
  
    (test-sends)

    (test-flags)
 
    (test-mailboxes)

    (test-pop)
  
  
    ))


(if* *do-test* then (do-test :imap #'test-imap))
    
    
    
    
  
