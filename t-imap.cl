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
  
  (let ((mb (mb:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn
    
	  (test-t (not (null mb)))  ; make sure we got a mailbox object
    
	  ; check that we've stored resonable values in the mb object
	  (test-equal "/" (mb:mailbox-separator mb)) 
    
	  (test-t (mb::select-mailbox mb "inbox"))
    
	  (test-t (> (mb:mailbox-uidvalidity mb) 0))
	  (test-t (not (null (mb:mailbox-flags mb)))))
    
      (test-t (mb:close-imap-connection mb)))))


(defun test-sends ()
  ;; test sending and reading mail
  (let ((mb (mb:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn
	  (test-t (not (null mb)))  ; make sure we got a mailbox object

	  ;; go through the mailboxes and delete all letters
	  (dolist (mblist (mb:mailbox-list mb :pattern "*"))
	    (if* (not (member :\\noselect (mb:mailbox-list-flags mblist)))
	       then (mb:select-mailbox mb (mb:mailbox-list-name mblist))
		    (let ((count (mb:mailbox-message-count mb)))
		      ; remove all old mail
		      (if* (> count 0)
			 then (mb:alter-flags mb `(:seq 1 ,count) :add-flags :\\deleted)
			      (mb:expunge-mailbox mb)
			      (test-eql 0 (mb:mailbox-message-count mb)))
		      ; remove mailbox (except inbox)
		      (if* (not (equalp "inbox" (mb:mailbox-list-name mblist)))
			 then (mb:delete-mailbox mb (mb:mailbox-list-name mblist)))
      
		      )))
      
    
	  ;; send five letters
	  (dotimes (i 5)
	    (smtp:send-smtp *test-machine*
			    *test-email*
			    *test-email*
			    (format nil "message number ~d" (1+ i))))
    
	  ; test to see if imap figures out that the letters are there
	  (mb:select-mailbox mb "inbox")

	  ; wait a bit for the mail to be delivered
	  (dotimes (i 5) 
	    (if* (not (eql 5 (mb:mailbox-message-count mb)))
	       then (sleep 1)
		    (mb: noop mb)))
	      
	  (test-eql 5 (mb:mailbox-message-count mb))
    
	  ; test the search facility
	  ; look for the message number we put in each message.
	  ; I hope the letters get delivered in order...
	  (dotimes (i 5)
	    (let ((mn (1+ i)))
	      (test-equal (list mn)
			  (mb:search-mailbox mb 
					     `(:body ,(format nil "~d" mn))))))
	  
	  ; test getting data from mail message
	  (let ((fetch-info (mb:fetch-letter mb 
					   1
					   "(envelope body[1])")))
	    (let ((envelope (mb:fetch-field 1 "envelope" fetch-info))
		  (body (mb:fetch-field 1 "body[1]" fetch-info)))
	      (test-equal "jkfmail" (mb:address-mailbox
				     (car (mb:envelope-from envelope))))
	      (test-nil (mb:address-mailbox
			 (car (mb:envelope-to envelope))))
	      
	      (test-equal (format nil "message number 1~c" #\newline)
			  body))))
      (test-t (mb:close-imap-connection mb)))))
    
    

(defun test-flags ()
  ;; test setting and getting flags
  ;;
  ;; assume we have 5 messages in inbox at this time
  ;;
  (let ((mb (mb:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn
	  (mb:select-mailbox mb "inbox")
	  
	  (let ((flags (mb:fetch-field 3 
				       "flags"
				       (mb:fetch-letter 
					mb 3 "flags"))))
	    (test-nil flags))
				       
	  ;; add flags
	  (let ((info (mb:alter-flags mb 3 :add-flags :\\deleted)))
	    (test-equal '(:\\deleted)
			(mb:fetch-field 3 "flags" info)))

	  ; good bye message
	  (test-equal '(3) (mb:expunge-mailbox mb))
	  
	  (mb:alter-flags mb 4 :add-flags ':\\bbbb)
	  (test-equal '(:\\bbbb)
		      (mb:fetch-field 4 "flags"
				      (mb:fetch-letter mb 4
						       "flags")))
	  
	  
	  )
      (test-t (mb:close-imap-connection mb)))))

(defun test-mailboxes ()
  ;; should be 4 messages now in inbox
  ;; let's create 4 mailboxes, one for each letter
  (let ((mb (mb:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn 
	  (mb:select-mailbox mb "inbox")
	  (dotimes (i 4)
	    (let ((mbname (format nil "temp/mb~d" i)))
	      (test-t (mb:create-mailbox mb mbname))
	      (mb:copy-to-mailbox mb (1+ i) mbname)))
	  
	  ; now check that each new mailbox has one message
	  (dotimes (i 4)
	    (let ((mbname (format nil "temp/mb~d" i)))
	      (mb:select-mailbox mb mbname)
	      (test-eql 1 (mb:mailbox-message-count mb)))))
      (test-t (mb:close-imap-connection mb)))))
  
(defun test-imap ()
  (test-connect)
  
  (test-sends)

  (test-flags)
 
  (test-mailboxes)
  
  )


(if* *do-test* then (do-test :imap #'test-imap))
    
    
    
    
  
