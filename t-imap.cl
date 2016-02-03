;; copyright (c) 2002-2016 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: t-imap.cl,v 1.9 2007/04/17 22:01:42 layer Exp $

;; imap testing
;; requires smtp module too

(eval-when (compile load eval)
  (require :rfc2822)
  (require :smtp)
  (require :imap)
  (require :test))


(in-package :test)


(defparameter *test-machine* "tiger.franz.com")
(defparameter *test-account* "jkfmail")
(defparameter *test-password* "jkf.imap")


(defparameter *test-email* (format nil "~a@~a" *test-account* *test-machine*))


(defun test-connect ()
  ;; test connecting and disconnecting from the server
  
  (let ((mb (net.post-office:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn
    
	  (test-t (not (null mb)))  ; make sure we got a mailbox object
    
	  ; check that we've stored resonable values in the mb object
	  (test-equal "/" (net.post-office:mailbox-separator mb)) 
    
	  (test-t (net.post-office::select-mailbox mb "inbox"))
    
	  (test-t (> (net.post-office:mailbox-uidvalidity mb) 0))
	  (test-t (not (null (net.post-office:mailbox-flags mb)))))
    
      (test-t (net.post-office:close-connection mb)))))


(defun test-sends ()
  ;; test sending and reading mail
  (let ((mb (net.post-office:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn
	  (test-t (not (null mb)))  ; make sure we got a mailbox object

	  ;; go through the mailboxes and delete all letters
	  (dolist (mblist (net.post-office:mailbox-list mb :pattern "*"))
	    (if* (not (member :\\noselect (net.post-office:mailbox-list-flags mblist)))
	       then (net.post-office:select-mailbox mb (net.post-office:mailbox-list-name mblist))
		    (let ((count (net.post-office:mailbox-message-count mb)))
		      ; remove all old mail
		      (if* (> count 0)
			 then (net.post-office:alter-flags mb `(:seq 1 ,count) :add-flags :\\deleted)
			      (net.post-office:expunge-mailbox mb)
			      (test-eql 0 (net.post-office:mailbox-message-count mb)))
		      ; remove mailbox (except inbox)
		      (if* (not (equalp "inbox" (net.post-office:mailbox-list-name mblist)))
			 then ; must not be selected if we want to del
			      (net.post-office:select-mailbox mb "inbox") 
			      (net.post-office:delete-mailbox mb (net.post-office:mailbox-list-name mblist)))
      
		      )))
      
    
	  ;; send five letters
	  (dotimes (i 5)
	    (net.post-office:send-smtp *test-machine*
			    *test-email*
			    *test-email*
			    (format nil "message number ~d" (1+ i))))
    
	  ; test to see if imap figures out that the letters are there
	  (net.post-office:select-mailbox mb "inbox")

	  ; wait a bit for the mail to be delivered
	  (dotimes (i 5) 
	    (if* (not (eql 5 (net.post-office:mailbox-message-count mb)))
	       then (sleep 1)
		    (net.post-office: noop mb)))
	      
	  (test-eql 5 (net.post-office:mailbox-message-count mb))
    
	  ; test the search facility
	  ; look for the message number we put in each message.
	  ; I hope the letters get delivered in order...
	  (dotimes (i 5)
	    (let ((mn (1+ i)))
	      (test-equal (list mn)
			  (net.post-office:search-mailbox mb 
					     `(:body ,(format nil "~d" mn))))))
	  
	  ; test getting data from mail message
	  (let ((fetch-info (net.post-office:fetch-parts mb 
					   1
					   "(envelope body[1])")))
	    (let ((envelope (net.post-office:fetch-field 1 "envelope" fetch-info))
		  (body (net.post-office:fetch-field 1 "body[1]" fetch-info)))
	      (test-equal "jkfmail" (net.post-office:address-mailbox
				     (car (net.post-office:envelope-from envelope))))
	      (test-nil (net.post-office:address-mailbox
			 (car (net.post-office:envelope-to envelope))))
	      
	      (test-equal (format nil "message number 1~c" #\newline)
			  body))))
      (test-t (net.post-office:close-connection mb)))))
    
    

(defun test-flags ()
  ;; test setting and getting flags
  ;;
  ;; assume we have 5 messages in inbox at this time
  ;;
  (let ((mb (net.post-office:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn
	  (net.post-office:select-mailbox mb "inbox")
	  
	  (let ((flags (net.post-office:fetch-field 3 
				       "flags"
				       (net.post-office:fetch-parts 
					mb 3 "flags"))))
	    (test-nil flags))
				       
	  ;; add flags
	  (let ((info (net.post-office:alter-flags mb 3 :add-flags :\\deleted)))
	    (test-equal '(:\\deleted)
			(net.post-office:fetch-field 3 "flags" info)))

	  ; good bye message
	  (test-equal '(3) (net.post-office:expunge-mailbox mb))
	  
	  (net.post-office:alter-flags mb 4 :add-flags ':\\bbbb)
	  (test-equal '(:\\bbbb)
		      (net.post-office:fetch-field 4 "flags"
				      (net.post-office:fetch-parts mb 4
						       "flags")))
	  
	  
	  )
      (test-t (net.post-office:close-connection mb)))))

(defun test-mailboxes ()
  ;; should be 4 messages now in inbox
  ;; let's create 4 mailboxes, one for each letter
  (let ((mb (net.post-office:make-imap-connection *test-machine*
				     :user *test-account*
				     :password *test-password*)))
    (unwind-protect
	(progn 
	  (net.post-office:select-mailbox mb "inbox")
	  (dotimes (i 4)
	    (let ((mbname (format nil "temp/mb~d" i)))
	      (test-t (net.post-office:create-mailbox mb mbname))
	      (net.post-office:copy-to-mailbox mb (1+ i) mbname)))
	  
	  ; now check that each new mailbox has one message
	  (dotimes (i 4)
	    (let ((mbname (format nil "temp/mb~d" i)))
	      (net.post-office:select-mailbox mb mbname)
	      (test-eql 1 (net.post-office:mailbox-message-count mb)))))
      (test-t (net.post-office:close-connection mb)))))


(defun test-pop ()
  ;; test out the pop interface to the mailbox.
  
  (let ((pb (net.post-office:make-pop-connection *test-machine*
				    :user *test-account*
				    :password *test-password*)))
    ; still from before
    (test-eql 4 (net.post-office:mailbox-message-count pb))
    
    (test-eql 4 (length (net.post-office:unique-id pb)))
			 
    (net.post-office:delete-letter pb '(:seq 2 3))
    
    (test-eql 2 (length (net.post-office:unique-id pb)))
    
    (test-eql 4 (and :second (net.post-office:mailbox-message-count pb)))
    
    (net.post-office:noop pb)
    
    (test-eql 2 (and :third (net.post-office:mailbox-message-count pb)))
    
    (net.post-office:fetch-letter pb 1)
    (test-err (net.post-office:fetch-letter pb 2))
    (test-err (net.post-office:fetch-letter pb 3))
    (net.post-office:fetch-letter pb 4)
    
    (net.post-office:close-connection pb)
    
    (setq pb (net.post-office:make-pop-connection *test-machine*
				    :user *test-account*
				    :password *test-password*))
    
    (test-eql 2 (and :fourth (net.post-office:mailbox-message-count pb)))
    
    (net.post-office:fetch-letter pb 1) ; just make sure there's no error
    
    (net.post-office:top-lines pb 1 1)  ; just make sure there's no error
    (net.post-office:make-envelope-from-text (net.post-office:top-lines pb 1 0))
    
    (net.post-office:close-connection pb)))


(defun test-mime ()
  (test-equal
   "foobar baz"
   (net.post-office:decode-header-text "=?utf-8?q?foo?=
  =?utf-8?q?bar?= baz"))
  (test-equal
   "before brucejones hello"
   (net.post-office:decode-header-text "before =?utf-8?q?bruce?=    =?utf-8?q?jones?= hello"))
  (test-equal
   "[Franz Wiki] Update of \"Office/EmployeeDirectory\" by SteveHaflich"
   (net.post-office:decode-header-text "=?utf-8?q?=5BFranz_Wiki=5D_Update_of_=22Office/EmployeeDirectory=22_by_St?=
 =?utf-8?q?eveHaflich?="))
  )

(defun test-parse-email-address ()
  (dolist (good `(("foo@bar.com" "foo" "bar.com")
		  ("layer@franz.com" "layer" "franz.com")
		  ("

layer@franz.com" "layer" "franz.com")
		  (,(replace-re "XXlayer@franz.comX  X"
				"X"
				(format nil "~c" #\newline)
				:single-line t)
		   "layer" "franz.com")
		  (,(replace-re "XXlayer@franz.comX  X"
				"X"
				(format nil "~c" #\return)
				:single-line t)
		   "layer" "franz.com")
		  ;; local-part length = 64
		  ("1234567890123456789012345678901234567890123456789012345678901234@foo.com"
		   "1234567890123456789012345678901234567890123456789012345678901234"
		   "foo.com")
		  ))
    (multiple-value-bind (local-part domain)
	(net.mail:parse-email-address (first good))
      (test-equal (second good) local-part)
      (test-equal (third good) domain)))
  (dolist (bad (list "@foo.com"
		     ;; local-part length = 65
		     "12345678901234567890123456789012345678901234567890123456789012345@foo.com"
		     ))
    (test-nil (net.mail:parse-email-address bad)))
  )
	  
    
(defun test-imap ()
  (handler-bind ((net.post-office:po-condition 
		  #'(lambda (con)
		      (format t "Got imap condition: ~a~%" con))))
    (test-mime)
    (test-parse-email-address)
;;;; Only jkf is setup to run the tests.
    (when (string= "jkf" (sys:getenv "USER"))
      (test-connect)
      (test-sends)
      (test-flags)
      (test-mailboxes)
      (test-pop)
      )))


(if* *do-test* then (do-test :imap #'test-imap))
