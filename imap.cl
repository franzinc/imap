;; -*- mode: common-lisp; package: net.post-office -*-
;;
;; imap.cl
;; imap and pop interface
;;
;; copyright (c) 1999 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: imap.cl,v 1.17 2001/05/04 16:01:45 jkf Exp $

;; Description:
;;
;;
;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(defpackage :net.post-office
  (:use :lisp :excl)
  (:export 
   #:address-name
   #:address-additional
   #:address-mailbox
   #:address-host
   
   #:alter-flags
   #:close-connection
   #:close-mailbox
   #:copy-to-mailbox
   #:create-mailbox
   #:delete-letter
   #:delete-mailbox
   
   #:envelope-date
   #:envelope-subject
   #:envelope-from
   #:envelope-sender
   #:envelope-reply-to
   #:envelope-to
   #:envelope-cc
   #:envelope-bcc
   #:envelope-in-reply-to
   #:envelope-message-id
   
   #:expunge-mailbox
   #:fetch-field
   #:fetch-letter
   #:fetch-parts
   #:*imap-version-number*
   #:make-envelope-from-text
   #:mailbox-flags      ; accessor
   #:mailbox-permanent-flags ; acc
   #:mailbox-list
   #:mailbox-list-flags
   #:mailbox-list-separator
   #:mailbox-list-name
   #:mailbox-message-count ; accessor
   #:mailbox-recent-messages ; ac
   #:mailbox-separator  ; accessor
   #:mailbox-uidvalidity
   #:make-imap-connection
   #:make-pop-connection
   #:noop
   #:parse-mail-header
   #:top-lines	; pop only
   #:unique-id  ; pop only
   
   #:po-condition
   #:po-condition-indentifier
   #:po-condition-server-string
   #:po-error
   
   #:rename-mailbox
   #:search-mailbox
   #:select-mailbox
   
   )
  )

(in-package :net.post-office)

(provide :imap)

(defparameter *imap-version-number* '(:major 1 :minor 8)) ; major.minor

;; todo
;;  have the list of tags selected done on a per connection basis to
;;  eliminate any possible multithreading problems
;;
;;

(defvar *debug-imap* nil)





(defclass post-office ()
  ((socket :initarg :socket
	   :accessor post-office-socket)
   
   (host :initarg :host
	 :accessor  post-office-host
	 :initform nil)
   (user  :initarg :user
	  :accessor post-office-user
	  :initform nil)
   
   (state :accessor post-office-state
	  :initarg :state
	  :initform :unconnected)
   
   (timeout 
    ;; time to wait for network activity for actions that should
    ;; happen very quickly when things are operating normally
    :initarg :timeout
    :initform 60
    :accessor timeout) 
  ))

(defclass imap-mailbox (post-office)
  ((mailbox-name   ; currently selected mailbox
    :accessor mailbox-name
    :initform nil)

   (separator 
    ;; string that separates mailbox names in the hierarchy
    :accessor mailbox-separator
    :initform "")
   
   ;;; these slots hold information about the currently selected mailbox:
   
    (message-count  ; how many in the mailbox
    :accessor mailbox-message-count
    :initform 0)
   
   (recent-messages ; how many messages since we last checked
    :accessor mailbox-recent-messages
    :initform 0)
   
   (uidvalidity  ; used to denote messages uniquely
    :accessor mailbox-uidvalidity 
    :initform 0)
   
   (uidnext 
    :accessor mailbox-uidnext ;; predicted next uid
    :initform 0)
   
   (flags	; list of flags that can be stored in a message
    :accessor mailbox-flags 
    :initform nil)
   
   (permanent-flags  ; list of flags that be stored permanently
    :accessor mailbox-permanent-flags
    :initform nil)
   
   (first-unseen   ; number of the first unseen message
    :accessor first-unseen
    :initform 0)
   
   ;;; end list of values for the currently selected mailbox
   )
  )


(defclass pop-mailbox (post-office)
  ((message-count  ; how many in the mailbox
    :accessor mailbox-message-count
    :initform 0)))



(defstruct (mailbox-list (:type list))
  ;; a list of these are returned by mailbox-list
  flags
  separator
  name)



(defstruct (envelope (:type list))
  ;; returned by fetch-letter as the value of the envelope property
  date
  subject
  from
  sender
  reply-to
  to
  cc
  bcc
  in-reply-to
  message-id)


(defstruct (address (:type list))
  name     ;; often the person's full name
  additional
  mailbox  ;; the login name
  host	   ;; the name of the machine 
  )



;--------------------------------
; conditions
;
; We define a set of conditions that are signalled due to events
; in the imap interface.
; Each condition has an indentifier which is a keyword.  That can
; be used in the handling code to identify the class of error.
; All our conditions are po-condition or po-error (which is a subclass of
; po-condition).
;
; A condition will have a server-string value if it as initiated by 
; something returned by the server.
; A condition will have a format-control value if we want to display 
; something we generated in response to 
; 
;
;
;; identifiers used in conditions/errors

; :problem  condition
;	the server responded with 'no' followed by an explanation.
;	this mean that something unusual happend and doesn't necessarily
;	mean that the command has completely failed (but it might).
;	
; :unknown-ok   condition
;	the server responded with an 'ok' followed by something
;	we don't recognize.  It's probably safe to ignore this.
;
;  :unknown-untagged condition
;	the server responded with some untagged command we don't
;	recognize.  it's probaby ok to ignore this.
;
;  :error-response  error
;	the command failed.
;
;  :syntax-error   error
;	the data passed to a function in this interface was malformed
;
;  :unexpected    error
;	the server responded an unexpected way.
;
;  :server-shutdown-connection error
;	the server has shut down the connection, don't attempt to
;       send any more commands to this connection, or even close it.
;
;  :timeout  error
;	server failed to respond within the timeout period
;
;  :response-too-large error
;	contents of a response is too large to store in a Lisp array.


;; conditions
(define-condition po-condition ()
  ;; used to notify user of things that shouldn't necessarily stop
  ;; program flow
  ((identifier 
    ;; keyword identifying the error (or :unknown)
    :reader po-condition-identifier	
    :initform :unknown
    :initarg :identifier
    )
   (server-string 
    ;; message from the imap server
    :reader po-condition-server-string
    :initform ""
    :initarg :server-string
    ))
  (:report
   (lambda (con stream)
     (with-slots (identifier server-string) con
       ;; a condition either has a server-string or it has a 
       ;; format-control string
       (format stream "Post Office condition: ~s~%" identifier)
       (if* (and (slot-boundp con 'excl::format-control)
		 (excl::simple-condition-format-control con))
	  then (apply #'format stream
		      (excl::simple-condition-format-control con)
		      (excl::simple-condition-format-arguments con)))
       (if* server-string
	  then (format stream
		       "~&Message from server: ~s"
		       (string-left-trim " " server-string)))))))
	       
    

(define-condition po-error (po-condition error) 
  ;; used to denote things that should stop program flow
  ())



;; aignalling the conditions

(defun po-condition (identifier &key server-string format-control 
			  format-arguments)
  (signal (make-instance 'po-condition
	    :identifier identifier
	    :server-string server-string
	    :format-control format-control
	    :format-arguments format-arguments
	    )))
	    
(defun po-error (identifier &key server-string
		      format-control format-arguments)
  (error (make-instance 'po-error
	    :identifier identifier
	    :server-string server-string
	    :format-control format-control
	    :format-arguments format-arguments)))

			   

;----------------------------------------------






(defparameter *imap-tags* '("t01" "t02" "t03" "t04" "t05" "t06" "t07"))
(defvar *cur-imap-tags* nil)

(defvar *crlf*
    (let ((str (make-string 2)))
      (setf (aref str 0) #\return)
      (setf (aref str 1) #\linefeed)
      str))

(defun make-imap-connection (host &key (port 143) 
				       user 
				       password
				       (timeout 30))
  (let* ((sock (socket:make-socket :remote-host host
				   :remote-port port))
	 (imap (make-instance 'imap-mailbox
		 :socket sock
		 :host   host
		 :timeout timeout
		 :state :unauthorized)))
    
    (multiple-value-bind (tag cmd count extra comment)
	(get-and-parse-from-imap-server imap)
      (declare (ignore cmd count extra))
      (if* (not (eq :untagged tag))
	 then  (po-error :error-response
			 :server-string comment)))
      
    ; now login
    (send-command-get-results imap 
			      (format nil "login ~a ~a" user password)
			      #'handle-untagged-response
			      #'(lambda (mb command count extra comment)
				  (check-for-success mb command count extra
						     comment
						     "login")))
    
    ; find the separator character
    (let ((res (mailbox-list imap)))
      ;; 
      (let ((sep (cadr  (car res))))
	(if* sep
	   then (setf (mailbox-separator imap) sep))))
    
				    
				    
    imap))


(defmethod close-connection ((mb imap-mailbox))
  
  (let ((sock (post-office-socket mb)))
    (if* sock
       then (ignore-errors
	     (send-command-get-results 
	      mb
	      "logout"
	      ; don't want to get confused by untagged
	      ; bye command, which is expected here
	      #'(lambda (mb command count extra)
		  (declare (ignore mb command count extra))
		  nil)
	      #'(lambda (mb command count extra comment)
		  (check-for-success mb command count extra
				     comment
				     "logout")))))
    (setf (post-office-socket mb) nil)
    (if* sock then (ignore-errors (close sock)))
    t))


(defmethod close-connection ((pb pop-mailbox))
  (let ((sock (post-office-socket pb)))
    (if* sock
       then (ignore-errors
	     (send-pop-command-get-results 
	      pb
	      "QUIT")))
    (setf (post-office-socket pb) nil)
    (if* sock then (ignore-errors (close sock)))
    t))



(defun make-pop-connection (host &key (port 110)
				      user
				      password
				      (timeout 30))
  (let* ((sock (socket:make-socket :remote-host host
				   :remote-port port))
	 (pop (make-instance 'pop-mailbox
		:socket sock
		:host   host
		:timeout timeout
		:state :unauthorized)))
    
    (multiple-value-bind (result)
	(get-and-parse-from-pop-server pop)
      (if* (not (eq :ok result))
	 then  (po-error :error-response
			 :format-control
			 "unexpected line from server after connect")))
      
    ; now login
    (send-pop-command-get-results pop (format nil "user ~a" user))
    (send-pop-command-get-results pop (format nil "pass ~a" password))

    (let ((res (send-pop-command-get-results pop "stat")))
      (setf (mailbox-message-count pop) (car res)))
    
    			    
				    
    pop))
			    

(defmethod send-command-get-results ((mb imap-mailbox) 
				     command untagged-handler tagged-handler)
  ;; send a command and retrieve results until we get the tagged
  ;; response for the command we sent
  ;;
  (let ((tag (get-next-tag)))
    (format (post-office-socket mb)
	    "~a ~a~a" tag command *crlf*)
    (force-output (post-office-socket mb))
    
    (if* *debug-imap*
       then (format t
		    "~a ~a~a" tag command *crlf*)
	    (force-output))
    (loop
      (multiple-value-bind (got-tag cmd count extra comment)
	  (get-and-parse-from-imap-server mb)
	(if* (eq got-tag :untagged)
	   then (funcall untagged-handler mb cmd count extra comment)
	 elseif (equal tag got-tag)
	   then (funcall tagged-handler mb cmd count extra comment)
		(return)
	   else (po-error :error-response
			  :format-control "received tag ~s out of order" 
			  :format-arguments (list got-tag)
			  :server-string comment))))))


(defun get-next-tag ()
  (let ((tag (pop *cur-imap-tags*)))
    (if*  tag
       thenret
       else (setq *cur-imap-tags* *imap-tags*)
	    (pop *cur-imap-tags*))))

(defun handle-untagged-response (mb command count extra comment)
  ;; default function to handle untagged responses, which are 
  ;; really just returning general state information about
  ;; the mailbox
  (case command
    (:exists (setf (mailbox-message-count mb) count))
    (:recent (setf (mailbox-recent-messages mb) count))
    (:flags  (setf (mailbox-flags mb) (mapcar #'kwd-intern extra)))
    (:bye ; occurs when connection times out or mailbox lock is stolen
     (ignore-errors (close (post-office-socket mb)))
     (po-error :server-shutdown-connection
		 :server-string "server shut down the connection"))
    (:no ; used when grabbing a lock from another process
     (po-condition :problem :server-string comment))
    (:ok ; a whole variety of things
     (if* extra
	then (if* (equalp (car extra) "unseen")
		then (setf (first-unseen mb) (cadr extra))
	      elseif (equalp (car extra) "uidvalidity")
		then (setf (mailbox-uidvalidity mb) (cadr extra))
	      elseif (equalp (car extra) "uidnext")
		then (setf (mailbox-uidnext mb) (cadr extra))
	      elseif (equalp (car extra) "permanentflags")
		then (setf (mailbox-permanent-flags mb) 
		       (mapcar #'kwd-intern (cadr extra)))
		else (po-condition :unknown-ok :server-string comment))))
    (t (po-condition :unknown-untagged :server-string comment)))
	     
  )



(defun send-pop-command-get-results (pop command &optional extrap)
  ;; send the given command to the pop server
  ;; if extrap is true and if the response is +ok, then data
  ;;  will follow the command (up to and excluding the first line consisting 
  ;;  of just a period)
  ;; 
  ;; if the pop server returns an error code we signal a lisp error.
  ;; otherwise
  ;; return
  ;;  extrap is nil -- return the list of tokens on the line after +ok
  ;;  extrap is true -- return the extra object (a big string)
  ;;
  (format (post-office-socket pop) "~a~a" command *crlf*)
  (force-output (post-office-socket pop))
  
  (if* *debug-imap*
     then (format t "~a~a" command *crlf*)
	  (force-output t))

  (multiple-value-bind (result parsed line)
      (get-and-parse-from-pop-server pop)
    (if* (not (eq result :ok))
       then (po-error :error-response
		      :server-string line))

    (if* extrap
       then ;; get the rest of the data
	    ;; many but not all pop servers return the size of the data
	    ;; after the +ok, so we use that to initially size the 
	    ;; retreival buffer.
	    (let ((buf (get-line-buffer (+ (if* (fixnump (car parsed))
					      then (car parsed) 
					      else 2048 ; reasonable size
						   )
					   50)))
		  (pos 0)
		  ; states
		  ;  1 - after lf
		  ;  2 - seen dot at beginning of line
		  ;  3 - seen regular char on line
		  (state 1)
		  (sock (post-office-socket pop)))
	      (flet ((add-to-buffer (ch)
		       (if* (>= pos (length buf))
			  then ; grow buffer
			       (if* (>= (length buf) 
					(1- array-total-size-limit))
				  then ; can't grow it any further
				       (po-error
					:response-too-large
					:format-control
					"response from mail server is too large to hold in a lisp array"))
			       (let ((new-buf (get-line-buffer
					       (* (length buf) 2))))
				 (init-line-buffer new-buf buf)
				 (free-line-buffer buf)
				 (setq buf new-buf)))
		       (setf (schar buf pos) ch)
		       (incf pos)))
		(loop
		  (let ((ch (read-char sock nil nil)))
		    (if* (null ch)
		       then (po-error :unexpected
				      :format-control "premature end of file from server"))
		    (if* (eq ch #\return)
		       thenret ; ignore crs
		       else (case state
			      (1 (if* (eq ch #\.)
				    then (setq state 2)
				  elseif (eq ch #\linefeed)
				    then (add-to-buffer ch)
					 ; state stays at 1
				    else (add-to-buffer ch)
					 (setq state 3)))
			      (2 ; seen first dot
			       (if* (eq ch #\linefeed)
				  then ; end of message
				       (return)
				  else (add-to-buffer ch)
				       (setq state 3)))
			      (3 ; normal reading
			       (add-to-buffer ch)
			       (if* (eq ch #\linefeed)
				  then (setq state 1))))))))
	      (prog1 (subseq buf 0 pos)
		(free-line-buffer buf)))
       else parsed)))
  

  
  
(defun convert-flags-plist (plist)
  ;; scan the plist looking for "flags" indicators and 
  ;; turn value into a list of symbols rather than strings
  (do ((xx plist (cddr xx)))
      ((null xx) plist)
    (if* (equalp "flags" (car xx))
       then (setf (cadr xx) (mapcar #'kwd-intern (cadr xx))))))


(defmethod select-mailbox ((mb imap-mailbox) name)
  ;; select the given mailbox
  (send-command-get-results mb
			    (format nil "select ~a" name)
			    #'handle-untagged-response
			    #'(lambda (mb command count extra comment)
				(declare (ignore mb count extra))
				(if* (not (eq command :ok))
				   then (po-error 
					 :problem
					 :format-control 
					 "imap mailbox select failed"
					 :server-string comment))))
  (setf (mailbox-name mb) name)
  t
  )


(defmethod fetch-letter ((mb imap-mailbox) number &key uid)
  ;; return the whole letter
  (fetch-field number "body[]"
	       (fetch-parts mb number "body[]" :uid uid)
	       :uid uid))


(defmethod fetch-letter ((pb pop-mailbox) number &key uid)
  (declare (ignore uid))
  (send-pop-command-get-results pb 
				(format nil "RETR ~d" number) 
				t ; extra stuff
				))

(defmethod fetch-parts ((mb imap-mailbox) number parts &key uid)
  (let (res)
    (send-command-get-results 
     mb
     (format nil "~afetch ~a ~a"
	     (if* uid then "uid " else "")
	     (message-set-string number)
	     (or parts "body[]")
	     )
     #'(lambda (mb command count extra comment)
	 (if* (eq command :fetch)
	    then (push (list count (internalize-flags extra)) res)
	    else (handle-untagged-response
		  mb command count extra comment)))
     #'(lambda (mb command count extra comment)
	 (declare (ignore mb count extra))
	 (if* (not (eq command :ok))
	    then (po-error :problem
			   :format-control "imap mailbox fetch failed"
			   :server-string comment))))
    res))

		      
(defun fetch-field (letter-number field-name info &key uid)
  ;; given the information from a fetch-letter, return the 
  ;; particular field for the particular letter
  ;;
  ;; info is as returned by fetch
  ;; field-name is a string, case doesn't matter.
  ;;
  (dolist (item info)
    ;; item is (messagenumber plist-info)
    ;; the same messagenumber may appear in multiple items
    (let (use-this)
      (if* uid
	 then ; uid appears as a property in the value, not
	      ; as the top level message sequence number
	      (do ((xx (cadr item) (cddr xx)))
		  ((null xx))
		(if* (equalp "uid" (car xx))
		   then (if* (eql letter-number (cadr xx))
			   then (return (setq use-this t))
			   else (return))))
	 else ; just a message sequence number
	      (setq use-this (eql letter-number (car item))))
    
      (if* use-this
	 then (do ((xx (cadr item) (cddr xx)))
		  ((null xx))
		(if* (equalp field-name (car xx))
		   then (return-from fetch-field (cadr xx))))))))

	 

(defun internalize-flags (stuff)
  ;; given a plist like object, look for items labelled "flags" and 
  ;; convert the contents to internal flags objects
  (do ((xx stuff (cddr xx)))
      ((null xx))
    (if* (equalp (car xx) "flags")
       then (setf (cadr xx) (mapcar #'kwd-intern (cadr xx)))
	    (return)))
  
  stuff)

					


(defmethod delete-letter ((mb imap-mailbox) messages &key (expunge t) uid)
  ;; delete all the mesasges and do the expunge to make 
  ;; it permanent if expunge is true
  (alter-flags mb messages :add-flags :\\deleted :uid uid)
  (if* expunge then (expunge-mailbox mb)))

(defmethod delete-letter ((pb pop-mailbox) messages  &key (expunge nil) uid)
  ;; delete all the messages.   We can't expunge without quitting so
  ;; we don't expunge
  (declare (ignore expunge uid))
  
  (if* (or (numberp messages) 
	   (and (consp messages) (eq :seq (car messages))))
     then (setq messages (list messages)))
  
  (if* (not (consp messages))
     then (po-error :syntax-error
		    :format-control "expect a mesage number or list of messages, not ~s"
		 :format-arguments (list messages)))
  
  (dolist (message messages)
    (if* (numberp message)
       then (send-pop-command-get-results pb
					  (format nil "DELE ~d" message))
     elseif (and (consp message) (eq :seq (car message)))
       then (do ((start (cadr message) (1+ start))
		 (end (caddr message)))
		((> start end))
	      (send-pop-command-get-results pb
					    (format nil "DELE ~d" start)))
       else (po-error :syntax-error
		      :format-control "bad message number ~s" 
		      :format-arguments (list message)))))
	    
	    
			    
					

(defmethod noop ((mb imap-mailbox))
  ;; just poke the server... keeping it awake and checking for
  ;; new letters
  (send-command-get-results mb
			    "noop"
			    #'handle-untagged-response
			    #'(lambda (mb command count extra comment)
				(check-for-success
				 mb command count extra
				 comment
				 "noop"))))


(defmethod noop ((pb pop-mailbox))
  ;; send the stat command instead so we can update the message count
  (let ((res (send-pop-command-get-results pb "stat")))
      (setf (mailbox-message-count pb) (car res)))
  )


(defmethod unique-id ((pb pop-mailbox) &optional message)
  ;; if message is given, return the unique id of that
  ;; message, 
  ;; if message is not given then return a list of lists:
  ;;  (message  unique-id)
  ;; for all messages not marked as deleted
  ;;
  (if* message
     then (let ((res (send-pop-command-get-results pb
						   (format nil 
							   "UIDL ~d" 
							   message))))
	    (cadr res))
     else ; get all of them
	  (let* ((res (send-pop-command-get-results pb "UIDL" t))
		 (end (length res))
		 kind
		 mnum
		 mid
		 (next 0))
		      
		
	    (let ((coll))
	      (loop
		(multiple-value-setq (kind mnum next) 
		  (get-next-token res next end))
		
		(if* (eq :eof kind) then (return))
		
		(if* (not (eq :number kind))
		   then ; hmm. bogus
			(po-error :unexpected
				  :format-control "uidl returned illegal message number in ~s"
				  :format-arguments (list res)))
		
		; now get message id
		
		(multiple-value-setq (kind mid next)
		    (get-next-token res next end))
		
		(if* (eq :number kind)
		   then ; looked like a number to the tokenizer,
			; make it a string to be consistent
			(setq mid (format nil "~d" mid))
		 elseif (not (eq :string kind))
		   then ; didn't find the uid
			(po-error :unexpected
				  :format-control "uidl returned illegal message id in ~s"
				  :format-arguments (list res)))
		
		(push (list mnum mid) coll))
	      
	      (nreverse coll)))))

(defmethod top-lines ((pb pop-mailbox) message lines)
  ;; return the header and the given number of top lines of the message
  
  (let ((res (send-pop-command-get-results pb
					   (format nil 
						   "TOP ~d ~d"
						   message
						   lines)
					   t ; extra
					   )))
    res))
			     
			
		
						   


(defun check-for-success (mb command count extra comment command-string )
  (declare (ignore mb count extra))
  (if* (not (eq command :ok))
     then (po-error :error-response
		    :format-control "imap ~a failed" 
		    :format-arguments (list command-string)
		    :server-string comment)))

  
			    


(defmethod mailbox-list ((mb imap-mailbox) &key (reference "") (pattern ""))
  ;; return a list of mailbox names with respect to a given
  (let (res)
    (send-command-get-results mb
			      (format nil "list ~s ~s" reference pattern)
			      #'(lambda (mb command count extra comment)
				  (if* (eq command :list)
				     then (push extra res)
				     else (handle-untagged-response
					   mb command count extra
					   comment)))
			      #'(lambda (mb command count extra comment)
				  (check-for-success 
				   mb command count extra 
				   comment "list")))
    
    ;; the car of each list is a set of keywords, make that so
    (dolist (rr res)
      (setf (car rr) (mapcar #'kwd-intern (car rr))))
    
    res
				
  
    ))


(defmethod create-mailbox ((mb imap-mailbox) mailbox-name)
  ;; create a mailbox name of the given name.
  ;; use mailbox-separator if you want to create a hierarchy
  (send-command-get-results mb
			    (format nil "create ~s" mailbox-name)
			    #'handle-untagged-response
			    #'(lambda (mb command count extra comment)
				  (check-for-success 
				   mb command count extra 
				   comment "create")))
  t)


(defmethod delete-mailbox ((mb imap-mailbox) mailbox-name)
  ;; create a mailbox name of the given name.
  ;; use mailbox-separator if you want to create a hierarchy
  (send-command-get-results mb
			    (format nil "delete ~s" mailbox-name)
			    #'handle-untagged-response
			    #'(lambda (mb command count extra comment)
				  (check-for-success 
				   mb command count extra 
				   comment "delete"))))

(defmethod rename-mailbox ((mb imap-mailbox) old-mailbox-name new-mailbox-name)
  ;; create a mailbox name of the given name.
  ;; use mailbox-separator if you want to create a hierarchy
  (send-command-get-results mb
			    (format nil "rename ~s ~s" 
				    old-mailbox-name
				    new-mailbox-name)
			    #'handle-untagged-response
			    #'(lambda (mb command count extra comment)
				  (check-for-success 
				   mb command count extra 
				   comment
				   "rename"))))



(defmethod alter-flags ((mb imap-mailbox)
			messages &key (flags nil flags-p) 
				      add-flags remove-flags
				      silent uid)
  ;;
  ;; change the flags using the store command
  ;;
  (let (cmd val res)
    (if* flags-p
       then (setq cmd "flags" val flags)
     elseif add-flags
       then (setq cmd "+flags" val add-flags)
     elseif remove-flags
       then (setq cmd "-flags" val remove-flags)
       else (return-from alter-flags nil))
    
    (if* (atom val) then (setq val (list val)))
    
    (send-command-get-results mb
			      (format nil "~astore ~a ~a~a ~a"
				      (if* uid then "uid " else "")
				      (message-set-string messages)
				      cmd
				      (if* silent 
					 then ".silent"
					 else "")
				      (if* val
					 thenret
					 else "()"))
			      #'(lambda (mb command count extra comment)
				  (if* (eq command :fetch)
				     then (push (list count 
						      (convert-flags-plist
						       extra))
						res)
				     else (handle-untagged-response
					   mb command count extra
					   comment)))
			      
			      #'(lambda (mb command count extra comment)
				  (check-for-success 
				   mb command count extra 
				   comment "store")))
    res))


(defun message-set-string (messages)
  ;; return a string that describes the messages which may be a
  ;; single number or a sequence of numbers
  
  (if* (atom messages)
     then (format nil "~a" messages)
     else (if* (and (consp messages)
		    (eq :seq (car messages)))
	     then (format nil "~a:~a" (cadr messages) (caddr messages))
	     else (let ((str (make-string-output-stream))
			(precomma nil))
		    (dolist (msg messages)
		      (if* precomma then (format str ","))
		      (if* (atom msg)
			 then (format str "~a" msg)
		       elseif (eq :seq (car msg))
			 then (format str
				      "~a:~a" (cadr msg) (caddr msg))
			 else (po-error :syntax-error
					:format-control "bad message list ~s" 
					:format-arguments (list msg)))
		      (setq precomma t))
		    (get-output-stream-string str)))))
				   
				   
				   
			      
					      
     
(defmethod expunge-mailbox ((mb imap-mailbox))
  ;; remove messages marked as deleted
  (let (res)
    (send-command-get-results mb
			      "expunge"
			      #'(lambda (mb command count extra
					 comment)
				  (if* (eq command :expunge)
				     then (push count res)
				     else (handle-untagged-response
					   mb command count extra
					   comment)))
			      #'(lambda (mb command count extra comment)
				  (check-for-success 
				   mb command count extra 
				   comment "expunge")))
    (nreverse res)))
    
    
	    
(defmethod close-mailbox ((mb imap-mailbox))
  ;; remove messages marked as deleted
  (send-command-get-results mb
			    "close"
			    #'handle-untagged-response
			      
			    #'(lambda (mb command count extra comment)
				(check-for-success 
				 mb command count extra 
				 comment "close")))
  t)
  


(defmethod copy-to-mailbox ((mb imap-mailbox) message-list destination
			    &key uid)
  (send-command-get-results mb
			    (format nil "~acopy ~a ~s"
				    (if* uid then "uid " else "")
				    (message-set-string message-list)
				    destination)
			    #'handle-untagged-response
			    #'(lambda (mb command count extra comment)
				(check-for-success 
				 mb command count extra 
				 comment "copy")))
  t)


;; search command

(defmethod search-mailbox ((mb imap-mailbox) search-expression &key uid)
  (let (res)
    (send-command-get-results mb
			      (format nil "~asearch ~a" 
				      (if* uid then "uid " else "")
				      (build-search-string search-expression))
			      #'(lambda (mb command count extra comment)
				  (if* (eq command :search)
				     then (setq res (append res extra))
				     else (handle-untagged-response
					   mb command count extra
					   comment)))
			      #'(lambda (mb command count extra comment)
				  (check-for-success 
				   mb command count extra 
				   comment "search")))
    res))
    
		       
(defmacro defsearchop (name &rest operands)
  (if* (null operands)
     then `(setf (get ',name 'imap-search-no-args) t)
     else `(setf (get ',name 'imap-search-args) ',operands)))

(defsearchop :all)
(defsearchop :answered)
(defsearchop :bcc :str)
(defsearchop :before :date)
(defsearchop :body :str)
(defsearchop :cc :str)
(defsearchop :deleted)
(defsearchop :draft)
(defsearchop :flagged)
(defsearchop :from :str)
(defsearchop :header :str :str)
(defsearchop :keyword :flag)
(defsearchop :larger :number)
(defsearchop :new)
(defsearchop :old)
(defsearchop :on :date)
(defsearchop :recent)
(defsearchop :seen)
(defsearchop :sentbefore :date)
(defsearchop :senton :date)
(defsearchop :sentsince :date)
(defsearchop :since :date)
(defsearchop :smaller :number)
(defsearchop :subject :str)
(defsearchop :text :str)
(defsearchop :to :str)
(defsearchop :uid :messageset)
(defsearchop :unanswered)
(defsearchop :undeleted)
(defsearchop :undraft)
(defsearchop :unflagged)
(defsearchop :unkeyword :flag)
(defsearchop :unseen)



(defun build-search-string (search)
  ;; take the lisp search form and turn it into a string that can be
  ;; passed to imap

  (if* (null search)
     then ""
     else (let ((str (make-string-output-stream)))
	    (bss-int search str)
	    (get-output-stream-string str))))

(defun bss-int (search str)
  ;;* it turns out that imap (on linux) is very picky about spaces....
  ;; any extra whitespace will result in failed searches
  ;;
  (labels ((and-ify (srch str)
	     (let ((spaceout nil))
	       (dolist (xx srch) 
		 (if* spaceout then (format str " "))
		 (bss-int xx str)
		 (setq spaceout t))))
	   (or-ify (srch str)
	     ; only binary or allowed in imap but we support n-ary 
	     ; or in this interface
	     (if* (null (cdr srch))
		then (bss-int (car srch) str)
	      elseif (cddr srch)
		then ; over two clauses
		     (format str "or (")
		     (bss-int (car srch) str)
		     (format str  ") (")
		     (or-ify (cdr srch) str)
		     (format str ")")
		else ; 2 args
		     (format str "or (" )
		     (bss-int (car srch) str)
		     (format str ") (")
		     (bss-int (cadr srch) str)
		     (format str ")")))
	   (set-ify (srch str)
	     ;; a sequence of messages
	     (do* ((xsrch srch (cdr xsrch))
		   (val (car xsrch) (car xsrch)))
		 ((null xsrch))
	       (if* (integerp val)
		  then (format str "~s" val)
		elseif (and (consp val) 
			    (eq :seq (car val))
			    (eq 3 (length val)))
		  then (format str "~s:~s" (cadr val) (caddr val))
		  else (po-error :syntax-error
				 :format-control "illegal set format ~s" 
				 :format-arguments (list val)))
	       (if* (cdr xsrch) then (format str ","))))
	   (arg-process (str args arginfo)
	     ;; process and print each arg to str
	     ;; assert (length of args and arginfo are the same)
	     (do* ((x-args args (cdr x-args))
		   (val (car x-args) (car x-args))
		   (x-arginfo arginfo (cdr x-arginfo)))
		 ((null x-args))
	       (ecase (car x-arginfo)
		 (:str
		  ; print it as a string
		  (format str " \"~a\"" (car x-args)))
		 (:date
		  
		  (if* (integerp val)
		     then (setq val (universal-time-to-rfc822-date
				     val))
		   elseif (not (stringp val))
		     then (po-error :syntax-error
				    :format-control "illegal value for date search ~s"
				    :format-arguments (list val)))
		  ;; val is now a string
		  (format str " ~s" val))
		 (:number
		  
		  (if* (not (integerp val))
		     then (po-error :syntax-error
				    :format-control "illegal value for number in search ~s" 
				    :format-arguments (list val)))
		  (format str " ~s" val))
		 (:flag
		  
		  ;; should be a symbol in the kwd package
		  (setq val (string val))
		  (format str " ~s" val))
		 (:messageset
		  (if* (numberp val) 
		     then (format str " ~s" val)
		   elseif (consp val)
		     then (set-ify val str)
		     else (po-error :syntax-error
				    :format-control "illegal message set ~s" 
				    :format-arguments (list val))))
		  
		 ))))
    
    (if* (symbolp search)
       then (if* (get search 'imap-search-no-args)
	       then (format str "~a"  (string-upcase
				       (string search)))
	       else (po-error :syntax-error
			      :format-control "illegal search word: ~s" 
			      :format-arguments (list search)))
     elseif (consp search)
       then (case (car search)
	      (and (if* (null (cdr search))
		      then (bss-int :all str)
		    elseif (null (cddr search))
		      then (bss-int (cadr search) str)
		      else (and-ify (cdr search)  str)))
	      (or  (if* (null (cdr search))
		      then (bss-int :all str)
		    elseif (null (cddr search))
		      then (bss-int (cadr search) str)
		      else (or-ify (cdr search)  str)))
	      (not (if* (not (eql (length search) 2))
		      then (po-error :syntax-error 
				     :format-control "not takes one argument: ~s" 
				     :format-arguments (list search)))
		   (format str "not (" )
		   (bss-int (cadr search) str)
		   (format str ")"))
	      (:seq
	       (set-ify (list search) str))
	      (t (let (arginfo) 
		   (if* (and (symbolp (car search))
			     (setq arginfo (get (car search)
						'imap-search-args)))
		      then 
			   (format str "~a" (string-upcase
					     (string (car search))))
			   (if* (not (equal (length (cdr search))
					    (length arginfo)))
			      then (po-error :syntax-error 
					     :format-control "wrong number of arguments to ~s" 
					     :format-arguments search))
			   
			   (arg-process str (cdr search) arginfo)
			   
		    elseif (integerp (car search))
		      then (set-ify search str)
		      else (po-error :syntax-error 
				     :format-control "Illegal form ~s in search string" 
				     :format-arguments (list search))))))
     elseif (integerp search)
       then ;  a message number
	    (format str "~s" search)
       else (po-error :syntax-error
		      :format-control "Illegal form ~s in search string" 
		      :format-arguments (list search)))))





(defun parse-mail-header (text)  
  ;; given the partial text of a mail message that includes
  ;; at least the header part, return an assoc list of
  ;; (header . content)  items
  ;; Note that the header is string with most likely mixed case names
  ;; as it's conventional to capitalize header names.
  (let ((next 0)
	(end (length text))
	header
	value
	kind
	headers)
    (labels ((next-header-line ()
	       ;; find the next header line return
	       ;; :eof - no more
	       ;; :start - beginning of header value, header and
	       ;;	         value set
	       ;; :continue - continuation of previous header line
	     
		       
	       (let ((state 1)
		     beginv  ; charpos beginning value
		     beginh  ; charpos beginning header
		     ch
		     )
		 (tagbody again
		   
		   (return-from next-header-line
		     
		     (loop  ; for each character
		       
		       (if* (>= next end)
			  then (return :eof))
		 
		       (setq ch (char text next))
		       (if* (eq ch #\return) 
			  thenret  ; ignore return, (handle following linefeed)
			  else (case state
				 (1 ; no characters seen
				  (if* (eq ch #\linefeed)
				     then (incf next)
					  (return :eof)
				   elseif (member ch
						  '(#\space
						    #\tab))
				     then ; continuation
					  (setq state 2)
				     else (setq beginh next)
					  (setq state 3)
					  ))
				 (2 ; looking for first non blank in value
				  (if* (eq ch #\linefeed)
				     then ; empty continuation line, ignore
					  (incf next)
					  (go again)
				   elseif (not (member ch
						       (member ch
							       '(#\space
								 #\tab))))
				     then ; begin value part
					  (setq beginv next)
					  (setq state 4)))
				 (3 ; reading the header
				  (if* (eq ch #\linefeed)
				     then ; bogus header line, ignore
					  (go again)
				   elseif (eq ch #\:)
				     then (setq header
					    (subseq text beginh next))
					  (setq state 2)))
				 (4 ; looking for the end of the value
				  (if* (eq ch #\linefeed)
				     then (setq value
					    (subseq text beginv 
						    (if* (eq #\return
							     (char text
								   (1- next)))
						       then (1- next)
						       else next)))
					  (incf next)
					  (return (if* header
						     then :start
						     else :continue)))))
			       (incf next))))))))
					 
	       
    
      (loop ; for each header line
	(setq header nil)
	(if* (eq :eof (setq kind (next-header-line)))
	   then (return))
	(case kind
	  (:start (push (cons header value) headers))
	  (:continue
	   (if* headers
	      then ; append to previous one
		   (setf (cdr (car headers))
		     (concatenate 'string (cdr (car headers))
				  " " 
				  value)))))))
    (values headers
	    (subseq text next end))))


(defun make-envelope-from-text (text)
  ;; given at least the headers part of a message return
  ;; an envelope structure containing the contents
  ;; This is useful for parsing the headers of things returned by
  ;; a pop server
  ;;
  (let ((headers (parse-mail-header text)))
  
    (make-envelope
     :date     (cdr (assoc "date" headers :test #'equalp))
     :subject  (cdr (assoc "subject" headers :test #'equalp))
     :from     (cdr (assoc "from" headers :test #'equalp))
     :sender   (cdr (assoc "sender" headers :test #'equalp))
     :reply-to (cdr (assoc "reply-to" headers :test #'equalp))
     :to       (cdr (assoc "to" headers :test #'equalp))
     :cc       (cdr (assoc "cc" headers :test #'equalp))
     :bcc      (cdr (assoc "bcc" headers :test #'equalp))
     :in-reply-to (cdr (assoc "in-reply-to" headers :test #'equalp))
     :message-id (cdr (assoc "message-id" headers :test #'equalp))
     )))

		  
	      
				 
	      




    
(defmethod get-and-parse-from-imap-server ((mb imap-mailbox))
  ;; read the next line and parse it
  ;;
  ;;
  (multiple-value-bind (line count)
      (get-line-from-server mb)
    (if* *debug-imap* 
       then (format t "from server: ")
	    (dotimes (i count)(write-char (schar line i)))
	    (terpri)
	    (force-output))
    
    (parse-imap-response line count)
    ))



(defmethod get-and-parse-from-pop-server ((mb pop-mailbox))
  ;; read the next line from the pop server
  ;;
  ;; return 3 values:
  ;;   :ok or :error 
  ;;   a list of rest of the tokens on the line
  ;;   the whole line after the +ok or -err

  (multiple-value-bind (line count)
      (get-line-from-server mb)
    
    (if* *debug-imap* 
       then (format t "from server: " count)
	    (dotimes (i count)(write-char (schar line i)))
	    (terpri))
    
    (parse-pop-response line count)))

  
  
;; Parse and return the data from each line
;; values returned
;;  tag -- either a string or the symbol :untagged
;;  command -- a keyword symbol naming the command, like :ok
;;  count -- a number which preceeded the command, or nil if
;;	     there wasn't a command
;;  bracketted - a list of objects found in []'s after the command
;;            or in ()'s after the command  or sometimes just 
;;	      out in the open after the command (like the search)
;;  comment  -- the whole of the part after the command
;;
(defun parse-imap-response (line end)
  (let (kind value next
	tag count command extra-data
	comment)
    
    ;; get tag
    (multiple-value-setq (kind value next)
      (get-next-token line 0 end))
    
    (case kind
      (:string (setq tag (if* (equal value "*")
			    then :untagged
			    else value)))
      (t (po-error :unexpected
		   :format-control "Illegal tag on response: ~s" 
		   :format-arguments (list (subseq line 0 count))
		   :server-string (subseq line 0 end)
		   )))
      
    ;; get command
    (multiple-value-setq (kind value next)
      (get-next-token line next end))
      
    (tagbody again
      (case kind
	(:number (setq count value)
		 (multiple-value-setq (kind value next)
		   (get-next-token line next end))
		 (go again))
	(:string (setq command (kwd-intern value)))
	(t (po-error :unexpected 
		     :format-control "Illegal command on response: ~s" 
		     :format-arguments (list (subseq line 0 count))
		     :server-string (subseq line 0 end)))))

    (setq comment (subseq line next end))
    
    ;; now the part after the command... this gets tricky
    (loop
      (multiple-value-setq (kind value next)
	(get-next-token line next end))
      
      (case kind
	((:lbracket :lparen)
	 (multiple-value-setq (kind value next)
	   (get-next-sexpr line (1- next) end))
	 (case kind
	   (:sexpr (push value extra-data))
	   (t (po-error :syntax-error :format-control "bad sexpr form"))))
	(:eof (return nil))
	((:number :string :nil) (push value extra-data))
	(t  ; should never happen
	 (return)))
      
      (if* (not (member command '(:list :search) :test #'eq))
	 then ; only one item returned
	      (setq extra-data (car extra-data))
	      (return)))

    (if* (member command '(:list :search) :test #'eq)
       then (setq extra-data (nreverse extra-data)))
    
      
    (values tag command count extra-data comment)))
      


(defun get-next-sexpr (line start end)
  ;; read a whole s-expression
  ;; return 3 values
  ;;   kind -- :sexpr  or :rparen or :rbracket
  ;;   value - the sexpr value
  ;;   next  - next charpos to scan
  ;;  
  (let ( kind value next)
    (multiple-value-setq (kind value next) (get-next-token line start end))
    
    (case kind
      ((:string :number :nil)
       (values :sexpr value next))
      (:eof (po-error :syntax-error 
		      :format-control "eof inside sexpr"))
      ((:lbracket :lparen)
       (let (res)
	 (loop
	   (multiple-value-setq (kind value next)
	     (get-next-sexpr line next end))
	   (case kind
	     (:sexpr (push value res))
	     ((:rparen :rbracket) 
	      (return (values :sexpr (nreverse res) next)))
	     (t (po-error :syntax-error
			  :format-control "bad sexpression"))))))
      ((:rbracket :rparen)
       (values kind nil next))
      (t (po-error :syntax-error
		   :format-control "bad sexpression")))))


(defun parse-pop-response (line end)
  ;; return 3 values:
  ;;   :ok or :error 
  ;;   a list of rest of the tokens on the line, the tokens
  ;;	 being either strings or integers
  ;;   the whole line after the +ok or -err
  ;;
  (let (res lineres result)
    (multiple-value-bind (kind value next)
	(get-next-token line 0 end)
    
      (case kind
	(:string (setq result (if* (equal "+OK" value) 
				 then :ok
				 else :error)))
	(t (po-error :unexpected
		     :format-control "bad response from server" 
		     :server-string (subseq line 0 end))))
    
      (setq lineres (subseq line next end))

      (loop
	(multiple-value-setq (kind value next)
	  (get-next-token line next end))
	
	(case kind
	  (:eof (return))
	  ((:string :number) (push value res))))
      
      (values result (nreverse res) lineres))))
    
	
    
    
    
    
      
      
			 
    
(defparameter *char-to-kind*
    (let ((arr (make-array 256 :initial-element nil)))
      
      (do ((i #.(char-code #\0) (1+ i)))
	  ((> i #.(char-code #\9)))
	(setf (aref arr i) :number))
      
      (setf (aref arr #.(char-code #\space)) :space)
      (setf (aref arr #.(char-code #\tab)) :space)
      (setf (aref arr #.(char-code #\return)) :space)
      (setf (aref arr #.(char-code #\linefeed)) :space)
      
      (setf (aref arr #.(char-code #\[)) :lbracket)
      (setf (aref arr #.(char-code #\])) :rbracket)
      (setf (aref arr #.(char-code #\()) :lparen)
      (setf (aref arr #.(char-code #\))) :rparen)
      (setf (aref arr #.(char-code #\")) :dquote)
      
      (setf (aref arr #.(char-code #\^b)) :big-string) ; our own invention
      
      arr))
	
      
(defun get-next-token (line start end)
  ;; scan past whitespace for the next token
  ;; return three values:
  ;;  kind:  :string , :number, :eof, :lbracket, :rbracket,
  ;;		:lparen, :rparen
  ;;  value:  the value, either a string or number or nil
  ;;  next:   the character pos to start scanning for the next token
  ;;
  (let (ch chkind colstart (count 0) (state :looking)
	collector right-bracket-is-normal) 
    (loop 
      ; pick up the next character
      (if* (>= start end)
	 then (if* (eq state :looking)
		 then (return (values :eof nil start))
		 else (setq ch #\space))
	 else (setq ch (schar line start)))
      
      (setq chkind (aref *char-to-kind* (char-code ch)))
      
      (case state
	(:looking
	 (case chkind
	   (:space nil)
	   (:number (setq state :number)
		    (setq colstart start)
		    (setq count (- (char-code ch) #.(char-code #\0))))
	   ((:lbracket :lparen :rbracket :rparen)
	    (return (values chkind nil (1+ start))))
	   (:dquote
	    (setq collector (make-array 10 
					:element-type 'character
					:adjustable t 
					:fill-pointer 0))
	    (setq state :qstring))
	   (:big-string
	    (setq colstart (1+ start))
	    (setq state :big-string))
	   (t (setq colstart start)
	      (setq state :literal))))
	(:number
	 (case chkind
	   ((:space :lbracket :lparen :rbracket :rparen 
	     :dquote) ; end of number
	    (return (values :number count  start)))
	   (:number ; more number
	    (setq count (+ (* count 10) 
			   (- (char-code ch) #.(char-code #\0)))))
	   (t ; turn into an literal
	    (setq state :literal))))
	(:literal
	 (case chkind
	   ((:space :rbracket :lparen :rparen :dquote) ; end of literal
	    (if* (and (eq chkind :rbracket)
		      right-bracket-is-normal)
	       then nil ; don't stop now
	       else (let ((seq (subseq line colstart start)))
		      (if* (equal "NIL" seq)
			 then (return (values :nil
					      nil
					      start))
			 else (return (values :string 
					      seq
					      start))))))
	   (t (if* (eq chkind :lbracket)
		 then ; imbedded left bracket so right bracket isn't
		      ; a break char
		      (setq right-bracket-is-normal t))
	      nil)))
	(:qstring
	 ;; quoted string
	 ; (format t "start is ~s  kind is ~s~%" start chkind)
	 (case chkind
	   (:dquote
	    ;; end of string
	    (return (values :string collector (1+ start))))
	   (t (if* (eq ch #\\)
		 then ; escaping the next character
		      (incf start)
		      (if* (>= start end)
			 then (po-error :unexpected
					:format-control "eof in string returned"))
		      (setq ch (schar line start)))
	      (vector-push-extend ch collector)
	      
	      (if* (>= start end)
		 then ; we overran the end of the input
		      (po-error :unexpected
				:format-control "eof in string returned")))))
	(:big-string
	 ;; super string... just a block of data
	 ; (format t "start is ~s  kind is ~s~%" start chkind)
	 (case chkind
	   (:big-string
	    ;; end of string
	    (return (values :string 
			    (subseq line colstart start)
			    (1+ start))))
	   (t nil)))
	
		      
	)
      
      (incf start))))
	    
	    

;  this used to be exported from the excl package
#+(version>= 6 0)
(defvar *keyword-package* (find-package :keyword))
	   
      
(defun kwd-intern (string)
  ;; convert the string to the current preferred case
  ;; and then intern
  (intern (case excl::*current-case-mode*
	    ((:case-sensitive-lower
	      :case-insensitive-lower) (string-downcase string))
	    (t (string-upcase string)))
	  *keyword-package*))
      
      
      
    
      
      
	
      
    

  
    
    
  
;; low level i/o to server

(defun get-line-from-server (mailbox)
  ;; Return two values:  a buffer and a character count.
  ;; The character count includes up to but excluding the cr lf that
  ;;  was read from the socket.
  ;; 
  (let* ((buff (get-line-buffer 0))
	 (len  (length buff))
	 (i 0)
	 (p (post-office-socket mailbox))
	 (ch nil)
	 (whole-count) 
	 )

    (handler-case 
	(flet ((grow-buffer (size)
		 (let ((newbuff (get-line-buffer size)))
		   (dotimes (j i)
		     (setf (schar newbuff j) (schar buff j)))
		   (free-line-buffer buff)
		   (setq buff newbuff)
		   (setq len (length buff)))))
	     
	  ;; increase the buffer to at least size
	  ;; this is somewhat complex to ensure that we aren't doing
	  ;; buffer allocation within the with-timeout form, since 
	  ;; that could trigger a gc which could then cause the 
	  ;; with-timeout form to expire.
	  (loop
      
	    (if* whole-count
	       then ; we should now read in this may bytes and 
		    ; append it to this buffer
		    (multiple-value-bind (ans this-count)
			(get-block-of-data-from-server mailbox whole-count)
		      ; now put this data in the current buffer
		      (if* (> (+ i whole-count 5) len)
			 then  ; grow the initial buffer
			      (grow-buffer (+ i whole-count 100)))
		
		      (dotimes (ind this-count)
			(setf (schar buff i) (schar ans ind))
			(incf i))
		      (setf (schar buff i) #\^b) ; end of inset string
		      (incf i)
		      (free-line-buffer ans)
		      (setq whole-count nil)
		      )
	     elseif ch
	       then ; we're growing the buffer holding the line data
		    (grow-buffer (+ len 200))
		    (setf (schar buff i) ch)
		    (incf i))

	    
	    (block timeout
	      (mp:with-timeout ((timeout mailbox)
				(po-error :timeout
					  :format-control "imap server failed to respond"))
		;; read up to lf  (lf most likely preceeded by cr)
		(loop
		  (setq ch (read-char p))
		  (if* (eq #\linefeed ch)
		     then ; end of line. Don't save the return
			  (if* (and (> i 0)
				    (eq (schar buff (1- i)) #\return))
			     then ; remove #\return, replace with newline
				  (decf i)
				  (setf (schar buff i) #\newline)
				  )
			  ;; must check for an extended return value which
			  ;; is indicated by a {nnn} at the end of the line
			  (block count-check
			    (let ((ind (1- i)))
			      (if* (and (>= i 0) (eq (schar buff ind) #\}))
				 then (let ((count 0)
					    (mult 1))
					(loop
					  (decf ind)
					  (if* (< ind 0) 
					     then ; no of the form {nnn}
						  (return-from count-check))
					  (setf ch (schar buff ind))
					  (if* (eq ch #\{)
					     then ; must now read that many bytes
						  (setf (schar buff ind) #\^b)
						  (setq whole-count count)
						  (setq i (1+ ind))
						  (return-from timeout)
					   elseif (<= #.(char-code #\0)
						      (char-code ch)
						      #.(char-code #\9))
					     then ; is a digit
						  (setq count 
						    (+ count
						       (* mult
							  (- (char-code ch)
							     #.(char-code #\0)))))
						  (setq mult (* 10 mult))
					     else ; invalid form, get out
						  (return-from count-check)))))))
					
		  
			  (return-from get-line-from-server
			    (values buff i))
		     else ; save character
			  (if* (>= i len)
			     then ; need bigger buffer
				  (return))
			  (setf (schar buff i) ch)
			  (incf i)))))))
      (error (con)
	;; most likely error is that the server went away
	(ignore-errors (close p))
	(po-error :server-shutdown-connection
		  :format-control "condition  signalled: ~a~%most likely server shut down the connection."
		  :format-arguments (list con)))
      )))


(defun get-block-of-data-from-server  (mb count &key save-returns)
  ;; read count bytes from the server returning it in a line buffer object
  ;; return as a second value the number of characters saved 
  ;; (we drop #\return's so that lines are sepisarated by a #\newline
  ;; like lisp likes).
  ;;
  (let ((buff (get-line-buffer count))
	(p (post-office-socket mb))
	(ind 0))
    (mp:with-timeout ((timeout mb)
		      (po-error :timeout
				:format-control "imap server timed out"))
      
      (dotimes (i count)
	(if* (eq #\return (setf (schar buff ind) (read-char p)))
	   then (if* save-returns then (incf ind)) ; drop #\returns
	   else (incf ind)))
	
      
      (values buff ind))))
      
    
;;-- reusable line buffers

(defvar *line-buffers* nil)

(defun get-line-buffer (size)
  ;; get a buffer of at least size bytes
  (setq size (min size (1- array-total-size-limit)))
  (mp::without-scheduling
    (dolist (buff *line-buffers* (make-string size))
	(if* (>= (length buff) size)
	   then ; use this one
		(setq *line-buffers* (delete buff *line-buffers*))
		(return buff)))))


(defun free-line-buffer (buff)
  (mp:without-scheduling
    (push buff *line-buffers*)))

(defun init-line-buffer (new old)
  ;; copy old into new
  (declare (optimize (speed 3)))
  (dotimes (i (length old))
    (declare (fixnum i))
    (setf (schar new i) (schar old i))))


  

  ;;;;;;;

; date functions

(defun universal-time-to-rfc822-date (ut)
  ;; convert a lisp universal time to rfc 822 date
  ;;
  (multiple-value-bind
      (sec min hour date month year day-of-week dsp time-zone)
      (decode-universal-time ut 0)
    (declare (ignore time-zone sec min hour day-of-week dsp time-zone))
    (format nil "~d-~a-~d"
	    date
	    (svref
	     '#(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
		"Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
	     month
	     )
	    year)))
  
			  
	  
		  
