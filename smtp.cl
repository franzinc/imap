;; mail sending package
;;
;; smtp -  rfc821
;;
(defpackage :net.post-office
  (:use #:lisp #:excl)
  (:export 
   #:send-letter
   #:send-smtp))

(in-package :net.post-office)

;; the exported functions:

;; (send-letter "mail-server" "from" "to" "message" &key subject reply-to)
;;  
;;    sends a message to the mail server (which may be a relay server
;;    or the final destination).  "from" is the address to be given
;;    as the sender.  "to" can be a string or a list of strings naming
;;    recipients.   
;;    "message" is the message to be sent
;;    This builds a header and inserts the optional subject and reply-to
;;    lines.
;;
;; (send-smtp "mail-server" "from" "to" &rest messages)
;;    this is like send-letter except that it doesn't build a header.
;;    the messages should contain a header (and if not then sendmail
;;    notices this and builds one -- other MTAs may not be that smart).
;;    The messages ia  list of string to be concatenated together
;;    and sent as one message
;;
;;











(defmacro response-case ((ftp-stream) &rest case-clauses)
  (let ((response-class (gensym)))
    `(multiple-value-bind (,response-class ftp-response)
	 (progn (force-output ,ftp-stream)
		(wait-for-response ,ftp-stream))
       (declare (ignorable ftp-response))
       (case ,response-class
	 ,@case-clauses))))

(defvar *smtp-debug* nil)



(defun send-letter (server from to message
		    &key subject
			 reply-to)
  (let ((header (make-string-output-stream)))
    (format header "From: ~a~c~cTo: "
	    from
	    #\return
	    #\linefeed)
    (let ((tos (if* (stringp to) 
			then (list to) 
		      elseif (consp to)
			then to
			else (error "to should be a string or list, not ~s"
				    to))))
      (format header "~{ ~a~^,~}~c~c" tos #\return #\linefeed))
    
    (if* subject
       then (format header "Subject: ~a~c~c" subject #\return #\linefeed))
    
    (if* reply-to
       then (format header "Reply-To: ~a~c~c" reply-to #\return #\linefeed))
    
    
    (format header "~c~c" #\return #\linefeed)
    
    (send-smtp server from to (get-output-stream-string header) message)
  
  
  
    ))
    
    
	  
		    

(defun send-smtp (server from to &rest messages)
  ;; send the effective concatenation of the messages via
  ;; smtp to the mail server
  ;; Each message should be a string
  ;;
  ;; 'to' can be a single string or a list of strings.
  ;; each string should be in the official rfc822 format  "foo@bar.com"
  ;;
  (let ((sock (socket:make-socket :remote-host server
				  :remote-port 25  ; smtp
				  )))
    (unwind-protect
	(progn
	  (response-case (sock)
			 (2 ;; to the initial connect
			  nil)
			 (t (error "initial connect failed")))
	  
	  ;; now that we're connected we can compute our hostname
	  (let ((hostname (socket:ipaddr-to-hostname
			   (socket:local-host sock))))
	    (if* (null hostname)
	       then (format nil "[~a]" (socket:ipaddr-to-dotted
					(socket:local-host sock))))
	    (smtp-command sock "HELO ~a" hostname)
	    (response-case (sock)
			   (2 ;; ok
			    nil)
			   (t (error "hello greeting failed"))))
	    
	  (smtp-command sock "MAIL from:<~a>" from)
	  (response-case (sock)
			 (2 ;; cool
			  nil
			  )
			 (t (error "Mail from command failed")))

	  (let ((tos (if* (stringp to) 
			then (list to) 
		      elseif (consp to)
			then to
			else (error "to should be a string or list, not ~s"
				    to))))
	    (dolist (to tos)
	      (smtp-command sock "RCPT to:<~a>" to)
	      (response-case (sock)
			     (2 ;; cool
			      nil
			      )
			     (t (error "rcpt to command failed")))))
	
	  (smtp-command sock "DATA")
	  (response-case (sock)
			 (3 ;; cool
			  nil)
			 (t (error "Data command failed")))
	  ;(format t "sending message~%") (force-output t)	
	  
	  
	  (let ((at-bol t))
	    (dolist (message messages)
	      (dotimes (i (length message))
		(let ((ch (aref message i)))
		  (if* (and at-bol (eq ch #\.))
		     then ; to prevent . from being interpreted as eol
			  (write-char #\. sock))
		  (if* (eq ch #\newline)
		     then (setq at-bol t)
			  (write-char #\return sock)
		     else (setq at-bol nil))
		  (write-char ch sock)))))
	
	  (write-char #\return sock) (write-char #\linefeed sock)
	  (write-char #\. sock)
	  (write-char #\return sock) (write-char #\linefeed sock)
	
	  (response-case (sock)
			 (2 nil ; (format t "Message sent to ~a~%" to)
			    )
			 
			 (t (error "message not sent")))

	  (force-output t)
	  
	  (smtp-command sock "QUIT")
	  (response-case (sock)
			 (2 ;; cool
			  nil)
			 (t (error "quit failed"))))
      (close sock))))







	
      
(defun wait-for-response (stream)
  ;; read the response of the ftp server.
  ;; collect it all in a string.
  ;; Return two values:
  ;; 	response class
  ;;    whole string
  ;; The string should begin with a decimal digit, and that is converted
  ;; into a number which is returned as the response class.
  ;; If the string doesn't begin with a decimal digit then the
  ;; response class is -1.
  ;;
  (flet ((match-chars (string pos1 pos2 count)
	   ;; like strncmp
	   (dotimes (i count t)
	     (if* (not (eq (aref string (+ pos1 i))
			   (aref string (+ pos2 i))))
		then (return nil)))))

    (let ((res (make-array 20 :element-type 'character
			   :adjustable t
			   :fill-pointer 0)))
      (if* (null (read-a-line stream res))
	 then ; eof encountered before end of line
	      (return-from wait-for-response (values -1 res)))

      ;; a multi-line response begins with line containing
      ;; a hyphen in the 4th column:
      ;; xyz-  some text
      ;;
      ;;  and ends with a line containing the same reply code but no
      ;;  hyphen.
      ;; xyz  some text
      ;;

      (if* (and (>= (length res) 4) (eq #\- (aref res 3)))
	 then ;; multi line response
	      (let ((old-length (length res))
		    (new-length nil))
		(loop
		  (if* (null (read-a-line stream res))
		     then ; eof encountered before end of line
			  (return-from wait-for-response (values -1 res)))
		  (setq new-length (length res))
		  ;; see if this is the last line
		  (if* (and (>= (- new-length old-length) 4)
			    (eq (aref res (+ old-length 3)) #\space)
			    (match-chars res 0 old-length 3))
		     then (return))

		  (setq old-length new-length))))

      ;; complete response is in res
      ;; compute class and return the whole thing
      (let ((class (or (and (> (length res) 0)
			    (digit-char-p (aref res 0)))
		       -1)))
	(values class res)))))      

(defun smtp-command (stream &rest format-args)
  ;; send a command to the smtp server
  (let ((command (apply #'format nil format-args)))
    (if* *smtp-debug*
       then (format *smtp-debug* "to smtp command: ~s~%" command)
	    (force-output *smtp-debug*))
    (write-string command stream)
    (write-char #\return stream)
    (write-char #\newline stream)
    (force-output stream)))

(defun read-a-line (stream res)
  ;; read from stream and put the result in the adjust able array res
  ;; if line ends in cr-lf, only put a newline in res.
  ;; If we get an eof before the line finishes, return nil,
  ;; else return t if all is ok
  (let (ch last-ch)
    (loop
      (setq ch (read-char stream nil nil))
      (if* (null ch)
	 then ; premature eof
	      (return nil))

      (if* *smtp-debug*
	 then (format *smtp-debug* "~c" ch)
	      (force-output *smtp-debug*)
	      )

      (if* (eq last-ch #\return)
	 then (if* (eq ch #\linefeed)
		 then (vector-push-extend #\newline res)
		      (return t)
		 else (vector-push-extend last-ch res))
       elseif (eq ch #\linefeed)
	 then ; line ends with just lf, not cr-lf
	      (vector-push-extend #\newline res)
	      (return t)
       elseif (not (eq ch #\return))
	 then (vector-push-extend ch res))

      (setq last-ch ch))))
