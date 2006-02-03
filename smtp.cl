#+(version= 8 0)
(sys:defpatch "smtp" 1
  "v1: send-letter w/attachments; send-smtp* can take streams"
  :type :system
  :post-loadable t)

#+(version= 7 0)
(sys:defpatch "smtp" 2
  "v2: send-letter w/attachments; send-smtp* can take streams"
  :type :system
  :post-loadable t)

;; -*- mode: common-lisp; package: net.post-office -*-
;;
;; smtp.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2004 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;
;; $Id: smtp.cl,v 1.14 2006/02/03 18:02:26 layer Exp $

;; Description:
;;   send mail to an smtp server.  See rfc821 for the spec.

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(defpackage :net.post-office
  (:use #:lisp #:excl)
  (:export 
   #:send-letter
   #:send-smtp
   #:send-smtp-auth
   #:test-email-address))

(in-package :net.post-office)

(eval-when (compile load eval)
  (require :streamp)
  (require :sasl)
  (require :mime))

;; the exported functions:

;; (send-letter "mail-server" "from" "to" "message" 
;;		&key cc bcc subject reply-to headers)
;;								
;;  
;;    sends a message to the mail server (which may be a relay server
;;    or the final destination).  "from" is the address to be given
;;    as the sender.  "to" can be a string or a list of strings naming
;;    recipients.   
;;    "message" is the message to be sent.  It can be a string or a stream.
;;    cc and bcc can be either be a string or a  list of strings
;;	naming recipients.  All cc's and bcc's are sent the message
;;	but the bcc's aren't included in the header created.
;;    reply-to's value is a string and in cases a Reply-To header
;;	 to be created.
;;    headers is a string or list of stings. These are raw header lines
;;	added to the header build to send out.
;;
;;    This builds a header and inserts the optional cc, bcc, 
;;    subject and reply-to  lines.
;;
;; (send-smtp "mail-server" "from" "to" &rest messages)
;;    this is like send-letter except that it doesn't build a header.
;;    the messages should contain a header (and if not then sendmail
;;    notices this and builds one -- other MTAs may not be that smart).
;;    The messages ia  list of strings or streams to be concatenated together
;;    and sent as one message
;;
;;
;;  (test-email-address "user@machine.com")
;;    return t is this could be a valid email address on the machine
;;    named.  Do this by contacting the mail server and using the VRFY
;;    command from smtp.  Since some mail servers don't implement VRFY
;;    we return t if VRFY doesn't work.
;;    nil means that this address is bad (or we can't make contact with
;;    the mail server, which could of course be a transient problem).
;;





(defmacro response-case ((smtp-stream &optional smtp-response response-code) &rest case-clauses)
  ;; get a response from the smtp server and dispatch in a 'case' like
  ;; fashion to a clause based on the first digit of the return
  ;; code of the response.
  ;; smtp-response, if given, will be bound to string that is
  ;;  the actual response
  ;; 
  (let ((response-class (gensym)))
    `(multiple-value-bind (,response-class 
			   ,@(if* smtp-response then (list smtp-response))
			   ,@(if* response-code then (list response-code)))
	 (progn (force-output ,smtp-stream)
		(wait-for-response ,smtp-stream))
       ;;(declare (ignorable smtp-response))
       (case ,response-class
	 ,@case-clauses))))

(defvar *smtp-debug* nil)



(defun send-letter (server from to message
		    &key cc bcc subject reply-to headers
			 login password attachments)
  ;;
  ;; see documentation at the head of this file
  ;;
  
  (if* (mime-part-constructed-p message)
     then (if* (and (not (multipart-mixed-p message)) attachments)
	     then (error "~
attachments are not allowed for non-multipart/mixed messages."))
     else (let ((part
		 (if* (streamp message)
		    then 
			 (make-mime-part :file message)
		  elseif (stringp message)
		    then (make-mime-part :text message)
		    else (error "~
message must be a string, stream, or mime-part-constructed, not ~s" message))))
	    
	    (setf message
	      (if* attachments
		 then (make-mime-part :subparts (list part))
		 else part))))
  
  (let ((user-header "")
	(tos (if* (stringp to) 
		then (list to) 
	      elseif (consp to)
		then to
		else (error "to should be a string or list, not ~s" to)))
	(ccs
	 (if* (null cc)
	    then nil
	  elseif (stringp cc) 
	    then (list cc) 
	  elseif (consp cc)
	    then cc
	    else (error "cc should be a string or list, not ~s" cc)))
	(bccs (if* (null bcc)
		 then nil
	       elseif (stringp bcc) 
		 then (list bcc) 
	       elseif (consp bcc)
		 then bcc
		 else (error "bcc should be a string or list, not ~s" bcc))))
    
    (push (cons "From" from) (mime-part-headers message))
    (push (cons "To" (list-to-delimited-string tos ", ")) 
	  (mime-part-headers message))
    
    (if* ccs 
       then 
	    (push (cons "Cc" (list-to-delimited-string ccs ", ")) 
		  (mime-part-headers message)))
    
    (if* subject
       then (push (cons "Subject" subject) (mime-part-headers message)))
    
    (if* reply-to
       then (push (cons "Reply-To" reply-to) (mime-part-headers message)))
    
    (if* headers
       then (if* (stringp headers)
	       then (setq headers (list headers))
	     elseif (consp headers)
	       thenret
	       else (error "Unknown headers format: ~s." headers))
	    (setf user-header 
	      (with-output-to-string (header)
		(dolist (h headers) 
		  (format header "~a~%" h)))))

    (if* attachments
       then (if (not (consp attachments))
		(setf attachments (list attachments)))
	    
	    (dolist (attachment attachments)
	      (if* (mime-part-constructed-p attachment)
		 thenret
	       elseif (or (streamp attachment) (stringp attachment)
			  (pathnamep attachment))
		 then (setf attachment (make-mime-part :file attachment))
		 else (error "~
Attachments must be filenames, streams, or mime-part-constructed, not ~s"
			     attachment))
	      (nconc (mime-part-parts message) (list attachment))))
    
    (with-mime-part-constructed-stream (s message)
      (send-smtp-auth server from (append tos ccs bccs)
		      login password
		      user-header
		      s))))
    
    
(defun send-smtp (server from to &rest messages)
  (send-smtp-1 server from to nil nil messages))
	  
(defun send-smtp-auth (server from to login password &rest messages)
  (send-smtp-1 server from to login password messages))

(defun send-smtp-1 (server from to login password messages)
  ;; send the effective concatenation of the messages via
  ;; smtp to the mail server
  ;; Each message should be a string or a stream.
  ;;
  ;; 'to' can be a single string or a list of strings.
  ;; each string should be in the official rfc822 format  "foo@bar.com"
  ;;

  (let ((sock (connect-to-mail-server server login password)))
  
    (unwind-protect
	(progn
	  
	  (smtp-command sock "MAIL from:<~a>" from)
	  (response-case (sock msg)
	    (2 ;; cool
	     nil
	     )
	    (t (error "Mail from command failed: ~s" msg)))

	  (let ((tos (if* (stringp to) 
			then (list to) 
		      elseif (consp to)
			then to
			else (error "to should be a string or list, not ~s"
				    to))))
	    (dolist (to tos)
	      (smtp-command sock "RCPT to:<~a>" to)
	      (response-case (sock msg)
		(2 ;; cool
		 nil
		 )
		(t (error "rcpt to command failed: ~s" msg)))))
	
	  (smtp-command sock "DATA")
	  (response-case (sock msg)
	    (3 ;; cool
	     nil)
	    (t (error "Data command failed: ~s" msg)))
	  
	  
	  
	  (let ((at-bol t) 
		(prev-ch nil)
		ch stream)
	    (dolist (message messages)
	      (setf stream (if* (streamp message)
			      then message 
			      else (make-string-input-stream message)))
	      (unwind-protect 
		  (progn
		    (while (setf ch (read-char stream nil nil))
		      (if* (and at-bol (eq ch #\.))
			 then ;; to prevent . from being interpreted as eol
			      (write-char #\. sock))
		      (if* (eq ch #\newline)
			 then (setq at-bol t)
			      (if* (not (eq prev-ch #\return))
				 then (write-char #\return sock))
			 else (setq at-bol nil))
		      (write-char ch sock)
		      (setq prev-ch ch)))
		;; unwind-protect
		(if* (not (streamp message))
		   then (close stream)))))
		
	  (write-char #\return sock) (write-char #\linefeed sock)
	  (write-char #\. sock)
	  (write-char #\return sock) (write-char #\linefeed sock)
	
	  (response-case (sock msg)
	    (2 nil ; (format t "Message sent to ~a~%" to)
	       )
			 
	    (t (error "message not sent: ~s" msg)))

	  (force-output t)
	  
	  (smtp-command sock "QUIT")
	  (response-case (sock msg)
	    (2 ;; cool
	     nil)
	    (t (error "quit failed: ~s" msg))))
      (close sock))))

(defun connect-to-mail-server (server login password)
  ;; make that initial connection to the mail server
  ;; returning a socket connected to it and 
  ;; signaling an error if it can't be made.
  (let ((ipaddr (determine-mail-server server))
	(sock)
	(ok))
    
    (if* (null ipaddr)
       then (error "Can't determine ip address for mail server ~s" server))
    
    (setq sock (socket:make-socket :remote-host ipaddr
				   :remote-port 25  ; smtp
				   ))
    (unwind-protect
	(progn
	  (response-case (sock msg)
	    (2 ;; to the initial connect
	     nil)
	    (t (error "initial connect failed: ~s" msg)))
	  
	  ;; now that we're connected we can compute our hostname
	  (let ((hostname (socket:ipaddr-to-hostname
			   (socket:local-host sock))))
	    (if* (null hostname)
	       then (setq hostname
		      (format nil "[~a]" (socket:ipaddr-to-dotted
					  (socket:local-host sock)))))
	    (let ((mechs (smtp-ehlo sock hostname)))
	      (if (and mechs login password)
		  (setf sock 
		    (smtp-authenticate sock server mechs login password)))))
	  
	  ; all is good
	  (setq ok t))
      
      ; cleanup:
      (if* (null ok) 
	 then (close sock :abort t)
	      (setq sock nil)))
    
    ; return:
    sock
    ))
	    

;; Returns string with mechanisms, or nil if none.
;; This may need to be expanded in the future as we support
;; more of the features that EHLO responds with.
(defun smtp-ehlo (sock our-name)
  (smtp-command sock "EHLO ~A" our-name)
  (response-case (sock msg)
    (2 ;; ok
     ;; Collect the auth mechanisms.
     (multiple-value-bind (found whole mechs)
	 (match-regexp "250[- ]AUTH \\(.*\\)" msg)
       (declare (ignore whole))
       (if found
	   mechs)))
    (t
     (smtp-command sock "HELO ~A" our-name)
     (response-case (sock msg)
       (2 ;; ok
	nil)
       (t (error "hello greeting failed: ~s" msg))))))

(defun smtp-authenticate (sock server mechs login password)
  (let ((ctx (net.sasl:sasl-client-new "smtp" server
				       :user login
				       :pass password)))
    (multiple-value-bind (res selected-mech response)
	(net.sasl:sasl-client-start ctx mechs)
      (if (not (eq res :continue))
	  (error "sasl-client-start unexpectedly returned: ~s" res))
      (smtp-command sock "AUTH ~a" selected-mech)
      (loop
	(response-case (sock msg)
	  (3  ;; need more interaction
	   (multiple-value-setq (res response)
	     (net.sasl:sasl-step 
	      ctx 
	      (base64-string-to-usb8-array (subseq msg 4))))
	   (smtp-command sock "~a" 
			 (usb8-array-to-base64-string response nil)))
	  (2 ;; server is satisfied.
	   ;; Make sure the auth process really completed
	   (if (not (net.sasl:sasl-conn-auth-complete-p ctx))
	       (error "SMTP server indicated authentication complete  before mechanisms was satisfied"))
	   ;; It's all good.  
	   (return)) ;; break from loop
	  (t
	   (error "SMTP authentication failed")))))
    
    ;; Reach here if authentication completed.
    ;; If a security layer was negotiated, return an encapsulated sock,
    ;; otherwise just return the original sock.
    (if (net.sasl:sasl-conn-security-layer-p ctx)
	(net.sasl:sasl-make-stream ctx sock :close-base t)
      sock)))
  

  
(defun test-email-address (address)
  ;; test to see if we can determine if the address is valid
  ;; return nil if the address is bogus
  ;; return t if the address may or may not be bogus
  (if* (or (not (stringp address))
	   (zerop (length address)))
     then (error "mail address should be a non-empty string: ~s" address))
  
  ; split on the @ sign
  (let (name hostname)
    (let ((pos (position #\@ address)))
      (if* (null pos)
	 then (setq name address
		    hostname "localhost")
       elseif (or (eql pos 0)
		  (eql pos (1- (length address))))
	 then ; @ at beginning or end, bogus since we don't do route addrs
	      (return-from test-email-address nil)
	 else (setq name (subseq address 0 pos)
		    hostname (subseq address (1+ pos)))))
  
    (let ((sock (ignore-errors (connect-to-mail-server hostname nil nil))))
      (if* (null sock) then (return-from test-email-address nil))
    
      (unwind-protect
	  (progn
	    (smtp-command sock "VRFY ~a" name)
	    (response-case (sock msg code)
	      (5
	       (if* (eq code 550)
		  then ; no such user
		       msg ; to remove unused warning
		       nil
		  else ;; otherwise we don't know
		       (return-from test-email-address t)))
	      (t (return-from test-email-address t)))
	    (smtp-command sock "VRFY ~a" address)
	    (response-case (sock msg code)
	      (5
	       (if* (eq code 550)
		  then ; no such user
		       msg ; to remove unused warning
		       nil
		  else t))
	      (t t)))
	(close sock :abort t)))))
	    
	    
    
    
    
	    
	    
	    





	
      
(defun wait-for-response (stream)
  ;; read the response of the smtp server.
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
	(values class res
		(if* (>= (length res) 3)
		   then ; compute the whole response value
			(+ (* (or (digit-char-p (aref res 0)) 0) 100)
			   (* (or (digit-char-p (aref res 1)) 0) 10)
			   (or (digit-char-p (aref res 2)) 0))))))))

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


(defun determine-mail-server (name)
  ;; return the ipaddress to be used to connect to the 
  ;; the mail server.
  ;; name is any method for naming a machine:
  ;;   integer ip address
  ;;   string with dotted ip address
  ;;   string naming a machine
  ;; we can only do the mx lookup for the third case, the rest 
  ;; we just return the ipaddress for what we were given
  ;;
  (let (ipaddr)
    (if* (integerp name)
       then name
     elseif (integerp (setq ipaddr
			(socket:dotted-to-ipaddr name :errorp nil)))
       then ipaddr
       else ; do mx lookup if acldns is being used
	    (if* (or (eq socket:*dns-mode* :acldns)
		     (and (consp socket:*dns-mode*)
			  (member :acldns socket:*dns-mode* :test #'eq)))
	       then (let ((res (socket:dns-query name :type :mx)))
		      (if* (and (consp res) (cadr res))
			 then (cadr res) ; the ip address
			 else (dolist (suffix socket::*domain-search-list*
					(socket:dns-lookup-hostname name))
				(declare (special socket:*domain-search-list*))
				(let ((name 
				       (concatenate 'string name "." suffix)))
				  (setq res (socket:dns-query name :type :mx))
				  (if* (and res (cadr res))
				     then (return (cadr res)))))))
			      
			      
	       else ; just do a hostname lookup
		    (ignore-errors (socket:lookup-hostname name))))))
		    
  
    
(provide :smtp)
