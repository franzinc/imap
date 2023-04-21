;; -*- mode: common-lisp; package: net.mail -*-
;;
;; See the file LICENSE for the full license governing this code.

#+(version= 10 1)
(sys:defpatch "rfc2822" 1
  "v1: dns-record-exists-p: find correct cname entry to query."
  :type :system
  :post-loadable t)

#+(version= 9 0)
(sys:defpatch "rfc2822" 1
  "v1: parse-email-address: check length of local-part"
  :type :system
  :post-loadable t)

(defpackage :net.mail
  (:use #:lisp #:excl)
  (:export #:parse-email-address
	   #:extract-email-addresses
	   #:valid-email-domain-p))

(in-package :net.mail)

#|
Email address parser.  If parsing succeeds, then the email address
has valid syntax.  

The parser should be RFC2822 compliant except: 

* It optionally allows for domain-less addresses.
* By default, it requires the domain part to have two components (not
  actually required by the spec).
* It does not allow domain literals (e.g., "joe-user@[192.132.95.23]")
* It does not allow quoted strings.

Exports:

Function: parse-email-address 
Args: string &key require-domain require-dotted-domain

Parses an email address string and returns two values: the username 
part of the address and the domain part of the address.  

Keyword arguments:

:require-domain  

defaults to true.  If true, then the @domain part of the email address
is required.  If nil, then the @domain part of the email address is
not required.  If it is not found, then the second return value of
this function will be nil.

:require-dotted-domain

defaults to true.  If true, then the domain part of the email address
must have two dotted components (e.g., "franz.com").  If nil, then a
single-component domain part is accepted (e.g., "com").

---

Function: valid-email-domain-p
Args: domain

Returns information on whether or not the DNS configuration for
'domain' is configured properly for Internet email reception.  

Possible return values:

nil

This means that the DNS records for 'domain' are not properly
configured for Internet email. 

:unknown

This means that no information was successfully collected.  No
conclusion can be drawn.

t

This means that 'domain' has DNS records that are suitable for
Internet email reception.  This does not necessarily mean that email
delivery will succeed.  

Note:  This function is more useful for its negative response (nil)
than any other response.  If it returns nil, it means that no standard
mail transfer agent would be able to locate the mail server for the
domain.

|#

(eval-when (compile eval)
  (defconstant *controls* "\\x0-\\x1f")
  
  (defconstant *specials* "()<>\\[\\]:;@\\,.\"")
  
  (defconstant *atext* 
      (format nil "[^\\s~a~a]" *controls* *specials*))

  (defconstant *atom* (format nil "^~a+" *atext*))

  (defconstant *dot-atom-text* (format nil "~a+(?:\\.~a+)*" *atext* *atext*))
  
  ;; More strict than the RFC, but good for verifying syntax of email
  ;; addresses that a user supplies.
  
  (defconstant *email-address-re*
      (format nil "^\\s*(~a)(?:@(~a))?\\s*$" *dot-atom-text* *dot-atom-text*))
  
  )
      
(defun parse-email-address (string &key (require-domain t)
					(require-dotted-domain t))
  (multiple-value-bind (matched x local-part domain)
      (match-re #.*email-address-re* string)
    (declare (ignore x))
    (if* (or 
	  ;; Failure cases
	  (not matched) 
	  (and require-domain (null domain))
	  (and require-dotted-domain domain (zerop (count #\. domain)))
	  ;; From rfc3696
	  (> (length local-part) 64))
       then nil
       else (values local-part domain))))

;; Returns a list of entries like so: 
;;  (:mailbox display-name user domain)
;;  or
;;  (:group display-name mailbox-list)
;; or, if 'compact' keyword arg is true, returns a flattened list of 
;;  user@domain strings.

(defun extract-email-addresses (string &key (start 0) (end (length string))
					    (require-domain t) (errorp t)
					    compact)
  (declare (optimize (speed 3))
	   (fixnum start end))
  (with-underlying-simple-vector (string string disp)
    (declare (simple-string string)
	     (fixnum disp))
    (incf start disp)
    (incf end disp)
    
    ;; Unfold.
    (when (match-re "\\r?\\n\\s" string :start start :end end)
      (setf string (replace-re string "\\r?\\n\\s" " " 
			       :start start :end end))
      (setf start 0)
      (setf end (length string)))
    
    (let ((res 
	   (catch 'syntax-error
	     (parse-address-list string start end require-domain))))
      (if* (stringp res)
	 then (if errorp (error res))
       elseif (null res)
	 then (if errorp 
		  (error "Failed to parse: ~s" (subseq string start end)))
	      nil
       elseif compact
	 then (compact-extracted-addresses res)
	 else res))))

(defun compact-extracted-addresses (list)
  (declare (optimize (speed 3)))
  (let (res)
    (dolist (entry list)
      (let ((type (car entry)))
	(ecase type
	  (:mailbox
	   (let ((user (third entry))
		 (domain (fourth entry)))
	     (push (if* domain
		      then (concatenate 'string user "@" domain)
		      else user)
		   res)))
	  (:group
	   (dolist (addr (compact-extracted-addresses (third entry)))
	     (push addr res))))))
    (nreverse res)))

(macrolet ((parse-special (char skip-ws)
	     `(multiple-value-bind (type value newpos)
		  (rfc2822-lex string start end ,skip-ws)
		(declare (ignore type))
		(when (eq value ,char)
		  (setf start newpos)))))

  ;; Supports obsolete format which allows for null members in the list.
  (defun parse-address-list (string start end require-domain)
    (let (res)
      (loop
	(while (parse-special #\, t))
	(multiple-value-bind (addr newpos)
	    (parse-address string start end require-domain)
	  (if (null addr)
	      (return))
	  (setf start newpos)
	  (push addr res)))
      (values (nreverse res) start)))
    
  (defun parse-address (string start end require-domain)
    (multiple-value-bind (mb newpos)
	(parse-mailbox string start end require-domain)
      (if* mb
	 then (values mb newpos)
	 else (parse-group string start end require-domain))))
  
  
  (defun parse-mailbox (string start end require-domain)
    (multiple-value-bind (ok display-name localpart domain newpos)
	(parse-name-addr string start end require-domain)
      (if ok
	  (return-from parse-mailbox 
	    (values
	     (list :mailbox display-name localpart domain)
	     newpos))))
    (multiple-value-bind (localpart domain newpos)
	(parse-addr-spec string start end require-domain)
      (when localpart
	(setf start newpos)
	;; Check for a trailing comment and use that as the display name
	(multiple-value-bind (display-name newpos)
	    (grab-next-comment string start end)
	  (if display-name
	      (setf start newpos))
	  (values
	   (list :mailbox display-name localpart domain)
	   start)))))
  
  (defun grab-next-comment (string start end)
    (loop
      (multiple-value-bind (type value newpos)
	  (rfc2822-lex string start end nil)
	(if (eq type :comment)
	    (return (values (replace-re value "^\\((.*)\\)$" "\\1") newpos)))
	(if* (eq type :wsp)
	   then (setf start newpos)
	   else (return)))))
	
  (defun parse-group (string start end require-domain)
    (multiple-value-bind (display-name newpos)
	(parse-phrase string start end)
      (when display-name
	(setf start newpos)
	(when (parse-special #\: t)
	  (multiple-value-bind (mailbox-list newpos)
	      (parse-mailbox-list string start end require-domain)
	    (setf start newpos)
	    (when (parse-special #\; t)
	      (values (list :group display-name mailbox-list) newpos)))))))

  (defun parse-mailbox-list (string start end require-domain)
    (let (res)
      (loop
	(multiple-value-bind (mailbox newpos)
	    (parse-mailbox string start end require-domain)
	  (if (null mailbox)
	      (return))
	  (push mailbox res)
	  (setf start newpos)
	  (if (not (parse-special #\, t))
	      (return))))
      (values (nreverse res) start)))
  
  (defun parse-name-addr (string start end require-domain)
    (multiple-value-bind (display-name newpos)
	(parse-phrase string start end)
      (if display-name
	  (setf start newpos))
      (multiple-value-bind (localpart domain newpos)
	  (parse-angle-addr string start end require-domain)
	(when localpart
	  (values t display-name localpart domain newpos)))))
  
  ;; This is obs-phrase, which is seen often.  For example:
  ;; From: Mr. T <mr.t@pitythefool.com>
  (defun parse-phrase (string start end)
    (let ((first t)
	  res type value newpos)
      (loop
	(multiple-value-setq (type value newpos)
	  (rfc2822-lex string start end first))
	(if* (or (eq type :atom)
		 (eq type :quoted-string)
		 (and (not first) (or (eq value #\.) (eq type :wsp))))
	   then (push value res)
		(setf first nil)
		(setf start newpos)
	   else (return)))
      ;; Dump any trailing whitespace we collected
      (if (and (stringp res) (match-re "^\\s" (first res)))
	  (pop res))
      (if res 
	  (values (list-to-delimited-string (nreverse res) "") start))))
    
  (defun parse-angle-addr (string start end require-domain)
    (when (parse-special #\< t)
      (multiple-value-bind (localpart domain newpos)
	  (parse-addr-spec string start end require-domain)
	(setf start newpos)
	(when (and localpart (parse-special #\> t))
	  (values localpart domain start)))))
  
  (defun parse-addr-spec (string start end require-domain)
    (multiple-value-bind (localpart newpos)
	(parse-local-part string start end)
      (when localpart
	(setf start newpos)
	(when (not (parse-special #\@ t))
	  (if* require-domain
	     then (return-from parse-addr-spec)
	     else (return-from parse-addr-spec 
		    (values localpart nil start))))
	(multiple-value-bind (domain newpos)
	    (parse-dot-atom string start end)
	  (when domain
	    (values localpart domain newpos)))))))

(defun parse-local-part (string start end)
  (multiple-value-bind (type value newpos)
      (rfc2822-lex string start end t)
    (if* (eq type :quoted-string)
       then (values value newpos)
     elseif (eq type :atom)
       then (parse-dot-atom string start end))))

(defun parse-dot-atom (string start end)
  (let ((first t)
	res)
    (loop
      (multiple-value-bind (type value newpos)
	  (rfc2822-lex string start end first)
	(setf first nil)
	(if (null type)
	    (return))
	(if* (eq type :atom)
	   then (push value res)
	 elseif (not (eq value #\.))
	   then (return))
	(setf start newpos)))
    (if res
	(values (list-to-delimited-string (nreverse res) #\.) start))))

(eval-when (compile)
  (defconstant *max-comment-nesting* 3)
  
  (defparameter *cchar* "(?:[^()\\\\]|\\\\.)")
  (defparameter *comment* nil)
  
  (dotimes (n *max-comment-nesting*)
    (if* *comment*
       then (setf *comment* (format nil "(?:\\((?:~a|~a)*\\))"
				   *cchar* *comment*))
       else (setf *comment* (format nil "(?:\\(~a*\\))" *cchar*))))
  
  (setf *comment* (format nil "^~a" *comment*)))

(defun rfc2822-lex (string start end skip-ws)
  (declare (optimize (speed 3))
	   (simple-string string)
	   (fixnum start end))
  (when (< start end)
    (let ((char (schar string start)))
      (if* (eq char #\") 
	 then ;; quoted string.
	      (multiple-value-bind (matched whole)
		  (match-re "^\"((?:[^\\\\\"]|\\\\.)*)\"" string
			    :start start :end end
			    :return :index)
		(if (not matched)
		    (throw 'syntax-error "Unterminated quoted string"))
		(values :quoted-string 
			(subseq string (car whole) (cdr whole))
			(cdr whole)))
       elseif (or (eq char #\space) (eq char #\tab)
		  (eq char #\return) (eq char #\newline))
	 then ;; whitespace
	      (multiple-value-bind (x match)
		  (match-re "^\\s+" string 
			    :start start :end end :return :index)
		(declare (ignore x))
		(if* skip-ws
		   then (rfc2822-lex string (cdr match) end t)
		   else (values :wsp
				(subseq string (car match) (cdr match))
				(cdr match))))
       elseif (eq char #\()
	 then ;; comment
	      (multiple-value-bind (matched whole)
		  (match-re #.*comment* string
			    :start start :end end :return :index)
		(if (not matched)
		    (throw 'syntax-error 
		      "Unterminated comment or nesting too deep"))
		(if* skip-ws
		   then (rfc2822-lex string (cdr whole) end t)
		   else (values :comment
				(subseq string (car whole) (cdr whole))
				(cdr whole))))
	 else (multiple-value-bind (matched whole)
		  (match-re *atom* string :start start :end end 
			    :return :index)
		(if* (not matched)
		   then ;; must be a special
			(values :special
				char
				(1+ start))
		   else ;; atom
			(values :atom
				(subseq string (car whole) (cdr whole))
				(cdr whole))))))))

#+ignore
(defun test (file &key errorp (compact t) temp)
  (let ((seen-addrs (make-hash-table :test #'equal)))
    (with-open-file (f file)
      (let* ((part (net.post-office:parse-mime-structure f))
	     (hdrs (net.post-office:mime-part-headers part)))
	(dolist (type '("From" "To" "Cc"))
	  (let ((hdr (cdr (assoc type hdrs :test #'equalp))))
	    (when (and hdr 
		       (string/= hdr "")
		       (not (gethash hdr seen-addrs)))
	      (setf (gethash hdr seen-addrs) t)
	      (if* (setq temp
		     (extract-email-addresses hdr :require-domain nil
					      :errorp errorp
					      :compact compact))
		 then (format t "have:~{ ~a~}~%" temp)
		 else (format t "Failed to parse: ~s~%" hdr)))))))))

;; Ripped from maild:dns.cl and modified.

(eval-when (compile load eval)
  (require :acldns))

;; Only follows one CNAME lookup.  If there is any more than that, the
;; domain has a really jacked up setup.

;; possible answers
;;  t -- yes, there exists a record of that type.
;;  nil -- no record of that type exists
;;  :nxdomain -- the domain itself doesn't exist
;;  :unknown -- couldn't get any answers.
(defun dns-record-exists-p (domain type &key (try-cname t))
  (block nil
    (let ((resp (socket:dns-query domain :decode nil :type type))
	  cname-item)
      (if (null resp)
	  (return :unknown))
      (let ((flags (socket:dns-response-flags resp))
	    (answer (socket:dns-response-answer resp)))
	(cond 
	 ((member :nameserver-internal-error flags)
	  (return :unknown))
	 ((member :no-such-domain flags)
	  (return :nxdomain))
	 ((null answer)
	  (return nil)) ;; no records of that type for that name
	 ((setq cname-item
	    (find :cname answer :test #'eq :key #'socket:dns-rr-type))
	  (if* (not try-cname)
	     then (return nil)
	     else ;; There should only be one cname answer.
		  (return (dns-record-exists-p (socket:dns-rr-answer cname-item)
					       type :try-cname nil))))
	 (t
	  t))))))
  
;; A valid email domain is one that has an MX record or an A record
;; [or a CNAME to an MX or A record (illegal, but people do it)]

;; possible answers:  
;;  t -- there is either an MX or A record for that domain
;;  nil -- there is neither an MX nor A record for that domain
;          (possibly because the domain does not exist at all)
;; :unknown -- couldn't get answers
(defun valid-email-domain-p (domain)
  (block nil
    (let ((res (dns-record-exists-p domain :mx)))
      (cond
       ((eq res t)
	(return t))
       ((eq res :nxdomain)
	(return nil))
       ((eq res :unknown)
	(return :unknown)))
      (setf res (dns-record-exists-p domain :a))
      (cond
       ((eq res t)
	(return t))
       ((eq res :nxdomain)
	(return nil))
       ((eq res :unknown)
	(return :unknown)))
      nil)))

(provide :rfc2822)
