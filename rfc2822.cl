;; -*- mode: common-lisp; package: net.post-office -*-
;;
;; copyright (c) 1999-2002 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: rfc2822.cl,v 1.4 2007/05/31 23:13:08 dancy Exp $

#+(version= 8 0)
(sys:defpatch "rfc2822" 0
  "v0: New module.  See documentation."
  :type :system
  :post-loadable t)

(defpackage :net.mail
  (:use #:lisp #:excl)
  (:export #:parse-email-address
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
  
  (defconstant *no-ws-ctl* "\\x1-\\x8\\xb-\\xc\\xe-\\x1f\\x7f")
  
  (defconstant *fws* "(?:(?:[ \\t]*\\r?\\n)?[ \\t]+)")

  (defconstant *text* "[^\\r\\n]")
  
  (defconstant *quoted-pair* (format nil "\\\\~a" *text*))

  (defconstant *ctext* "[^\\s()\\\\]")

  ;; 1 means (xx)
  ;; 2 means (xxx (yyy) zzz)
  (defconstant *max-comment-level* 2)
  
  (defparameter *ccontent nil)
  (defparameter *comment* nil)
  
  (dotimes (n *max-comment-level*)
    (if* (null *comment*)
       then (setf *ccontent* (format nil "(?:~a|~a)" *ctext* *quoted-pair*))
       else (setf *ccontent* (format nil "(?:~a|~a|~a)" 
				     *ctext* *quoted-pair* *comment*)))
    
    (setf *comment* (format nil "\\((?:~a?~a)*~a?\\)"
			    *fws* *ccontent* *fws*)))
  
  (defconstant *cfws* (format nil "(?:~a?~a)*(?:(?:~a?~a)|~a)"
			      *fws* *comment* *fws* *comment* *fws*))
  
  (defconstant *atext* 
      (format nil "[^\\s~a~a]" *controls* *specials*))
  
  (defconstant *atom* (format nil "~a?(~a+)~a?" *cfws* *atext* *cfws*))

  (defconstant *dot-atom-text* (format nil "~a+(?:\\.~a+)*" *atext* *atext*))
  
  (defconstant *dot-atom* (format nil "~a?(~a)~a?" 
				  *cfws* *dot-atom-text* *cfws*))
  
  ;; no control chars, no backslash, no quote
  (defconstant *qtext* (format nil "[^~a\\\\\"]" *controls*))
  
  (defconstant *qcontent* (format nil "~a|~a" *qtext* *quoted-pair*))

  (defconstant *quoted-string*
      (format nil "~a?\"((?:~a?~a)*~a?)\"~a?"
	      *cfws* *fws* *qcontent* *fws* *cfws*))
  
  (defconstant *local-part* 
      (format nil "(~a)|(~a)" *dot-atom* *quoted-string*))
  
  ;; domain literals not supported.
  (defconstant *domain* *dot-atom*)
  
  (defconstant *addr-spec* (format nil "(~a)@(~a)" *local-part* *domain*))
  
  (defconstant *angle-addr* (format nil "~a?<~a>~a?" 
				    *cfws* *addr-spec* *cfws*))
  
  (defconstant *word* (format nil "(?:~a|~a)" *atom* *quoted-string*))
  
  (defconstant *phrase* (format nil "~a+" *word*))
  
  (defconstant *display-name* *phrase*)
  
  (defconstant *name-addr* (format nil "~a?~a" *display-name* *angle-addr*))
  
  (defconstant *mailbox* (format nil "(?:~a|~a)" *name-addr* *addr-spec*))
  
  (defconstant *mailbox-list* 
      (format nil "(?:~a(?:,~a)*)" *mailbox* *mailbox*))

  (defconstant *group* 
      (format nil "~a:(?:~a|~a)?;~a?" *display-name* *mailbox-list* *cfws*
	      *cfws*))
  
  ;; More strict than the RFC.
  
  (defconstant *email-address-re*
      (format nil "^\\s*(~a)(?:@(~a))?\\s*$" *dot-atom-text* *dot-atom-text*))
  
  )
      
(defun parse-email-address (string &key (require-domain t)
					(require-dotted-domain t))
  (multiple-value-bind (matched x user domain)
      (match-re #.*email-address-re* string)
    (declare (ignore x))
    (if* (or 
	  ;; Failure cases
	  (not matched) 
	  (and require-domain (null domain))
	  (and require-dotted-domain domain (zerop (count #\. domain))))
       then nil
       else (values user domain))))

;; Returns a list of entries like so: 
;;  (:mailbox user domain display-name)
;;  or
;;  (:group display-name mailbox-list)

(defun extract-email-addresses (string &key (start 0) (end (length string))
					    (errorp t))
  )

(defmacro parse-common (re)
  (let ((matched (gensym))
	(whole (gensym))
	(inner (gensym)))
    (setf re (format nil "^~a" (symbol-value re)))
    `(multiple-value-bind (,matched ,whole, inner)
	 (match-re ,re string :start start :end end :return :index)
       (when ,matched
	 (values (subseq string (car ,inner) (cdr ,inner))
		 (cdr ,whole))))))

;; Domain literals not supported
;; local-part @ domain ==>
;; dot-atom/quoted-string @ dot-atom
;; Optionally allows domain-less addrspecs.  However, doing so
;; makes parsing ambiguous.
(defun parse-addr-spec (string start end require-domain)
  (declare (optimize (speed 3))
	   (fixnum start end))
  (block nil
    (multiple-value-bind (local-part newpos)
	(parse-local-part string start end)
      (if (null local-part)
	  (return))
      (setf start newpos)
      (when (or (eq start end)
		(not (eq (char string start) #\@)))
	;; no domain part.
	(if* require-domain
	   then (return)
	   else (return (values local-part nil start))))
      (incf start)
      (multiple-value-bind (domain newpos)
	  (parse-common *dot-atom*)
	(if domain
	    (values local-part domain newpos))))))

(defun parse-local-part (string &optional (start 0) (end (length string)))
  (multiple-value-bind (dot-atom newpos)
      (parse-common *dot-atom*)
    (if* dot-atom
       then (values dot-atom newpos)
       else (multiple-value-bind (quoted-string newpos)
		(parse-common *quoted-string*)
	      (when quoted-string
		(values quoted-string newpos))))))

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
    (let ((resp (socket:dns-query domain :decode nil :type type)))
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
	 ((member :cname answer
		  :test #'eq :key #'socket:dns-rr-type)
	  (if* (not try-cname)
	     then (return nil)
	     else ;; There should only be one cname answer.
		  (return (dns-record-exists-p (socket:dns-rr-answer 
						(first answer))
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
