(in-package :user)

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
  ;; dash at the end to avoid mistaking it for a character range
  ;; indicator.
  (defconstant *atext-chars*
      "!#$%&'*+/0123456789=?ABCDEFGHIJKLMNOPQRSTUVWXYZ^_`abcdefghijklmnopqrstuvwxyz{|}~-")
  
  (defconstant *dot-atom* 
      (format nil "[~a]+(\\.[~a]+)*" *atext-chars* *atext-chars*))

  (defconstant *dotted-dot-atom* 
      (format nil "[~a]+(\\.[~a]+)+"  *atext-chars* *atext-chars*))
  
  (defvar *rfc822-dotted-domain-re*
      (format nil "^(~a)(@(~a))?$" *dot-atom* *dotted-dot-atom*))

  (defvar *rfc822-re* (format nil "^(~a)(@(~a))?$" *dot-atom* *dot-atom*))
  )

(defun parse-email-address (string &key (require-domain t)
					(require-dotted-domain t))
  (multiple-value-bind (matched whole user dummy1 dummy2 domain)
      (if* require-dotted-domain
	 then (match-re #.*rfc822-dotted-domain-re* string)
	 else (match-re #.*rfc822-re* string))
    (declare (ignore whole dummy1 dummy2))
    (if (or (not matched) (and require-domain (null domain)))
	nil
      (values user domain))))

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
