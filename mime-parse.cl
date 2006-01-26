;; $Id: mime-parse.cl,v 1.1 2006/01/26 23:53:27 dancy Exp $

(defpackage :net.post-office
  (:use #:lisp #:excl)
  (:export
   #:parse-mime-structure
   #:mime-dequote
   
   ;; accessors
   #:mime-part-headers-size
   #:mime-part-body-size
   #:mime-part-lines
   #:mime-part-position
   #:mime-part-body-position
   #:mime-part-message

   ;; class name
   #:mime-part-parsed
   
   ))

(in-package :net.post-office)

(eval-when (compile)
  (declaim (optimize (speed 3))))

;;; MIME structure parser.
;;; Ref: RFC2045/2046

(defconstant *whitespace* '(#\space #\tab #\return #\newline))

(defclass mime-part-parsed (mime-part)
  (
   (headers-size ;; in bytes. Includes the bytes for the blank line
    :accessor mime-part-headers-size :initform nil)
   (body-size ;; in bytes.
    :accessor mime-part-body-size :initform nil)
   (lines ;; line count of body (for non-multipart types)
    :accessor mime-part-lines :initform nil)
   (position ;; file position of start of headers
    :accessor mime-part-position :initform nil)
   (body-position ;; file position of start of body
    :accessor mime-part-body-position :initform nil)
   (message ;; for message/rfc822 encapsulated message.  
    ;; This will be a mime-part
    :accessor mime-part-message :initform nil)))

(excl::defresource mime-line 
  :constructor (lambda () (make-array 16000 :element-type 'character
				      :fill-pointer 0))
  :reinitializer (lambda (x) (setf (fill-pointer x) 0)))

;; Return values:
;;  First is the part.  
;;  Second is :eof if end of file was reached or
;;            :boundary if a boundary was reached
;;            nil, otherwise
;;  Third is new position
(defun parse-mime-structure (stream &key boundary digest (pos 0) mbox)
  (parse-mime-structure-1 stream boundary digest pos mbox))

(defun parse-mime-structure-1 (stream boundary digest pos mbox)
  (let ((part (make-instance 'mime-part-parsed)))
    (setf (mime-part-position part) pos)
    (setf (mime-part-boundary part) boundary)
    (multiple-value-bind (headers bytes)
	(parse-headers stream mbox)
      (setf (mime-part-headers-size part) bytes)
      (incf pos bytes)
      (setf (mime-part-body-position part) pos)
      (setf (mime-part-headers part) headers)
      
      (let ((content-type (mime-get-header "content-type" part)))
	(setf (mime-part-id part) (mime-get-header "Content-Id" part))
	(setf (mime-part-description part) 
	  (mime-get-header "Content-description" part))
	(setf (mime-part-encoding part) 
	  (or (mime-get-header "Content-transfer-encoding" part)
	      "7bit"))
	
	(multiple-value-bind (type subtype params)
	    (parse-content-type content-type)
	  
	  (if* (null type)
	     then
		  (if* digest
		     then
			  (setf (mime-part-type part) "message")
			  (setf (mime-part-subtype part) "rfc822")
			  (setf (mime-part-parameters part) 
			    '(("charset" . "us-ascii")))
			  (mime-parse-message-rfc822 part stream boundary pos
						     mbox)
		     else
			  (setup-text-plain-part part stream boundary pos mbox))
	     else
		  (setf (mime-part-type part) type)
		  (setf (mime-part-subtype part) subtype)
		  (setf (mime-part-parameters part) params)
		  
		  (cond 
		   ((equalp type "multipart")
		    (mime-parse-multipart part stream boundary pos mbox))
		   ((message-rfc822-p type subtype)
		    (mime-parse-message-rfc822 part stream boundary pos mbox))
		   (t
		    (mime-parse-non-multipart part stream boundary pos mbox)))))))))

;; OK if 'string' is nil.
;; Might return nil
;; called by parse-mime-structure-1
(defun parse-content-type (string)
  (block nil
    (if (null string)
	(return))
    (let ((max (length string))
	  pos type subtype)
      (multiple-value-setq (type pos)
	(mime-get-token string 0 max))
      (if (string= type "")
	  (return))
      
      (setf pos (skip-whitespace string pos max))
      
      (if (or (>= pos max) (char/= (char string pos) #\/))
	  (return)) ;; bogus input
      
      (multiple-value-setq (subtype pos)
	(mime-get-token string (1+ pos) max))
      
      (if (string= subtype "")
	  (return)) ;; bogus input
      
      (values type subtype (parse-parameters string pos max)))))

;; called by parse-content-type.
(defun parse-parameters (string pos max)
  (let (char pairs param value)
    (while (< pos max)
      (setf pos (skip-whitespace string pos max))
      (setf char (char string pos))
      
      (if (char/= char #\;)
	  (return))
      
      (multiple-value-setq (param pos)
	(mime-get-token string (1+ pos) max))
      (setf pos (skip-whitespace string pos max))
      (if (or (>= pos max) (char/= (char string pos) #\=))
	  (return))
      (multiple-value-setq (value pos)
	(mime-get-parameter-value string (1+ pos) max))

      (push (cons param value) pairs))
    (values (nreverse pairs) pos)))
  

(defconstant *mime-tspecials*
    '(#\( #\) #\< #\> #\@ 
      #\, #\; #\: #\\ #\" 
      #\/ #\[ #\] #\? #\=))

(defun skip-whitespace (string pos max)
  (declare (optimize (speed 3))
	   (fixnum pos max))
  (while (< pos max)
    (if (not (excl::whitespace-char-p (schar string pos)))
	(return))
    (incf pos))
  pos)

(defun mime-get-parameter-value (string pos max)
  (setf pos (skip-whitespace string pos max))
  (if* (>= pos max)
     then
	  (values "" pos)
     else
	  (if (char= (char string pos) #\")
	      (mime-get-quoted-string string pos max)
	    (mime-get-token string pos max))))

(defun mime-get-token (string pos max)
  (setf pos (skip-whitespace string pos max))
  (let ((startpos pos)
	char)
    (while (< pos max)
      (setf char (char string pos))
      (if (or (char= #\space char) (member char *mime-tspecials*))
	  (return))
      (incf pos))
    (values (subseq string startpos pos) pos)))

;; Doesn't attempt to dequote
(defun mime-get-quoted-string (string pos max)
  (let ((res (make-string (- max pos)))
	(outpos 0)
	char inquote inbackslash)
    (while (< pos max)
      (setf char (char string pos))
      
      (if* (and (char= char #\") (not inbackslash))
	 then
	      (if* inquote
		 then
		      (setf (schar res outpos) char)
		      (incf outpos)
		      (incf pos)
		      (return))
	      (setf inquote t))

      (if* inbackslash
	 then
	      (setf inbackslash nil)
	 else
	      (if (char= char #\\)
		  (setf inbackslash t)))
      
      (setf (schar res outpos) char)
      (incf outpos)
      (incf pos))
    
    (values (subseq res 0 outpos) pos)))

(defun mime-dequote (string)
  (block nil
    (if (or (string= string "") (char/= (char string 0) #\"))
	(return string))
    
    (let* ((max (length string))
	   (pos 1)
	   (res (make-string max))
	   (outpos 0)
	   char inbackslash)
      
      (while (< pos max)
	(setf char (char string pos))
	
	(if (and (char= char #\") (not inbackslash))
	    (return))
	
	(if* (and (not inbackslash) (char= char #\\))
	   then
		(setf inbackslash t)
		(incf pos)
	   else
		(setf (schar res outpos) char)
		(incf outpos)
		(incf pos)
		(setf inbackslash nil)))
      
      (subseq res 0 outpos))))

(defun setup-text-plain-part (part stream boundary pos mbox)
  (setf (mime-part-type part) "text")
  (setf (mime-part-subtype part) "plain")
  (setf (mime-part-parameters part) '(("charset" . "us-ascii")))
  (mime-parse-non-multipart part stream boundary pos mbox))

(defun mime-parse-non-multipart (part stream boundary pos mbox)
  (let ((startpos pos))
    (multiple-value-bind (endpos lines eof pos)
	(read-until-boundary stream boundary pos mbox)
      
      (setf (mime-part-lines part) lines)
      (setf (mime-part-body-position part) startpos)
      (setf (mime-part-body-size part) (- endpos startpos))
      
      (values part eof pos))))

(defun mime-parse-message-rfc822 (part stream boundary pos mbox)
  (let ((startpos pos))
    (multiple-value-bind (message eof pos)
	(parse-mime-structure-1 stream boundary nil pos mbox)
      
      (setf (mime-part-message part) message)
      
      (setf (mime-part-body-position part) startpos)
      (setf (mime-part-body-size part) (- pos startpos))
      
      (values part eof pos))))
  

(defun mime-parse-multipart (part stream parent-boundary pos mbox)
  (let* ((params (mime-part-parameters part))
	 (boundary (cdr (assoc "boundary" params :test #'equalp)))
	 (startpos pos)
	 parts eof newpart)
    
    (setf (mime-part-boundary part) parent-boundary)
    
    ;; If boundary isn't specified.. try to compensate by using our
    ;; parent's boundary.
    (if (null boundary)
	(setf boundary parent-boundary)
      (setf boundary (mime-dequote boundary)))
    
    ;; Locate the first boundary.
    (multiple-value-bind (ignore1 ignore2 ignore3 newpos)
	(read-until-boundary stream boundary pos mbox)
      (declare (ignore ignore1 ignore2 ignore3))
      (setf pos newpos))
    
    (until eof
      (multiple-value-setq (newpart eof pos)
	(parse-mime-structure-1 stream boundary
				(equalp (mime-part-subtype part) "digest")
				pos mbox))
      (push newpart parts))
    
    (setf (mime-part-parts part) (nreverse parts))
    
    ;; Discard everything that follows until we reach the parent-boundary.
    (multiple-value-bind (ignore1 ignore2 eof pos)
	(read-until-boundary stream parent-boundary pos mbox)
      (declare (ignore ignore1 ignore2))
      
      (setf (mime-part-body-size part) (- pos startpos))
      
      (values part eof pos))))
;; support

;; Returns headers alist and the number of bytes read.
(defun parse-headers (stream mbox)
  (declare (optimize (speed 3) (safety 0)))
  (let ((count 0) headers colonpos name value)
    (excl::with-resource (line mime-line)
      (loop
	(let ((bytes (mime-read-line line stream mbox)))
	  (if (null bytes)
	      (return))
	  
	  (incf count bytes)

	  (mime-line-string-right-trim line)
	  (if (string= line "")
	      (return))
	
	  ;; Continuation line
	  (if* (and (excl::whitespace-char-p (char line 0)) headers)
	     then ;; yes
		  (setf (cdr (car headers)) 
		    (concatenate 'string (cdr (car headers)) " "
				 (string-left-trim *whitespace* line)))
	     else (setf colonpos (position #\: line))
		  (if (null colonpos) ;; bogus input
		      (return))
		  (setf name 
		    (string-trim *whitespace* (subseq line 0 colonpos)))
		  (let ((startpos (position-if-not #'excl::whitespace-char-p
					       line :start (1+ colonpos))))
		    (setf value 
		      (if* (null startpos)
			 then ""
			 else (subseq line startpos))))

		  (push (cons name value) headers)))))
    
    (values (nreverse headers) count)))

;; Returns: (1) position of the end of the part
;;          (2) number of lines read
;;          (3) :eof if EOF, :boundary if close delimiter was seen, else nil
;;          (4) new stream position (post boundary read)
(defun read-until-boundary (stream boundary pos mbox)
  (let ((lines 0)
	(lastpos pos)
	bytes delimiter close-delimiter)
    
    (excl::with-resource (line mime-line)
    
      (when boundary
	(setf delimiter (concatenate 'string "--" boundary))
	(setf close-delimiter (concatenate 'string delimiter "--")))
      
      (loop
	(setf bytes (mime-read-line line stream mbox))
	
	(if (or (null bytes)
		(and delimiter (prefixp delimiter line)))
	    (return))
	
	(incf pos bytes)
	
	(setf lastpos pos)
	(incf lines))
      
      (values lastpos 
	      lines
	      (cond ((null bytes)
		     :eof)
		    ((and close-delimiter (prefixp close-delimiter line))
		     :boundary)
		    (t nil))
	      pos))))

;; Returns values:
;; Number of characters read, including CR/LFs. Returns nil if EOF.
(defun mime-read-line (buffer stream mbox)
  (declare (optimize (speed 3) (safety 0)))
  (excl::with-underlying-simple-vector (buffer sbuf)
    (declare (type string sbuf))
    (let* ((pos 0)
	   (count 0)
	   (max (array-dimension buffer 0))
	   (crlf (eq (eol-convention stream) :dos))
	   char)
      (declare (fixnum pos count max))
      (while (and (< pos max) (setf char (read-char stream nil nil)))
	(incf count)
	(when (char= char #\newline)
	  (if crlf
	      (incf count)) ;; account for carriage return as well
	  (return))
	
	(setf (schar sbuf pos) char)
	(incf pos))
      
      (setf (fill-pointer buffer) pos)

      ;; Treat mbox "From " line as EOF 
      (if (and mbox (prefixp "From " buffer))
	  (setf count 0))
      
      (if (/= count 0) count))))

(defun mime-line-string-right-trim (line)
  (let ((pos (position-if-not #'excl::whitespace-char-p line :from-end t)))
    (if pos
	(setf (fill-pointer line) (1+ pos)))))

;;; body streams stuff

(defun body-stream-func (outstream instream boundary)
  (let ((delimiter (if boundary (concatenate 'string "--" boundary)))
	line)
    
    (while (setf line (read-line instream nil nil))
      (if (and delimiter (prefixp delimiter line))
	  (return))
      
      (write-line line outstream))))

(defun body-stream-func-with-count (outstream instream count)
  (declare (optimize (speed 3))
	   (fixnum count))
  (let (char)
    (dotimes (n count)
      (declare (fixnum n))
      (setf char (read-char instream nil nil))
      (if* (null char)
	 then (return)
	 else (write-char char outstream)))))


(defmacro with-part-body-stream ((sym instream part &key count) &body body)
  (if* count
     then
	  `(with-function-input-stream (,sym #'body-stream-func-with-count
					     ,instream ,count)
	     ,@body)
     else
	  `(with-function-input-stream (,sym #'body-stream-func
					     ,instream 
					     (mime-part-boundary ,part))
	     ,@body)))

;;; testing

#|
(defun test-parse-mime (file &key (pretty t))
  (with-open-file (f file)
    (let ((res     (parse-mime-structure f)))
      (if pretty
	  (pprint-rfc822-part res)
	res))))

(defun pprint-rfc822-part (thing &optional (prefix ""))
  (if (null thing)
      (error "Strange. something called pprint-rfc822-part with nil"))
  (let ((type (mime-part-type thing))
	(subtype (mime-part-subtype thing)))
    (format t "~AHEADER ([RFC-2822] header of the message)~%" prefix)
    (format t "~ATEXT   ([RFC-2822] text body of the message) ~A/~A~%" prefix type subtype)
    
    (if* (message-rfc822-p type subtype)
       then ;; XXX .. what should the new prefix be? 
	    (pprint-rfc822-part (mime-part-message thing) prefix)
     elseif (equalp type "multipart")
       then (pprint-multipart thing prefix))))

(defun pprint-multipart (thing prefix)
  (let ((id 1))
    (dolist (part (mime-part-parts thing))
      (let ((type (mime-part-type part))
	    (subtype (mime-part-subtype part)))
	(format t "~a~a      ~a/~a~%" prefix id type subtype)
	(if* (message-rfc822-p type subtype)
	   then (pprint-rfc822-part (mime-part-message part) 
				    (format nil "~a~a." prefix id))
	 elseif (equalp type "multipart")
	   then (pprint-multipart part (format nil "~a~a." prefix id)))
	(incf id)))))
|#
