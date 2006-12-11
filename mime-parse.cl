;; $Id: mime-parse.cl,v 1.2 2006/12/11 22:45:38 layer Exp $

(defpackage :net.post-office
  (:use #:lisp #:excl)
  (:export
   #:parse-mime-structure
   #:mime-dequote
   #:with-part-stream
   
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

(eval-when (compile load eval)
  (require :streamp))

;;; MIME structure parser.
;;; Ref: RFC2045/2046

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

(defmacro get-header (name headers)
  `(cdr (assoc ,name ,headers :test #'equalp)))

(defun parse-mime-structure (stream &key mbox)
  (multiple-value-bind (part stop newpos)
      (parse-mime-structure-1 stream nil nil 0 mbox)
    (declare (ignore stop))
    (values part newpos)))

;; Returns values:
;; 1) The part
;; 2) The stop reason (:eof, :close-boundary, nil (meaning regular boundary))
;; 3) The new position

;: mime-parse-message-rfc822, parse-mime-structure, mime-parse-multipart
;: 
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
      
      (let ((content-type (get-header "content-type" headers)))
	(setf (mime-part-id part) (get-header "Content-Id" headers))
	(setf (mime-part-description part) 
	  (get-header "Content-description" headers))
	(setf (mime-part-encoding part) 
	  (or (get-header "Content-transfer-encoding" headers)
	      "7bit"))
	
	(multiple-value-bind (type subtype params)
	    (parse-content-type content-type)
	  
	  (if* (null type)
	     then (if* digest
		     then (setf (mime-part-type part) "message")
			  (setf (mime-part-subtype part) "rfc822")
			  (setf (mime-part-parameters part) 
			    '(("charset" . "us-ascii")))
			  (mime-parse-message-rfc822 part stream boundary pos
						     mbox)
		     else (setup-text-plain-part part stream boundary pos 
						 mbox))
	     else (setf (mime-part-type part) type)
		  (setf (mime-part-subtype part) subtype)
		  (setf (mime-part-parameters part) params)
		  
		  (cond 
		   ((equalp type "multipart")
		    (mime-parse-multipart part stream boundary pos 
					  mbox))
		   ((message-rfc822-p type subtype)
		    (mime-parse-message-rfc822 part stream boundary pos 
					       mbox))
		   (t
		    (mime-parse-non-multipart part stream boundary pos 
					      mbox)))))))))

;: skip-whitespace, parse-header-line, parse-headers
;: 
(defmacro whitespace-char-p (char)
  (let ((c (gensym)))
    `(let ((,c ,char))
       (or (char= ,c #\space) (char= ,c #\tab) (char= ,c #\newline)))))

;; OK if 'string' is nil.
;; Might return nil
;; called by parse-mime-structure-1
;: parse-mime-structure-1
;: 
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
      
      (if (or (>= pos max) (char/= (schar string pos) #\/))
	  (return)) ;; bogus input
      
      (multiple-value-setq (subtype pos)
	(mime-get-token string (1+ pos) max))
      
      (if (string= subtype "")
	  (return)) ;; bogus input
      
      (values type subtype (parse-parameters string pos max)))))





;; called by parse-content-type.
;: parse-content-type
;: 
(defun parse-parameters (string pos max)
  (let (char pairs param value)
    (while (< pos max)
      (setf pos (skip-whitespace string pos max))
      (setf char (schar string pos))
      
      (if (char/= char #\;)
	  (return))
      
      (multiple-value-setq (param pos)
	(mime-get-token string (1+ pos) max))
      (setf pos (skip-whitespace string pos max))
      (if (or (>= pos max) (char/= (schar string pos) #\=))
	  (return))
      (multiple-value-setq (value pos)
	(mime-get-parameter-value string (1+ pos) max))

      (push (cons param value) pairs))
    (values (nreverse pairs) pos)))
  

(defconstant *mime-tspecials*
    '(#\( #\) #\< #\> #\@ 
      #\, #\; #\: #\\ #\" 
      #\/ #\[ #\] #\? #\=))

;: parse-content-type, parse-parameters, mime-get-parameter-value
;: mime-get-token, blank-line-p, parse-header-line
;: 
(defun skip-whitespace (string pos max)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum pos max))
  (while (< pos max)
    (if (not (whitespace-char-p (schar string pos)))
	(return))
    (incf pos))
  pos)

;: parse-parameters
;: 
(defun mime-get-parameter-value (string pos max)
  (setf pos (skip-whitespace string pos max))
  (if* (>= pos max)
     then (values "" pos)
     else (if (char= (schar string pos) #\")
	      (mime-get-quoted-string string pos max)
	    (mime-get-token string pos max))))

;: parse-content-type, parse-parameters, mime-get-parameter-value
;: 
(defun mime-get-token (string pos max)
  (setf pos (skip-whitespace string pos max))
  (let ((startpos pos)
	char)
    (while (< pos max)
      (setf char (schar string pos))
      (if (or (char= #\space char) (member char *mime-tspecials*))
	  (return))
      (incf pos))
    (values (subseq string startpos pos) pos)))

;; Doesn't attempt to dequote
;: mime-get-parameter-value
;: 
(defun mime-get-quoted-string (string pos max)
  (let ((res (make-string (- max pos)))
	(outpos 0)
	char inquote inbackslash)
    (while (< pos max)
      (setf char (schar string pos))
      
      (when (and (char= char #\") (not inbackslash))
	(if* inquote
	   then	(setf (schar res outpos) char)
		(incf outpos)
		(incf pos)
		(return))
	(setf inquote t))
      
      (if* inbackslash
	 then (setf inbackslash nil)
	 else (if (char= char #\\)
		  (setf inbackslash t)))
      
      (setf (schar res outpos) char)
      (incf outpos)
      (incf pos))
    
    (values (subseq res 0 outpos) pos)))

;; mime-parse-multipart
;: 
(defun mime-dequote (string)
  (block nil
    (if (or (string= string "") (char/= (schar string 0) #\"))
	(return string))
    
    (let* ((max (length string))
	   (pos 1)
	   (res (make-string max))
	   (outpos 0)
	   char inbackslash)
      
      (while (< pos max)
	(setf char (schar string pos))
	
	(if (and (char= char #\") (not inbackslash))
	    (return))
	
	(if* (and (not inbackslash) (char= char #\\))
	   then	(setf inbackslash t)
		(incf pos)
	   else	(setf (schar res outpos) char)
		(incf outpos)
		(incf pos)
		(setf inbackslash nil)))
      
      (subseq res 0 outpos))))

;: parse-mime-structure-1
;: 
(defun setup-text-plain-part (part stream boundary pos mbox)
  (setf (mime-part-type part) "text")
  (setf (mime-part-subtype part) "plain")
  (setf (mime-part-parameters part) '(("charset" . "us-ascii")))
  (mime-parse-non-multipart part stream boundary pos mbox))

;: setup-text-plain-part, parse-mime-structure-1
;: 
(defun mime-parse-non-multipart (part stream boundary pos mbox)
  (let ((startpos pos))
    (multiple-value-bind (size lines eof pos)
	(read-until-boundary stream boundary pos mbox)
      
      (setf (mime-part-lines part) lines)
      (setf (mime-part-body-position part) startpos)
      (setf (mime-part-body-size part) size)
      
      (values part eof pos))))

;: parse-mime-structure-1
;: 
(defun mime-parse-message-rfc822 (part stream boundary pos mbox)
  (let ((startpos pos))
    (multiple-value-bind (message eof pos)
	(parse-mime-structure-1 stream boundary nil pos mbox)
      
      (setf (mime-part-message part) message)
      
      (setf (mime-part-body-position part) startpos)
      (setf (mime-part-body-size part) (- pos startpos))
      
      (values part eof pos))))
  

;: parse-mime-structure-1
;: 
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

(defconstant *whitespace* '(#\space #\tab #\return #\newline))


;: parse-headers
;: 
(defun blank-line-p (line len)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum len))
  (= len (skip-whitespace line 0 len)))

;: parse-headers
;: 
(defun parse-header-line (line len)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pos 0)
	colonpos
	spacepos)
    (declare (fixnum len pos spacepos))
    
    (while (< pos len)
      (let ((char (schar line pos))) 
	(when (char= char #\:)
	  (setf colonpos pos)
	  (return))
	
	(if (and (null spacepos) (whitespace-char-p char))
	    (setf spacepos pos)))
      
      (incf pos))
 
    (if (null colonpos) ;; bogus header line
	(return-from parse-header-line))
    
    (if (null spacepos)
	(setf spacepos colonpos))
    
    (if (= 0 spacepos) ;; bogus header line (no name)
	(return-from parse-header-line))
    
    (values (subseq line 0 spacepos)
	    (subseq line (skip-whitespace line (1+ colonpos) len) len))))

;; Returns offset of end of line in buffer.  Or nil if EOF
;; Second value is the number of characters read (including EOL chars)
;; This is slower than a read-line call, but in the long run can
;; lead to big savings in gc time.
;: parse-headers, read-until-boundary, collect-message-data-from-mbox
;: 
(defun mime-read-line (stream buffer)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pos 0)
	(end (length buffer))
	(count 0)
	char)
    (declare (fixnum pos end count))
    
    (while (and (< pos end) (setf char (read-char stream nil nil)))
      (incf count)
      (if (char= char #\newline)
	  (return))
      (setf (schar buffer pos) char)
      (incf pos))
    
    (if* (= count 0)
       then nil ;; EOF
       else ;; Check for CR/LF combo
	    (if (and (> pos 0) (char= (schar buffer (1- pos)) #\return))
		(decf pos))
	    
	    (values pos count))))
	    

;; Returns:
;; 1) headers alist
;; 2) # of characters composing the header and terminator.
;:
;: parse-mime-structure-1
;: 
(defun parse-headers (stream mbox)
  (declare (optimize (speed 3) (safety 0)))
  (let ((count 0)
	(line (make-array 1024 :element-type 'character))
	headers 
	lastcons
	current)
    (declare (fixnum count)
	     (dynamic-extent line))

    (loop
      (multiple-value-bind (end bytes)
	  (mime-read-line stream line)
	(declare (fixnum end))
	
	(if (or (null end)
		(and mbox (my-prefixp "From " line end)))
	    (return))

	(incf count bytes)
	
	(if (blank-line-p line end)
	    (return))
	
	(if* (whitespace-char-p (schar line 0))
	   then ;; Continuation line
		(if (null current)
		    (return)) 
	      
		(let ((newcons (cons (subseq line 0 end) nil)))
		  (setf (cdr lastcons) newcons)
		  (setf lastcons newcons))
	      
	   else ;; Fresh header line
		(multiple-value-bind (name value)
		    (parse-header-line line end)
		  (if (null name)
		      (return)) 
		
		  (setf lastcons (cons value nil))
		  (setf current (cons name lastcons))
		  (push current headers)))))

    ;; Finalize strings.
    (dolist (header headers)
      (setf (cdr header) (coalesce-header header)))
    
    (values (nreverse headers) count)))

;: parse-headers
;: 
(defun coalesce-header (header)
  (declare (optimize (speed 3) (safety 0)))
  (let ((stringlist (cdr header)))
    (if* (= (length stringlist) 1)
       then (first stringlist)
       else (let ((len 0))
	      (declare (fixnum len))
	      (dolist (string stringlist)
		(incf len (1+ (the fixnum (length string)))))
	      (decf len)
	      (let ((res (make-string len))
		    (pos 0)
		    (first t))
		(declare (fixnum pos))
		(dolist (string stringlist)
		  (if* first
		     then (setf first nil)
		     else (setf (schar res pos) #\newline)
			  (incf pos))
		  (dotimes (n (length string))
		    (declare (fixnum n))
		    (setf (schar res pos) (schar string n))
		    (incf pos)))
		res)))))

;; Returns: (1) size of part 
;;          (2) number of lines read
;;          (3) stop reason (:eof, :close-boundary, or nil (meaning regular
;;                                                          boundary)
;;          (4) new stream position (post boundary read)
;: mime-parse-multipart, mime-parse-non-multipart
;: 
(defun read-until-boundary (stream boundary pos mbox)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum pos))
  (if* (and (null boundary) (null mbox))
     then 
	  (multiple-value-bind (lines count)
	      (count-lines-to-eof stream)
	    (declare (fixnum count))
	    (values count lines :eof (+ pos count)))
     else 
	  (let ((line (make-array 16000 :element-type 'character))
		(size 0)
		(lines 0)
		(stop-reason :eof)
		delimiter close-delimiter)
	    (declare (dynamic-extent line)
		     (fixnum count size lines))
	    
	    (when boundary
	      (setf delimiter (concatenate 'string "--" boundary))
	      (setf close-delimiter (concatenate 'string delimiter "--")))
	    
	    (loop
	      (multiple-value-bind (end bytes)
		  (mime-read-line stream line)
		(declare (fixnum end bytes))
		
		(if (or (null end)
			(and mbox (my-prefixp "From " line end)))
		    (return))
		
		(incf pos bytes)
		
		(when (my-prefixp delimiter line end)
		  (if* (my-prefixp close-delimiter line end)
		     then (setf stop-reason :close-boundary)
		     else (setf stop-reason nil))
		  (return))
		
		(incf size bytes)
		(incf lines))) 
	    
	    (values size lines stop-reason pos))))

;; Returns:
;; 1) number of lines
;; 2) number of bytes read
;: read-until-boundary
;: 
(defun count-lines-to-eof (stream)
  (declare (optimize (speed 3) (safety 0)))
  (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8)))
	(lines 0)
	(pos 0)
	(lastbyte -1)
	(count 0)
	end)
    (declare (dynamic-extent buffer)
	     (fixnum lines pos end lastbyte count))
    ;; count 10's
    ;; XXX: The count will be off if the file has CR/LF convention and
    ;; there are bare LFs.  
    (loop
      (setf end (read-vector buffer stream))
      (incf count end)
      
      (if (= end 0)
	  (return))
      
      (while (< pos end)
	(if (= (aref buffer pos) 10)
	    (incf lines))
	(incf pos))
      
      (setf lastbyte (aref buffer (1- pos))))
    
    ;; Count last partial line.
    (if (and (> lastbyte 0) (/= lastbyte 10))
	(incf lines))
    
    (values lines count)))

(defun my-prefixp (prefix string &optional end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((lenprefix (length prefix))
	(end (or end (length string))))
    (declare (fixnum lenprefix lenstring end))
    (when (>= end lenprefix)
      (dotimes (n lenprefix)
	(declare (fixnum n))
	(if (char/= (schar prefix n) (schar string n))
	    (return-from my-prefixp)))
      t)))

;;; misc

(defun stream-to-stream-copy (outstream instream count)
  (declare (optimize (speed 3))
	   (fixnum count))
  (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buf))
    (while (> count 0)
      (let ((got (read-sequence buf instream :end (min count 4096))))
	(declare (fixnum got))
	(if (zerop got)
	    (error "Unexpected EOF while reading from ~a" instream))
	(write-sequence buf outstream :end got)
	(decf count got)))))

;; 'instream' must be positioned appropriately by the caller beforehand.
(defmacro with-part-stream ((sym part instream &key (header t)) &body body)
  (let ((p (gensym))
	(stream (gensym))
	(count (gensym)))
    `(let* ((,p ,part)
	    (,stream ,instream)
	    (,count (mime-part-body-size ,p)))
       (if ,header
	   (incf ,count (mime-part-headers-size ,p)))
       (excl:with-function-input-stream 
	   (,sym #'stream-to-stream-copy ,stream ,count)
	 ,@body))))
	 


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
