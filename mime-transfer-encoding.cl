;; $Id: mime-transfer-encoding.cl,v 1.6 2007/01/05 21:31:25 dancy Exp $

(defpackage :net.post-office
  (:use #:lisp #:excl)
  (:import-from #:excl #:base64-encode-stream
		#+(or (version= 8 0)
		      (version>= 8 1 pre-beta 4))
		#:base64-decode-stream)
  (:export
   #:base64-encode-stream
   #:base64-decode-stream
   #:qp-encode-stream
   #:qp-decode-stream
   #:with-decoded-part-body-stream))

(in-package :net.post-office)

;;; Supported transfer encodings

;; encoders

(defun raw-encode-stream (instream outstream)
  (declare (optimize (speed 3) (safety 0)))
  (let ((buf (make-array 4096 :element-type '(unsigned-byte 8)))
	got)
    (declare (dynamic-extent buf)
	     (fixnum got))
    (while (/= 0 (setf got (read-vector buf instream)))
      (write-vector buf outstream :end got))))

(defconstant *qp-hex-digits* "0123456789ABCDEF")

;; wrap-at is not a hard limit but more of a suggestion.  it may be
;; late by by 3 characters.
(defun qp-encode-stream (instream outstream &key (wrap-at 72))
  (declare (optimize (speed 3)))
  (let ((prev 0)
	(outcol 0)
	byte)
    (declare (fixnum byte prev))
	    
    (macrolet ((whitespace (x) 
		 (let ((xx (gensym)))
		   `(let ((,xx ,x))
		      (or (= ,xx 9) (= ,xx 32))))))
	      
      (labels ((check-linewrap ()
		 (if* (and wrap-at (>= outcol wrap-at))
		    then (format outstream "=~%" outstream)
			 (setf outcol 0)))
	       (check-deferred ()
		 (if* (and (= prev 13) (/= byte 10))
		    then ;; previous byte was bare CR.  Handle
			 (check-linewrap)
			 (write-string "=0D" outstream)
			 (incf outcol 3))
		       
		 (if* (whitespace prev)
		    then (if* (or (= byte 0) (= byte 10) (= byte 13))
			    then ;; EOF, EOL, probable EOL.  Encode.
				 (check-linewrap)
				 (format outstream "=20")
				 (incf outcol 3)
			    else ;; Safe to print deferred whitespace
				 (check-linewrap)
				 (write-char (code-char prev) outstream)
				 (incf outcol 1)))))
		
	(while (setf byte (read-byte instream nil nil))
	  (check-deferred)
		  
	  (if* (or (and (>= byte 33) (<= byte 60))
		   (and (>= byte 62) (<= byte 126)))
	     then (check-linewrap)
		  (write-char (code-char byte) outstream)
		  (incf outcol)
	   elseif (or (= byte 13) (whitespace byte))
	     thenret ;; defer handling
	   elseif (= byte 10) ;; LF
	     then (write-char #\newline outstream)
		  (setf outcol 0)
	     else (check-linewrap)
		  (format outstream "=~c~c"
			  (schar *qp-hex-digits* 
				 (ash byte -4))
			  (schar *qp-hex-digits*
				 (logand byte #xf)))
		  (incf outcol 3))
		  
	  (setf prev byte))
		
	;; Handle final deferred data
	(setf byte 0)
	(check-deferred)))))

;; Decoding stuff


;; Used by qp-decode-stream
(defconstant *qp-digit-values*
    #.(let ((arr (make-array 257 :element-type 'fixnum)))
	(dotimes (n 256)
	  (setf (aref arr n)
	    (if* (<= (char-code #\0) n (char-code #\9))
	       then (- n (char-code #\0))
	     elseif (<= (char-code #\A) n (char-code #\F))
	       then (- n (- (char-code #\A) 10))
	       else -1)))
	(setf (aref arr 256) -2)
	arr))


(defun qp-decode-stream (instream outstream &key count)
  (declare (optimize (speed 3)))
  
  (let (unread-buf)
  
    (macrolet ((unread (byte)
		 `(progn
		    (setf unread-buf ,byte)
		    (if count
			(incf count))))
	       (get-byte (&key eof-value)
		 `(block get-byte
		    (if* count
		       then (if (zerop count)
				(return-from get-byte ,eof-value))
			    (decf count))
		    (if* unread-buf
		       then (prog1 unread-buf
			      (setf unread-buf nil))
		       else (read-byte instream nil ,eof-value))))
	       (out (byte)
		 `(write-byte ,byte outstream))
	       (eol-p (byte)
		 `(or (eq ,byte 10) (eq ,byte 13))))
	       
      (let (byte)
	(while (setf byte (get-byte))
	  (if* (eq byte #.(char-code #\=))
	     then (let ((nextbyte (get-byte)))
		    (if* (null nextbyte)
		       then ;; stray equal sign.  just dump and terminate.
			    (out byte)
			    (return))
		    (if* (eol-p nextbyte)
		       then ;; soft line break.  
			    (if (eq nextbyte 13) ;; CR
				(setf nextbyte (get-byte)))
			    (if (not (eq nextbyte 10)) ;; LF
				(unread nextbyte))
		       else ;; =XY encoding
			    (let* ((byte3 (get-byte :eof-value 256))
				   (high (aref *qp-digit-values* nextbyte))
				   (low (aref *qp-digit-values* byte3))
				   (value (logior (the fixnum (ash high 4)) low)))
			      (declare (fixnum byte3 high low value))
			      (if* (< value 0)
				 then ;; Invalid or truncated encoding. just dump it.
				      (out byte)
				      (out nextbyte)
				      (if* (eq low -2) ;; EOF
					 then (return)
					 else (out byte3))
				 else (out value)))))
	     else (out byte)))
	
	t))))

;; 'instream' must be positioned at the beginning of the part body 
;; by the caller beforehand.
(defmacro with-decoded-part-body-stream ((sym part instream) &body body)
  (let ((p (gensym))
	(encoding (gensym))
	(count (gensym)))
    `(let* ((,p ,part)
	    (,encoding (mime-part-encoding ,p))
	    (,count (mime-part-body-size ,p)))
       (excl:with-function-input-stream (,sym #'mime-decode-transfer-encoding
					      ,instream
					      ,encoding
					      ,count)
	 ,@body))))
					  
(defun mime-decode-transfer-encoding (outstream instream encoding count)
  (cond
   ((equalp encoding "quoted-printable")
    (qp-decode-stream instream outstream :count count))
   ((equalp encoding "base64")
    (excl:base64-decode-stream instream outstream :count count :error-p nil))
   (t
    ;; defined in mime-parse.cl
    (stream-to-stream-copy outstream instream count))))
    

