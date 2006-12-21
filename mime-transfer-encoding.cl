;; $Id: mime-transfer-encoding.cl,v 1.5 2006/12/21 18:22:15 layer Exp $

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

;; 'instream' must be positioned at the beginning of the part body 
;; by the caller beforehand.
(defmacro with-decoded-part-body-stream ((sym part instream) &body body)
  (let ((bodystream (gensym))
	(p (gensym))
	(encoding (gensym)))
    `(let* ((,p ,part)
	    (,encoding (mime-part-encoding ,p)))
       (with-part-stream (,bodystream ,p ,instream :header nil)
	 (excl:with-function-input-stream (,sym #'mime-decode-transfer-encoding
						,bodystream
						,encoding)
	   ,@body)))))
					  
(defun mime-decode-transfer-encoding (outstream instream encoding)
  (funcall 
   (cond
    ((equalp encoding "quoted-printable")
     #'qp-decode-stream)
    ((equalp encoding "base64")
     #'excl::base64-decode-stream)
    (t
     #'sys:copy-file))
   instream outstream))

(defun qp-decode-stream (instream outstream)
  (declare (optimize (speed 3)))
  (let ((linebuf (make-array 4096 :element-type 'character))
	pos char char2 softlinebreak)
    (declare (dynamic-extent linebuf)
	     (fixnum pos))
    
    (loop
      (multiple-value-bind (line dummy max)
	  (simple-stream-read-line instream nil nil linebuf)
	(declare (ignore dummy)
		 (fixnum max)
		 (simple-string line))
	(if (null line)
	    (return))
	
	(if (null max)
	    (setf max (length line)))
	
	(setf pos 0)
	
	(macrolet ((getchar () 
		     `(if (< pos max)
			  (prog1 (schar line pos) (incf pos))))
		   (decode-dig (char)
		     `(the (integer 0 256) (decode-qp-hex-digit ,char))))
		   
	  (while (< pos max)
	    (setf char (getchar))
	    
	    (if* (eq char #\=)
	       then ;; Check for soft line break.
		    (if* (= pos max)
		       then (setf softlinebreak t)
		       else (setf char (getchar))
			    (setf char2 (getchar))
			    
			    (let ((value (logior 
					  (ash (decode-dig char) 4)
					  (decode-dig char2))))
			      (if* (< value 256)
				 then (write-byte value outstream)
				 else ;; We got some bogus input.  
				      ;; Leave it untouched
				      (write-char #\= outstream)
				      (if char
					  (write-char char outstream))
				      (if char2
					  (write-char char2 outstream)))))
	       else (write-char char outstream)))
	  ;; outside 'while' loop.
	
	  (if* softlinebreak
	     then (setf softlinebreak nil)
	     else (write-char #\newline outstream)))))))


(defun decode-qp-hex-digit (char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if* (char<= #\0 char #\9)
     then (- (the (integer 0 255) (char-code char)) #.(char-code #\0))
   elseif (char<= #\A char #\F)
     then (- (the (integer 0 255) (char-code char)) #.(- (char-code #\A) 10))
     else 256))

