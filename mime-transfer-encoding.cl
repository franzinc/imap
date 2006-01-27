;; $Id: mime-transfer-encoding.cl,v 1.2 2006/01/27 01:28:58 layer Exp $

(defpackage :net.post-office
  (:use #:lisp #:excl)
  (:export
   #:base64-encode-stream
   #:qp-encode-stream))

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


#|

(defun mime-decode-transfer-encoding (outstream instream encoding)
  (cond
   ((equalp encoding "quoted-printable")
    (decode-quoted-printable outstream instream))
   (t
    (decode-unmodified outstream instream))))

(defmacro with-decoded-transfer-encoding-stream ((sym instream encoding) &body body)
  `(with-function-input-stream (,sym #'mime-decode-transfer-encoding ,instream 
				     ,encoding)
     ,@body))

(defun decoded-part-body-stream-func (outstream instream part)
  (with-part-body-stream (part-body-stream instream part)
    (mime-decode-transfer-encoding outstream part-body-stream 
				   (mime-part-encoding part))))

(defmacro with-decoded-part-body-stream ((sym instream part) &body body)
  `(with-function-input-stream (,sym #'decoded-part-body-stream-func
				     ,instream ,part)
     ,@body))


;; The decoders

(defun decode-unmodified (outstream instream)
  (let (line)
    (while (setf line (read-line instream nil nil))
      (write-line line outstream))))

(defun decode-quoted-printable (outstream instream)
  (declare (optimize (speed 3)))
  (let (line max pos char char2 softlinebreak)
    (while (setf line (read-line instream nil nil))
      (setf max (length line))
      (setf pos 0)
      
      (macrolet ((getchar () 
		   `(if* (>= pos max)
		       then (setf softlinebreak t)
			    (return)
		       else (prog1 (schar line pos) (incf pos)))))
	
	(while (< pos max)
	  (setf char (getchar))
	  
	  (if* (char= char #\=)
	     then ;; If EOL occurs during the attempt to get the next
		  ;; two chars, it will be treated as a soft line break.
		  (setf char (getchar))
		  (setf char2 (getchar))
		  
		  (let ((value (logior 
				(ash (or (position char *qp-hex-digits*) -1) 4)
				(or (position char2 *qp-hex-digits*) -1))))
		    (if* (> value -1)
		       then
			    (write-byte value outstream)
		       else
			    ;; We got some bogus input.  Leave it untouched
			    (write-char #\= outstream)
			    (write-char char outstream)
			    (write-char char2 outstream)))
	     else (write-char char outstream)))
	
	(if* softlinebreak
	   then (setf softlinebreak nil)
	   else (write-char #\newline outstream))))))
|#
