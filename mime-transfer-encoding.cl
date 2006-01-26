;; $Id: mime-transfer-encoding.cl,v 1.1 2006/01/26 23:53:27 dancy Exp $

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

;; Temporary until this change is made in excl.cl.
(defparameter excl::*to-base64*
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;; This should be added to the excl package.
(defun base64-encode-stream (instream outstream &key (wrap-at 72))
  (declare (optimize (speed 3)))
  ;; inbuf _must_ be a size which is a multiple of three.  The 
  ;; encoding code depends on it.  outbuf must be 4/3rds bigger than
  ;; inbuf.
  (let ((inbuf (make-array #.(* 3 4096) :element-type '(unsigned-byte 8)))
	(outbuf (make-array #.(* 4 4096) :element-type 'character))
	remaining end outpos inpos value)
    (declare (dynamic-extent inbuf outbuf)
	     (fixnum remaining outpos end inpos value))
	    
    (macrolet ((outchar (char)
		 `(progn
		    (setf (schar outbuf outpos) ,char)
		    (incf outpos)))
	       (outchar-base64 (x)
		 `(outchar (schar excl::*to-base64* (logand ,x 63)))))
      
      (flet ((read-full-vector (buf stream)
	       (let ((pos 0)
		     (max (length buf))
		     newpos)
		 (declare (fixnum pos max got newpos))
		 (while (< pos max)
		   (setf newpos (read-vector buf stream :start pos))
		   (if* (= newpos pos)
		      then (return))
		   (setf pos newpos))
		 pos)))

	(while (/= 0 (setf end (read-full-vector inbuf instream)))
	  (setf remaining end)
	  (setf inpos 0)
	  (setf outpos 0)
	  (while (> remaining 0)
	    (if* (>= remaining 3)
	       then (setf value (logior (ash (aref inbuf inpos) 16)
					(ash (aref inbuf (+ 1 inpos)) 8)
					(aref inbuf (+ 2 inpos))))
		    (incf inpos 3)
		    (decf remaining 3)
		    (outchar-base64 (ash value -18))
		    (outchar-base64 (ash value -12))
		    (outchar-base64 (ash value -6))
		    (outchar-base64 value)
	     elseif (= remaining 2)
	       then (setf value (logior (ash (aref inbuf inpos) 16)
					(ash (aref inbuf (+ 1 inpos)) 8)))
		    (incf inpos 2)
		    (decf remaining 2)
		    (outchar-base64 (ash value -18))
		    (outchar-base64 (ash value -12))
		    (outchar-base64 (ash value -6))
		    (outchar #\=)
	       else (setf value (ash (aref inbuf inpos) 16))
		    (incf inpos)
		    (decf remaining)
		    (outchar-base64 (ash value -18))
		    (outchar-base64 (ash value -12))
		    (outchar #\=)
		    (outchar #\=)))
		
	  ;; generate output.
	  (if* (null wrap-at)
	     then (write-string outbuf outstream :end outpos)
	     else (setf inpos 0)
		  (while (< inpos outpos)
		    (let ((len (min (- outpos inpos) wrap-at)))
		      (write-string outbuf outstream 
				    :start inpos
				    :end (+ inpos len))
		      (incf inpos len)
		      (write-char #\newline outstream)))))))))

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
