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
;; $Id: mime-transfer-encoding.cl,v 1.11 2007/06/01 16:24:40 dancy Exp $

(defpackage :net.post-office
  (:use #:lisp #:excl)
  (:import-from #:excl #:base64-encode-stream
		#+(or (version= 7 0)
		      (version= 8 0)
		      (version>= 8 1 pre-beta 5))
		#:base64-decode-stream)
  (:export
   #:base64-encode-stream
   #:base64-decode-stream
   #:qp-encode-stream
   #:qp-decode-stream
   #:qp-decode-usb8
   #:qp-decode-string
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
(eval-when (compile)
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
	arr)))

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

;; 'out' should be at least the size of 'in'.  If it is nil,
;; a usb8 array will be allocated and used.   It is okay if 'out' is the
;; same buffer as 'in'.
;; Returns:
;;  1) the supplied or allocated array
;;  2) the just past the last byte populated in the array.
(defun qp-decode-usb8 (in out &key (start1 0) (end1 (length in))
				   (start2 0) end2)
  (declare (optimize (speed 3))
	   ((simple-array (unsigned-byte 8) (*)) in out)
	   (fixnum start1 end1 start2 end2))
  
  (if (null out)
      (setf out (make-array (length in) :element-type '(unsigned-byte 8))))
  
  (if (null end2)
      (setf end2 (length out)))
  
  (let ((count (- end1 start1)))
    (declare (fixnum count))
    
    (if (< count 0)
	(error "start1 must be less than end1"))
    
    (if (> start2 end2)
	(error "start2 must be less than end2"))
    
    (if (< (the fixnum (- end2 start2)) count)
	(error "Not enough room in output array"))
    
    (macrolet ((unread (byte)
		 (declare (ignore byte))
		 `(decf start1))
	       (get-byte (&key eof-value)
		 `(if* (>= start1 end1)
		     then ,eof-value
		     else (prog1 (aref in start1)
			    (incf start1))))
	       (out (byte)
		 `(prog1 (setf (aref out start2) ,byte)
		    (incf start2)))
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
	
	(values out start2)))))

(defun qp-decode-string (string &key (start 0) (end (length string))
				     (return :string)
				     (external-format :default))
  (multiple-value-bind (vec len)
      (string-to-octets string :start start :end end :null-terminate nil
			:external-format :latin1)
    (multiple-value-setq (vec len)
      (qp-decode-usb8 vec vec :end1 len))
    (ecase return
      (:string
       (octets-to-string vec :end len :external-format external-format))
      (:usb8
       (subseq vec 0 len)))))

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
   #+(or (version= 7 0)
	 (version= 8 0)
	 (version>= 8 1 pre-beta 5))
   ((equalp encoding "base64")
    (excl:base64-decode-stream instream outstream :count count :error-p nil))
   (t
    ;; defined in mime-parse.cl
    (stream-to-stream-copy outstream instream count))))
    

