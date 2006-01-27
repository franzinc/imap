;; $Id: mime.cl,v 1.2 2006/01/27 01:22:47 layer Exp $

(defpackage :net.post-office
  (:use #:lisp #:excl)
  (:export
   ;; functions/methods
   #:make-mime-part
   #:mime-part-writer
   #:mime-part-p
   #:mime-part-constructed-p
   
   ;; macros
   #:mime-get-header
   #:with-mime-part-constructed-stream
   
   ;; slot accessors
   #:mime-part-type
   #:mime-part-subtype
   #:mime-part-parameters
   #:mime-part-id
   #:mime-part-description
   #:mime-part-encoding
   #:mime-part-headers
   #:mime-part-parts
   #:mime-part-boundary))

(provide :mime)

(in-package :net.post-office)

(eval-when (compile load eval)
  (require :osi)
  (require :regexp2))

(defclass mime-part ()
  (
   (type :accessor mime-part-type :initform nil)
   (subtype :accessor mime-part-subtype :initform nil)
   (parameters :accessor mime-part-parameters :initform nil) ;; alist
   (id :accessor mime-part-id :initform nil)
   (description :accessor mime-part-description :initform nil)
   (encoding :accessor mime-part-encoding :initform nil)
   (headers ;; parsed headers alist
    :accessor mime-part-headers :initform nil)
   (parts ;; list of subparts (for multipart types)
    :accessor mime-part-parts :initform nil)
   (boundary :accessor mime-part-boundary :initform nil)))

(defclass mime-part-constructed (mime-part)
  (
   (source-type :accessor source-type)
   (source :accessor source)
   (disposition :accessor disposition :initform nil)
   (disposition-name :accessor disposition-name :initform nil)))

(defmacro mime-get-header (header part)
  `(cdr (assoc ,header (mime-part-headers ,part) :test #'equalp)))

(defun message-rfc822-p (type subtype)
  (and (equalp type "message") (equalp subtype "rfc822")))

(defun multipart-p (part)
  (equalp (mime-part-type part) "multipart"))

(defun multipart-mixed-p (part)
  (and (equalp (mime-part-type part) "multipart")
       (equalp (mime-part-subtype part) "mixed")))

(defun mime-part-p (thing)
  (typep thing 'mime-part))

(defun mime-part-constructed-p (thing)
  (typep thing 'mime-part-constructed))

(defun generate-boundary ()
  (declare (optimize (speed 3)))
  (let ((hex "01234567890abcdef"))
    (with-output-to-string (s)
      (write-string "----------_" s)
      (dotimes (n 32)
	(write-char (schar hex (random 16)) s)))))

(defun make-mime-part (&key content-type encoding headers 
			    (attachmentp nil attachmentp-supplied)
			    name text (start 0) end file
			    subparts (external-format :default)
			    parameters charset id description)
  (let ((part (make-instance 'mime-part-constructed))
	type subtype multipart textp filepath)
    
    (if* (and text file)
       then (error "Only one of :text or :file may be specified"))
    
    (if* (and text (null end))
       then (setf end (length text)))

    (when file
      (if* (streamp file)
	 then (setf filepath (ignore-errors (namestring file)))
	 else (setf filepath file)))
    
    (when (null content-type)
      (if* filepath
	 then (setf content-type (lookup-mime-type filepath)))
      
      (when (null content-type)
	(setf content-type
	  (if* subparts 
	     then "multipart/mixed"
	   elseif file
	     then "application/octet-stream"
	   elseif (and text (stringp text))
	     then "text/plain"
	     else "application/octet-stream"))))
      
    (let ((pos (position #\/ content-type)))
      (if (null pos)
	  (error "Invalid content-type: ~s" content-type))
      
      (setf type (subseq content-type 0 pos))
      (setf subtype (subseq content-type (1+ pos))))
    
    (setf multipart (equalp type "multipart"))
    (setf textp (or (equalp type "text") (message-rfc822-p type subtype)))

    (if* (and subparts (not multipart))
       then (error "subparts may not be specified for non-multipart parts"))
    
    (if* (and (not multipart) (null text) (null file))
       then (error "One of :text or :file must be specified"))
    
    (if* (and (null charset) textp)
       then (setf charset 
	      (or 
	       (guess-charset-from-ef (find-external-format external-format))
	       "us-ascii")))
    
    (when (and (not multipart) (null encoding))
      (if* textp
	 then (if* (equalp charset "us-ascii")
		 then (setf encoding "7bit")
		 else (setf encoding "quoted-printable"))
	 else (setf encoding "base64")))

    (setf (mime-part-type part) type)
    (setf (mime-part-subtype part) subtype)
    (setf (mime-part-parameters part) parameters)
    (if* charset
       then (push (cons "charset" charset) (mime-part-parameters part)))
    (setf (mime-part-encoding part) encoding)
    (setf (mime-part-id part) id)
    (setf (mime-part-description part) description)
    (setf (mime-part-headers part) headers)
    
    (if* file
       then (setf (source part) file)
	    (if* (streamp file)
	       then (setf (source-type part) :stream)
	       else (with-open-file (f file)) ;; make sure we can read it.
		    (setf (source-type part) :file))
	    (if* (not attachmentp-supplied)
	       then (setf attachmentp t))
       else (setf (source-type part) :usb8)
	    (setf (source part) 
	      (if* (stringp text)
		 then (string-to-octets text :null-terminate nil
					:external-format external-format)
		 else (subseq text start end))))

    (if* (and (not textp) (not attachmentp) (not multipart))
       then (setf (disposition part) "inline"))

    (when attachmentp
      (setf (disposition part) "attachment")
      (if* (and (null name) file)
	 then (setf name (excl.osi:basename filepath))))

    (if* name
       then (setf (disposition-name part) name))
  
    (if* multipart
       then (let ((boundary (generate-boundary)))
	      (setf (mime-part-boundary part) boundary)
	      (push (cons "boundary" boundary) 
		    (mime-part-parameters part))))
    
    (setf (mime-part-parts part) subparts)
    
    part))

(defparameter *ef-nick-to-mime-charset*
    '((:ascii . "us-ascii")
      (:iso-2022-jp . "iso-2022-jp")
      (:koi8-r . "koi8-r")
      (:shiftjis . "shift_jis")
      (:euc . "euc-jp")
      (:gb2312 . "gb2312")
      (:big5 . "big5")
      (:utf8 . "utf-8")))
      
(defun guess-charset-from-ef (ef)
  (dolist (nick (ef-nicknames (find-external-format ef)))
    (let ((charset (cdr (assoc nick *ef-nick-to-mime-charset*))))
      (if charset (return-from guess-charset-from-ef charset))))
  (let ((ef-name (string-downcase (symbol-name (ef-name (crlf-base-ef ef))))))
    ;; Try iso-8559-x names.
    (multiple-value-bind (found ignore suffix)
	(match-re (load-time-value "^iso8859-(\\d+)-base") ef-name)
      (declare (ignore ignore))
      (if found
	  (return-from guess-charset-from-ef 
	    (format nil "iso-8859-~a" suffix))))
    
    ;; Try windows- names.
    (multiple-value-bind (found whole value)
	(match-re (load-time-value "^(\\d+)-base$") ef-name)
      (declare (ignore whole))
      (if found
	  (return-from guess-charset-from-ef
	    (format nil "windows-~a" value))))))

(defmethod mime-part-writer ((part mime-part-constructed) 
			     &key (stream *terminal-io*))
  (mime-part-constructed-writer part stream t))

(defun mime-part-constructed-writer (part stream top-level)
  (if* top-level
     then (format stream "MIME-Version: 1.0~%"))
  
  ;; First dump user-supplied headers.
  (dolist (h (mime-part-headers part))
    (format stream "~a: ~a~%" (car h) (cdr h)))

  ;; Now dump headers that are based on class fields.

  (let* ((type (mime-part-type part))
	 (multipart (equalp type "multipart")))
    (format stream "Content-Type: ~a/~a" type (mime-part-subtype part))
    (dolist (param (mime-part-parameters part))
      (format stream ";~%    ~a=~s" (car param) (cdr param)))
    (format stream "~%")
    
    (if* (mime-part-encoding part)
       then (format stream "Content-Transfer-Encoding: ~a~%" 
		    (mime-part-encoding part)))
  
    (if* (mime-part-id part)
       then (format stream "Content-Id: ~a~%" (mime-part-id part)))
  
    (if* (mime-part-description part)
       then (format stream "Content-Description: ~a~%" 
		    (mime-part-description part)))
  
  
    (if* (disposition part)
       then (format stream "Content-Disposition: ~a" (disposition part))
	    (if* (disposition-name part)
	       then (format stream ";~%    filename=~s" (disposition-name part)))
	    (format stream "~%"))
  
    (format stream "~%") ;; terminate headers
  
    (if* multipart 
       then (let ((boundary (mime-part-boundary part)))
	      (if top-level
		  (format stream "~
This is a multi-part message in MIME format.~%"))
	      (dolist (subpart (mime-part-parts part))
		(format stream "~%--~a~%" boundary)
		(mime-part-constructed-writer subpart stream nil))
	      (format stream "~%--~a--~%" boundary))
       else (let ((instream (if* (eq (source-type part) :stream)
			       then (source part)
			     elseif (eq (source-type part) :file)
			       then (open (source part))
			       else (make-buffer-input-stream (source part)))))
	      (unwind-protect
		  (let ((encoding (mime-part-encoding part)))
		    (if* (equalp encoding "base64")
		       then (base64-encode-stream instream stream)
		     elseif (equalp encoding "quoted-printable")
		       then (qp-encode-stream instream stream)
		       else (raw-encode-stream instream stream)))
		    ;; cleanup
		(if* (not (eq (source-type part) :stream))
		   then (close instream)))))))

(defun mime-part-writer-1 (stream part)
  (mime-part-writer part :stream stream))

(defmacro with-mime-part-constructed-stream ((stream part) &body body)
  `(excl::with-function-input-stream (,stream #'mime-part-writer-1 ,part)
     ,@body))

;; Stuff ripped off from aserve

(defun split-namestring (file)
  ;; split the namestring into root and tail and then the tail
  ;; into name and type
  ;; 
  ;; any of the return value can be nil if the corresponding item
  ;; isn't present.
  ;;
  ;; rules for splitting the tail into name and type components:
  ;;  if the last period in the tail is at the beginning or end of the
  ;;  tail, then the name is exactly the tail and type is nil.
  ;;  Thus .foo and bar.  are just names, no type
  ;;  but .foo.c  has a name of ".foo" and a type of "c"
  ;;  Thus if there is a non-nil type then it means that 
  ;;    1. there will be a non nil name as well
  ;;    2. to reconstruct the filename you need to add a period between
  ;;       the name and type.
  ;;
  (let ((pos (min (or (or (position #\/ file :from-end t) most-positive-fixnum)
		      #+mswindows (position #\\ file :from-end t))))
	root
	tail)
    
    (if* (equal file "") then (return-from split-namestring nil))
    
    (if* (and pos (< pos most-positive-fixnum))
       then ; we have root and tail
	    (if* (eql pos (1- (length file)))
	       then ; just have root
		    (return-from split-namestring
		      (values file nil nil nil)))
	    
    
	    (setq root (subseq file 0 (1+ pos))
		  tail (subseq file (1+ pos)))
       else (setq tail file))
    
    
    ; split the tail
    (let ((pos (position #\. tail :from-end t)))
      (if* (or (null pos)
	       (zerop pos)
	       (equal pos (1- (length tail))))
	 then ; name begins or ends with . so it's not
	      ; a type separator
	      (values root tail tail nil)
	 else ; have all pieces
	      (values root tail
		      (subseq tail 0 pos)
		      (subseq tail (1+ pos)))))))


; we can specify either an exact url or one that handles all
; urls with a common prefix.
;;
;; if the prefix is given as a list: e.g. ("ReadMe") then it says that
;; this mime type applie to file named ReadMe.  Note that file types
;; are checked first and if no match then a filename match is done.
;
(defparameter *file-type-to-mime-type*
    ;; this list constructed by generate-mime-table in parse.cl
    '(("application/EDI-Consent") ("application/EDI-X12") ("application/EDIFACT")
      ("application/activemessage") ("application/andrew-inset" "ez")
      ("application/applefile") ("application/atomicmail")
      ("application/batch-SMTP") ("application/beep+xml") ("application/cals-1840")
      ("application/commonground") ("application/cybercash")
      ("application/dca-rft") ("application/dec-dx") ("application/dvcs")
      ("application/eshop") ("application/http") ("application/hyperstudio")
      ("application/iges") ("application/index") ("application/index.cmd")
      ("application/index.obj") ("application/index.response")
      ("application/index.vnd") ("application/iotp") ("application/ipp")
      ("application/isup") ("application/font-tdpfr")
      ("application/mac-binhex40" "hqx") ("application/mac-compactpro" "cpt")
      ("application/macwriteii") ("application/marc") ("application/mathematica")
      ("application/mathematica-old") ("application/msword" "doc")
      ("application/news-message-id") ("application/news-transmission")
      ("application/ocsp-request") ("application/ocsp-response")
      ("application/octet-stream" "bin" "dms" "lha" "lzh" "exe" "class" "so" "dll"
       "img" "iso")
      ("application/ogg" "ogg") ("application/parityfec") ("application/pdf" "pdf")
      ("application/pgp-encrypted") ("application/pgp-keys")
      ("application/pgp-signature") ("application/pkcs10")
      ("application/pkcs7-mime") ("application/pkcs7-signature")
      ("application/pkix-cert") ("application/pkix-crl") ("application/pkixcmp")
      ("application/postscript" "ai" "eps" "ps")
      ("application/prs.alvestrand.titrax-sheet") ("application/prs.cww")
      ("application/prs.nprend") ("application/qsig")
      ("application/remote-printing") ("application/riscos")
      ("application/rtf" "rtf") ("application/sdp") ("application/set-payment")
      ("application/set-payment-initiation") ("application/set-registration")
      ("application/set-registration-initiation") ("application/sgml")
      ("application/sgml-open-catalog") ("application/sieve") ("application/slate")
      ("application/smil" "smi" "smil") ("application/timestamp-query")
      ("application/timestamp-reply") ("application/vemmi")
      ("application/vnd.3M.Post-it-Notes") ("application/vnd.FloGraphIt")
      ("application/vnd.accpac.simply.aso") ("application/vnd.accpac.simply.imp")
      ("application/vnd.acucobol") ("application/vnd.aether.imp")
      ("application/vnd.anser-web-certificate-issue-initiation")
      ("application/vnd.anser-web-funds-transfer-initiation")
      ("application/vnd.audiograph") ("application/vnd.businessobjects")
      ("application/vnd.bmi") ("application/vnd.canon-cpdl")
      ("application/vnd.canon-lips") ("application/vnd.claymore")
      ("application/vnd.commerce-battelle") ("application/vnd.commonspace")
      ("application/vnd.comsocaller") ("application/vnd.contact.cmsg")
      ("application/vnd.cosmocaller") ("application/vnd.cups-postscript")
      ("application/vnd.cups-raster") ("application/vnd.cups-raw")
      ("application/vnd.ctc-posml") ("application/vnd.cybank")
      ("application/vnd.dna") ("application/vnd.dpgraph") ("application/vnd.dxr")
      ("application/vnd.ecdis-update") ("application/vnd.ecowin.chart")
      ("application/vnd.ecowin.filerequest") ("application/vnd.ecowin.fileupdate")
      ("application/vnd.ecowin.series") ("application/vnd.ecowin.seriesrequest")
      ("application/vnd.ecowin.seriesupdate") ("application/vnd.enliven")
      ("application/vnd.epson.esf") ("application/vnd.epson.msf")
      ("application/vnd.epson.quickanime") ("application/vnd.epson.salt")
      ("application/vnd.epson.ssf") ("application/vnd.ericsson.quickcall")
      ("application/vnd.eudora.data") ("application/vnd.fdf")
      ("application/vnd.ffsns") ("application/vnd.framemaker")
      ("application/vnd.fsc.weblaunch") ("application/vnd.fujitsu.oasys")
      ("application/vnd.fujitsu.oasys2") ("application/vnd.fujitsu.oasys3")
      ("application/vnd.fujitsu.oasysgp") ("application/vnd.fujitsu.oasysprs")
      ("application/vnd.fujixerox.ddd") ("application/vnd.fujixerox.docuworks")
      ("application/vnd.fujixerox.docuworks.binder") ("application/vnd.fut-misnet")
      ("application/vnd.grafeq") ("application/vnd.groove-account")
      ("application/vnd.groove-identity-message")
      ("application/vnd.groove-injector") ("application/vnd.groove-tool-message")
      ("application/vnd.groove-tool-template") ("application/vnd.groove-vcard")
      ("application/vnd.hhe.lesson-player") ("application/vnd.hp-HPGL")
      ("application/vnd.hp-PCL") ("application/vnd.hp-PCLXL")
      ("application/vnd.hp-hpid") ("application/vnd.hp-hps")
      ("application/vnd.httphone") ("application/vnd.hzn-3d-crossword")
      ("application/vnd.ibm.afplinedata") ("application/vnd.ibm.MiniPay")
      ("application/vnd.ibm.modcap") ("application/vnd.informix-visionary")
      ("application/vnd.intercon.formnet") ("application/vnd.intertrust.digibox")
      ("application/vnd.intertrust.nncp") ("application/vnd.intu.qbo")
      ("application/vnd.intu.qfx") ("application/vnd.irepository.package+xml")
      ("application/vnd.is-xpr") ("application/vnd.japannet-directory-service")
      ("application/vnd.japannet-jpnstore-wakeup")
      ("application/vnd.japannet-payment-wakeup")
      ("application/vnd.japannet-registration")
      ("application/vnd.japannet-registration-wakeup")
      ("application/vnd.japannet-setstore-wakeup")
      ("application/vnd.japannet-verification")
      ("application/vnd.japannet-verification-wakeup") ("application/vnd.koan")
      ("application/vnd.lotus-1-2-3") ("application/vnd.lotus-approach")
      ("application/vnd.lotus-freelance") ("application/vnd.lotus-notes")
      ("application/vnd.lotus-organizer") ("application/vnd.lotus-screencam")
      ("application/vnd.lotus-wordpro") ("application/vnd.mcd")
      ("application/vnd.mediastation.cdkey") ("application/vnd.meridian-slingshot")
      ("application/vnd.mif" "mif") ("application/vnd.minisoft-hp3000-save")
      ("application/vnd.mitsubishi.misty-guard.trustweb")
      ("application/vnd.mobius.daf") ("application/vnd.mobius.dis")
      ("application/vnd.mobius.msl") ("application/vnd.mobius.plc")
      ("application/vnd.mobius.txf") ("application/vnd.motorola.flexsuite")
      ("application/vnd.motorola.flexsuite.adsi")
      ("application/vnd.motorola.flexsuite.fis")
      ("application/vnd.motorola.flexsuite.gotap")
      ("application/vnd.motorola.flexsuite.kmr")
      ("application/vnd.motorola.flexsuite.ttc")
      ("application/vnd.motorola.flexsuite.wem")
      ("application/vnd.mozilla.xul+xml") ("application/vnd.ms-artgalry")
      ("application/vnd.ms-asf") ("application/vnd.ms-excel" "xls")
      ("application/vnd.ms-lrm") ("application/vnd.ms-powerpoint" "ppt")
      ("application/vnd.ms-project") ("application/vnd.ms-tnef")
      ("application/vnd.ms-works") ("application/vnd.mseq")
      ("application/vnd.msign") ("application/vnd.music-niff")
      ("application/vnd.musician") ("application/vnd.netfpx")
      ("application/vnd.noblenet-directory") ("application/vnd.noblenet-sealer")
      ("application/vnd.noblenet-web") ("application/vnd.novadigm.EDM")
      ("application/vnd.novadigm.EDX") ("application/vnd.novadigm.EXT")
      ("application/vnd.osa.netdeploy") ("application/vnd.palm")
      ("application/vnd.pg.format") ("application/vnd.pg.osasli")
      ("application/vnd.powerbuilder6") ("application/vnd.powerbuilder6-s")
      ("application/vnd.powerbuilder7") ("application/vnd.powerbuilder7-s")
      ("application/vnd.powerbuilder75") ("application/vnd.powerbuilder75-s")
      ("application/vnd.previewsystems.box")
      ("application/vnd.publishare-delta-tree") ("application/vnd.pvi.ptid1")
      ("application/vnd.pwg-xhtml-print+xml") ("application/vnd.rapid")
      ("application/vnd.s3sms") ("application/vnd.seemail")
      ("application/vnd.shana.informed.formdata")
      ("application/vnd.shana.informed.formtemplate")
      ("application/vnd.shana.informed.interchange")
      ("application/vnd.shana.informed.package") ("application/vnd.sss-cod")
      ("application/vnd.sss-dtf") ("application/vnd.sss-ntf")
      ("application/vnd.sun.xml.writer" "sxw")
      ("application/vnd.sun.xml.writer.template" "stw")
      ("application/vnd.sun.xml.calc" "sxc")
      ("application/vnd.sun.xml.calc.template" "stc")
      ("application/vnd.sun.xml.draw" "sxd")
      ("application/vnd.sun.xml.draw.template" "std")
      ("application/vnd.sun.xml.impress" "sxi")
      ("application/vnd.sun.xml.impress.template" "sti")
      ("application/vnd.sun.xml.writer.global" "sxg")
      ("application/vnd.sun.xml.math" "sxm") ("application/vnd.street-stream")
      ("application/vnd.svd") ("application/vnd.swiftview-ics")
      ("application/vnd.triscape.mxs") ("application/vnd.trueapp")
      ("application/vnd.truedoc") ("application/vnd.tve-trigger")
      ("application/vnd.ufdl") ("application/vnd.uplanet.alert")
      ("application/vnd.uplanet.alert-wbxml")
      ("application/vnd.uplanet.bearer-choice-wbxml")
      ("application/vnd.uplanet.bearer-choice") ("application/vnd.uplanet.cacheop")
      ("application/vnd.uplanet.cacheop-wbxml") ("application/vnd.uplanet.channel")
      ("application/vnd.uplanet.channel-wbxml") ("application/vnd.uplanet.list")
      ("application/vnd.uplanet.list-wbxml") ("application/vnd.uplanet.listcmd")
      ("application/vnd.uplanet.listcmd-wbxml") ("application/vnd.uplanet.signal")
      ("application/vnd.vcx") ("application/vnd.vectorworks")
      ("application/vnd.vidsoft.vidconference") ("application/vnd.visio")
      ("application/vnd.vividence.scriptfile") ("application/vnd.wap.sic")
      ("application/vnd.wap.slc") ("application/vnd.wap.wbxml" "wbxml")
      ("application/vnd.wap.wmlc" "wmlc")
      ("application/vnd.wap.wmlscriptc" "wmlsc") ("application/vnd.webturbo")
      ("application/vnd.wrq-hp3000-labelled") ("application/vnd.wt.stf")
      ("application/vnd.xara") ("application/vnd.xfdl")
      ("application/vnd.yellowriver-custom-menu") ("application/whoispp-query")
      ("application/whoispp-response") ("application/wita")
      ("application/wordperfect5.1") ("application/x-bcpio" "bcpio")
      ("application/x-bittorrent" "torrent") ("application/x-bzip2" "bz2")
      ("application/x-cdlink" "vcd") ("application/x-chess-pgn" "pgn")
      ("application/x-compress") ("application/x-cpio" "cpio")
      ("application/x-csh" "csh") ("application/x-director" "dcr" "dir" "dxr")
      ("application/x-dvi" "dvi") ("application/x-futuresplash" "spl")
      ("application/x-gtar" "gtar") ("application/x-gzip" "gz" "tgz")
      ("application/x-hdf" "hdf") ("application/x-javascript" "js")
      ("application/x-kword" "kwd" "kwt") ("application/x-kspread" "ksp")
      ("application/x-kpresenter" "kpr" "kpt") ("application/x-kchart" "chrt")
      ("application/x-killustrator" "kil")
      ("application/x-koan" "skp" "skd" "skt" "skm")
      ("application/x-latex" "latex") ("application/x-netcdf" "nc" "cdf")
      ("application/x-rpm" "rpm") ("application/x-sh" "sh")
      ("application/x-shar" "shar") ("application/x-shockwave-flash" "swf")
      ("application/x-stuffit" "sit") ("application/x-sv4cpio" "sv4cpio")
      ("application/x-sv4crc" "sv4crc") ("application/x-tar" "tar")
      ("application/x-tcl" "tcl") ("application/x-tex" "tex")
      ("application/x-texinfo" "texinfo" "texi")
      ("application/x-troff" "t" "tr" "roff") ("application/x-troff-man" "man")
      ("application/x-troff-me" "me") ("application/x-troff-ms" "ms")
      ("application/x-ustar" "ustar") ("application/x-wais-source" "src")
      ("application/x400-bp") ("application/xhtml+xml" "xhtml" "xht")
      ("application/xml") ("application/xml-dtd")
      ("application/xml-external-parsed-entity") ("application/zip" "zip")
      ("audio/32kadpcm") ("audio/basic" "au" "snd") ("audio/g.722.1") ("audio/l16")
      ("audio/midi" "mid" "midi" "kar") ("audio/mp4a-latm") ("audio/mpa-robust")
      ("audio/mpeg" "mpga" "mp2" "mp3") ("audio/parityfec") ("audio/prs.sid")
      ("audio/telephone-event") ("audio/tone") ("audio/vnd.cisco.nse")
      ("audio/vnd.cns.anp1") ("audio/vnd.cns.inf1") ("audio/vnd.digital-winds")
      ("audio/vnd.everad.plj") ("audio/vnd.lucent.voice") ("audio/vnd.nortel.vbk")
      ("audio/vnd.nuera.ecelp4800") ("audio/vnd.nuera.ecelp7470")
      ("audio/vnd.nuera.ecelp9600") ("audio/vnd.octel.sbc") ("audio/vnd.qcelp")
      ("audio/vnd.rhetorex.32kadpcm") ("audio/vnd.vmx.cvsd")
      ("audio/x-aiff" "aif" "aiff" "aifc") ("audio/x-mpegurl" "m3u")
      ("audio/x-pn-realaudio" "ram" "rm") ("audio/x-realaudio" "ra")
      ("audio/x-wav" "wav") ("chemical/x-pdb" "pdb") ("chemical/x-xyz" "xyz")
      ("image/bmp" "bmp") ("image/cgm") ("image/g3fax") ("image/gif" "gif")
      ("image/ief" "ief") ("image/jpeg" "jpeg" "jpg" "jpe") ("image/naplps")
      ("image/png" "png") ("image/prs.btif") ("image/prs.pti")
      ("image/tiff" "tiff" "tif") ("image/vnd.cns.inf2")
      ("image/vnd.djvu" "djvu" "djv") ("image/vnd.dwg") ("image/vnd.dxf")
      ("image/vnd.fastbidsheet") ("image/vnd.fpx") ("image/vnd.fst")
      ("image/vnd.fujixerox.edmics-mmr") ("image/vnd.fujixerox.edmics-rlc")
      ("image/vnd.mix") ("image/vnd.net-fpx") ("image/vnd.svf")
      ("image/vnd.wap.wbmp" "wbmp") ("image/vnd.xiff") ("image/x-cmu-raster" "ras")
      ("image/x-portable-anymap" "pnm") ("image/x-portable-bitmap" "pbm")
      ("image/x-portable-graymap" "pgm") ("image/x-portable-pixmap" "ppm")
      ("image/x-rgb" "rgb") ("image/x-xbitmap" "xbm") ("image/x-xpixmap" "xpm")
      ("image/x-xwindowdump" "xwd") ("message/delivery-status")
      ("message/disposition-notification") ("message/external-body")
      ("message/http") ("message/news") ("message/partial") ("message/rfc822")
      ("message/s-http") ("model/iges" "igs" "iges")
      ("model/mesh" "msh" "mesh" "silo") ("model/vnd.dwf")
      ("model/vnd.flatland.3dml") ("model/vnd.gdl") ("model/vnd.gs-gdl")
      ("model/vnd.gtw") ("model/vnd.mts") ("model/vnd.vtu")
      ("model/vrml" "wrl" "vrml") ("multipart/alternative")
      ("multipart/appledouble") ("multipart/byteranges") ("multipart/digest")
      ("multipart/encrypted") ("multipart/form-data") ("multipart/header-set")
      ("multipart/mixed") ("multipart/parallel") ("multipart/related")
      ("multipart/report") ("multipart/signed") ("multipart/voice-message")
      ("text/calendar") ("text/css" "css") ("text/directory") ("text/enriched")
      ("text/html" "html" "htm") ("text/parityfec") ("text/plain" "asc" "txt")
      ("text/prs.lines.tag") ("text/rfc822-headers") ("text/richtext" "rtx")
      ("text/rtf" "rtf") ("text/sgml" "sgml" "sgm")
      ("text/tab-separated-values" "tsv") ("text/t140") ("text/uri-list")
      ("text/vnd.DMClientScript") ("text/vnd.IPTC.NITF") ("text/vnd.IPTC.NewsML")
      ("text/vnd.abc") ("text/vnd.curl") ("text/vnd.flatland.3dml")
      ("text/vnd.fly") ("text/vnd.fmi.flexstor") ("text/vnd.in3d.3dml")
      ("text/vnd.in3d.spot") ("text/vnd.latex-z") ("text/vnd.motorola.reflex")
      ("text/vnd.ms-mediapackage") ("text/vnd.wap.si") ("text/vnd.wap.sl")
      ("text/vnd.wap.wml" "wml") ("text/vnd.wap.wmlscript" "wmls")
      ("text/x-setext" "etx") ("text/xml" "xml" "xsl")
      ("text/xml-external-parsed-entity") ("video/mp4v-es")
      ("video/mpeg" "mpeg" "mpg" "mpe") ("video/parityfec") ("video/pointer")
      ("video/quicktime" "qt" "mov") ("video/vnd.fvt") ("video/vnd.motorola.video")
      ("video/vnd.motorola.videop") ("video/vnd.mpegurl" "mxu") ("video/vnd.mts")
      ("video/vnd.nokia.interleaved-multimedia") ("video/vnd.vivo")
      ("video/x-msvideo" "avi") ("video/x-sgi-movie" "movie")
      ("x-conference/x-cooltalk" "ice")))

(defvar *mime-types* nil)

(defun build-mime-types-table ()
  (if* (null *mime-types*)
     then (setf *mime-types* (make-hash-table :test #'equalp))
	  (dolist (ent *file-type-to-mime-type*)
	    (dolist (type (cdr ent))
	      (setf (gethash type *mime-types*) (car ent))))))
  

(build-mime-types-table)  ;; build the table now

;; return mime type if known
(defmethod lookup-mime-type (filename) 
  (if* (pathnamep filename)
     then (setq filename (namestring filename)))
  (multiple-value-bind (root tail name type)
      (split-namestring filename)
    (declare (ignore root name))
    (if* (and type (gethash type *mime-types*))
       thenret
     elseif (gethash (list tail) *mime-types*) 
       thenret)))
