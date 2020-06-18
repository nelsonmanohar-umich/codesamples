;; ********************************************************************************************
;;                       	"GENERIC?" ATS SEGMENT TABLE PRINTER
;; ********************************************************************************************


;; ********************************************************************************************
;; ********************************************************************************************
;; THIS MODULE IS PART OF AN IMPLEMENTATION OF AN ONLINE HPS TRANSFORM.  THE HPS TRANSFORM WAS 
;; DESCRIBED IN THE PAPER ENTITLED: 
;; 			    THE HPS TRANSFORM AND ITS APPLICATIONS
;;                          www3.webng.com/nelsonmanohar/research/
;;
;; 	          THIS CODE WAS WRITTEN AND ITS COPYRIGHTED (2007) BY 
;; 			    DR. NELSON R. MANOHAR ALERS
;; 			      nelsonmanohar@yahoo.com
;; 			           AUGUST 2007
;;
;; THIS CODE IS NOT IN THE PUBLIC DOMAIN. THIS CODE IS NOT INTENDED FOR DISTRIBUTION. PLEASE DO
;; NOT MODIFY THIS CODE. PLEASE INFORM THE AUTHOR OF ANY UNINTENDED OR UNAUTHORIZED DISTRIBUTION 
;; AND/OR ACCESS TO THIS CODE.
;; ********************************************************************************************
;; ********************************************************************************************


;; ****************************************************************************************************
;; CHECK - CHANGE THIS FUNCTION TO SOMETHING ELSE INCORPORATED ALREADY
;; this special shared function writes segments to the terminal in an easy to read format
;; ****************************************************************************************************
(defun STENO_HPS_writer_segtable_to_screen ( &key (nchars 		8)
					     	  (gapdurationguess 	4) 
						  (decoderflag 		'ENCODER))

	;; write the resultant segment table to the screen
	(format t "~& STENO SEGMENT TABLE: " nil)
	(setf *STENO_iter_temp* 0)
	(dolist (seg *STENO_SEG_table* 'SEGPRINTER)
	  	(when   (equal decoderflag 'DECODER)
	  		(if (equal (mod *STENO_iter_temp* nchars ) 0)
		  		(if (> (- (second seg) (first seg)) gapdurationguess )
	  				(format t "~& STENO SEGMENT TABLE: [~{~6d ~} ~6,2f  ~2:S ]	" 
									(subseq seg 0 3) (fourth seg) (STENO_getmapletter (fourth seg)))
	  				(format t "~& STENO SEGMENT TABLE: [~{~6d ~} ~6,2f  ~1:S* ]	" 
									(subseq seg 0 3) (fourth seg) (STENO_getmapletter (fourth seg))))
		  		(if (> (- (second seg) (first seg)) gapdurationguess )
	  				(format t                         "[~{~6d ~} ~6,2f  ~2:S ]	" 
									(subseq seg 0 3) (fourth seg) (STENO_getmapletter (fourth seg)))
	  				(format t                         "[~{~6d ~} ~6,2f  ~1:S* ]	" 
									(subseq seg 0 3) (fourth seg) (STENO_getmapletter (fourth seg))))))
	  	(unless (equal decoderflag 'DECODER)
	  		(if (equal (mod *STENO_iter_temp* nchars ) 0)
		  		(if (> (- (second seg) (first seg)) gapdurationguess )
	  				(format t "~& STENO SEGMENT TABLE: ~23:S ~2:S		" seg (STENO_getmapletter (third seg)))
	  				(format t "~& STENO SEGMENT TABLE: ~23:S ~1:S*		" seg (STENO_getmapletter (third seg))))
		  		(if (> (- (second seg) (first seg)) gapdurationguess )
	  				(format t                         "~23:S ~2:S		" seg (STENO_getmapletter (third seg)))
	  				(format t                         "~23:S ~1:S*		" seg (STENO_getmapletter (third seg))))))
		(incf *STENO_iter_temp* )))
;; ****************************************************************************************************







