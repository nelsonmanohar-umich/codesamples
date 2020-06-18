;; ****************************************************************************************************
;;                        EXAMPLE INVOCATION OF THE ITERATIVE STENO DECODER
;; ****************************************************************************************************


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
;; function decodes a steno time series looking for the presence of HPS states which are then 
;; mapped into a text string by using a shared knowledge alphabet-code mapping (shown below). 
;; the function reads the HPS time series, which is required to be of 3601 samples, as produced,
;; by the simulator and then generates the corresponding HPS state sequence. These HPS states
;; are of variable length and their mean value represents a representative value coding for a letter.
;; ****************************************************************************************************
(defun STENO_decoder_driver ()
  	(pprint 'STENO_DECODER_START)

	;; initialize the decoder with the shared secret (alphabet) from the encoder
	(STENO_decoder_driver_init)

	;; where the output report will be placed
	(setf *STENO_reportfile* "F:/HPS_DATA/HPS_REPORT_STENO.HTM" )
	(setf *STENO_outputdir*  "F:/HPS_DATA/HPS_OUTPUTS/" )
	(setf *STENO_basedir*    "F:/HPS_DATA/" )

	;; the invocation of the STENO decoder, it takes a series of ATS segments hidden behind an HPS-removable noise cloud
	;; and extract the HPS fundamental frequencies of these ATS segments
	(STENO_iterative_decoder :MAIN_htmlreport *STENO_reportfile*
				 :seggoals 	  '(90) 
				 :basedir	  *STENO_basedir*
				 :outputdir	  *STENO_outputdir*
				 :segthresholds   '(  30 35  ))

	(format t "~% ANALYSIS REPORT FOUND ON: ~S" *STENO_reportfile* )

  	(pprint 'STENO_DECODER_END ))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; ****************************************************************************************************
					    (if 
				   HPS_COMPILE_AND_LOAD_IS_ON 
				     (STENO_decoder_driver)
				             nil)
;; ****************************************************************************************************








