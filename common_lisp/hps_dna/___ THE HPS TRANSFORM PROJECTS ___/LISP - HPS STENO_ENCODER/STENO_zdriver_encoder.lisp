;; ******************************************************************************************************
;; ****************           INVOCATION EXAMPLE FOR THE STENO ENCODER       ****************************
;; ******************************************************************************************************


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


;; ******************************************************************************************************
;; ******************************************************************************************************
(defun STENO_encoder_driver ()
	(pprint 'HPS_STENO_ENCODER_START)

	    	(STENO_encoder_driver_init) 

		;; the invocation of the STENO encoder, it takes a series of characters and encodes them into ATS segments hidden behind
		;; the specified HPS-removable noise cloud
		( STENO_encoder_main    	      :avgdur 		120   			  ;; the maximum duration of encoded segments
					              :avgdurvar     	30			  ;; the maximum variability in segment duration
					              :gapdur 	     	3		  	  ;; the maximum duration of gaps between segments
					              :noisefactor   	26			  ;; the (1/2) magnitude of the HPS uncoverable noise cloud
					              :inputtext     	nil		  	  ;; the text being encoded
					              :alphabet	     	*STENO_mapalphabet*  	  ;; the alphabet in use
					              :mapcode       	*STENO_mapcode*      	  ;; the numerical mapping code use to map letters to numbers
					              :randomvals_file 	"F:/HPS_DATA/HPS_INPUTS/HPS_RANDOM_VALUES.DAT"
			  	                      :intofilename 	"F:/HPS_DATA/HPS_INPUTS/HPS_INPUT_SERIES.DAT" )


	(pprint 'HPS_STENO_ENCODER_DONE))
;; ******************************************************************************************************



;; ****************************************************************************************************
;; ****************************************************************************************************
					    (if 
				   HPS_COMPILE_AND_LOAD_IS_ON 
				     (STENO_encoder_driver)
				             nil)
;; ****************************************************************************************************






