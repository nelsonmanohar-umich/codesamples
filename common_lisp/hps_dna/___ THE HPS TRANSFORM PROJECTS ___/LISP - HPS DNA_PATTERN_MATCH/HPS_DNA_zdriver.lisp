;; ********************************************************************************************
;;                           INVOCATION EXAMPLE OF THE DNA PATTERN MINER
;; ********************************************************************************************
;; this illustrates how to use the HPS DNA PATTERN MATCHER by creating a sample data mining 
;; application for finding all the matches of a DNA sequence (referred to as the signature)
;; within another DNA sequence  (referred to as the test sequence).
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


;; ********************************************************************************************
;; ********************************************************************************************

(when  HPS_COMPILE_AND_LOAD_IS_ON 
	;; the invocation of the STENO decoder, it takes a series of ATS segments hidden behind an HPS-removable noise cloud
	;; and extract the HPS fundamental frequencies of these ATS segments
	(setf *HPS_DNA_SEG_goaldur*      15) 			; previously was 40 - very coarse tracking - very few segments
	(setf *HPS_DNA_SEG_thresholddur* 4) 			; previously was 10 - very coarse tracking - very few segments

	(setf *HPS_DNA_inputfile* 	"F:/HPS_DATA/HPS_OUTPUTS/HPS_3DNA_APPROXIMATION_SERIES.DNA" )
	(setf *HPS_DNA_segtable_file*   "F:/HPS_DATA/HPS_OUTPUTS/HPS_DNA_SEGMENT_TABLE.DNA" )

	(format t "~% CONSTRUCTING HPS SEGMENT TABLE FOR  : ~A" *HPS_DNA_inputfile* )
	(format t "~% DNA SEGMENT TABLE WILL BE PLACED IN : ~A" *HPS_DNA_segtable_file* )

	;; initialize the 3DNA alphabet cyphers
	(HPS_DNA_main_init 	   :alphabet    		nil  
				   :mapcode     		nil)

	;; apply the HPS SEGment decoder
	(STENO_decoder_main 	   :meandur_guess 		*HPS_DNA_SEG_goaldur*    
				   :mindur_threshold 		*HPS_DNA_SEG_thresholddur*  
				   :symbols_per_line 		 14
				   :hps_inputfile  		"F:/HPS_DATA/HPS_OUTPUTS/HPS_3DNA_APPROXIMATION_SERIES.DNA"
				   :hps_debugfile  		"F:/HPS_DATA/HPS_OUTPUTS/HPS_3DNA_ROUNDED_APPROXIMATION_ENCODED.DNA"
				   :hps_outputfile 		"F:/HPS_DATA/HPS_OUTPUTS/HPS_DNA_SEGMENT_TABLE.DNA"
				   :hps_external_call   	 t
				   :LARGESEG_FRAGMENTER_ON  	 nil
				   :hps_terse 			 nil )


	;; apply the HPS DNA pattern miner
	(HPS_DNA_main 		   :alphabet    		nil
				   :mapcode     		nil
		      	   	   :goaldur     		*HPS_DNA_SEG_goaldur*		
			   	   :mindur      		*HPS_DNA_SEG_thresholddur*	
			   	   :match_goal  		0.75
			   	   :input_file  		"HPS_3DNA_APPROXIMATION_SERIES.DNA"  
			   	   :base_dir    		"F:/HPS_DATA/"
			   	   :output_dir  		"F:/HPS_DATA/HPS_OUTPUTS/" 
			   	   :report_file 		"HPS_DNA.HTM" )

	nil)

(unless HPS_COMPILE_AND_LOAD_IS_ON 
	nil)

;; ********************************************************************************************












