;; ****************************************************************************************************************
;;                           *****     INVOCATION OF THE MAIN ENTRY POINT         ******
;;      THIS FILE CONTAINS EXAMPLES OF THE APPLICATION AND USE OF THE HPS TRANSFORM AND THE PROVIDED ROUTINES
;; ****************************************************************************************************************


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


;; ****************************************************************************************************************
;; ****************************************************************************************************************
(defun HPS_TEST_APPLICATION ( &key (output_mode nil) )
 	;; set to a detailed report with graphics and analyses in addition to the HPS transform results
  	(setf *HPS_FULL_OUTPUT_MODE*            output_mode)

	;; invoke the main entry point of the HPS transform
   	(HPS_driver     :inputfile             "HPS_INPUT_SERIES.DAT" 
	 		:basedir               *HPS_BASE_PATH*  
	 		:outputfile            "HPS_APPROXIMATION.DAT"
	 		:outputdir             *HPS_OUTPUT_PATH*
	 		:reportfile            "RESULTANT_HPS_APPROXIMATION.HTM" 
	 		:datasetfile           "HPS_FULLDATA.DAT" 
						:m          	  30 			       
						:mp 	    	  15
					        :timeshift  	  60
					        :seglimit   	  300
					        :alphalevel 	  0.005 	       
						:forecast_winsize 60
						:K                3 			
						:segtrivial       1 
						:mserelax   (sqrt 2) 	  
						:msedelay         1 	
						:printdur         3 
	 		:HPS_FULL_OUTPUT_MODE	*HPS_FULL_OUTPUT_MODE* )

	;; prepare the data of the autogenerated analyses
	(when *HPS_FULL_OUTPUT_MODE*
		(HPS_gnuplot_interface  :COMMAND 	'PLOT
		   	       		:OUTPUT_DIR 	*HPS_OUTPUT_PATH*
		   	       		:WITH_CMD_FILE 	(format nil "~A~A" *HPS_BASE_PATH* "HPS_TRANSFORM_WINDOWS_CMDS.BAT" )))
	(unless *HPS_FULL_OUTPUT_MODE*
	  	nil))
;; ****************************************************************************************************************




