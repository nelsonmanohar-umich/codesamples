;; ****************************************************************************************************
;;                             FROM HPS SERIES TO TEXT - ITERATIVE DECODER
;; ****************************************************************************************************
;; function decodes a steno time series looking for the presence of HPS states which are then 
;; mapped into a text string by using a shared knowledge alphabet-code mapping (shown below). 
;; the function reads the HPS time series, which is required to be of 3601 samples, as produced,
;; by the simulator and then generates the corresponding HPS state sequence. These HPS states
;; are of variable length and their mean value represents a representative value coding for a letter.
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


;; ****************************************************************************************************
;; THE ITERATIVE STENO DECODER
;; ****************************************************************************************************
(defun STENO_iterative_decoder ( &key (MAIN_htmlreport    "C:/HPS_DATA/HPS_REPORT_STENO.HTM" )
				 	(basedir	  "C:/HPS_DATA/")
				 	(outputdir	  "C:/HPS_DATA/HPS_OUTPUTS/")
  				        (seggoals      	 '(60 70 80 90 100 110))
				        (segthresholds 	 '(5 10 15 20 25 30 35 40)))
	;; initialize the iterative decoder
	(setf *THRESHOLD_LIST_A* seggoals)
	(setf *THRESHOLD_LIST_B* segthresholds)

	;; where the autodecoded message will be stored
	(setf *STENO_msg* nil)

	;; generate the contents of the html report, iterate over each possibility
	;; this combinatorial setup seems worthless now (as it really consisting of 
	;; one apparent set of values and not two), but it is there for later use
	;; as threshold detections have different contribution value to the meaning
	;; and ranking of autodecoded messages
	(with-output-to-string ( *HPS_internal_buffer* ) 
	    (dolist (seggoal *THRESHOLD_LIST_A* 'DECODER_ITERATIONS_A)
	    	(dolist (threshold *THRESHOLD_LIST_B* 'DECODER_ITERATIONS_B)
			;; construct a filename to hold this iteration's detail results for later examination
		  	(setf *HPS_filename_temp_index* (format nil "~D_~D.HTM" seggoal threshold))
			(setf *HPS_filename_for_iteration* (format nil "~A~A~A" outputdir "HPS_STENO_SEGTABLE" *HPS_filename_temp_index* ))
			;; (setf *HPS_filename_for_iteration* (format nil basedir HPS_concat "E:/HPS_DATA/HPS_OUTPUTS/HPS_STENO_SEGTABLE" *HPS_filename_temp_index* ))
	
			;; decode the message given the specified threshold values
			(setf *STENO_msg* (STENO_decoder_main :meandur_guess seggoal :mindur_threshold threshold
							      :basedir	      	      "E:/HPS_DATA/"
				 	 		      :outputdir      	      "E:/HPS_DATA/HPS_OUTPUTS/"
							      :hps_inputfile  	      "E:/HPS_DATA/HPS_OUTPUTS/HPS_APPROXIMATION.DAT"
							      :hps_debugfile  	      "E:/HPS_DATA/HPS_OUTPUTS/HPS_STENO_TIMESERIES.DAT"
							      :hps_outputfile 	      *HPS_filename_for_iteration* 
							      :LARGESEG_FRAGMENTER_ON  t))
	
			;; *******************************************************************************************************
			;; print to the summary html report
			;; *******************************************************************************************************
			(format  *HPS_internal_buffer* "~A~A~A"  STENO_HTML_MAIN_ITEM_P0 *HPS_filename_for_iteration* STENO_HTML_MAIN_ITEM_P1 )
			(format  *HPS_internal_buffer* "[GOAL=~3D THRESHOLD=~3D]" seggoal threshold )
			(format  *HPS_internal_buffer* "~A"	  STENO_HTML_MAIN_ITEM_P2 )
			(format  *HPS_internal_buffer* "~A"	  STENO_HTML_MAIN_ITEM_P3 )
			(HPS_pretty_printer *STENO_msg* *HPS_internal_buffer* :nchars 44 :headermsg  "AUTO-DECODING:	" :htmlmode t)
			(format  *HPS_internal_buffer* 		 (HPS_STENO_FIG_ENTRY) seggoal threshold seggoal threshold)
			(format  *HPS_internal_buffer* "~%~A ~%"  STENO_HTML_MAIN_ITEM_P4 )
	
			;; *******************************************************************************************************
			;; print to the detail html report component
			;; *******************************************************************************************************
			(with-open-file (ITERATION-output-stream *HPS_filename_for_iteration* :direction :output :if-exists :supersede :if-does-not-exist :create)
				(format  ITERATION-output-stream "~A~A~A" STENO_HTML_MAIN_ITEM_P0 *HPS_filename_for_iteration* 
					 		                  STENO_HTML_MAIN_ITEM_P1 )
				(format  ITERATION-output-stream "[GOAL=~3D THRESHOLD=~3D]" seggoal threshold )
				(format  ITERATION-output-stream "~A"  	  STENO_HTML_MAIN_ITEM_P2 )
				(format  ITERATION-output-stream "~%~A"	  STENO_HTML_MAIN_ITEM_P3 )
	
				(HPS_pretty_printer *STENO_mapalphabet* ITERATION-output-stream  :nchars 28 :headermsg  "APHABET CURRENTLY IN USE...:	")
				(HPS_pretty_printer *STENO_mapcode*     ITERATION-output-stream  :nchars 14 :headermsg  "MAPPING CODE BEING USED ...:	")

				(STENO_autodecoder_printer *STENO_msg*  ITERATION-output-stream  :nchars 28)

				(format  ITERATION-output-stream "~%~&~%"	  nil ))

			;; INDENTED ONLY FOR SEMANTIC-READING CONVENIENCE WITH RESPECT TO REPORT GENERATION
			(STENO_decoder_writer_segtable (STENO_get_segment_table) *HPS_filename_for_iteration* nil )
	
			(with-open-file (ITERATION-output-stream *HPS_filename_for_iteration* :direction :output :if-exists :append )
				(format  ITERATION-output-stream "~%~A"	  STENO_HTML_MAIN_ITEM_P4 ))
	
			;; *******************************************************************************************************
			;; print to the screen 
			;; *******************************************************************************************************
			(STENO_autodecoder_printer *STENO_msg* t :nchars 28)))

	    	;; retrieve the contents of the dynamic report
	   	(setf HPS_STENO_main_report_dynamic_contents (get-output-stream-string *HPS_internal_buffer*)))
		  	
	;; write the autogenerated html report
	(with-open-file (MAIN-htmlreport-stream MAIN_htmlreport :direction :output :if-exists :supersede :if-does-not-exist :create)
	  	(format  MAIN-htmlreport-stream (HPS_STENO_HTML_REPORT) HPS_STENO_main_report_dynamic_contents))

	;; free the links to the character strings
	(setf STENO_HTML_MAIN_HEADER nil)
	(setf STENO_HTML_MAIN_FOOTER nil)


	(pprint 'DONE))
;; ****************************************************************************************************








