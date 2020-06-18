(proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;; ****************************************************************************************************
;;                             FROM HPS SERIES TO TEXT - DECODER
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
;; this function initializes the state of the STENO decoder 
;; ****************************************************************************************************
(defun STENO_decoder_init ( meandur_guess mindur_threshold )
	;; input file I/O - the results of the read are stored on the following table array, 
	(setq *STENO_HPS_tupleset* nil)

	;; this is used in the generation of the condensation of the data tuples into HPS states.
	(setq *STENO_SEG_table* nil)
	(setq *STENO_SEG_start* 0)
	(setq *STENO_SEG_end*   0)
	(setq *STENO_SEG_size*  0)

	;; this is used in an attempted automatic deciphering of the message, however, manual handling may be necessary
	(setq *STENO_decoded_msg* nil)
	
	;; this controls the number of HPS segments printed per output line
	(setq *STENO_SEG_NITEMS_PER_LINE* 4) 

	;; code used to identify threshold-identification of segments
	(setq *STENO_MISPELLING_CODE* 'MS)

	;; this is done by using rules related to stability and minimum segment duration.
	(setq *STENO_SEG_meandur* meandur_guess )
	(setq *STENO_SEG_mindur*  mindur_threshold))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; check duration + 1
;; this generates a tuple that represents a HPS state of type=nil (default is this, a true HPS state).
;; states are expressed in the format of (start, end, duration, meanvalue, [type]) 
;; where type represents an optional code to identify questionable or fractional, small duration, HPS states
;; ****************************************************************************************************
(defun STENO_HPS_print_segment (start end dur value)
  	(push (list start end dur value) *STENO_SEG_table*))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; check on how to merge various small fragmented segments if such is applicable.
;; this generates a tuple that represents a fractional HPS state of type=*STENO_MISPELLING_CODE*
;; ****************************************************************************************************
(defun STENO_HPS_merge_segment (start end dur value)
  	(push (list start end dur value *STENO_MISPELLING_CODE* ) *STENO_SEG_table*))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; this function retrieve the input tuple set already loaded into memory, in case if needed by the caller
;; ****************************************************************************************************
(defun STENO_get_input_tupleset ( &key ( mode 'EXACT  ))
  	(cond ((equal mode 'EXACT) 	(return-from STENO_get_input_tupleset *STENO_HPS_tupleset* ))
  	      ((equal mode 'ROUNDED) 	(return-from STENO_get_input_tupleset *STENO_HPS_approx_tupleset*))
  	      (T 			(return-from STENO_get_input_tupleset *STENO_HPS_approx_tupleset*))))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; this function retrieve the input tuple set already loaded into memory, in case if needed by the caller
;; it also can be used to extract a particular portion of a segment table usually corresponding to a pattern match
;; ****************************************************************************************************
(defun STENO_get_segment_table ( &key (SEG_table 	nil) 
				      (fromindex 	nil) 
     			     	      (toindex 		nil))

  	(if (not SEG_table)
        	(return-from STENO_get_segment_table *STENO_SEG_table* )
		nil)

	(if (AND (NOT fromindex) (NOT toindex))
        	(return-from STENO_get_segment_table *STENO_SEG_table* )
	  	nil)

  	(setq *STENO_SEG_table_temp* nil)
	(dolist (a_segment SEG_table 'HPSSTENO_SEG_MENTTABLE) 
		(setq *STENO_SEG_start* (first  a_segment))
		(setq *STENO_SEG_end*   (second a_segment))

		(when   (AND (HPS_is_this_between *STENO_SEG_start* fromindex toindex)
			     (HPS_is_this_between *STENO_SEG_end*   fromindex toindex))
			(push a_segment *STENO_SEG_table_temp* ))

		(unless (AND (HPS_is_this_between *STENO_SEG_start* fromindex toindex) 
			     (HPS_is_this_between *STENO_SEG_end*   fromindex toindex))
		  	nil))

	(setq *STENO_SEG_table_temp* (reverse *STENO_SEG_table_temp*))
	(return-from STENO_get_segment_table  *STENO_SEG_table_temp*))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; this generates the condensation and implements the condensation rules
;; ****************************************************************************************************
(defun STENO_HPS_timeseries_to_segments ( atimeseries goaldur mindur &key (LARGESEG_FRAGMENTER_ON nil))
	(setq *STENO_iter* 1)
	(setq *STENO_nval* -999999)
	(setq *STENO_new_segment_flag* nil)
	(dolist (x atimeseries 'HPSSTATECONDENSATION) 
		(when   (STENO_similar_nval *STENO_nval* (second x)) 
			(setq *STENO_SEG_size*  (+ *STENO_SEG_size* 1))
		  	(if (AND (> *STENO_SEG_size* goaldur) LARGESEG_FRAGMENTER_ON )
			  	(setq *STENO_new_segment_flag* t)
				nil))

		(unless (STENO_similar_nval *STENO_nval* (second x)) 
		  	(setq *STENO_new_segment_flag* t))

		(when   *STENO_new_segment_flag*
			(setq *STENO_SEG_end*  (first x))
			(setq *STENO_SEG_size* (- *STENO_SEG_end* *STENO_SEG_start*))
			(cond 	((> *STENO_SEG_size* *STENO_SEG_meandur*)   
				    (STENO_HPS_print_segment *STENO_SEG_start* *STENO_SEG_end* *STENO_SEG_size* *STENO_nval* )) 
				((> *STENO_SEG_size* *STENO_SEG_mindur*) 
				    (STENO_HPS_merge_segment *STENO_SEG_start* *STENO_SEG_end* *STENO_SEG_size* *STENO_nval* )) 
				(T  (if nil (pprint 'TRANSIENT) nil)))
			(setq *STENO_new_segment_flag* nil)
			(setq *STENO_SEG_start* (first x)))

		(unless *STENO_new_segment_flag*
		  	nil)

		(setq *STENO_iter* (first x))
		(setq *STENO_nval* (second x)))

	(setq *STENO_SEG_table* (reverse *STENO_SEG_table*)))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; KLUDGE to save I/O in external calls
;; ****************************************************************************************************
(defun HPS_STENO_set_input_series ( inputseries )
  	(setq *STENO_HPS_tupleset* inputseries))
;; ****************************************************************************************************

;; ****************************************************************************************************
;; this function is the main entry point for the decoder for HPS-based timeseries stenography
;; ****************************************************************************************************
(defun STENO_decoder_main ( &key (MEANDUR_GUESS 	90) 
				 (MINDUR_THRESHOLD 	30) 
				 (SYMBOLS_PER_LINE	28)
			 	 (BASEDIR	  	"C:/HPS_DATA/")
			 	 (OUTPUTDIR	  	"C:/HPS_DATA/HPS_OUTPUTS/")
				 (HPS_INPUTFILE  	"C:/HPS_DATA/HPS_OUTPUTS/HPS_APPROXIMATION.DAT")
				 (HPS_DEBUGFILE  	"C:/HPS_DATA/HPS_OUTPUTS/HPS_STENO_TIMESERIES.DAT")
				 (HPS_OUTPUTFILE 	"C:/HPS_DATA/HPS_OUTPUTS/HPS_STENO_SEGTABLE.DAT")
				 (HPS_TIMESERIES_ARG	 nil)
				 (LARGESEG_FRAGMENTER_ON nil)
				 (HPS_EXTERNAL_CALL      nil)
				 (HPS_TERSE 		 t))
 
  	;; this initializes the tracking tables 
	(STENO_decoder_init 	MEANDUR_GUESS 
				MINDUR_THRESHOLD )

	;; feedback the input parameters
	(format t "~%" nil)
	(format t "~%" nil)
	(format t "~& AVE SEG DURATION THRESHL: ~D" MEANDUR_GUESS )
	(format t "~& MIN SEG DURATION THRESHL: ~D" MINDUR_THRESHOLD )
	(format t "~& READING INPUTSERIES FROM: ~S" HPS_INPUTFILE )

	;; this reads all data tuples from the specified file, expected to contain an HPS approximation extracted from an noise series 
	(when HPS_TIMESERIES_ARG
	  	(HPS_STENO_set_input_series HPS_TIMESERIES_ARG)) 

	(when (not *STENO_HPS_tupleset*)
		(setq *STENO_HPS_tupleset* (STENO_HPS_read_series :INPUT_FILENAME HPS_INPUTFILE))
		nil)

	;; * ********************************CHECK                * ***********************************************
	;; this generates a rounding off of the HPS values, from rational numbers to integers.
	;; CHECK BUG WITH NIL WHEN SWITCHED, SHOULD NOT, --- NON-ROUNDUP PROBABLY MAPPING RESULTS IN NONMAP
	(setq *STENO_HPS_approx_tupleset* *STENO_HPS_tupleset*)
	(setq *STENO_HPS_approx_tupleset* (mapcar #'(lambda (x) (list (first x) (round (second x)))) *STENO_HPS_tupleset*))
	;; * ********************************CHECK                * ***********************************************

	;; this extracts the HPS segment table embedded into an STENO HPS timeseries
	(setq *STENO_SEG_table* (STENO_HPS_timeseries_to_segments *STENO_HPS_approx_tupleset* 
								   MEANDUR_GUESS 
								   MINDUR_THRESHOLD 
								  :LARGESEG_FRAGMENTER_ON LARGESEG_FRAGMENTER_ON))

	(when   (NOT HPS_TERSE)
		;; write the previous terminal output also to the specified output file in html format
		(with-open-file (output-stream hps_outputfile :direction :output :if-exists :supersede :if-does-not-exist :create)
			(format output-stream "~A" STENO_HTML_MAIN_HEADER )
			(format output-stream "~& READING INPUT SERIS FROM: ~S" HPS_INPUTFILE )
			(format output-stream "~& WRITING TIMESERIES TO   : ~S" HPS_DEBUGFILE )
			(format output-stream "~& WRITING SEGMENT TABLE TO: ~S" HPS_OUTPUTFILE )
			(format output-stream "~& AVE SEG DURATION THRESHL: ~D" MEANDUR_GUESS )
			(format output-stream "~& MIN SEG DURATION THRESHL: ~D" MINDUR_THRESHOLD ))
	
		;; this writes the decoded segments from the input time series into the specified file
		(format t "~& WRITING SEGMENT TABLE TO: ~S" 			HPS_OUTPUTFILE )
		(setq *STENO_decoded_msg* (STENO_decoder_writer_segtable 	*STENO_SEG_table* 
								  		HPS_OUTPUTFILE 
								  		HPS_EXTERNAL_CALL ))

		;; this generates a test mapping to inspect the original contents of the time series prior to condesation into HPS states
		(setq *STENO_HPS_mapped_tupleset* 
		      (mapcar #'(lambda (x) (list (first x) (STENO_getmapletter (round (second x))))) *STENO_HPS_approx_tupleset*))

		(format t "~& WRITING TIMESERIES TO   : ~S" HPS_DEBUGFILE )

		(STENO_decoder_writer_timeseries *STENO_HPS_mapped_tupleset* 
						  HPS_DEBUGFILE )
	
		;; print the whole output of the decoder to the screen, evaluated for the given thresholds
		(STENO_HPS_writer_segtable_to_screen :NCHARS 	   3 
						     :DECODERFLAG 'DECODER)
	
		;; print the auto-decoded message to the specified subcomponent of the auto-generated html report
		;; print the auto-decoded message, even if user-handling may be needed
		(with-open-file (output-stream hps_outputfile :direction :output :if-exists :supersede :if-does-not-exist :create)
			(STENO_autodecoder_printer  *STENO_decoded_msg* 
					    	     output-stream 
					   	    :NCHARS symbols_per_line ))

		;; end the generation of the html report file
		(with-open-file (output-stream hps_outputfile :direction :output :if-exists :append :if-does-not-exist :create)
			(format output-stream "~& ~A" STENO_HTML_MAIN_FOOTER ))

		nil)

	(unless (NOT HPS_TERSE)
	  	nil)
	

	(when (not HPS_EXTERNAL_CALL)
		;; autogenerate a gnuplot command file needed to generate the automated report and analysis
		(setq *HPS_STENO_gnuplot_cmdfile* (format nil "~A~A" basedir "PNG-HPS-STENO-PLOTS.PLT" ))
		(setq *HPS_STENO_gnuplot_figfile* (format nil "~A_~D_~D.PNG" "HPS_OUTPUTS/HPS-STENO-TIMEPLOTS-AND-HISTOGRAMS" 
							  		      MEANDUR_GUESS 
									      MINDUR_THRESHOLD))
		(HPS_STENO_decoder_gnuplot *STENO_SEG_table* 
					    BASEDIR 
					   *HPS_STENO_gnuplot_cmdfile* 
					   *HPS_STENO_gnuplot_figfile* ) 

		;; prepare the data of the autogenerated analyses for the STENO decoder
		(setq *HPS_SYSTEM_CMDS* (format nil "~A~A" basedir "HPS_STENO_WINDOWS_CMDS.BAT"))
		(HPS_gnuplot_interface 		:COMMAND 	'PLOT
		   	       			:OUTPUT_DIR 	 OUTPUTDIR
		   	       			:WITH_CMD_FILE 	*HPS_SYSTEM_CMDS* ))
	(unless (not HPS_EXTERNAL_CALL)
	  	nil)

	(return-from STENO_decoder_main *STENO_decoded_msg*))
;; ****************************************************************************************************





