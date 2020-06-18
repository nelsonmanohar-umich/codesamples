;; ********************************************************************************************
;;                            GENERIC DRIVER FOR THE ONLINE HPS TRANSFORM
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


;; ********************************************************************************************************
;; these lines simply separate the screen output from anything previous
;; ********************************************************************************************************
(defun HPS_print_separator ( numlines )
	;; print couple of separator lines as needed
	(dotimes (j numlines 'HPSPRINTSEPARATOR)
		(format t "~%" nil) (format t "~%" nil)))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function efficiently frees up references to datums in VERY BIG lists and tables, by doing so, their
;; aging can be accelerated and the corresponding locations can be marked quicker by the gargabe collector
;; ********************************************************************************************************
(defun HPS_cleanup ( HPS_FULL_OUTPUT_MODE )
  	(when (AND HPS_FULL_OUTPUT_MODE (NOT *HPSdebug*)) 
		(HPS_wipeout *HPS_input_timeseries_dataset*)
		(HPS_wipeout *HPS_transformed_response_dataset*)
		(HPS_wipeout *HPS_GLOBAL_computation_state*)
		(HPS_wipeout *HPS_input_timeseries_data*)
	  	nil)

  	;; invoke the garbage collector, the aging is NOT this fast, but it may pick some
	(HPS_system_interface 	  :COMMAND		'CLGC))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; ****************************************    HPS TRANSFORM    *******************************************
;; ****************************************   EXAMPLE  DRIVER   *******************************************
;; ********************************************************************************************************
(defun HPS_driver 	( &key  (INPUTFILE  		"HPS_INPUT_SERIES.DAT" )
			 	(BASEDIR       		"E:/HPS_DATA/" )
			 	(OUTPUTFILE    		"HPS_APPROXIMATION.DAT")
			 	(OUTPUTDIR     		"E:/HPS_DATA/" )
			 	(REPORTFILE    		"RESULTANT_HPS_APPROXIMATION.HTM" )
			 	(DATASETFILE   		"HPS_FULLDATA.DAT" )
			 	(M             		60 )
			 	(MP 	        	30 )
		         	(TIMESHIFT     		30 )
		         	(SEGLIMIT      		90 )
		         	(ALPHALEVEL    		0.001 )
			 	(K             		3 )
				(FORECAST_WINSIZE       m)
			 	(SEGTRIVIAL    		1 )
			 	(MSERELAX      		(sqrt 2) )
			 	(MSEDELAY      		1 )
			 	(PRINTDUR      		3 )
				(CONDITIONING_TYPE	nil)
				(HPS_TRANSIENT_FILLER   nil)
				(HPS_TERSE		nil)
			 	(HPS_FULL_OUTPUT_MODE	t))

	;; ****************************************************************************************************************
	;; set global variables related to where to find the input, where executing from, and where to store results
	;; ****************************************************************************************************************
	(setf *HPS_base_directory* basedir )
	(ensure-directories-exist *HPS_base_directory* )

	(setf *HPS_input_directory* (HPS_pathname basedir "HPS_INPUTS/"))
	(ensure-directories-exist *HPS_input_directory* )

	(setf *HPS_output_directory* outputdir)
	(ensure-directories-exist *HPS_output_directory* )

	(setf *HPS_input_timeseries_filename* (HPS_pathname *HPS_input_directory* inputfile))
	(setf *HPS_output_dataset_filename* (HPS_pathname *HPS_output_directory* datasetfile ))
	(setf *HPS_output_timeseries_filename* (HPS_pathname *HPS_output_directory* outputfile))
	(setf *HPS_output_report_filename* (HPS_pathname *HPS_base_directory* reportfile ))

	(if HPS_FULL_OUTPUT_MODE
		(setf *HPS_output_mode* 'FULL_OUTPUT_SLOW_PERFORMANCE)
		(setf *HPS_output_mode* 'LEAN_OUTPUT_FAST_PERFORMANCE))
	;; ****************************************************************************************************************


	;; ****************************************************************************************************************
	;; feedback the input parameter values
	;; ****************************************************************************************************************
	(when (NOT HPS_TERSE)
		(setf *HPS_PARAMETER_VALUES* (LIST 	INPUTFILE BASEDIR OUTPUTFILE OUTPUTDIR REPORTFILE DATASETFILE M MP 
					   		TIMESHIFT SEGLIMIT ALPHALEVEL 
							K FORECAST_WINSIZE SEGTRIVIAL MSERELAX MSEDELAY 
							PRINTDUR CONDITIONING_TYPE HPS_TRANSIENT_FILLER HPS_FULL_OUTPUT_MODE))
		(setf *HPS_PARAMETER_FIELDS* '(		INPUTFILE BASEDIR OUTPUTFILE OUTPUTDIR REPORTFILE DATASETFILE M MP 
							TIMESHIFT SEGLIMIT ALPHALEVEL 
							K FORECAST_WINSIZE SEGTRIVIAL MSERELAX MSEDELAY 
							PRINTDUR CONDITIONING_TYPE HPS_TRANSIENT_FILLER HPS_FULL_OUTPUT_MODE))
		(setf *HPS_FEEDBACK_MESSAGE* nil)
		(dolist (item *HPS_PARAMETER_FIELDS* 'FEEDBACKPARAMETERS) 
  			(push (list item '==> (pop *HPS_PARAMETER_VALUES*)) *HPS_FEEDBACK_MESSAGE*))
		(HPS_pretty_printer *HPS_FEEDBACK_MESSAGE* t :nchars 1 :headermsg  " PARAMETERS VALUES ARE	: 	")

		(format t "~% HPS Transform being executed from directory                        : [~S]" *HPS_base_directory* )
		(format t "~% Verifying path to input directory for the HPS transform to be  ....: [~S]" *HPS_input_directory* )
		(format t "~% Resultant HPS Transform data files being written to directory      : [~S]" *HPS_output_directory* )
		(format t "~% HPS Transform being applied to input time series in file           : [~S]" *HPS_input_timeseries_filename* )
		(format t "~% Computed dataset (approx. size (500 * N) B) will be written to file: [~S]" *HPS_output_dataset_filename* )
		(format t "~% Resultant HPS Approximation time series will be placed in file     : [~S]" *HPS_output_timeseries_filename* )
		(format t "~% Analysis of the resultant HPS Approximation will be placed in file : [~S]" *HPS_output_report_filename* )
		(format t "~% HPS output mode for the resultant HPS Approximation is set to      : [~S]" *HPS_output_mode* )
		nil)

	;; ****************************************************************************************************************


	;; ****************************************************************************************************************
	;; read the input time series from the file specified below on the given path
	;; ****************************************************************************************************************
	(setf *HPS_input_timeseries_dataset* (HPS_timeseries_reader *HPS_input_timeseries_filename* )) 
		(setf *HPS_input_timeseries_head* (HPS_reader_get_file_column -1 *HPS_input_timeseries_dataset* ))
		(setf *HPS_input_timeseries_indx* (HPS_reader_get_file_column  0 *HPS_input_timeseries_dataset* ))
		(setf *HPS_input_timeseries_data* (HPS_reader_get_file_column  1 *HPS_input_timeseries_dataset* ))
	;; ****************************************************************************************************************

	
	;; ****************************************************************************************************************
	;; apply any specialized conditioning, the DNA conditioning using ternary DNA-base tuples
	;; ****************************************************************************************************************
	(when 	CONDITIONING_TYPE
		(HPS_specialized_conditioning  tstest :cond_type CONDITIONING_TYPE))

	(unless CONDITIONING_TYPE
	  	nil)
	;; ****************************************************************************************************************


	;; ****************************************************************************************************************
	;; apply the HPS transform over the specified input series
	;; ****************************************************************************************************************
	(setf *HPS_transformed_response_dataset* (HPS_transform *HPS_input_timeseries_data*
								:M           	  M
								:MP 	     	  MP
							        :TIMESHIFT   	  TIMESHIFT
							        :SEGLIMIT    	  SEGLIMIT
							        :ALPHALEVEL  	  ALPHALEVEL
								:K           	  K
								:FORECAST_WINSIZE FORECAST_WINSIZE
								:SEGTRIVIAL  	  SEGTRIVIAL
								:MSERELAX    	  MSERELAX
								:MSEDELAY    	  MSEDELAY
								:PRINTDUR    	  PRINTDUR
								:FILLER_MODE	  HPS_TRANSIENT_FILLER
								:OUTPUT_MODE      HPS_FULL_OUTPUT_MODE ))
	

	;; ****************************************************************************************************************
	;; extract the results desired from the computation
	;; ****************************************************************************************************************
	(setf *HPS_transformed_outlier_dataset* (HPS_extract_ts_from *HPS_transformed_response_dataset* :label 'OUTLIER)) 
	(setf *HPS_transformed_error_dataset*   (HPS_extract_ts_from *HPS_transformed_response_dataset* :label 'ERROR)) 
	(setf *HPS_transformed_monitor_ts* 	(HPS_extract_ts_from *HPS_transformed_response_dataset* :label 'MONITOR)) 

	;; ****************************************************************************************************************
	;; write the full timeseries out to the terminal screen - use the pretty printer for this CHECK
	;; 	    (write *HPS_transformed_monitor_ts* :length (length *HPS_transformed_monitor_ts* ))
	

	;; ****************************************************************************************************************
	;; write the full timeseries out to the specified file
	;; ****************************************************************************************************************
	(when (NOT HPS_TERSE)
		(HPS_timeseries_printer *HPS_transformed_monitor_ts* *HPS_output_timeseries_filename* ))
	;; ****************************************************************************************************************


	;; ****************************************************************************************************************
	;; CHECK report file name specification not conformed in summary stats, need to pass filename
	;; ****************************************************************************************************************
	(when   HPS_FULL_OUTPUT_MODE 
		;; extract the internal computational state of the HPS transform for post-mortem analysis
	  	(setf *HPS_GLOBAL_computation_state* (HPS_get_internal_computational_state)) 

		;; perform offline analysis for the resultant approximation; computes summary statistics, histograms, 
	  	(HPS_compute_summary_statistics_for "*HPS_GLOBAL_computation_state*" :nbins 64) 

		;; print the computational state to a data file used for interfacing to gnupot
	  	(HPS_print_computational_state "*HPS_GLOBAL_computation_state*" 
	    			:output_filename *HPS_output_dataset_filename*
            			:from 0	  
	    			:upto (length *HPS_transformed_monitor_ts* )
	    			:appenddata nil ))

	(when (AND (NOT HPS_TERSE) (NOT HPS_FULL_OUTPUT_MODE))
		(setf *HPS_filename_temp* (format nil "~A~A" *HPS_output_directory* "HPS_HISTOGRAM_INPUT_SERIES.DAT"))
		(HPS_histogram *HPS_input_timeseries_data* *HPS_filename_temp* :numbins 32)
		(setf *HPS_filename_temp* (format nil "~A~A" *HPS_output_directory* "HPS_HISTOGRAM_HPS_APPROXIMATION.DAT"))
		(HPS_histogram *HPS_transformed_monitor_ts* *HPS_filename_temp* :numbins 32))

	;; ****************************************************************************************************************

	(return-from HPS_driver *HPS_transformed_monitor_ts*))	
;; ****************************************************************************************************************













