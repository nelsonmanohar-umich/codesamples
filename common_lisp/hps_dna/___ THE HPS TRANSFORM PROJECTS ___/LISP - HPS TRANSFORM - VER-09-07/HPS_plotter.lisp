(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;; ********************************************************************************************************
;; ************************************ HPS TIMESERIES PRINTING UTILITIES  ********************************
;;                      HPS DATA PLOTTER - WRITES DATA TO BE READ BY THE GNUPLOT INTERFACE
;; ************************************ HPS TIMESERIES PRINTING UTILITIES  ********************************
;; ********************************************************************************************************


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
;; this function extracts a data column from the results' dataset returned by the application of the HPS transform
;; if the label equals 'monitor, then the transformed time series is returned, 
;; if the label equals 'error, then a table containing the various computed error forms (i.e., slow, fast, and real),  
;; if the label equals 'outlier, then the series of identified heavy tail outliers is returned.
;; ********************************************************************************************************
(defun HPS_extract_ts_from ( dataset &key ( label 'MONITOR ))
  	(cond 	((equal label 'MONITOR) (return-from HPS_extract_ts_from (reverse (first dataset))))
		((equal label 'ERROR) 	(return-from HPS_extract_ts_from (reverse (second dataset))))
		((equal label 'OUTLIER) (return-from HPS_extract_ts_from (reverse (third dataset))))
		(t 			(return-from HPS_extract_ts_from (reverse (first dataset))))))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function extracts the internal computational state of the HPS transform, actually because the state
;; is quite huge (500 * N, N being the size of the input), this state is never passed but rather shared.
;; moreover, because the state was being as each iteration progressed, it is in a reversed order, so this
;; function reverses it (using a destructive operation of order N).  the computational state is defined 
;; as the snapshot of all (relevant) global variables evaluated at the end of each kernel iteration
;; ********************************************************************************************************
(defun HPS_get_internal_computational_state ()
  	(return-from HPS_get_internal_computational_state (reverse *HPS_computation_state* )))
;; ********************************************************************************************************


;; ********************************************************************************************
;; this function extracts one or more (up to four time series from the GLOBAL computational state 
;; ********************************************************************************************
(defun HPS_extract_timeseries_from ( col_1  &key      (col_2 nil) (col_3 nil) (col_4 nil))
  	(if col_4 
		(setf *HPS_histogram_ts* 
	       	(mapcar #'(lambda (a) (list (nth col_1 a) (nth col_2 a) (nth col_3 a) (nth col_4 a))) *HPS_GLOBAL_computation_state* ))
  	(if col_3 
		(setf *HPS_histogram_ts* 
	       	(mapcar #'(lambda (a) (list (nth col_1 a) (nth col_2 a) (nth col_3 a)))               *HPS_GLOBAL_computation_state* ))
  	(if col_2 
		(setf *HPS_histogram_ts* 
	      	(mapcar #'(lambda (a) (list (nth col_1 a) (nth col_2 a)))                             *HPS_GLOBAL_computation_state* ))
  	(if col_1 
		(setf *HPS_histogram_ts* 
	      	(mapcar #'(lambda (a)       (nth col_1 a))                                            *HPS_GLOBAL_computation_state* ))
		nil)))))
;; ********************************************************************************************


;; ********************************************************************************************
;; Note that such iteration value is specified by the value given to the field :upto. 
;; As a matter of fact, this function can be called at any point in time even during the execution 
;; of the HPS transform to examine/print the selected contents of the past computational state. 
;; Otherwise, this function could be implemented as a loop with an inner-loop that blindly 
;; dumps the tuple elements, unless when such is a list too, and then loops through each. 
;; Alternatively, applicative functions could be used instead to extract and print elements 
;; (even within sublists).
;; ********************************************************************************************
(defun HPS_print_computational_state ( RESULTANT_HPS_APPROXIMATION_DATASET_LABEL 
				       &key (from 0) (upto 0) (appenddata t) (output_filename "C:/HPS_DATASET.DAT")
			               (fielddescriptor 
							'(t 
							  t t t t t t t 
							  t t t t t t 
							  t t t t t 
							  t t t t t 
							  t t t  
							  t t t  
							  t t t t t t 
							  t t t t 
							  t t t t 
							  t t t t t t t 
							  t t t
					)))

	(setf *HPS_output_file* output_filename)
  	(format t "~& Now writing resultant dataset for resultant HPS approximation to   : [~S]" *HPS_output_file* )

	(setf *HPS_output_file_header_0*  '(ITER____ DEC_BIT_ FORECAST Y_I_____ LAMBDA__ INDEX_I_ SEGSTART ))
	(setf *HPS_output_file_header_1*  '(MP_START MP_END__ INX_PRES MC_START MC_END__ INX_RECP ))
	(setf *HPS_output_file_header_2*  '(CLIPPD_Y OUTL_LCL RUN_MEAN OUTL_UCL CLIPPING ))
	(setf *HPS_output_file_header_3*  '(TTESTVAL M_MEAN__ MP_MEAN_ M_VAR___ MP_VAR__ ))
	(setf *HPS_output_file_header_4*  '(M_______ MP______ TMAX____ ))
	(setf *HPS_output_file_header_5*  '(ERR_REAL ERR_SLOW ERR_FAST ))
	(setf *HPS_output_file_header_6*  '(MSEBND_S MSEBND_W MSEBND_I MSEMET_S MSEMET_W MSEMET_I ))
	(setf *HPS_output_file_header_7*  '(MEANDIFF CONFINTL CONFINTH POOLDVAR ))
	(setf *HPS_output_file_header_8*  '(MSE_MAX_ ERRCOSQI POOLDSTD MSE_ERRC ))
	(setf *HPS_output_file_header_9*  '(AUTODIAG Z_MEANS_ Z_ERRORS FRATIO_I CORRMEAN MIN_N_SZ REL_ERR_ ))
	(setf *HPS_output_file_header_10* '(SEGTRACK FINETRCK AUTO_MSE ))

	(setf *HPS_output_file_header* (append *HPS_output_file_header_0* *HPS_output_file_header_1*  *HPS_output_file_header_2* 
					       *HPS_output_file_header_3* *HPS_output_file_header_4*  *HPS_output_file_header_5*  
					       *HPS_output_file_header_6* *HPS_output_file_header_7*  *HPS_output_file_header_8*
					       *HPS_output_file_header_9* *HPS_output_file_header_10* ))

	;; this sets flags used to determine which fields are to be printed when extracted from the computational state.  
	;; there are over 46 fields and the first is always t, as it is the newline separator
	(setf *HPS_fielddescriptor_array* (coerce fielddescriptor 'VECTOR))

	(if (NOT appenddata) 
	  	(with-open-file (dataset_stream *HPS_output_file* :direction :output :if-exists :supersede :if-does-not-exist :create) 
		  	(dolist (field (rest fielddescriptor) 'FIELDDESCRIPTORS)
			  	(setf *HPS_output_file_fieldlabel* (pop *HPS_output_file_header* ))
				(if field 
					(format dataset_stream "~S	" *HPS_output_file_fieldlabel*) 
				  	nil)))) 


	(setf *HPS_iter* 1) 
	(with-open-file (dataset_stream *HPS_output_file* :direction :output :if-exists :append :if-does-not-exist :create)

	  	;; this inner loop is used to buffer the output of a number of iterations to memory before writing it to the specified file
	  	(with-output-to-string ( HPS_internal_buffer ) 

	  		(dolist (tuple *HPS_GLOBAL_computation_state* 'HPSCOMPSTATEWRITER) 
	  			(when (AND (>= *HPS_iter* from) (<= *HPS_iter* upto))
					(setf fieldnum 0) 
					(if (aref *HPS_fielddescriptor_array* fieldnum)
						(format HPS_internal_buffer "~%" ) 		        ;; print newline
						nil)
		
		  			;; field check starts at 1, first one is the new line
		  			(dolist (field (rest fielddescriptor) 'FIELDDESCRIPTORS)
			    			(setf *HPS_temp_field*	         (pop tuple)) 		;; retrieve the field
		
						(incf fieldnum)   
			    			(if (aref *HPS_fielddescriptor_array* fieldnum)	        ;; check whether field is enabled
			    				(format HPS_internal_buffer "~8S	" *HPS_temp_field*)
			  				nil)))

	       			(unless (AND (>= *HPS_iter* from) (<= *HPS_iter* upto))
    					nil)
	
				(when   (>= *HPS_iter* upto) 
	  				;; in case of lingered buffered output, write it to the specified file
					(princ (get-output-stream-string HPS_internal_buffer ) dataset_stream )
		   			(return-from HPS_print_computational_state nil))
	
				(unless (>= *HPS_iter* upto) 
					nil)
	
	  			;; this writes the memory buffered output to the specified file
				(when (equal (mod *HPS_iter* 100) 99)
					(princ (get-output-stream-string HPS_internal_buffer ) dataset_stream )
					nil)
	
	       			;; end of the do loop
	       			(incf *HPS_iter*))
	
	  			;; in case of remainder buffered output, write it to the specified file
				(when HPS_internal_buffer
					(princ (get-output-stream-string HPS_internal_buffer ) dataset_stream ))
				nil)))
;; ********************************************************************************************************





