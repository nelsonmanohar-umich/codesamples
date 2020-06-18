(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;; ********************************************************************************************************
;; ****************************       HPS COMPUTATION STATE DATASET     ***********************************
;; ***************  INDEX TO FIELDNAME TABLE USED TO PRINT/ACCESS COMPUTATIONAL STATE INTERNALS ***********
;; ********************************************************************************************************
;; 		001        002      003      004      005      006      007
;;		'(ITER____ DEC_BIT_ FORECAST Y_I_____ LAMBDA__ INDEX_I_ SEGSTART )
;;		008        009      010      011      012      013
;;		'(MP_START MP_END__ INX_PRES MC_START MC_END__ INX_RECP )
;;		014        015      016      017      018
;;		'(CLIPPD_Y OUTL_LCL RUN_MEAN OUTL_UCL CLIPPING )
;;		019        020      021      022      023
;;		'(TTESTVAL M_MEAN__ MP_MEAN_ M_VAR___ MP_VAR__ )
;;		024        025      026 
;;		'(M_______ MP______ TMAX____ )
;;		027        028      029
;;		'(ERR_REAL ERR_SLOW ERR_FAST )
;;		030        031      032      033      034      035
;;		'(MSEBND_S MSEBND_W MSEBND_I MSEMET_S MSEMET_W MSEMET_I )
;;		036        037      038      039
;;		'(MEANDIFF CONFINTL CONFINTH POOLDVAR )
;;		040        041      042      043
;;		'(MSE_MAX_ ERRCOSQI POOLDSTD MSE_ERRC )
;;		044        045      046      047      048      049      050
;;		'(AUTODIAG Z_MEANS_ Z_ERRORS FRATIO_I CORRMEAN MIN_N_SZ REL_ERR_ )
;;		051        052      053
;;		'(SEGTRACK FINETRCK AUTO_MSE)
;; ********************************************************************************************************
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


;; ********************************************************************************************
;; ********************************************************************************************
(setq *HPS_PRINT_SPECIAL_BLANK_MARKER*	(sqrt pi))
;; ********************************************************************************************


;; ********************************************************************************************
;; THE PRINTER FOR A SUMMARY STATISTIC - PRINTS NAME, DESCRIPTION, AND VALUE
;; ********************************************************************************************
(defun HPS_PRINTSTAT ( statname statdescription statvalue &key (level 1))
    (with-open-file (*HPS_stats_output-stream* *HPS_stats_output_file* :direction :output :if-exists :append ) 
  	(if (equal statvalue *HPS_PRINT_SPECIAL_BLANK_MARKER*)
	  	(setq *HPS_temp_print_statvalue* '__________ )
	  	(setq *HPS_temp_print_statvalue* statvalue ))

  	(cond 	((equal level 1) (format t "~&~46:S	~80:S	~10,3F" 
					 statname statdescription *HPS_temp_print_statvalue* ))
	  	((equal level 2) (format t "~&	~36:S	~80:S	~10,3F" 
					 statname statdescription *HPS_temp_print_statvalue* ))
	  	(T               (format t "~&		~28:S	~80:S	~10,3F" 
					 statname statdescription *HPS_temp_print_statvalue* )))

  	(format *HPS_stats_output-stream* "~&<TR> <TD WIDTH=400> <P CLASS=MSONORMAL>          ~70:S            </P> </TD>		
						  <TD WIDTH=100> <P CLASS=MSONORMAL> <STRONG> ~10,3F </STRONG> </P> </TD> 
					     </TR>" 
					 statdescription *HPS_temp_print_statvalue* )))
;; ********************************************************************************************

			
;; ********************************************************************************************
;; THE PRINTER FOR A HEADER LINES - PRINTS NAME, DESCRIPTION, AND VALUE
;; ********************************************************************************************
(defun HPS_PRINTSTATXY ( statdescription statname statvalue &key (level 1))
    	(with-open-file (*HPS_stats_output-stream* *HPS_stats_output_file* :direction :output :if-exists :append ) 
  		(format *HPS_stats_output-stream* "~& ~&
			</TABLE> 
			<BR> 
			<TABLE> 
			~& ~&"
			nil))

   	(setq tempstatdescription (format nil "<B> ~S </B>" statdescription ))
	(HPS_PRINTSTAT statname tempstatdescription statvalue :level level ))
;; ********************************************************************************************
  	

;; ********************************************************************************************
;; THE TRANSFORMATION OF THE ONLINE LAMBDA TIMESERIES (004) INTO AN OFFLINE SEGMENT DURATION
;; ********************************************************************************************
(defun HPS_compute_lambdastack ( )
	(setq *HPS_original_lambda_ts* (mapcar #'(lambda (a) (nth  4 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_lambdastack* nil)
	(setq *HPS_previouslambda* (first *HPS_original_lambda_ts* ))
	(dolist (lambdaitem *HPS_original_lambda_ts* 'HPSLAMBDATRIMMING)
	  	(cond   ((>  lambdaitem *HPS_previouslambda* )
		  		(push 0 *HPS_lambdastack*)
				(setq *HPS_previouslambda* lambdaitem))
		        ((<  lambdaitem *HPS_previouslambda* )
		  	        (push *HPS_previouslambda* *HPS_lambdastack*)
		  	        (setq *HPS_previouslambda* 0))
			(T
				(push *HPS_previouslambda* *HPS_lambdastack*)
				(setq *HPS_previouslambda* lambdaitem))))
	(setq *HPS_lambdastack* (reverse *HPS_lambdastack*))

	;; the full time series of lambda values at nontrivial ATS segments' endpoints
	(setq *HPS_lambda_at_endpoints_ts*     *HPS_lambdastack* ))
;; ********************************************************************************************


;; ********************************************************************************************
;; METRICS COMPUTED FOR NON-TRIVIAL SEGMENTS
;; ********************************************************************************************
(defun HPS_compute_fractalities ( )
	;; location of segments by endpoint and duration
	(HPS_compute_lambdastack )

	;; nontrivial segment threshold definition
	(HPS_PRINTSTAT "*HPS_ATS_TRIVIAL_SEGMENT_DURATION*" 
		       "(duration of a trivial ATS segments is equal/less than this)" *HPS_ATS_TRIVIAL_SEGMENT_DURATION* :level 2 )

	;; the set of nontrivial segments - identified by segment duration at its endpoints
	(setq *HPS_non_trivial_segments* (remove-if-not #'(lambda (a) (> a *HPS_ATS_TRIVIAL_SEGMENT_DURATION*)) *HPS_lambda_at_endpoints_ts* ))

	;; the HPS fractality or number of nontrivial segments
	(setq *HPS_fractality* (length *HPS_non_trivial_segments*))
	(HPS_PRINTSTAT "*HPS_fractality*" 
		       "(number of nontrivial ATS segments)" *HPS_fractality* :level 2 )

	;; the total amount of unearthed true or approximate stationarity
	(setq *HPS_stationarity* (reduce #'+ *HPS_non_trivial_segments* ))
	(HPS_PRINTSTAT "*HPS_stationarity*" 
		       "(sum of the duration of all nontrivial ATS segments)" *HPS_stationarity* :level 2 )

	;; the average duration of nontrivial segments
	(setq *HPS_average_segment_duration* (HPS_mean *HPS_non_trivial_segments*))
	(HPS_PRINTSTAT "*HPS_average_segment_duration*" 
		       "(mean across all nontrivial ATS segments)" *HPS_average_segment_duration* :level 2 )

	;; the set of printable nontrivial segments - identified by segment duration at its endpoints
	(setq *HPS_significant_segments* (remove-if-not #'(lambda (a) (> a *HPS_ATS_PRINTABLE_SEGMENT_DURATION*)) *HPS_lambda_at_endpoints_ts* ))
	
	;; large (aka usable, printable, significant) segment threshold definition
	(HPS_PRINTSTAT "*HPS_ATS_PRINTABLE_SEGMENT_DURATION*" 
		       "(minimum duration of large ATS segments)" *HPS_ATS_PRINTABLE_SEGMENT_DURATION* :level 2 )

	;; the number of printable segments
	(setq *HPS_largeseg_fractality* (length *HPS_significant_segments*))
	(HPS_PRINTSTAT "*HPS_largeseg_fractality*" 
		       "(number of large ATS segments)" *HPS_largeseg_fractality* :level 2 )

	;; the total amount of unearthed true or approximate stationarity
	(setq *HPS_largeseg_stationarity* (reduce #'+ *HPS_significant_segments* ))
	(HPS_PRINTSTAT "*HPS_largeseg_stationarity*" 
		       "(sum of the duration of all large ATS segments)" *HPS_largeseg_stationarity* :level 2 )

	;; the average duration of significant segments
	(setq *HPS_largeseg_avg_duration* (HPS_mean *HPS_significant_segments* ))
	(HPS_PRINTSTAT "*HPS_largeseg_avg_duration*" 
		       "(mean duration for large ATS segments)" *HPS_largeseg_avg_duration* :level 2 )

	;; the maximum segment duration
	(setq *HPS_maximum_segment_duration* (HPS_maximum *HPS_significant_segments*))
	(HPS_PRINTSTAT "*HPS_maximum_segment_duration*" 
		       "(observed maximum duration of ATS segments)" *HPS_maximum_segment_duration* :level 2 ))
;; ********************************************************************************************


;; ********************************************************************************************
;; RATIO METRICS RELATED TO INFORMATION CONTENT 
;; ********************************************************************************************
(defun HPS_compute_information_metrics ()
  	;; the number of samples in the input series - unknown to the online kernel but loaded here for optional offline analysis
	(setq *HPS_num_samples* (length *HPS_input_series*))
	(HPS_PRINTSTAT "*HPS_num_samples*" 
		       "(number of samples in HPS input series)" *HPS_num_samples* :level 2 )

	;; the percentage of displayed, significant, or usable stationary from nontrivial segments
	(if (equal *HPS_stationarity* 0) 
		(setq *HPS_usable_stationarity* 0)
		(setq *HPS_usable_stationarity* (* (/ *HPS_largeseg_stationarity* *HPS_stationarity* ) 100)))
	(HPS_PRINTSTAT "*HPS_usable_stationarity*" 
		       "(percentage of stationary due to large ATS segments)" *HPS_usable_stationarity* :level 2 )

	;; the irreduceability of the input series from samples into segments, i.e., degree of fine tracking pressent
	(setq *HPS_irreduceability* (- *HPS_num_samples* *HPS_stationarity*))
	(HPS_PRINTSTAT "*HPS_irreduceability*" 
		       "(resultant number of fine-grain tracking forecasts)" *HPS_irreduceability* :level 2 )

	;; the number of (ternary) tokens needed to represent the input series using the HPS approximation (segments + fine tracking)
	(setq *HPS_representation_size* (+ *HPS_fractality* *HPS_irreduceability* ))
	(HPS_PRINTSTAT "*HPS_representation_size*" 
		       "(sum of coarse-tracking plus fine-tracking elements)" *HPS_representation_size* :level 2 )

	;; the HPS compressibility of the input series from samples
	(if (equal *HPS_num_samples* 0) 
		(setq *HPS_compressibility* 0)
		(setq *HPS_compressibility* (* (- 1 (/ *HPS_representation_size* *HPS_num_samples* )) 100)))
	(HPS_PRINTSTAT "*HPS_compressibility*" 
		       "(compression savings b/w HPS approximation vs. input signal)" *HPS_compressibility* :level 2 ))
;; ********************************************************************************************


;; ********************************************************************************************
;; DECISION SIGNAL STATS
;; ********************************************************************************************
(defun HPS_compute_decisionsignal_metrics ( )
	;; the lambda-based indicator series, binary indicator for the presence of a segment signal - 
	;; it is used as a "identity vector" to generate dot products wrt other columns
	(setq *HPS_segment_span_identifier_ts* (mapcar #'(lambda (a) (if (> a *HPS_ATS_TRIVIAL_SEGMENT_DURATION*) 1 0)) *HPS_original_lambda_ts* ))

	;; the series of lambda values at nontrivial ATS segments' endpoints
	(setq *HPS_segment_endp_identifier_ts* (mapcar #'(lambda (a) (if (> a 0) 1 0)) *HPS_lambda_at_endpoints_ts*   ))

	;; the multivalued decision signal
	(setq *HPS_multiv_decision_signal_ts* (mapcar #'(lambda (a) (nth  1 a)) *HPS_GLOBAL_computation_state* ))

	;; the binary decision signal - it is used as a "identity vector" to generate dot products wrt other columns
	;; (setq *HPS_binary_decision_signal_ts* (mapcar #'(lambda (a) (if (> a *HPS_ATS_DM_STATIONARY_FAILED*) 1 0)) *HPS_multiv_decision_signal_ts* ))
	;; the number of decision signal resets - this does not represent the number of ATS segments as it includes trivial states
	;; (setq *HPS_num_decision_resets* (reduce #'+ *HPS_binary_decision_signal_ts* ))
	;; (HPS_PRINTSTAT "*HPS_num_decision_resets* (number of detections made)  " *HPS_num_decision_resets* :level 2 )

	;; the number of MSEmax exceeded constraint resets found in the multivalued decision signal
	(setq *HPS_num_MSEbound_resets* (length (remove-if-not #'(lambda (a) (equal a *HPS_ATS_DM_MSEERR_CONSTRAINT*)) *HPS_multiv_decision_signal_ts* )))
	(HPS_PRINTSTAT "*HPS_num_MSEbound_resets*" 
		       "(number of 'maximum MSE bound exceeded' events)" *HPS_num_MSEbound_resets* :level 2 )

	;; the number of SEGmax exceeded constraint resets found in the multivalued decision signal
	(setq *HPS_num_SEGbound_resets* (length (remove-if-not #'(lambda (a) (equal a *HPS_ATS_DM_LAMBDA_CONSTRAINT*)) *HPS_multiv_decision_signal_ts* )))
	(HPS_PRINTSTAT "*HPS_num_SEGbound_resets*" 
		       "(number of 'maximum ATS segment duration exceeded' events)" *HPS_num_SEGbound_resets* :level 2 )
	
	;; the density of MSEmax resets - if fit is good density is low
	(setq *HPS_MSEbound_resets_density* (* (/ *HPS_num_MSEbound_resets* *HPS_num_samples*) 100))
	(HPS_PRINTSTAT "*HPS_MSEbound_resets_density*" 
		       "(density of MSE-reset events across input signal)" *HPS_MSEbound_resets_density* :level 2 ))
;; ********************************************************************************************


;; ********************************************************************************************
;; DOT PRODUCT WITH RESPECT TO ATS SEGMENT SPANS
;; ********************************************************************************************
(defun HPS_compute_dotproduct_wrt_segment_span_for ( atimeseries )
	(setq *HPS_histogram_ts_2_subset* (remove-if-not #'(lambda (a) (> (abs a) 0)) 
					  (mapcar #'(lambda (a b) (* a b )) atimeseries *HPS_segment_span_identifier_ts* ))))
;; ********************************************************************************************
	

;; ********************************************************************************************
;; ERROR STATS
;; ********************************************************************************************
(defun HPS_compute_simpleerror_metrics ()
	;; average and deviation error for HPS real error 
	(setq *HPS_histogram_ts* (mapcar #'(lambda (a) (nth 26 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_real_error_mean* (HPS_mean   *HPS_histogram_ts*))
	(setq *HPS_real_error_stdd* (HPS_stddev *HPS_histogram_ts* *HPS_real_error_mean* ))
	(HPS_PRINTSTAT "*HPS_real_error_mean*" 
		       "(mean of the true residuals)" *HPS_real_error_mean* :level 2 )
	(HPS_PRINTSTAT "*HPS_real_error_stdd*" 
		       "(standard deviation of the true residuals)" *HPS_real_error_stdd* :level 2 )

	(setq *HPS_histogram_ts_2_subset* (HPS_compute_dotproduct_wrt_segment_span_for *HPS_histogram_ts* ))
	(setq *HPS_real_error_mean_segset* (HPS_mean   *HPS_histogram_ts_2_subset* ))
	(setq *HPS_real_error_stdd_segset* (HPS_stddev *HPS_histogram_ts_2_subset* *HPS_real_error_mean_segset* ))
	(HPS_PRINTSTAT "*HPS_real_error_mean_segset*" 
		       "(mean of true residuals across nontrivial ATS segments)" *HPS_real_error_mean_segset* :level 2 )
	(HPS_PRINTSTAT "*HPS_real_error_stdd_segset*" 
		       "(standard deviation across nontrivial ATS segments)" *HPS_real_error_stdd_segset* :level 2 )

	;; average and deviation error for HPS slow error 
	(setq *HPS_histogram_ts* (mapcar #'(lambda (a) (nth 27 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_slow_error_mean* (HPS_mean   *HPS_histogram_ts*))
	(setq *HPS_slow_error_stdd* (HPS_stddev *HPS_histogram_ts* *HPS_slow_error_mean* ))
	(HPS_PRINTSTAT "*HPS_slow_error_mean*" 
		       "(mean for residuals derived from HPS slow error)" *HPS_slow_error_mean* :level 2 )
	(HPS_PRINTSTAT "*HPS_slow_error_stdd*" 
		       "(stddev for residuals derived from HPS slow error)" *HPS_slow_error_stdd* :level 2 )

	;; average and deviation error for HPS fast error 
	(setq *HPS_histogram_ts* (mapcar #'(lambda (a) (nth 28 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_fast_error_mean* (HPS_mean   *HPS_histogram_ts*))
	(setq *HPS_fast_error_stdd* (HPS_stddev *HPS_histogram_ts* *HPS_fast_error_mean* ))
	(HPS_PRINTSTAT "*HPS_fast_error_mean*" 
		       "(mean for residuals derived from HPS fast error)" *HPS_fast_error_mean* :level 2 )
	(HPS_PRINTSTAT "*HPS_fast_error_stdd*" 
		       "(stddev for residuals derived from HPS fast error)" *HPS_fast_error_stdd* :level 2 )
	
	;; average and deviation error for HPS relative error 
	(setq *HPS_histogram_ts* (mapcar #'(lambda (a) (nth 49 a)) *HPS_GLOBAL_computation_state* ))
	;; relative error HAS NILS!!!!!!!!!!!!! within - when the signal values are negative
	(setq *HPS_relt_error_mean* (HPS_mean   *HPS_histogram_ts*))
	(setq *HPS_relt_error_stdd* (HPS_stddev *HPS_histogram_ts* *HPS_relt_error_mean* ))
	(HPS_PRINTSTAT "*HPS_relt_error_mean*" 
		       "(mean for relative error residuals)" *HPS_relt_error_mean* :level 2 )
	(HPS_PRINTSTAT "*HPS_relt_error_stdd*" 
		       "(stddev for relative error residuals)" *HPS_relt_error_stdd* :level 2 )

	(setq *HPS_histogram_ts_2_subset* (HPS_compute_dotproduct_wrt_segment_span_for *HPS_histogram_ts* ))
	(setq *HPS_relt_error_mean_segset* (HPS_mean   *HPS_histogram_ts_2_subset* ))
	(setq *HPS_relt_error_stdd_segset* (HPS_stddev *HPS_histogram_ts_2_subset* *HPS_relt_error_mean_segset* ))
	(HPS_PRINTSTAT "*HPS_relt_error_mean_segset*" 
		       "(mean for relative error residuals across nontrivial ATS segments)" *HPS_relt_error_mean_segset* :level 2 )
	(HPS_PRINTSTAT "*HPS_relt_error_stdd_segset*" 
		       "(stddev for relative error residuals across nontrivial ATS segments)" *HPS_relt_error_stdd_segset* :level 2 ))
;; ********************************************************************************************


;; ********************************************************************************************
;; MSE ERROR STATS
;; ********************************************************************************************
(defun HPS_compute_mseerror_metrics ()
	(setq *HPS_histogram_ts_1* (mapcar #'(lambda (a) (nth 33 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_histogram_ts_2_subset* (HPS_compute_dotproduct_wrt_segment_span_for *HPS_histogram_ts_1* ))
	
	;; total MSE error form
	(setq *HPS_MSE_total_forseg*  (reduce #'+ *HPS_histogram_ts_2_subset* ))
	(setq *HPS_MSE_total_overall* (reduce #'+ *HPS_histogram_ts_1*))
	(HPS_PRINTSTAT "*HPS_MSE_total_forseg*" 
		       "(total windowed MSE form (for m') across nontrivial ATS segments)" *HPS_MSE_total_forseg*  :level 2 )
	(HPS_PRINTSTAT "*HPS_MSE_total_overall*" 
		       "(total windowed MSE form (for m') across all samples)" *HPS_MSE_total_overall* :level 2 )

	;; average MSE error form per ATS segment
	(setq *HPS_MSE_mean_forseg*   (HPS_mean *HPS_histogram_ts_2_subset* ))
	(setq *HPS_MSE_mean_overall*  (HPS_mean *HPS_histogram_ts_1*))
	(HPS_PRINTSTAT "*HPS_MSE_mean_forseg*" 
		       "(mean of windowed MSE form across non trivial ATS segments)" *HPS_MSE_mean_forseg*   :level 2 )
	(HPS_PRINTSTAT "*HPS_MSE_mean_overall*" 
		       "(mean of windowed MSE form across all samples)" *HPS_MSE_mean_overall*  :level 2 )

	;; stddev on average MSE error form per ATS segment
	(setq *HPS_MSE_stdd_forseg*   (HPS_stddev *HPS_histogram_ts_2_subset* *HPS_MSE_mean_forseg* ))
	(setq *HPS_MSE_stdd_overall*  (HPS_stddev *HPS_histogram_ts_1*        *HPS_MSE_mean_overall* ))
	(HPS_PRINTSTAT "*HPS_MSE_stdd_forseg*" 
		       "(stddev of windowed MSE form across nontrivial ATS segments)" *HPS_MSE_stdd_forseg*   :level 2 )
	(HPS_PRINTSTAT "*HPS_MSE_stdd_overall*" 
		       "(stddev of windowed MSE form across all samples)" *HPS_MSE_stdd_overall*  :level 2 ))
;; ********************************************************************************************


;; ********************************************************************************************
;; SUM OF Z-SCORES WRT FAST ERROR SIGNAL STATS
;; ********************************************************************************************
(defun HPS_compute_zscore_metrics ()
	(setq *HPS_histogram_ts_1* (mapcar #'(lambda (a) (nth 45 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_histogram_ts_2_subset* (HPS_compute_dotproduct_wrt_segment_span_for *HPS_histogram_ts_1* ))

	;; TOTAL SUM OF SQUARES OF Z-SCORES ERROR FORM
	;; (setq *HPS_histogram_ts_2* (mapcar #'(lambda (a b) (abs (* a b ))) *HPS_histogram_ts_1* *HPS_segment_span_identifier_ts* ))
	;; (setq *HPS_SZS_range_forseg*  (HPS_mean *HPS_histogram_ts_2*))
	;; (HPS_PRINTSTAT "*HPS_SZS_range_forseg* (avg. abs z-scores segms (temp) " *HPS_SZS_range_forseg*  :level 2 )
	;; (setq *HPS_histogram_ts_2* (mapcar #'(lambda (a) (abs a )) *HPS_histogram_ts_1* ))
	;; (setq *HPS_SZS_range_overall* (HPS_mean *HPS_histogram_ts_2*))
	;; (HPS_PRINTSTAT "*HPS_SZS_range_overall* (avg. abs z-scores all samples " *HPS_SZS_range_overall* :level 2 )

	;; AVERAGE SZS ERROR FORM PER ATS SEGMENT
	(setq *HPS_SZS_mean_forseg*   (HPS_mean *HPS_histogram_ts_2_subset* ))
	(setq *HPS_SZS_mean_overall*  (HPS_mean *HPS_histogram_ts_1*))
	(HPS_PRINTSTAT "*HPS_SZS_mean_overall*" 
		       "(mean z-scores for fast error residuals - across all samples)" *HPS_SZS_mean_overall*  :level 2 )
	(HPS_PRINTSTAT "*HPS_SZS_mean_forseg*" 
		       "(mean z-scores for fast error residuals - nontrivial ATS segments)" *HPS_SZS_mean_forseg*   :level 2 )

	;; STDDEV ON AVERAGE SZS ERROR FORM PER ATS SEGMENT
	(setq *HPS_SZS_stdd_forseg*   (HPS_stddev *HPS_histogram_ts_2_subset* *HPS_SZS_mean_forseg* ))
	(setq *HPS_SZS_stdd_overall*  (HPS_stddev *HPS_histogram_ts_1*        *HPS_SZS_mean_overall* ))
	(HPS_PRINTSTAT "*HPS_SZS_stdd_overall*" 
		       "(stddev of z-scores for fast error residuals - across all samples)" *HPS_SZS_stdd_overall*  :level 2 )
	(HPS_PRINTSTAT "*HPS_SZS_stdd_forseg*" 
		       "(stddev of z-scores for fast error residuals - nontrivial ATS segments)" *HPS_SZS_stdd_forseg*   :level 2 ))
;; ********************************************************************************************


;; ********************************************************************************************
;; AVERAGE AND STD OVER WINDOWED CORRELATION INDEXES OVER WINDOWED OUTLOOKS
;; ********************************************************************************************
(defun HPS_compute_pcorrindex_metrics ()
	(setq *HPS_histogram_ts_1* (mapcar #'(lambda (a) (nth 47 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_histogram_ts_2_subset* (HPS_compute_dotproduct_wrt_segment_span_for *HPS_histogram_ts_1* ))

	;; AVERAGE WINDOWED CORRELATION INDEX FORM PER ATS SEGMENT
	(setq *HPS_pco_mean_forseg*   (HPS_mean *HPS_histogram_ts_2_subset* ))
	(setq *HPS_pco_mean_overall*  (HPS_mean *HPS_histogram_ts_1*))
	(HPS_PRINTSTAT "*HPS_pco_mean_overall*" 
		       "(mean corr. index between windowed outlooks - across all samples)" *HPS_pco_mean_overall*  :level 2 )
	(HPS_PRINTSTAT "*HPS_pco_mean_forseg*" 
		       "(mean corr. index b/w windowed outlooks - nontrivial ATS segments)" *HPS_pco_mean_forseg*   :level 2 )

	;; STDDEV ON AVERAGE WINDOWED CORRELATION INDEX FORM PER ATS SEGMENT
	(setq *HPS_pco_stdd_forseg*   (HPS_stddev *HPS_histogram_ts_2_subset* *HPS_pco_mean_forseg* ))
	(setq *HPS_pco_stdd_overall*  (HPS_stddev *HPS_histogram_ts_1*        *HPS_pco_mean_overall* ))
	(HPS_PRINTSTAT "*HPS_pco_stdd_overall*" 
		       "(stddev of corr. index between windowed outlooks - across all samples)" *HPS_pco_stdd_overall*  :level 2 )
	(HPS_PRINTSTAT "*HPS_pco_stdd_forseg*" 
		       "(stddev of corr. index b/w windowed outlooks - nontrivial ATS segments)" *HPS_pco_stdd_forseg*   :level 2 ))
;; ********************************************************************************************


;; ********************************************************************************************
;; AVERAGE WINDOWED CORRELATION INDEX FORM PER ATS SEGMENT
;; ********************************************************************************************
(defun HPS_compute_outlier_metrics ()
	;; OUTLIER STATISTICS ACROSS ALL SAMPLES
	(setq *HPS_histogram_ts_1* (mapcar #'(lambda (a) (nth 17 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_histogram_ts_3* (remove-if-not #'(lambda (a) (> a 0)) *HPS_histogram_ts_1*))

	(setq *HPS_number_outliers*   (length *HPS_histogram_ts_3*))
	(HPS_PRINTSTAT "*HPS_number_outliers*" 
		       "(total number of HPS outlier events across all samples)" *HPS_number_outliers* :level 2 )

	(if (> *HPS_number_outliers* 0)
		(setq *HPS_mean_outlier_clipping* (HPS_mean *HPS_histogram_ts_3* ))
		(setq *HPS_mean_outlier_clipping* 0))
	(HPS_PRINTSTAT "*HPS_mean_outlier_clipping*" 
		       "(average outlier-event clipping across all samples)" *HPS_mean_outlier_clipping* :level 2 )

	(setq *HPS_number_outliers_density* (* (/ *HPS_number_outliers*  *HPS_num_samples*) 100))
	(HPS_PRINTSTAT "*HPS_number_outliers_density*" 
		       "(density of outliers across all samples)" *HPS_number_outliers_density* :level 2 )

	;; OUTLIER STATISTICS ONLY ACROSS SEGMENTS
	(setq *HPS_histogram_ts_2_subset* (HPS_compute_dotproduct_wrt_segment_span_for *HPS_histogram_ts_1* ))
	(setq *HPS_histogram_ts_3* (remove-if-not #'(lambda (a) (> a 0)) *HPS_histogram_ts_2_subset* ))

	(setq *HPS_number_outliers_insegs*   (length *HPS_histogram_ts_3*))
	(HPS_PRINTSTAT "*HPS_number_outliers_insegs*" 
		       "(total number of outliers found within nontrivial ATS segments)" *HPS_number_outliers_insegs* :level 2 )

	(if (> *HPS_number_outliers_insegs* 0)
		(setq *HPS_mean_outlier_clipping_insegs* (HPS_mean *HPS_histogram_ts_3*))
		(setq *HPS_mean_outlier_clipping_insegs* 0))
	(HPS_PRINTSTAT "*HPS_mean_outlier_clipping_insegs*" 
		       "(average outlier-event clipping within nontrivial ATS segments)" *HPS_mean_outlier_clipping_insegs* :level 2 )

	(setq *HPS_outlier_density_insegs* (* (/ *HPS_number_outliers_insegs*  *HPS_num_samples*) 100))
	(HPS_PRINTSTAT "*HPS_outlier_density_insegs*" 
		       "(density of outliers within the span of nontrivial ATS segments)" *HPS_outlier_density_insegs* :level 2 ))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function prints out input parameters to the terminal as well as to the file specified
;; ********************************************************************************************
(defun HPS_print_input_parameters ()
	(HPS_PRINTSTAT "HPS_m" "(the size of the windowed outlook into the RECENT PAST)" *HPS_m* :level 2 )
	(HPS_PRINTSTAT "HPS_m'" "(the size of the windowed outlook into the PRESENT)" *HPS_mp* :level 2 )
	(HPS_PRINTSTAT "HPS_alpha" "(the alpha level to use in HPS decision-making)" *HPS_alpha* :level 2 )
	(HPS_PRINTSTAT "HPS_tmax" "(the probability limit used in decision making)" *HPS_tmax* :level 2 )

	(HPS_PRINTSTAT "HPS_K" "(the number of sigma levels used to detect heavy tail outliers)" *HPS_K* :level 2 )
	(HPS_PRINTSTAT "TIMESHIFT" "(the delay at which the RECENT PAST outlook pivots)" *HPS_timeshift* :level 2 )
	(HPS_PRINTSTAT "OVERLAP" "the overlap between windowed outlooks" (- *HPS_mp* *HPS_timeshift*) :level 2 )
	(HPS_PRINTSTAT "SEGLIMIT" "(the maximum duration of ATS segments)" *HPS_MAX_SEGMENT_DURATION* :level 2 )
	(HPS_PRINTSTAT "TRIVIALSEGLIMIT" "(the threshold for detecting trivial ATS segments)" *HPS_ATS_TRIVIAL_SEGMENT_DURATION* :level 2 )
	(HPS_PRINTSTAT "MSE-RELAXATION FACTOR" "(a scaling factor to apply over MSE constraintment)" *HPS_ATS_MSEERR_RELAXATION_FC* :level 2 )

	(HPS_PRINTSTAT "MSE-BUILDUP DELAY" "(a delay to wait before applying MSE constraintment)" *HPS_ATS_MSEERR_BUILDUP_DELAY* :level 2 )
	(HPS_PRINTSTAT "LARGE SEGMENT THRESHOLD" "(pivot for detecting large ATS segments)" *HPS_ATS_PRINTABLE_SEGMENT_DURATION* :level 2 )
	(HPS_PRINTSTAT "HISTOGRAM NBINS" "(number of bins used to produce histograms)" *HPS_histogram_nbins* :level 2 )
	(HPS_PRINTSTAT "HPS INPUT SERIES" "(the first element of the input series)" (first *HPS_input_series*) :level 2))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function a closeup of the resultant approximation to the specified file, one per file
;; ********************************************************************************************
(defun HPS_print_timeseries_closeups ( &key (numcloseups 6))
	;; get the resultant HPS approximation's forecast timeseries (002) 
	(setq *HPS_histogram_ts* (HPS_extract_timeseries_from 2 :col_2 3 :col_3 51 :col_4 52))

	;; the number of elements in the resultant HPS approximation
	(setq *HPS_subsequence_size* (length *HPS_histogram_ts*))
	(setq *HPS_subsequence_size* (floor (/ *HPS_subsequence_size* numcloseups ))) 

	;; for efficiency, since current histogram setup loops through the data for each bin
	(setq *HPS_histogram_array* (coerce *HPS_histogram_ts* 'VECTOR)) 

	(dotimes (index numcloseups 'HPS_SERIES_CLOSEUPS)
		(setq *HPS_index_start*      (*    index    *HPS_subsequence_size* ))
		(setq *HPS_index_end*        (* (+ index 1) *HPS_subsequence_size* ))
		(setq *HPS_approximation_subseq* (coerce (subseq *HPS_histogram_array* *HPS_index_start* *HPS_index_end* ) 'LIST))

		;; for clarity construct *here* the full filename each time
		(setq *HPS_filename_index*  (format nil  "~S.DAT" index ))
		(setq *HPS_subseq_filename* (HPS_pathname *HPS_output_directory* "HPS_SERIES_CLOSEUP_"))
		(setq *HPS_subseq_filename* (HPS_pathname *HPS_subseq_filename* *HPS_filename_index* ))

		;; print each different closeup timeseries into the specified file
		(HPS_timeseries_printer *HPS_approximation_subseq* *HPS_subseq_filename* :from *HPS_index_start* :to *HPS_index_end*
					:xy_headerline "i	y(i)	HPS(y(i))	ATSSEG(i)	FINETRK(i)")))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function compute optional offline metrics about an HPS approximation, note that these 
;; may be time consuming and increase the time complexity of the run.
;; ********************************************************************************************
(defun HPS_compute_stats_for_hps ( )
	(setq *HPS_stats_output-stream* nil)

	;; ************************************************************************************
	;; HEADER AND FOOTER FOR HTML REPORT 
	;; ************************************************************************************
	(setq *HPS_html_header* " 
<HTML> 
<HEAD> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:7.0pt; font-family:Verdana;}
@page Section1 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in;}
div.Section1 {page:Section1;}
-->
</STYLE> </HEAD> 
<BODY LANG=EN-US> 
<DIV CLASS=SECTION1 ALIGN=CENTER> 
	")

	(setq *HPS_html_footer* " 
</DIV> 
</BODY> 
</HTML> 
	" )   
	;; ************************************************************************************

	(setq *HPS_stats_output_file* (HPS_pathname *HPS_output_directory* "HPS_SUMM_STATS_1.DAT" ))
	(format t "~& HPS approximation's summary statistics being written to file       : [~S]" *HPS_stats_output_file* ) 

	(with-open-file (*HPS_stats_output-stream* *HPS_stats_output_file* :direction :output :if-exists :supersede :if-does-not-exist :create) 
  		(format *HPS_stats_output-stream* "~&~A" *HPS_html_header* )
  		(format *HPS_stats_output-stream* "~&<TABLE>" nil)
  		(format *HPS_stats_output-stream* "~&SUMMARY STATISTICS FOR HPS APPROXIMATION FOR ~& <BR> [~S]" *HPS_input_timeseries_filename* ))

	(HPS_PRINTSTATXY "PARAMETER OR SUMMARY STATISTIC_______________" 
		       "DESCRIPTION OF THE PARAMETER OR SUMMARY STATISTIC__________" "_____VALUE")

	;; ************************************************************************************
	;; printout of the input parameters used to generate this resultant HPS approximation
	;; ************************************************************************************
	(HPS_PRINTSTATXY "HPS TRANSFORM _ PARAMETER VALUES USED" 	
		       "_____________________________________________________" *HPS_PRINT_SPECIAL_BLANK_MARKER*)
	(HPS_print_input_parameters )


	;; ************************************************************************************
	;; stability of the HPS approximation
	;; ************************************************************************************
	(HPS_PRINTSTATXY "HPS FRACTALITY STATS (NUMBER OF ATS SEGMENTS)" 	
		       "_____________________________________________________" *HPS_PRINT_SPECIAL_BLANK_MARKER*)
	(HPS_compute_fractalities )


	;; ************************************************************************************
	;; rudimentary information content measures of the HPS approximation
	;; ************************************************************************************
	(HPS_PRINTSTATXY "INF_CONTENT STATS FOR RESULTANT HPS APPROX" 	
		       "_____________________________________________________" *HPS_PRINT_SPECIAL_BLANK_MARKER*)
	(HPS_compute_information_metrics )


	;; ************************************************************************************
	;; metrics about the decision making process and nature of decisions 
	;; ************************************************************************************
	(HPS_PRINTSTATXY "HPS DECISION SIGNAL STATS" 	
		       "_____________________________________________________" *HPS_PRINT_SPECIAL_BLANK_MARKER*)
	(HPS_compute_decisionsignal_metrics )

	(with-open-file (*HPS_stats_output-stream* *HPS_stats_output_file* :direction :output :if-exists :append ) 
  		(format *HPS_stats_output-stream* "~&</TABLE>" nil)
  		(format *HPS_stats_output-stream* "~&~A" *HPS_html_footer*))

	(setq *HPS_stats_output_file* (HPS_pathname *HPS_output_directory* "HPS_SUMM_STATS_2.DAT" ))
	(format t "~& HPS approximation's summary statistics being written to file       : [~S]" *HPS_stats_output_file* ) 

	(with-open-file (*HPS_stats_output-stream* *HPS_stats_output_file* :direction :output :if-exists :supersede :if-does-not-exist :create) 
  		(format *HPS_stats_output-stream* "~&~A " *HPS_html_header* )
  		(format *HPS_stats_output-stream* "~&<TABLE> " nil)
  		(format *HPS_stats_output-stream* "~&SUMMARY STATISTICS FOR HPS APPROXIMATION FOR ~& <BR> [~S] " *HPS_input_timeseries_filename* ))

	;; ************************************************************************************
	;; accuracy of the forecast 	__> on well on target are they
	;; bias of the forecast		__> what overall bias they exhibit
	;; ************************************************************************************
	(HPS_PRINTSTATXY "HPS RESIDUALS STATS FOR VARIOUS ERRORS" 	
		       "_____________________________________________________" *HPS_PRINT_SPECIAL_BLANK_MARKER*)
	(HPS_compute_simpleerror_metrics )


	;; ************************************************************************************
	;; overall comparative metric	__> how do you compare multiple runs
	;; precision of the forecast 	__> how tight they follow the target
	;; ************************************************************************************
	(HPS_PRINTSTATXY "HPS MSE STATS FOR SPECIAL ERROR FORM" 	
		       "_____________________________________________________" *HPS_PRINT_SPECIAL_BLANK_MARKER*)
	(HPS_compute_mseerror_metrics )


	;; ************************************************************************************
	;; precision of the forecast 	__> how tight they follow the target
	;; consistency of the forecast	__> how varied is this tightness
	;; bias of the forecast		__> what overall bias they exhibit
	;; ************************************************************************************
	(HPS_PRINTSTATXY "HPS Z_SCORE STATS FOR SELECTED RESIDUALS" 	
		       "_____________________________________________________" *HPS_PRINT_SPECIAL_BLANK_MARKER*)
	(HPS_compute_zscore_metrics )

	;; ************************************************************************************
	;; correlation index metrics, presence of correlation between windowed outlooks
	;; ************************************************************************************
	(HPS_PRINTSTATXY "HPS CORRELATION INDEX FOR WINDOWED OUTLOOKS" 	
		       "_____________________________________________________" *HPS_PRINT_SPECIAL_BLANK_MARKER*)
	(HPS_compute_pcorrindex_metrics )

	;; ************************************************************************************
	;; outlier density metrics and outlier effect over HPS computations
	;; ************************************************************************************
	(HPS_PRINTSTATXY "HPS OUTLIER STATS" 	
		       "_____________________________________________________" *HPS_PRINT_SPECIAL_BLANK_MARKER*)
	(HPS_compute_outlier_metrics )

	(with-open-file (*HPS_stats_output-stream* *HPS_stats_output_file* :direction :output :if-exists :append ) 
  		(format *HPS_stats_output-stream* "~&</TABLE>" nil)
  		(format *HPS_stats_output-stream* "~&~A" *HPS_html_footer*))

  	;; print out close up datafiles for the resultant HPS approximation 
	(format t "~& Computing detailed close ups of resultant HPS approximation        :" ) 
	(HPS_print_timeseries_closeups :numcloseups 6))
;; ********************************************************************************************


;; ********************************************************************************************
;; generate histograms for selected fields of the computational state as documented below
;; ********************************************************************************************
(defun HPS_compute_histograms_for ( nbins )
	;; 003 - HPS input signal
	(setq *HPS_histogram_ts* (mapcar #'(lambda (a) (nth  3 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_hist_output_file* (HPS_pathname *HPS_output_directory* "HPS_HISTOGRAM_1.DAT" ))
	(HPS_histogram *HPS_histogram_ts* *HPS_hist_output_file* :numbins nbins )

	;; 002 - HPS forecast
	(setq *HPS_histogram_ts* (mapcar #'(lambda (a) (nth  2 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_hist_output_file* (HPS_pathname *HPS_output_directory* "HPS_HISTOGRAM_2.DAT" ))
	(HPS_histogram *HPS_histogram_ts* *HPS_hist_output_file* :numbins nbins )

	;; 045 - HPS z-scores of errors wrt fast signal
	(setq *HPS_histogram_ts* (mapcar #'(lambda (a) (nth 45 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_hist_output_file* (HPS_pathname *HPS_output_directory* "HPS_HISTOGRAM_3.DAT" ))
	(HPS_histogram *HPS_histogram_ts* *HPS_hist_output_file* :numbins nbins )
	
	;; 004 - HPS lambda - changed to HPS z-scores of errors wrt fast signal 44
	(setq *HPS_histogram_ts*   *HPS_lambda_at_endpoints_ts*)
	(setq *HPS_hist_output_file* (HPS_pathname *HPS_output_directory* "HPS_HISTOGRAM_4.DAT" ))
	(HPS_histogram *HPS_histogram_ts* *HPS_hist_output_file* :numbins nbins )

	;; 026 - HPS real error
	(setq *HPS_histogram_ts* (mapcar #'(lambda (a) (nth 26 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_hist_output_file* (HPS_pathname *HPS_output_directory* "HPS_HISTOGRAM_5.DAT" ))
	(HPS_histogram *HPS_histogram_ts* *HPS_hist_output_file* :numbins nbins )

	;; 028 - HPS fast error
	(setq *HPS_histogram_ts* (mapcar #'(lambda (a) (nth 28 a)) *HPS_GLOBAL_computation_state* ))
	(setq *HPS_hist_output_file* (HPS_pathname *HPS_output_directory* "HPS_HISTOGRAM_6.DAT" ))
	(HPS_histogram *HPS_histogram_ts* *HPS_hist_output_file* :numbins nbins ))

	;; (HPS_wipeout *HPS_histogram_ts*))
;; ********************************************************************************************


;; ********************************************************************************************
;; driver to the computation of summary statistics for the resultant HPS approximation
;; ********************************************************************************************
(defun HPS_compute_summary_statistics_for ( RESULTANT_HPS_APPROXIMATION_DATASET_LABEL &key (nbins 128 ) )
  	(setq *HPS_histogram_nbins* nbins)

	;; perform offline analysis for the resultant HPS approximation
  	(format t "~& Now computing statistics for resultant HPS approximation ......... : [~S]" RESULTANT_HPS_APPROXIMATION_DATASET_LABEL )

	(HPS_compute_stats_for_hps)

	;; compute and setup histogram analysis datasets for selected fields of the 
	;; resultant HPS approximation's computational state
	(format t "~& Now computing data for histograms of resultant HPS approximation...: [~S]" RESULTANT_HPS_APPROXIMATION_DATASET_LABEL )

	(HPS_compute_histograms_for nbins))
;; ********************************************************************************************






