;; ********************************************************************************************************
;; *************************** THE HPS TRANSFORM :   INITIALIZATION       *********************************
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


;;; ***********************************************************************************************************
;; this function initializes the sensor parameters used to fine tune the response of the HPS transform
;;; ***********************************************************************************************************
(defun HPS_set_sensor_parameters (m mp alpha K timeshift seglimit segtrivial mserelax msedelay printdur)
  	(when (NOT (AND (numberp K) (numberp timeshift) (numberp seglimit) (numberp segtrivial) 
		      (numberp mserelax) (numberp msedelay) (numberp printdur)))
	  	(exit (pprint 'INVALID_SENSOR_PARAMETERS))
		nil)

	;; the number of sigma levels used for statistical significance of outliers
  	(setq *HPS_K* K)					;; default is 3 sigma levels, i.e., 3 up and 3 down					

	;; timeshift may be used to intersect windowed outlooks 
  	(if (not (HPS_is_this_between timeshift 1 m))
		(setq *HPS_timeshift* mp) 			;; the default decorrelation point c is hardset to m/2
		(setq *HPS_timeshift* timeshift))		

	;; a sensor parameter to control the maximum duration of an ATS segment
	(setq *HPS_MAX_SEGMENT_DURATION* (max seglimit mp ))  	

	;; the minimum duration of an ATS segment is by default 1
	(if (HPS_is_this_between segtrivial 1 mp)
		(setq *HPS_ATS_TRIVIAL_SEGMENT_DURATION* (round segtrivial))		
		(setq *HPS_ATS_TRIVIAL_SEGMENT_DURATION* 1))	

	(setq *HPS_ATS_MSEERR_RELAXATION_FC* mserelax)

	(if (HPS_is_this_between msedelay 1 mp)
		(setq *HPS_ATS_MSEERR_BUILDUP_DELAY* msedelay)
		(setq *HPS_ATS_MSEERR_BUILDUP_DELAY* 1))

	;; duration of (non-trivial) ATS segments suitable for graphical printing on small displays
	(if (HPS_is_this_between printdur 1 (/ *HPS_MAX_SEGMENT_DURATION* 2))
		(setq *HPS_ATS_PRINTABLE_SEGMENT_DURATION* printdur) 
		(setq *HPS_ATS_PRINTABLE_SEGMENT_DURATION* 8)) 

	;; transient region last m+mp time units plus confidence buildup of mp over the HPS forecast during initialization
	(setq *HPS_start_buildupdelay* (+ m mp mp ))			

	;; **************************************************************************************************************
	;; read the table of inverse student-t distribution values used to determine confidence limits
	;; **************************************************************************************************************
	(HPS_read_t_table)					

	;; return the t-max value to be used in HPS decision making given the input parameters
	(setq *HPS_tmax* (HPS_tdist_lookup alpha m mp )))
;;; ***********************************************************************************************************


;;; ***********************************************************************************************************
;; this function initializes externally visible global values produced by the HPS transform
;;; ***********************************************************************************************************
(defun HPS_global_init ( m mp outputmode )
  	(setq *HPS_outputmode* outputmode)			; determines whether or not diagnostics are computed and then printed 

	(setq *HPS_FIXED_STACK_LIMIT* (+ m m ))			; the size of fixed length stacks, used in windowed operations
	(setq *HPS_FIXED_STACK_BIGLIMIT* (* 4 *HPS_FIXED_STACK_LIMIT*))

	;; for convenience, used in several computations
	(setq *HPS_m_plus_mp* (+ m mp))
	(setq *HPS_POOLED_VAR_COMP* 	(sqrt (+ (/ 1 m) (/ 1 mp))))

	;; CHECK !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  	(setq *HPS_EPSILON*	     0.0000001)
	(setq *HPS_TEMP_KLUDGE*      1.0000001)			; to correct computation of std/var on boundary cases
	(setq *HPS_TEMP_KLUDGE_ZERO* 1.0000001)			; to correct computation of std/var on boundary cases
	(setq *HPS_TEMP_KLUDGE_ONE*  0.0000001)			; to correct computation of std/var on boundary cases
	(setq *HPS_TEMP_KLUDGE_INF*  100000001)			; to correct computation of std/var on boundary cases

  	;; the outputs of the HPS transform
	(setq *HPS_transformed_monitor_ts* nil)			; the output of the transform - ATS timeseries
	(setq *HPS_transformed_outlier_ts* nil)			; the output of the transform - timeseries of heavy tailed outliers
	(setq *HPS_transformed_error_ts*   nil)		  	; the output of the transform - approximation error time series

	;; the (optional output) control limits generated for this timeseries by the HPS transform 
	(setq *HPS_transform_outlier_lcl* nil)			; the output of the transform - lower control limit
	(setq *HPS_transform_outlier_ucl* nil)			; the output of the transform - upper control limit

	;; the input time series as a list and its alternative representation for efficient access of elements
	(setq *HPS_original_sample* 0)				; just the current online sample - for scoping reasons
	(setq *HPS_clipped_sample*  0)				; clipped online sample - for scoping reasons
			
	;; the speculated stationariness state tracking information
	(setq *HPS_old_decision_bit* 0) 			; the ATS decision previously decided at i-1
	(setq *HPS_new_decision_bit* 0) 			; the ATS decision currently being decided at i

	;; the ATS/HPS-based forecast value
	(setq *HPS_old_forecast* 0)
	(setq *HPS_new_forecast* 0)

	;; the segment duration control paramenters for error constraintment
	(setq *HPS_lambda*       0)				; at any point in time, the duration of the current ATS segment, if any
	(setq *HPS_ATS_segment_start_index* 0)			; at any point in time, the start index of the current ATS segment, if any
	(setq *HPS_old_ATS_segment_start_index* 0)		; redundant variable used to printout ATS segments
	(setq *HPS_ATS_segments_stack* nil)

	;; the MSE-based error constraintment accumulation metrics
  	(setq *HPS_MSE_bound_window*   0.0)
  	(setq *HPS_MSE_bound_instant*  0.0)
  	(setq *HPS_MSE_bound_segment*  0.0)

  	(setq *HPS_MSE_metric_window*  0.0)
  	(setq *HPS_MSE_metric_instant* 0.0)
  	(setq *HPS_MSE_metric_segment* 0.0)

	;; the memory of MSE error correlations used to compute the error bound, to be substituted for partial sums later
	(setq *HPS_MSE_pooled_std_stack* nil) 
	(setq *HPS_MSE_errorcorr_stack* nil)
	(setq *HPS_SSE_form_stack* nil)
	(setq *HPS_MSE_bound_stack* nil)
	(setq *HPS_usable_stacksize* 0)

	;; the error stacks used for optional debugging and diagnostic measures
	(setq *HPS_error_fast_stack* nil)
	(setq *HPS_error_slow_stack* nil)
	(setq *HPS_error_real_stack* nil)

	;; the various computational metrics
	(setq *HPS_mean_m* 	0)
	(setq *HPS_mean_mp* 	0)
	(setq *HPS_var_m* 	0)
	(setq *HPS_var_mp* 	0)
	(setq *HPS_std_m* 	0)
	(setq *HPS_std_mp* 	0)
	(setq *HPS_pooled_var*  *HPS_TEMP_KLUDGE*)
	(setq *HPS_pooled_var*  *HPS_TEMP_KLUDGE*)

	;; the ATS/HPS-based error components
	(setq *HPS_error_fast* 	0)
	(setq *HPS_error_slow* 	0)
	(setq *HPS_error_real* 	0)

	;; online performance metrics
  	(setq *HPS_perf_z_means_i* 0)
  	(setq *HPS_perf_f_s_rat_i* 0)
  	(setq *HPS_perf_p_mucor_i* 0)
  	(setq *HPS_perf_n_sampl_i* 0)
  	(setq *HPS_perf_e_relat_i* 0)
  	(setq *HPS_perf_z_error_i* 0)

	;; current window trackers, initial values are measured backwards from time i*=mp+m using window sizes m and an (m/2) time-delayed window size m'
	;; therefore, for i=m+mp+mp, the initial window outlooks are: [i-m,i) and [i-m/2-mp,i-m/2) for an input array [0..N)
	(setq *HPS_current_intervals* nil)			; at any point, it describes the active windowed intervals 
	(setq *HPS_winstart_mp*  0)				; at any point, it points to the start of the windowed outlook to the PRESENT
	(setq *HPS_winend_mp*    0)				; at any point, it points to the end of the windowed outlook to the PRESENT
	(setq *HPS_winstart_m*   0)   				; at any point, it points to the start of the windowed outlook to the RECENT PAST
	(setq *HPS_winend_m*     0)				; at any point, it points to the end of the windowed outlook to the RECENT PAST

	;; a counter of the number of sequentially autodiagnosed to be good iterations
	(setq *HPS_autodiag_basically_good_iter* 0)

	;; global computation state for the HPS transform
	;; this table is composed of tuples that track the state of the computation at any instance in terms of the main global variaqbles
	(setq *HPS_computation_state* nil))
;;; ***********************************************************************************************************










