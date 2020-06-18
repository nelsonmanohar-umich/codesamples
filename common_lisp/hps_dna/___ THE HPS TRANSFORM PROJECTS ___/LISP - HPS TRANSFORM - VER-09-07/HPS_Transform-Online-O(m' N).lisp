(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;;; ***********************************************************************************************************
;;; ****************          THE HPS TRANSFORM - AN ONLINE T-TEST-BASED IMPLEMENTATION   *********************
;;; ***********************************************************************************************************


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
;; these represents decision making values used by HPS decision making
;;; ***********************************************************************************************************
(setq *HPS_ATS_DM_STATIONARY_FAILED* 0)
(setq *HPS_ATS_DM_STATIONARY_UPHELD* 1)
(setq *HPS_ATS_DM_LAMBDA_CONSTRAINT* 2)
(setq *HPS_ATS_DM_MSEERR_CONSTRAINT* 3)

;; this is a temporary kludge to print out outliers since gnuplot does not handle nils
(setq *HPS_ATS_LACKOF_CLIPPING_MARK* nil)
;;; ***********************************************************************************************************


;;; ***********************************************************************************************************
;;; this function initializes the various partial sums used within the online HPS transform
;;; ***********************************************************************************************************
(defun HPS_transform_partial_sum_init ( window_slow window_fast window_fore )
  	(setq HPS_ps_slow_sum  		(reduce #'+ window_slow))
  	(setq HPS_ps_fast_sum  		(reduce #'+ window_fast))
  	(setq HPS_ps_fore_sum  		(reduce #'+ window_fore))

  	(setq HPS_ps_slow_mean 		(/ HPS_ps_slow_sum *HPS_m* ))
  	(setq HPS_ps_fast_mean 		(/ HPS_ps_fast_sum *HPS_mp* ))
  	(setq HPS_ps_fore_mean 		(/ HPS_ps_fore_sum *HPS_forecast_winsize*))

  	(setq HPS_ps_slow_ssoe 		(float (reduce #'+ (mapcar #'(lambda (a) (HPS_square_of (- a HPS_ps_slow_mean))) window_slow))))
  	(setq HPS_ps_fast_ssoe 		(float (reduce #'+ (mapcar #'(lambda (a) (HPS_square_of (- a HPS_ps_fast_mean))) window_fast))))
  	(setq HPS_ps_fore_ssoe 		(float (reduce #'+ (mapcar #'(lambda (a) (HPS_square_of (- a HPS_ps_fore_mean))) window_fore))))

  	(setq HPS_ps_slow_stdv 		(sqrt (/ HPS_ps_slow_ssoe (- *HPS_m* 1)))) 
  	(setq HPS_ps_fast_stdv 		(sqrt (/ HPS_ps_fast_ssoe (- *HPS_mp* 1)))) 
  	(setq HPS_ps_fore_stdv 		(sqrt (/ HPS_ps_fore_ssoe (- *HPS_forecast_winsize* 1)))) 

  	(setq HPS_ps_slow_var		(float (HPS_square_of HPS_ps_slow_stdv)))
  	(setq HPS_ps_fast_var		(float (HPS_square_of HPS_ps_fast_stdv)))
  	(setq HPS_ps_fore_var		(float (HPS_square_of HPS_ps_fore_stdv)))

	(setq HPS_ps_slow_lval		(float (first window_slow)))
	(setq HPS_ps_fast_lval		(float (first window_fast)))
	(setq HPS_ps_fore_lval		(float (first window_fore)))

  	nil)
;;; ***********************************************************************************************************


;;; ***********************************************************************************************************
;;; this macro updates the various partial sums used within the online HPS transform
;;; ***********************************************************************************************************
(defmacro HPS_transform_partial_sum_update ( oldest_window_slow oldest_window_fast oldest_window_fore 
                                             newest_window_slow newest_window_fast newest_window_fore )
  	`(progn 
	   	(incf HPS_ps_slow_sum  	(- ,newest_window_slow ,oldest_window_slow ))
	   	(incf HPS_ps_fast_sum  	(- ,newest_window_fast ,oldest_window_slow ))
  	   	(incf HPS_ps_fore_sum  	(- ,newest_window_fore ,oldest_window_slow ))
	
  		(incf HPS_ps_slow_ssoe 	(float (- (HPS_square_of (- ,newest_window_slow (/ HPS_ps_slow_sum *HPS_m*)))
					          (HPS_square_of (- ,oldest_window_slow HPS_ps_slow_mean )))))
  		(incf HPS_ps_fast_ssoe 	(float (- (HPS_square_of (- ,newest_window_fast (/ HPS_ps_fast_sum *HPS_mp*)))
					          (HPS_square_of (- ,oldest_window_fast HPS_ps_fast_mean )))))
  		(incf HPS_ps_fore_ssoe 	(float (- (HPS_square_of (- ,newest_window_fore (/ HPS_ps_fore_sum *HPS_forecast_winsize*)))
					          (HPS_square_of (- ,oldest_window_fore HPS_ps_fore_mean )))))

  	   	(setq HPS_ps_slow_mean 	(float (/ HPS_ps_slow_sum *HPS_m* )))
  	   	(setq HPS_ps_fast_mean 	(float (/ HPS_ps_fast_sum *HPS_mp* )))
  	   	(setq HPS_ps_fore_mean 	(float (/ HPS_ps_fore_sum *HPS_forecast_winsize*)))

  		(setq HPS_ps_slow_stdv 	(sqrt (/ HPS_ps_slow_ssoe (- *HPS_m* 1))))
  		(setq HPS_ps_fast_stdv 	(sqrt (/ HPS_ps_fast_ssoe (- *HPS_mp* 1)))) 
  		(setq HPS_ps_fore_stdv 	(sqrt (/ HPS_ps_fore_ssoe (- *HPS_forecast_winsize* 1)))) 
	
  		(setq HPS_ps_slow_var	(float (HPS_square_of HPS_ps_slow_stdv))) 		
  		(setq HPS_ps_fast_var	(float (HPS_square_of HPS_ps_fast_stdv)))
  		(setq HPS_ps_fore_var	(float (HPS_square_of HPS_ps_fore_stdv)))
  	  nil))
;;; ***********************************************************************************************************


;;; ***********************************************************************************************************
;;; ***********************************************************************************************************
(defun HPS_transform_partial_sum_get ( &key (what 'MEAN_SLOW))
  	(cond   ((equal what 'MEAN_SLOW) (return-from HPS_transform_partial_sum_get HPS_ps_slow_mean))
	  	((equal what 'MEAN_FAST) (return-from HPS_transform_partial_sum_get HPS_ps_fast_mean))
	  	((equal what 'MEAN_FORE) (return-from HPS_transform_partial_sum_get HPS_ps_fore_mean))
	  	((equal what 'STDV_SLOW) (return-from HPS_transform_partial_sum_get HPS_ps_slow_stdv))
	  	((equal what 'STDV_FAST) (return-from HPS_transform_partial_sum_get HPS_ps_fast_stdv))
	  	((equal what 'STDV_FORE) (return-from HPS_transform_partial_sum_get HPS_ps_fore_stdv))
	  	((equal what 'VARX_SLOW) (return-from HPS_transform_partial_sum_get HPS_ps_slow_var))
	  	((equal what 'VARX_FAST) (return-from HPS_transform_partial_sum_get HPS_ps_fast_var))
	  	((equal what 'VARX_FORE) (return-from HPS_transform_partial_sum_get HPS_ps_fore_var))
		))
;;; ***********************************************************************************************************



;;; ***********************************************************************************************************
;;; ****************                     THE HPS TRANSFORM - KERNEL                       *********************
;;; ***********************************************************************************************************
(defun HPS_transform_kernel ( original_sample &key (M 60) (MP 30) (OUTPUT_MODE t) )

		(HPS_VAR "original_sample" original_sample :level 1)

		;; the input datum
		(setq *HPS_original_sample* (float original_sample))	; at any time, it contains the actual (original) input sample
   		(setq *HPS_clipped_sample* *HPS_original_sample*) 	; initialize the clipped sample to the original sample
     		(setq *HPS_clipping* 	0)				; initialize the clipping to 0


		;; increment the iteration and tracking indexes
		(incf *HPS_iteration*) 					; at any time, iteration is the current time index being processed
		(incf *HPS_index_present*) 				; after the buildup, points to the lead index of the present (i)
		(incf *HPS_index_recpast*)				; after the buildup, points to the lead index of the recent past (i-m')

		;; update the tracking indicators for windowed outlook: PRESENT WINDOWED OUTLOOK
		(incf *HPS_winstart_mp*)				; the start of the windowed outlook to the PRESENT
		(incf *HPS_winend_mp*)					; the end of the windowed outlook to the PRESENT

		;; update the tracking indicators for windowed outlook: RECENT PAST WINDOWED OUTLOOK
		(incf *HPS_winstart_m*)					; the start of the windowed outlook to the RECENT PAST
		(incf *HPS_winend_m*) 					; the end of the windowed outlook to the RECENT PAST


		;; partial sums handlers
		(cond ((< *HPS_iteration* *HPS_FILLER_SIZE* )
			(setq *HPS_window_mp*  	(HPS_get_movwin_outlook :FROMINDEX *HPS_index_present* :OFSIZE *HPS_mp*))
			(setq *HPS_window_m* 	(HPS_get_movwin_outlook :FROMINDEX *HPS_index_recpast* :OFSIZE *HPS_m*))
			(setq *HPS_forecast_window*      (HPS_get_movwin_outlook :FROMINDEX *HPS_index_present* :OFSIZE *HPS_forecast_winsize* )) ;; CHECK m vs M'

			(setq *HPS_mean_m*     			(HPS_mean *HPS_window_m*)) 
			(setq *HPS_mean_mp*  			(HPS_mean *HPS_window_mp*)) 
			(setq *HPS_mean_forecast_window* 	(HPS_mean *HPS_forecast_window* :MEANTYPE 'TRUE))
			(setq *HPS_std_m*   			(HPS_stddev *HPS_window_m* *HPS_mean_m*))
			(setq *HPS_std_mp*  			(HPS_stddev *HPS_window_mp* *HPS_mean_mp*))
			(setq *HPS_var_m*   			(HPS_variance *HPS_window_m* *HPS_std_m*))
			(setq *HPS_var_mp*  			(HPS_variance *HPS_window_mp* *HPS_std_mp*))
			nil)

		     ((= *HPS_iteration* *HPS_FILLER_SIZE* )
			(setq *HPS_window_mp*  	(HPS_get_movwin_outlook :FROMINDEX *HPS_index_present* :OFSIZE *HPS_mp*))
			(setq *HPS_window_m* 	(HPS_get_movwin_outlook :FROMINDEX *HPS_index_recpast* :OFSIZE *HPS_m*))
			(setq *HPS_forecast_window*      (HPS_get_movwin_outlook :FROMINDEX *HPS_index_present* :OFSIZE *HPS_forecast_winsize* )) ;; CHECK m vs M'

			(HPS_transform_partial_sum_init 	*HPS_window_m* 
								*HPS_window_mp* 
								*HPS_forecast_window* )

			(setq *HPS_mean_m*     			(HPS_mean *HPS_window_m*)) 
			(setq *HPS_mean_mp*  			(HPS_mean *HPS_window_mp*)) 
			(setq *HPS_mean_forecast_window* 	(HPS_mean *HPS_forecast_window* :MEANTYPE 'TRUE))
			(setq *HPS_std_m*   			(HPS_stddev *HPS_window_m* *HPS_mean_m*))
			(setq *HPS_std_mp*  			(HPS_stddev *HPS_window_mp* *HPS_mean_mp*))
			(setq *HPS_var_m*   			(HPS_variance *HPS_window_m* *HPS_std_m*))
			(setq *HPS_var_mp*  			(HPS_variance *HPS_window_mp* *HPS_std_mp*))
			nil)

		     ((> *HPS_iteration* *HPS_FILLER_SIZE* )
			(let   (( newest_window_slow (HPS_getval_from_input_ts *HPS_index_recpast* ))		
				( newest_window_fast (HPS_getval_from_input_ts *HPS_index_present* ))
				( newest_window_fore (HPS_getval_from_input_ts *HPS_index_present* ))
				( oldest_window_slow (HPS_getval_from_input_ts (- *HPS_index_recpast* *HPS_m*)))
				( oldest_window_fast (HPS_getval_from_input_ts (- *HPS_index_present* *HPS_mp*)))
				( oldest_window_fore (HPS_getval_from_input_ts (- *HPS_index_present* *HPS_forecast_winsize*))))

				(HPS_transform_partial_sum_update 	oldest_window_slow 
									oldest_window_fast 
									oldest_window_fore 
				  					newest_window_slow 
									newest_window_fast 
									newest_window_fore ))

			(setq *HPS_mean_m*  			(HPS_transform_partial_sum_get :WHAT 'MEAN_SLOW))
			(setq *HPS_mean_mp*  			(HPS_transform_partial_sum_get :WHAT 'MEAN_FAST))
			(setq *HPS_mean_forecast_window* 	(HPS_transform_partial_sum_get :WHAT 'MEAN_FORE))
			(setq *HPS_std_m*   			(HPS_transform_partial_sum_get :WHAT 'STDV_SLOW))
			(setq *HPS_std_mp*  			(HPS_transform_partial_sum_get :WHAT 'STDV_FAST))
			(setq *HPS_var_m*   			(HPS_transform_partial_sum_get :WHAT 'VARX_SLOW))
			(setq *HPS_var_mp*  			(HPS_transform_partial_sum_get :WHAT 'VARX_FAST))
		  	nil))
		
		;; generation of the control limits used to check for the presence of heavy tail outliers
		;; (setf *HPS_median_mp*  	(HPS_median *HPS_window_mp*)) 
		(setq *HPS_lcl_mp* 	(float (- *HPS_mean_mp* (* *HPS_K* *HPS_std_mp*))))
		(setq *HPS_ucl_mp* 	(float (+ *HPS_mean_mp* (* *HPS_K* *HPS_std_mp*))))

		;; control limits do NOT need to be based on mean/stddev of neither m or m' but for convenience they we use m'.
		(setq *HPS_lcl* 	*HPS_lcl_mp*)
		(setq *HPS_ucl* 	*HPS_ucl_mp*)
	
		;; generation of heavy tail outliers given the control limits 
		(cond 	((< original_sample *HPS_lcl*) 	(setq *HPS_clipped_sample* *HPS_lcl*))
	  		((> original_sample *HPS_ucl*) 	(setq *HPS_clipped_sample* *HPS_ucl*))
			(T 				(setq *HPS_clipped_sample* *HPS_ATS_LACKOF_CLIPPING_MARK* )))

		;; generation of the resultant clipped input signal value due to said heavy tail outlier  
		(if (not (equal *HPS_clipped_sample* *HPS_ATS_LACKOF_CLIPPING_MARK* ))
			(setq *HPS_clipping* 		(- *HPS_original_sample* *HPS_clipped_sample* ))
			(setq *HPS_clipping* 		0))

		;; recomputation of the mean and var due to heavy tail outlier and associated clipping
		;; note that the operation is destructive over the internal input series array (but not 
		;; the list passed as an argument), as a result, all subsequent operations must be based
		;; on the array and not the list
		(when 	(not (equal *HPS_clipped_sample* *HPS_ATS_LACKOF_CLIPPING_MARK* ))
			(setq original_sample 		 *HPS_clipped_sample*)
			(setf (aref *HPS_input_array*    *HPS_index_present*) *HPS_clipped_sample*)
			;; (HPS_setthis nplace *HPS_index_present* *HPS_input_series* *HPS_clipped_sample* )

			(setq *HPS_window_mp*  		 (HPS_get_movwin_outlook :fromindex *HPS_index_present* :ofsize *HPS_mp*))
			(setq *HPS_mean_mp*  		 (HPS_mean *HPS_window_mp*)) 
			(setq *HPS_std_mp*  		 (HPS_stddev *HPS_window_mp* *HPS_mean_mp*))
			(setq *HPS_var_mp*  		 (HPS_variance *HPS_window_mp* *HPS_std_mp*))

			(setq *HPS_window_m* 		 (HPS_get_movwin_outlook :fromindex *HPS_index_recpast* :ofsize *HPS_m*))
		  	(setq *HPS_mean_m*     		 (HPS_mean *HPS_window_m*)) 
			(setq *HPS_std_m*   		 (HPS_stddev *HPS_window_m* *HPS_mean_m*))
			(setq *HPS_var_m*   		 (HPS_variance *HPS_window_m* *HPS_std_m*))

			;; update basic forecast value options - check between m and m'
			(setq *HPS_forecast_window*      (HPS_get_movwin_outlook :fromindex *HPS_index_present* :ofsize *HPS_mp* ))
			(setq *HPS_mean_forecast_window* (HPS_mean *HPS_forecast_window* :meantype 'TRIMMED)))


		;; memorize the old decision for decision making purposes
		(setq *HPS_old_decision_bit* *HPS_new_decision_bit*)
		(setq *HPS_old_forecast*     *HPS_new_forecast*)

		;; generation of the decision signal - if test is t then change in state and no ATS segment possible -
		(setq *HPS_state_change* (HPS_conjecture_for_approximate_stationarity 	*HPS_mean_m* *HPS_mean_mp* 
							   		      		*HPS_var_m*  *HPS_var_mp* 
							   		      		*HPS_m*      *HPS_mp*      ))

		;; memorize the current stationary bit
		(if (equal *HPS_state_change* t)
			(setq *HPS_new_decision_bit* *HPS_ATS_DM_STATIONARY_FAILED* )
			(setq *HPS_new_decision_bit* *HPS_ATS_DM_STATIONARY_UPHELD* ))

		(when   OUTPUT_MODE
		  	(setq *HPS_current_intervals* (list   (list (list *HPS_winstart_m* *HPS_winend_m*) (list *HPS_winstart_mp* *HPS_winend_mp*))
						         '--> (list *HPS_iteration* *HPS_index_present* *HPS_index_recpast* )))

		  	(when *HPSdebug* 
			  	(push *HPS_current_intervals* *HPS_current_intervals_stack*))

			;; the windowed outlooks and indexed values at this iteration
  			(HPS_VAR "*HPS_index_present* "      		      *HPS_index_present*           :level 2 ) 
  			(HPS_VAR "*HPS_index_recpast* "      	              *HPS_index_recpast*           :level 2 ) 
  			(HPS_VAR "*HPS_iteration*     "                       *HPS_iteration*               :level 2 )
			(HPS_VAR "Indexes of windowed outlooks"              (first *HPS_current_intervals*):level 3 )
			(HPS_VAR "PRESENT outlook m' values"                  *HPS_window_mp*               :level 4 )
			(HPS_VAR "RECPAST outlook m  values"                  *HPS_window_m*  	            :level 4 )

			;; the windowed stacks used in the computation of bounds and metrics
			(HPS_VAR "HPS_CONJECTURE (*HPS_state_change*)" 	      *HPS_state_change* 	    :level 2)
			(HPS_VAR "STACK (i-1th): *HPS_MSE_pooled_std_stack*"  *HPS_MSE_pooled_std_stack*    :level 4)
			(HPS_VAR "STACK (i-1th): *HPS_SSE_form_stack*"        *HPS_SSE_form_stack* 	    :level 4)
			(HPS_VAR "STACK (i-1th): *HPS_MSE_errorcorr_stack*"   *HPS_MSE_errorcorr_stack*     :level 4)
			(HPS_VAR "STACK (i-1th): *HPS_MSE_bound_stack*"       *HPS_MSE_bound_stack*         :level 4))

		(unless OUTPUT_MODE
		  	nil)

		;; update the various accumulated MSE tracking metrics - using the i-1th iteration values for error
		;; uses *HPS_SSE_form_stack* 
		(HPS_MSE_update_metrics )

		;; compute the previous MSEmax bound for this iteration with i-1th (forecast error) iteration values
	        ;;  uses "*HPS_MSE_bound_stack*" "*HPS_MSE_pooled_std_stack*" "*HPS_MSE_errorcorr_stack*" 
		(setq *HPS_MSE_MAX* (float (HPS_compute_MSE_errorbound )))


		;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		;; scaling factor to adjust the various maximum MSE bounds 
		(if (equal *HPS_usable_stacksize* 0) 			; note that *HPS_usable_stacksize* was set up in above function call
		  	(setq *HPS_MSE_divider*   1)
		  	(setq *HPS_MSE_divider*  *HPS_usable_stacksize*))		;;; CHECK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		  	;; (setq *HPS_MSE_divider* (sqrt *HPS_usable_stacksize*)))	;;; CHECK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		;; update the various maximum MSE bounds 
											;;; CHECK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  		(setq *HPS_MSE_bound_window*  (float (* *HPS_ATS_MSEERR_RELAXATION_FC* *HPS_MSE_MAX* ))) 
  		(setq *HPS_MSE_bound_instant* (sqrt  (float (/ (HPS_square_of *HPS_MSE_bound_window* ) *HPS_MSE_divider* ))))
		(setq *HPS_MSE_bound_segment* (float (* *HPS_lambda* *HPS_MSE_bound_instant*)))
		;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		

  		;; condition HPS forecast to segment length --- overrides previous decision, if necessary 
		(cond   ((>= *HPS_lambda* *HPS_MAX_SEGMENT_DURATION* ) 
				(when   OUTPUT_MODE
					(HPS_VAR "*ATS SEGMENT constrained at HPS_iteration*" *HPS_iteration* :level 3 :forced nil)
					(HPS_VAR "*HPS_lambda*"                               *HPS_lambda*    :level 3 :forced nil)
					nil)
				(setq *HPS_new_decision_bit* *HPS_ATS_DM_LAMBDA_CONSTRAINT* ))

  		;; condition HPS forecast to HPS error control - MSE constraintment to ATS segment construction --- overrides previous decision
			;; watch for temp kludge to manage mse error build up at the start of a segment
			;; also check code for similar transient decisions, whether uses it
			;; also check for trimming effect, window of influence
			      ;; (> *HPS_MSE_metric_segment* *HPS_MSE_bound_segment* ))
        		      ;; ((AND (> *HPS_lambda* (* *HPS_ATS_MSEERR_BUILDUP_DELAY* *HPS_ATS_TRIVIAL_SEGMENT_DURATION* )) 
        	       ((AND (> *HPS_lambda* *HPS_ATS_MSEERR_BUILDUP_DELAY*) 
			     (> *HPS_MSE_metric_window* *HPS_MSE_bound_window* ))
				(when   OUTPUT_MODE
					(HPS_VAR "*ATS_MSE ERROR constrained at HPS_iteration*" *HPS_iteration* 	 :level 3 :forced nil)
					(HPS_VAR "*HPS_MSE_bound_segment*		      " *HPS_MSE_bound_segment*	 :level 3 :forced nil)
		    			(HPS_VAR "*HPS_MSE_metric_segment*		      " *HPS_MSE_metric_segment* :level 3 :forced nil)
					nil)
				(setq *HPS_new_decision_bit* *HPS_ATS_DM_MSEERR_CONSTRAINT* ))
		
  		;; condition HPS forecast to segment length --- overrides if necessary previous decision
		       ((< *HPS_iteration* *HPS_start_buildupdelay*) 
				(when   OUTPUT_MODE
					(HPS_VAR "*ATS build-up constraint at HPS iteration*" *HPS_iteration* :level 3 :forced nil)
					(HPS_VAR "*HPS_lambda*"                               *HPS_lambda*    :level 3 :forced nil)
					nil)
				(setq *HPS_new_decision_bit* *HPS_ATS_DM_STATIONARY_FAILED* )))

		;; update of the HPS forecast based on approximate stationarity subject to forms of error constraintment
		(if (equal *HPS_new_decision_bit* *HPS_ATS_DM_STATIONARY_UPHELD* )
		  	(setq *HPS_new_forecast* *HPS_old_forecast*)
		  	(setq *HPS_new_forecast* *HPS_mean_forecast_window* ))

		;; reset the tracking metrics for both segment and MSE accounting
		(when (NOT (equal *HPS_new_decision_bit* *HPS_ATS_DM_STATIONARY_UPHELD* ))
			;; i.e., (when (equal *HPS_state_change* t)
  			(setq *HPS_lambda* (max (- *HPS_index_present* *HPS_ATS_segment_start_index* ) 0))

			(when 	(> *HPS_lambda* *HPS_ATS_TRIVIAL_SEGMENT_DURATION*) 
				(setq *HPS_ats_segment* (HPS_ATS_span_segment *HPS_ATS_segment_start_index* *HPS_lambda* *HPS_old_forecast* ))
				(push *HPS_ats_segment* *HPS_ATS_segments_stack*)

				(when   OUTPUT_MODE
					(HPS_VAR "*ATS SEGMENT ended at HPS_iteration*" *HPS_iteration*     :level 3 :forced nil)
					(HPS_VAR "*ATS SEGMENT (start index was)* :   " *HPS_ATS_segment_start_index* :level 3 :forced nil)
					(HPS_VAR "*HPS_lambda*"                         *HPS_lambda*        :level 3 :forced nil)
					(HPS_VAR "*ATS SEGMENT *HPS_ats_segment* DUMP:" *HPS_ats_segment*   :level 4)
					nil))

			(unless (> *HPS_lambda* *HPS_ATS_TRIVIAL_SEGMENT_DURATION*) 
  				nil)

			(setq *HPS_ATS_segment_start_index* *HPS_index_present*))

		(unless (NOT (equal *HPS_new_decision_bit* *HPS_ATS_DM_STATIONARY_UPHELD* ))
			;; i.e., (unless (equal *HPS_state_change* t)
  			(setq *HPS_lambda* (max (- *HPS_index_present* *HPS_ATS_segment_start_index* ) 0))

			(when 	(> *HPS_lambda* *HPS_ATS_TRIVIAL_SEGMENT_DURATION*) 
				(when   OUTPUT_MODE
					(HPS_VAR "*ATS SEGMENT furthered at HPS_iteration*" *HPS_iteration* :level 3 :forced nil)
					(HPS_VAR "*HPS_lambda*"                               *HPS_lambda*  :level 3 :forced nil)
					nil))

			(unless (> *HPS_lambda* *HPS_ATS_TRIVIAL_SEGMENT_DURATION*) 
			  	nil))

		;; generation of the current ith iteration error values
		(setq *HPS_error_slow* 		(float (- *HPS_new_forecast* *HPS_mean_m* )))
		(setq *HPS_error_fast* 		(float (- *HPS_new_forecast* *HPS_mean_mp* )))
		(setq *HPS_error_real* 		(float (- *HPS_new_forecast* *HPS_original_sample*)))
		(setq *HPS_errorcorr_SSE_i* 	(float (+ (HPS_square_of *HPS_error_fast* ) 
						          (HPS_square_of *HPS_error_slow* ))))

		;; memorize the resultant error data for this iteration into the three error stacks
		(HPS_PUSH *HPS_error_fast*      *HPS_error_fast_stack*)				; push 0a
		(HPS_PUSH *HPS_error_slow*      *HPS_error_slow_stack*)				; push 0b
		(HPS_PUSH *HPS_error_real*      *HPS_error_real_stack*)				; push 0c

		;; memorize the resultant MSE error data for this iteration into the three MSE stacks
		(HPS_PUSH *HPS_MSE_MAX*         *HPS_MSE_bound_stack*)				; push #1
		(HPS_PUSH *HPS_errorcorr_SSE_i* *HPS_SSE_form_stack*) 				; push #2
		(HPS_PUSH *HPS_pooled_std*      *HPS_MSE_pooled_std_stack*)			; push #3

		(setq *HPS_pooled_var* (HPS_square_of *HPS_pooled_std*))			; kludge

		;; memorize the resultant MSE error correlation data for this iteration
		(if (< *HPS_pooled_var* *HPS_EPSILON*) 						; push #4
			(setq *HPS_errorcorr_form* *HPS_TEMP_KLUDGE*)
			(setq *HPS_errorcorr_form* (/ (* *HPS_error_fast* *HPS_error_slow* ) *HPS_pooled_var* )))

		(HPS_PUSH *HPS_errorcorr_form*  *HPS_MSE_errorcorr_stack*)


		(when OUTPUT_MODE 
			;; compute online metrics for this iteration used later in offline performance analysis
		  	(HPS_compute_diagnostic_metrics) 

			;; this is a simpleminded autodiagnostic test but it is used only for illustrative purposes 
			;; --- values were set on the above call diagnostic checks are temporary - from the hip 
			;; selected so far check the value of correlation index
		  	(if (AND 	(HPS_is_this_between *HPS_perf_z_means_i* -1 1) 	  
			 		(HPS_is_this_between *HPS_perf_z_error_i* -2 2)
			 		(HPS_is_this_between *HPS_perf_f_s_rat_i* 0.7 1)         
			 		(HPS_is_this_between (abs *HPS_perf_p_mucor_i*) 0 0.5)
			 		(HPS_is_this_between (abs *HPS_perf_n_sampl_i*) 0 (+ m mp mp ))  
			 		(HPS_is_this_between *HPS_perf_e_relat_i* -0.5 0.5))
						(incf *HPS_autodiag_basically_good_iter* 1)
						(setq *HPS_autodiag_basically_good_iter* 0)) 

			;; memorize, for convenience, the HPS approximation, in terms of fine tracking and ATS segments
			(when   (AND (> *HPS_lambda* *HPS_ATS_PRINTABLE_SEGMENT_DURATION* ) 
				     (equal *HPS_new_decision_bit* *HPS_ATS_DM_STATIONARY_UPHELD* ))
	  			(setq *HPS_ATS_segment_tracking* *HPS_new_forecast*)
				(setq *HPS_ATS_fine_tracking* 0))

			(unless (AND (> *HPS_lambda* *HPS_ATS_PRINTABLE_SEGMENT_DURATION* ) 
				     (equal *HPS_new_decision_bit* *HPS_ATS_DM_STATIONARY_UPHELD* ))
	  			(setq *HPS_ATS_segment_tracking* 0 )
				(setq *HPS_ATS_fine_tracking* *HPS_new_forecast*))

			;; tracking of the computational state for this kernel iteration of the online HPS transform
		  	(setq *HPStuple* (list   
			   	*HPS_iteration*
	    		   	*HPS_new_decision_bit* 
	    		   	*HPS_new_forecast* 
			   	*HPS_original_sample* 
			   	*HPS_lambda* 				*HPS_index_present* *HPS_ATS_segment_start_index*
	    		   	*HPS_index_present* 			*HPS_winstart_mp* *HPS_winend_mp*  
	    		   	*HPS_index_recpast* 			*HPS_winstart_m*  *HPS_winend_m*  
			   	*HPS_clipped_sample*			*HPS_lcl* *HPS_mean_m* *HPS_ucl* *HPS_clipping*
	    		   	*HPS_TTEST* 				*HPS_mean_m* *HPS_mean_mp* *HPS_var_m* *HPS_var_mp* 
				*HPS_m* *HPS_mp* *HPS_tmax*
	    		   	*HPS_error_real*    			*HPS_error_slow* *HPS_error_fast*  
	    		   	*HPS_MSE_bound_segment*   		*HPS_MSE_bound_window*  *HPS_MSE_bound_instant*  
	    		   	*HPS_MSE_metric_segment*   		*HPS_MSE_metric_window* *HPS_MSE_metric_instant* 
			        *HPS_mean_diff* 			*HPS_confintv_low* *HPS_confintv_high* *HPS_pooled_var* 
				*HPS_MSE_MAX*				*HPS_errorcorr_SSE_i* *HPS_pooled_std* *HPS_errorcorr_form*  
				*HPS_autodiag_basically_good_iter* 	*HPS_perf_z_means_i* *HPS_perf_z_error_i* *HPS_perf_f_s_rat_i* 
								        *HPS_perf_p_mucor_i* *HPS_perf_n_sampl_i* *HPS_perf_e_relat_i* 
				*HPS_ATS_segment_tracking* 
				*HPS_ATS_fine_tracking* 
				(- *HPS_MSE_metric_window* *HPS_MSE_bound_window* ))) 

		  	(push *HPStuple* *HPS_computation_state*) 

		  	(HPS_VAR "*HPS computation tuple*: " *HPStuple* :level 3))

		(unless OUTPUT_MODE
		  	nil)
	
		;; update the various time series memories with the computed values if so desired here
		(setq *HPS_outlier_tuple*  (list (if *HPS_clipping* *HPS_original_sample* *HPS_clipping*) *HPS_lcl* *HPS_ucl*))
		(setq *HPS_error_tuple*    (list *HPS_error_real* *HPS_error_fast* *HPS_error_slow*))
	
		(push *HPS_new_forecast*   *HPS_transformed_monitor_ts*)	; the output of the transform - ATS timeseries
		(push *HPS_outlier_tuple*  *HPS_transformed_outlier_ts*)	; the output of the transform - timeseries of heavy tailed outliers
		(push *HPS_error_tuple*    *HPS_transformed_error_ts*)		; the output of the transform - approximation error time series
	
		;; the "return value" (a ternary tuple) of the HPS transform kernel iteration: consisting of forecast, outlier, and error
		(setq *HPS_nval* (list *HPS_new_forecast* *HPS_outlier_tuple* *HPS_error_tuple*))

		;; printout this computed result of this iteration
		(when OUTPUT_MODE 
			(HPS_VAR "*HPS_transformed_tuple (forecast, outlier, error)*:" *HPS_nval* :level 2)
			nil)

		*HPS_nval*)
		;; end of the loop
;; ************************************************************************************************************

	
;;; ***********************************************************************************************************
;; this function implements the HPS transform. This particular implementation of the HPS transform is based on the 
;; online version and does not optimize the output.  As such, it has O(N) time/space complexity relying on an 
;; o(1) time/space complexity kernel based on partial sums.
;;; ***********************************************************************************************************
(defun HPS_transform ( atimeseries &key (M 60) (MP 30) (ALPHALEVEL 0.001) (FORECAST_WINSIZE M)  
				   	(K 3) (TIMESHIFT MP) (SEGLIMIT (+ M MP)) 
					(SEGTRIVIAL 1) (MSERELAX 2) (MSEDELAY 1) 
					(PRINTDUR 8)
					(OUTPUT_MODE t)
					(FILLER_MODE nil))

	(format t "~% HPS Transform being applied with m=[~S] m'=[~S] at alpha=[~S]" M MP ALPHALEVEL )

	(print '-------------HPS_TRANSFORM_START-------------)

	;; gross correctness of inputs validity checks
  	(if (< (length atimeseries) (+ M MP))
	  	(return-from HPS_transform (print "size of the input series should be greater than (m+m')")) 
		(setq *HPS_m* M))
  	(if (< M MP) 
	  	(return-from HPS_transform (print "m must be greater than m'; default is m=60, m'=30")) 
		(setq *HPS_mp* mp))
  	(if (> ALPHALEVEL 0.5) 
		(return-from HPS_transform (print "alpha is a t-based confidence interval probability, i.e., range is (0,0.5]; default is 0.005")) 
		(setq *HPS_alpha* ALPHALEVEL))

	;; CHECK - IT WORKS FOR ANY VALUE, BUT FOR ANYTHING OTHER THAN ONE, IT DROPS ONE OR MORE MATCHING INSTANCES
	(when   FILLER_MODE
	  	(setq *HPS_FILLER_SIZE*	(round (* 1.0 (+ *hps_m* *hps_mp* timeshift ))))
		(setq *HPS_input_series* (append (subseq atimeseries 0 *HPS_FILLER_SIZE*) atimeseries )))

	(unless FILLER_MODE
		nil)


	;; the input data represented as a list of numbers where the size of the list is not necessarily known beforehand
	;; here, the input series is known but this is not necessary to the online loop (except for getmovwin)
	(setq *HPS_input_series* (HPS_conditioning *HPS_input_series*)) ; input time series represented as a list
	(setq *HPS_input_array* (coerce *HPS_input_series* 'VECTOR)) 	; used for efficient extraction of windowed outlooks


	;; set the default sensor parameters (must be called prior to HPS_global_init)
	(HPS_set_sensor_parameters *HPS_m* *HPS_mp* *HPS_alpha* K timeshift seglimit segtrivial mserelax msedelay printdur )


	;; initialization stage
	(HPS_global_init *HPS_m* *HPS_mp* OUTPUT_MODE )


	;; initialization done at top level for clarity purposes
	(setq *HPS_iteration* 0)					; the current index in time (i)
	(setq *HPS_current_intervals* nil)				; at each iteration, it tracks the current windowed outlooks
	(setq *HPS_current_intervals_stack* nil)			; the memory of indexes and outlooks used in computation

	(setq *HPS_index_present* 0) 					; after the buildup, points to the lead index of the present (i)
	(setq *HPS_winend_mp*       *HPS_index_present*)	   	; the end of the windowed outlook to the PRESENT
	(setq *HPS_winstart_mp*  (- *HPS_index_present* *HPS_mp*)) 	; the start of the windowed outlook to the PRESENT

	(setq *HPS_index_recpast* (- 0 *HPS_timeshift*))		; after the buildup, points to the lead index of the recent past (i-m')
	(setq *HPS_winend_m*        *HPS_index_recpast*)	   	; the end of the windowed outlook to the RECENT PAST
	(setq *HPS_winstart_m*   (- *HPS_index_recpast* *HPS_m*))  	; the start of the windowed outlook to the RECENT PAST

	(setq *HPS_forecast_winsize* forecast_winsize )			; initialize the adaptive forecast windowing scheme


	;; ***********************************************************************************************************
	;; ****************                     THE HPS TRANSFORM - KERNEL                       *********************
	;; 			(dolist (original_sample *HPS_temp_series* 'HPSmainloop) 
	;; ***********************************************************************************************************
	(setq *HPS_temp_series*  (nbutlast *HPS_input_series*))
	(mapcar #'(lambda (original_sample) 
		(HPS_transform_kernel original_sample 		:M  			m
		  						:MP 			mp
							    	:OUTPUT_MODE 		output_mode)) 
							*HPS_temp_series*)
	;; ***********************************************************************************************************
	

	(when   FILLER_MODE
		(setq *HPS_temp_series* (list   (subseq *HPS_transformed_monitor_ts* *HPS_FILLER_SIZE* )
					  	(subseq *HPS_transformed_outlier_ts* *HPS_FILLER_SIZE* )
					  	(subseq *HPS_transformed_error_ts*   *HPS_FILLER_SIZE* ))))

	(unless FILLER_MODE
		(setq *HPS_temp_series* (list 		*HPS_transformed_monitor_ts* 
							*HPS_transformed_outlier_ts* 
							*HPS_transformed_error_ts* )))

	(print '-------------HPS_TRANSFORM_DONE-------------)
	
	(return-from HPS_transform *HPS_temp_series* ))
;;; ***********************************************************************************************************





