(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;;; ***********************************************************************************************************
;;; ****************          ERROR CONSTRAINTMENT OF THE HPS APPROXIMATION              **********************
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


;; ********************************************************************************************
;; this function computes the HPS MSE bound smoothed with respect to a fixed size interval;
;; the bound holds true for any subinterval, but in this case, this interval was chosen to 
;; be a subinterval of the PRESENT windowed outlook (this being of size m). Furthermore, 
;; for particular computational and theorem step reduction convenience; this subinterval 
;; was forced to be of size m' and composed of the most recent m' samples in time.
;; ********************************************************************************************
(defun HPS_compute_MSE_errorbound ( )
  	;; check whether we are operating outside the initial transient region 
	;; note, if the basis (m') used in MSE-bound definitions changes, such
	;; should be reflected here
	(if (>= *HPS_iteration* *HPS_mp*)
        	(setq *HPS_usable_stacksize* (- *HPS_mp* 1))
        	(setq *HPS_usable_stacksize* (- *HPS_iteration* 1)))

	;; reset the computation (temp fix) for problem of computing i or i-1 since ith data is not yet ready at i iteration
	(setq *HPS_MSE_pooled_std_stack_len* (length *HPS_MSE_pooled_std_stack* ))
	(when   (< *HPS_MSE_pooled_std_stack_len*  *HPS_usable_stacksize*) 
	  	(setq *HPS_usable_stacksize* *HPS_MSE_pooled_std_stack_len*  ))

	(when 	(equal *HPS_usable_stacksize* 0)
		(setq *HPS_MSE_bound* *HPS_TEMP_KLUDGE*)
		(return-from HPS_compute_MSE_errorbound *HPS_MSE_bound*))

	(unless (equal *HPS_usable_stacksize* 0)
		;; ********************************************************************************************************
		;; ********************************************************************************************************
	  	(cond ((< *HPS_iteration* *HPS_FILLER_SIZE*)
			(setq *HPS_MSE_bound_1_window*             (subseq *HPS_MSE_pooled_std_stack* 0 *HPS_usable_stacksize* ))
			(setq *HPS_MSE_bound_1_mean_pooledstd_sum* (reduce #'+ *HPS_MSE_bound_1_window* )))

	  	      ((= *HPS_iteration* *HPS_FILLER_SIZE*)
			(setq *HPS_MSE_bound_1_window*             (subseq *HPS_MSE_pooled_std_stack* 0 *HPS_usable_stacksize* ))
			(setq *HPS_MSE_bound_1_mean_pooledstd_sum* (reduce #'+ *HPS_MSE_bound_1_window* ))
			(setq *HPS_MSE_bound_1_window_lastval*     (float (nth *HPS_usable_stacksize* *HPS_MSE_pooled_std_stack* ))))

	  	      ((> *HPS_iteration* *HPS_FILLER_SIZE*)
			(incf *HPS_MSE_bound_1_mean_pooledstd_sum* (- (first *HPS_MSE_pooled_std_stack*) 
								      *HPS_MSE_bound_1_window_lastval* ))
			(setq *HPS_MSE_bound_1_window_lastval*     (float (nth *HPS_usable_stacksize* *HPS_MSE_pooled_std_stack* )))))

		(setq *HPS_MSE_bound_1_mean_pooledstd_sum*         (float *HPS_MSE_bound_1_mean_pooledstd_sum* ))
		;; ********************************************************************************************************


		;; ********************************************************************************************************
		(setq *HPS_MSE_bound_1_mean_pooledstd* 	(float (/ *HPS_MSE_bound_1_mean_pooledstd_sum* *HPS_usable_stacksize* )))
		(setq *HPS_MSE_bound_1* 		(float (/ *HPS_MSE_bound_1_mean_pooledstd* *HPS_usable_stacksize* )))
		;; ********************************************************************************************************

		;; ********************************************************************************************************
		;; ********************************************************************************************************
	  	(cond ((< *HPS_iteration* *HPS_FILLER_SIZE*)
			(setq *HPS_MSE_bound_0_window* 		   (subseq *HPS_MSE_errorcorr_stack*  0 *HPS_usable_stacksize* ))
			(setq *HPS_MSE_bound_0_mean_errorcorr_sum* (reduce #'+ *HPS_MSE_bound_0_window*)))

	  	      ((= *HPS_iteration* *HPS_FILLER_SIZE*)
			(setq *HPS_MSE_bound_0_window*             (subseq *HPS_MSE_errorcorr_stack*  0 *HPS_usable_stacksize* ))
			(setq *HPS_MSE_bound_0_mean_errorcorr_sum* (reduce #'+ *HPS_MSE_bound_0_window*))
			(setq *HPS_MSE_bound_0_window_lastval*     (float (nth *HPS_usable_stacksize* *HPS_MSE_errorcorr_stack*))))

	  	      ((> *HPS_iteration* *HPS_FILLER_SIZE*)
			(incf *HPS_MSE_bound_0_mean_errorcorr_sum* (float (- (first *HPS_MSE_errorcorr_stack* ) 
								                    *HPS_MSE_bound_0_window_lastval* )))
			(setq *HPS_MSE_bound_0_window_lastval*     (float (nth *HPS_usable_stacksize* *HPS_MSE_errorcorr_stack*)))))

		(setq *HPS_MSE_bound_0_mean_errorcorr_sum*         (float *HPS_MSE_bound_0_mean_errorcorr_sum* ))
		;; ********************************************************************************************************


		;; ********************************************************************************************************
		;; ********************************************************************************************************
		(setq *HPS_MSE_bound_0_mean_errorcorr* 	(float (/ *HPS_MSE_bound_0_mean_errorcorr_sum* *HPS_usable_stacksize* )))
		(setq *HPS_MSE_bound_0_mean_tmax_form* 	(float (* *HPS_mp* (HPS_square_of *HPS_tmax*))))
		(setq *HPS_MSE_bound_0* 		(float (sqrt (float (+ *HPS_MSE_bound_0_mean_tmax_form* 
								 (* 2 (HPS_square_of *HPS_MSE_bound_0_mean_errorcorr* )))))))
		;; ********************************************************************************************************

		(setq *HPS_MSE_bound* 			(float (* *HPS_MSE_bound_0* *HPS_MSE_bound_1*)))

		(return-from HPS_compute_MSE_errorbound *HPS_MSE_bound*)))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function retrieves one of various form of current MSE error accumulation
;; note these are individual SSE forms not partial sums, yet
;; ********************************************************************************************
(defun HPS_MSE_update_metrics ( )
	(if (>= *HPS_iteration* *HPS_mp*)
        	(setq *HPS_usable_stacksize* (- *HPS_mp* 1))
        	(setq *HPS_usable_stacksize* (- *HPS_iteration* 1)))

	(setq *HPS_SSE_form_stack_len* (length *HPS_SSE_form_stack*))
	(when (< *HPS_SSE_form_stack_len*  *HPS_usable_stacksize*) 
	  	(setq *HPS_usable_stacksize* *HPS_SSE_form_stack_len*  ))

	(when 	(equal *HPS_usable_stacksize* 0)
  		(setq *HPS_MSE_metric_window*  0 )								;; outlet 1
  		(setq *HPS_MSE_metric_segment* 0 )								;; outlet 2
  		(setq *HPS_MSE_metric_instant* 0 )								;; outlet 3
		(return-from HPS_MSE_update_metrics nil ))

	(unless (equal *HPS_usable_stacksize* 0)
		;; ********************************************************************************************************
		;; ********************************************************************************************************
	  	(cond ((< *HPS_iteration* *HPS_FILLER_SIZE*)
			(setq *HPS_SSE_window*            (subseq *HPS_SSE_form_stack* 0 *HPS_usable_stacksize* ))
  			(setq *HPS_MSE_metric_window_sum* (reduce #'+ *HPS_SSE_window*)))

		      ;; CHECK BOUNDARY PROBLEM WITH STACK SIZE BELOW, ONE OR THE OTHER IS WRONG, ALTERNATIVE COMP THEN WEIGHTS
		      ;; A VERY LARGE TAIL NUMBER AT END OF STACK
	  	      ((= *HPS_iteration* *HPS_FILLER_SIZE*)
			(setq *HPS_SSE_window*            (subseq *HPS_SSE_form_stack* 0 *HPS_usable_stacksize* ))
  			(setq *HPS_MSE_metric_window_sum* (reduce #'+ *HPS_SSE_window*))				
			(setq *HPS_SSE_window_lastval*    (float (nth (1- *HPS_usable_stacksize*) *HPS_SSE_form_stack* ))))

	  	      ((> *HPS_iteration* *HPS_FILLER_SIZE*)
			(incf *HPS_MSE_metric_window_sum* (float (- (first *HPS_SSE_form_stack* ) *HPS_SSE_window_lastval* )))
			(setq *HPS_SSE_window_lastval*    (float (nth (1- *HPS_usable_stacksize*) *HPS_SSE_form_stack* )))))

  		(setq *HPS_MSE_metric_window_sum* 	  (float *HPS_MSE_metric_window_sum* ))
		;; ********************************************************************************************************


		;; ********************************************************************************************************
		;; ********************************************************************************************************
		;; (setq *HPS_SSE_window*            (subseq *HPS_SSE_form_stack* 0 *HPS_usable_stacksize* ))
  		;; (setq *HPS_MSE_metric_window_sum* (reduce #'+ *HPS_SSE_window*))				

  		(setq *HPS_MSE_metric_instant*    	 (float (sqrt (first *HPS_SSE_form_stack* ))))	  	;; outlet 3
  		(setq *HPS_MSE_metric_window*     	 (float (sqrt (float (/ *HPS_MSE_metric_window_sum* 
									 	*HPS_usable_stacksize* )))))	;; outlet 1
		;; ********************************************************************************************************


		(when 	(> *HPS_lambda* *HPS_ATS_TRIVIAL_SEGMENT_DURATION*) 
        		(setq *HPS_usable_stacksize_lambda* (- *HPS_lambda* 1))

			(when (< *HPS_SSE_form_stack_len*  *HPS_usable_stacksize_lambda*) 
	  			(setq *HPS_usable_stacksize_lambda* *HPS_SSE_form_stack_len*  ))

			(when 	(equal *HPS_usable_stacksize_lambda* 0)
  				(setq *HPS_MSE_metric_segment* *HPS_MSE_metric_instant* ))			;; outlet 2

			(unless (equal *HPS_usable_stacksize_lambda* 0)
				(setq *HPS_SSE_window* 		   (subseq *HPS_SSE_form_stack* 0 *HPS_usable_stacksize_lambda* ))
  				(setq *HPS_MSE_metric_segment_sum* (float (reduce #'+ *HPS_SSE_window*)))
  				(setq *HPS_MSE_metric_segment*     (sqrt (float (/ *HPS_MSE_metric_segment_sum* 
										   *HPS_usable_stacksize_lambda* ))))))
														;; outlet 2
		(unless (> *HPS_lambda* *HPS_ATS_TRIVIAL_SEGMENT_DURATION*) 
  			(setq *HPS_MSE_metric_segment* *HPS_MSE_metric_instant* ))))
;; ********************************************************************************************




;; ;; ********************************************************************************************
;; ;; this function retrieves one of various form of current MSE error accumulation
;; ;; note these are individual SSE forms not partial sums, yet
;; ;; ********************************************************************************************
;; (defun HPS_MSE_update_metrics ( HPS_SSE_FORM_STACK_DUMMY_LABEL )
;; 	(if (>= *HPS_iteration* *HPS_mp*)
;;         	(setq *HPS_usable_stacksize* (- *HPS_mp* 1))
;;         	(setq *HPS_usable_stacksize* (- *HPS_iteration* 1)))
;; 
;; 	(setq *HPS_SSE_form_stack_len* (length *HPS_SSE_form_stack*))
;; 	(when (< *HPS_SSE_form_stack_len*  *HPS_usable_stacksize*) 
;; 	  	(setq *HPS_usable_stacksize* *HPS_SSE_form_stack_len*  ))
;; 
;; 	(when 	(equal *HPS_usable_stacksize* 0)
;;   		(setq *HPS_MSE_metric_window*  0 )								;; outlet 1
;;   		(setq *HPS_MSE_metric_segment* 0 )								;; outlet 2
;;   		(setq *HPS_MSE_metric_instant* 0 ))								;; outlet 3
;; 
;; 	(unless (equal *HPS_usable_stacksize* 0)
;; 		(setq *HPS_SSE_window*            (subseq *HPS_SSE_form_stack* 0 *HPS_usable_stacksize* ))
;;   		(setq *HPS_MSE_metric_instant*    (sqrt (first *HPS_SSE_window*)))					;; outlet 3
;; 
;;   		(setq *HPS_MSE_metric_window_sum* (reduce #'+ *HPS_SSE_window*))				
;;   		(setq *HPS_MSE_metric_window*     (sqrt (/ *HPS_MSE_metric_window_sum* *HPS_usable_stacksize* )))	;; outlet 1
;; 
;; 		(when 	(> *HPS_lambda* *HPS_ATS_TRIVIAL_SEGMENT_DURATION*) 
;;         		(setq *HPS_usable_stacksize_lambda* (- *HPS_lambda* 1))
;; 
;; 			(when (< *HPS_SSE_form_stack_len*  *HPS_usable_stacksize_lambda*) 
;; 	  			(setq *HPS_usable_stacksize_lambda* *HPS_SSE_form_stack_len*  ))
;; 
;; 			(when 	(equal *HPS_usable_stacksize_lambda* 0)
;;   				(setq *HPS_MSE_metric_segment* *HPS_MSE_metric_instant* ))			;; outlet 2
;; 
;; 			(unless (equal *HPS_usable_stacksize_lambda* 0)
;; 				(setq *HPS_SSE_window* 		   (subseq *HPS_SSE_form_stack* 0 *HPS_usable_stacksize_lambda* ))
;;   				(setq *HPS_MSE_metric_segment_sum* (reduce #'+ *HPS_SSE_window*))
;;   				(setq *HPS_MSE_metric_segment*     (sqrt (/ *HPS_MSE_metric_segment_sum* *HPS_usable_stacksize_lambda* )))))
;; 														;; outlet 2
;; 		(unless (> *HPS_lambda* *HPS_ATS_TRIVIAL_SEGMENT_DURATION*) 
;;   			(setq *HPS_MSE_metric_segment* *HPS_MSE_metric_instant* ))))
;; ;; ********************************************************************************************
;; 
;; 
;; 
;; 
;; 
;; 
;; 
