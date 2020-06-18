(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;;; ***********************************************************************************************************
;;; ********************************* CHECK FOR APPROXIMATE LOCALIZED CONDITIONS ******************************
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
;; this function implements a deceptively simple test for approximate localized stationary conditions
;; using a paired unknown mean and unknown variance t-test for unequal sampled populations.
;; it returns 1 (true) if the windowed outlooks for the RECENT PAST and the PRESENT differ in a 
;; statistical significant manner and 0 otherwise. Therefore, a 1 always represents a change in whatever
;; was the previous stationary state (if any) of the HPS approximation.
;;; ***********************************************************************************************************
(defun HPS_conjecture_for_approximate_stationarity ( mean_m mean_mp var_m var_mp m mp )
	;; ***************************************************************************************************
	;; student t-test for sampled populations, unequal variances, unknown means, unknown distributions
	;; ***************************************************************************************************
	(when (>= *HPS_iteration* *HPS_m_plus_mp* ) 
		(setq *HPS_mean_diff* 		(float (- mean_m mean_mp )))		
		(setq *HPS_pooled_var_main* 	(float (/ (+ (* (1- m) var_m) (* (1- mp) var_mp)) (- *HPS_m_plus_mp* 2))))
		;; (setq *HPS_pooled_var_comp* 	(float (+ (/ 1 m) (/ 1 mp))))
		;; (setq *HPS_pooled_std* 		(* (sqrt *HPS_pooled_var_main*) (sqrt *HPS_pooled_var_comp*) ))
		(setq *HPS_pooled_std* 		(float (* (sqrt *HPS_pooled_var_main*) *HPS_POOLED_VAR_COMP* )))
		(setq *HPS_pooled_var* 		(float (HPS_square_of *HPS_pooled_std*)))

		(if (< (abs *HPS_pooled_std*)  *HPS_EPSILON*)
		  	(if (< (abs *HPS_mean_diff*) *HPS_EPSILON* )
				(setq *HPS_tval* *HPS_TEMP_KLUDGE_ZERO*)
				(setq *HPS_tval* *HPS_TEMP_KLUDGE*))
			(setq *HPS_tval* (float (/ *HPS_mean_diff* *HPS_pooled_std*))))

		(setq *HPS_TTEST* 		*HPS_tval*))
	;; ***************************************************************************************************

	;; ***************************************************************************************************
	(unless (>= *HPS_iteration* *HPS_m_plus_mp* ) 
		(setq *HPS_mean_diff* 		(float (- mean_m mean_mp )))
		(setq *HPS_pooled_std_window* 	(subseq *HPS_input_series* 0 *HPS_iteration*))
		(setq *HPS_pooled_std_mean* 	(float (HPS_mean *HPS_pooled_std_window* )))
		(setq *HPS_pooled_std* 		(float (HPS_stddev *HPS_pooled_std_window* *HPS_pooled_std_mean* )))
		(setq *HPS_pooled_var* 		(float (HPS_square_of *HPS_pooled_std*)))
		(setq *HPS_TTEST* 		*HPS_TMAX* ))
	;; ***************************************************************************************************

	;; ***************************************************************************************************
	;; two-sided confidence interval test - CHECK - TMAX OR TTEST?????????????????? 
	;; ***************************************************************************************************
	(setq *HPS_confintv_low*  (float (- *HPS_mean_diff* (* *HPS_pooled_std* *HPS_TMAX*))))
	(setq *HPS_confintv_high* (float (+ *HPS_mean_diff* (* *HPS_pooled_std* *HPS_TMAX*))))
	;; ***************************************************************************************************

	;; ***************************************************************************************************
  	;; (format t "~%L0: i-th:[~6D]	[delta_mu(~8,2F ~8,2F)	sigmas(~8,2F ~8,2F)]	[ttest(~8,2F ~8,2F)]	[delta_mu-ci(~8,2F ~8,2F)]" 
			;; *HPS_iteration* mean_m mean_mp var_m var_mp *HPS_TTEST* *HPS_tmax* *HPS_confintv_low* *HPS_confintv_high*   )
  	;; (when (equal (mod *HPS_iteration* *HPS_OUTPUT_MINOR_TICK* ) 0)
  		;; (format t "~%L0: i-th:[~6D]" *HPS_iteration* ))
	;; ***************************************************************************************************

	;; ***************************************************************************************************
	;; returns true if the confidence interval limits test fails wrt to equality of the two means which as a 
	;; probabilistic consequence indicate that the two windowed outlooks currently represent significantly 
	;; different populations
	(if (HPS_is_this_between 0.0 *HPS_confintv_low* *HPS_confintv_high*)
		(return-from HPS_conjecture_for_approximate_stationarity nil )
		(return-from HPS_conjecture_for_approximate_stationarity t )))
	;; ***************************************************************************************************

	;; old version, no longer reached 
	;; returns true if the test fails and the two windowed outlooks represent significantly different populations
	;; (if (> (abs *HPS_TTEST*) *HPS_tmax*) 
		;; (return-from HPS_conjecture_for_approximate_stationarity t )
		;; (return-from HPS_conjecture_for_approximate_stationarity nil )))
;;; ***********************************************************************************************************


;;; ***********************************************************************************************************
;; this function creates a data representation for an ATS SEGMENT
;;; ***********************************************************************************************************
(defun HPS_ATS_span_segment ( ats_startindex ats_lambdaval ats_segvalue )
	;; ***************************************************************************************************
	(setq *ATS_SEG* nil)
  	(setq *ATS_SEG_time_index* (+ ats_startindex 1) )				 ;; CHECK tempfix 
	;; ***************************************************************************************************

	;; ***************************************************************************************************
  	(dotimes (iter ats_lambdaval 'PRINTATSSEG)
	  	(setq *ATS_SEG_orig_val* (HPS_getval *ATS_SEG_time_index* ))
	  	(setq *ATS_SEG_tuple*    (list *ATS_SEG_time_index* ats_segvalue iter *ATS_SEG_orig_val* ))
	  	(push *ATS_SEG_tuple*    *ATS_SEG*)
		(incf *ATS_SEG_time_index*))
	;; ***************************************************************************************************

	(return-from HPS_ATS_span_segment (reverse *ATS_SEG*)))
;;; ***********************************************************************************************************












