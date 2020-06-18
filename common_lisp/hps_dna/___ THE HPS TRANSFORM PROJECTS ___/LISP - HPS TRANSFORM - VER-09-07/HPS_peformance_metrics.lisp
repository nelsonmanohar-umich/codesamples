(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;;; ***********************************************************************************************************
;;; ****************          ONLINE PERFORMANCE METRICS FOR THE HPS TRANSFORM            *********************
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


;; ***********************************************************************************************************
(setf *HPS_EPSILON* 0.000001)
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function computes "an approximate z-value" --- normality tracking index
;; ***********************************************************************************************************
(setf *HPS_BAD_ZVALUE* 6.001)
(defun HPS_compute_perf_z_value_for ( ivalue mean sigma )
  	(if (> (abs sigma) *HPS_EPSILON*) 
  		(return-from HPS_compute_perf_z_value_for (setf *HPS_temp_perf_nval* (/ (- ivalue mean) sigma)))
  		(return-from HPS_compute_perf_z_value_for *HPS_BAD_ZVALUE* )))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function computes "an approximate f-value" --- homogeneity of variances tracking index
;; ***********************************************************************************************************
(setf *HPS_BAD_FRATIO* -1.001)
(defun HPS_compute_perf_f_value_for ( var1 var2 )
  	(setf *HPS_temp_perf_maxvar*  (max var1 var2))
  	(setf *HPS_temp_perf_minvar*  (min var1 var2))
  	(if (> (abs *HPS_temp_perf_maxvar*  ) *HPS_EPSILON*) 
  		(return-from HPS_compute_perf_f_value_for (setf *HPS_temp_perf_nval* (/ *HPS_temp_perf_minvar*  *HPS_temp_perf_maxvar* )))
  		(return-from HPS_compute_perf_f_value_for *HPS_BAD_FRATIO* )))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function does compute the correlation between "sampling mean" windowed outlooks but rather an 
;; approximate p-value" correlation between "input_signal" windowed outlooks with size = min (m, mp)
;; ***********************************************************************************************************
(setf *HPS_BAD_P_INDEX_VALUE* pi)
(defun HPS_compute_perf_p_value_for ( win_outlook1 win_outlook2 )
  	(when (AND win_outlook1 win_outlook2)
  		(setf *HPS_temp_perf_covwindow* (mapcar #'(lambda (a b) (* a b)) win_outlook1 win_outlook2))
  		(setf *HPS_temp_perf_covwindow_mean_mp* (HPS_mean *HPS_temp_perf_covwindow* ))

  		(setf *HPS_temp_perf_truncated_window_m* (subseq *HPS_window_m* 0 (min *HPS_mp* (length *HPS_window_m*))))
		(setf *HPS_mean_m_truncated* (HPS_mean   *HPS_temp_perf_truncated_window_m* ))
		(setf *HPS_std_m_truncated*  (HPS_stddev *HPS_temp_perf_truncated_window_m* *HPS_mean_m_truncated* ))

		(setf *HPS_temp_perf_std_divider* (* *HPS_std_mp* *HPS_std_m_truncated*))
		(when   (< *HPS_temp_perf_std_divider* *HPS_EPSILON*)
			(return-from HPS_compute_perf_p_value_for *HPS_BAD_P_INDEX_VALUE* ))

		(unless (< *HPS_temp_perf_std_divider* *HPS_EPSILON*)
			(setf *HPS_temp_perf_nval* (* *HPS_mean_mp* *HPS_mean_m_truncated*))
			(setf *HPS_temp_perf_nval* (- *HPS_temp_perf_covwindow_mean_mp* *HPS_temp_perf_nval*))
			(setf *HPS_temp_perf_nval* (/ *HPS_temp_perf_nval* *HPS_temp_perf_std_divider* ))
			(return-from HPS_compute_perf_p_value_for *HPS_temp_perf_nval* )))

  	(unless (AND win_outlook1 win_outlook2)
		(return-from HPS_compute_perf_p_value_for 0)))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function computes a rough approximation used to illustrate idea behind aposteriori sampling size
;; semantics were changed to the inverse of the actual curve for analysis purposes CHECK !!!!!!!!!!!!
;; ***********************************************************************************************************
(setf *HPS_N_UPPER_LIMIT* 256)
(defun HPS_compute_perf_n_value_for ( mean1 mean2 sigma )
	(setf *HPS_temp_perf_divider* (abs (- mean1 mean2 )))

  	(when   (> *HPS_temp_perf_divider*  *HPS_EPSILON*) 
		(setf *HPS_temp_perf_nval* (+ *HPS_tmax* *HPS_tmax* ))
	  	(setf *HPS_temp_perf_nval* (* *HPS_temp_perf_nval* sigma ))
	  	(setf *HPS_temp_perf_nval* (/ *HPS_temp_perf_nval* *HPS_temp_perf_divider* ))
	  	(setf *HPS_temp_perf_nval* (HPS_square_of *HPS_temp_perf_nval*))

		(if (> *HPS_temp_perf_nval* *HPS_N_UPPER_LIMIT*)
  			(return-from HPS_compute_perf_n_value_for (-  *HPS_N_UPPER_LIMIT* *HPS_N_UPPER_LIMIT* ))
  			(return-from HPS_compute_perf_n_value_for (- *HPS_N_UPPER_LIMIT* *HPS_temp_perf_nval* ))))

  	(unless (> *HPS_temp_perf_divider*  *HPS_EPSILON*) 
  		(return-from HPS_compute_perf_n_value_for (- *HPS_N_UPPER_LIMIT* *HPS_N_UPPER_LIMIT* ))))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function computes the relative error of the HPS forecast
;; ***********************************************************************************************************
(setf *HPS_BAD_REL_ERROR* 1.001)
(defun HPS_compute_perf_rel_error_for ( forecast origsample )
  	(when   (> (abs origsample) *HPS_EPSILON*) 
		(setf *HPS_temp_perf_nval* (- forecast origsample ))
	  	(setf *HPS_temp_perf_nval* (/ *HPS_temp_perf_nval* origsample ))
  		(return-from HPS_compute_perf_rel_error_for *HPS_temp_perf_nval* )) ;;; CHECK !!!!!!!!!!!!!!!!!!!

  	(unless (> (abs origsample) *HPS_EPSILON*) 
  		(return-from HPS_compute_perf_rel_error_for *HPS_BAD_REL_ERROR* )))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function computes an approximate z-error over a window size of m' extracted from an error stack
;; ***********************************************************************************************************
(setf *HPS_BAD_Z_ERROR* 6.100)
(defun HPS_compute_perf_z_i_error_for ( error_stack )
  	(setf *HPS_usable_error_stacksize* (length error_stack ))
  	(when   (> *HPS_usable_error_stacksize* 0 ) 
	  	(if (> *HPS_usable_error_stacksize* *HPS_mp*)
	  		(setf *HPS_temp_perf_error_window* (subseq error_stack  0 *HPS_mp*))
	  		(setf *HPS_temp_perf_error_window* error_stack ))

	  	(setf *HPS_temp_perf_error_sample* (first *HPS_temp_perf_error_window* ))
	  	(setf *HPS_temp_perf_error_mean* (HPS_mean *HPS_temp_perf_error_window* ))
	  	(setf *HPS_temp_perf_error_std* (HPS_stddev *HPS_temp_perf_error_window* *HPS_temp_perf_error_mean* ))

		(when   (> *HPS_temp_perf_error_std* *HPS_EPSILON* )
  			(setf *HPS_temp_perf_nval* (- *HPS_temp_perf_error_sample* *HPS_temp_perf_error_mean* ))
  			(setf *HPS_temp_perf_nval* (/ *HPS_temp_perf_nval* *HPS_temp_perf_error_std* ))
  			(return-from HPS_compute_perf_z_i_error_for *HPS_temp_perf_nval* ))

		(unless (> *HPS_temp_perf_error_std* *HPS_EPSILON* )
  			(return-from HPS_compute_perf_z_i_error_for *HPS_BAD_Z_ERROR* )))

  	(unless (> *HPS_usable_error_stacksize* 0 ) 
  		(return-from HPS_compute_perf_z_i_error_for 0 )))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; ***********************************************************************************************************
(defun HPS_compute_diagnostic_metrics ( )
  	(setf *HPS_perf_z_means_i* (HPS_compute_perf_z_value_for *HPS_new_forecast* *HPS_mean_mp* *HPS_std_mp* ))
  	(setf *HPS_perf_f_s_rat_i* (HPS_compute_perf_f_value_for *HPS_std_m* *HPS_std_mp* ))
  	(setf *HPS_perf_p_mucor_i* (HPS_compute_perf_p_value_for *HPS_window_m* *HPS_window_mp*))
  	(setf *HPS_perf_n_sampl_i* (HPS_compute_perf_n_value_for *HPS_mean_mp* *HPS_mean_m* *HPS_pooled_std* ))
  	(setf *HPS_perf_e_relat_i* (HPS_compute_perf_rel_error_for *HPS_new_forecast* *HPS_original_sample* ))
  	(setf *HPS_perf_z_error_i* (HPS_compute_perf_z_i_error_for  *HPS_error_fast_stack* )))
;; ***********************************************************************************************************









