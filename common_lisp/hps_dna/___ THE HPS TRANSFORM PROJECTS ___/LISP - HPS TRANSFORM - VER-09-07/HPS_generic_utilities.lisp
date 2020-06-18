(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;; ********************************************************************************************
;;                            UTILITY FUNCTIONS OF THE HPS TRANSFORM                   
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


;; ********************************************************************************************************
;; these functions are used to accelerate the aging of a  global variable not longer needed, 
;; so that the garbage collector can pick the pieces of the elements that it referenced and
;; no longer are referenced. developed from templates from successful lisp online book
;; ********************************************************************************************************
(defun HPS_nilthis (nplace oflist)
  	(setf (nth nplace oflist) nil)
	oflist)
;; ********************************************************************************************************


;; ********************************************************************************************************
(defun HPS_setthis (nplace oflist toval)
  	(setf (nth nplace oflist) toval)
	toval)
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this fucntion wipes out to a list of nils the given list
;; ********************************************************************************************************
(defun HPS_wipeout ( thislist )
  	(when (listp thislist)
		(fill thislist nil))
	(setf thislist nil))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function implements a fixed length stack, this length defined by a global constant
;; ********************************************************************************************************
(defun HPS_PUSH_INTERNAL ( item intolist )
  	(if (> (length intolist) *HPS_FIXED_STACK_BIGLIMIT*)
	  	(setf intolist (push item (subseq intolist 0 *HPS_FIXED_STACK_LIMIT*)))
		(setf intolist (push item intolist))))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function implements a fixed length stack, this length defined by a global constant
;;   	`(if (> (length ,stack ) *HPS_FIXED_STACK_BIGLIMIT*)
;; ********************************************************************************************************
(defmacro HPS_PUSH ( item stack ) 
  	`(if (equal (mod *HPS_iteration* *HPS_FIXED_STACK_BIGLIMIT*) (1- *HPS_FIXED_STACK_BIGLIMIT*))
	  		(setf  ,stack 	(push ,item 	(subseq ,stack 0 *HPS_FIXED_STACK_LIMIT*)))
			(setf  ,stack 	(push ,item ,stack ))))
;; (defmacro HPS_PUSH ( item stack ) `(setf ,stack (HPS_PUSH_INTERNAL ,item ,stack)))
;; (defmacro HPS_PUSH ( item stack ) `(setf ,stack (PUSH ,item ,stack)))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this utility creates a replica of a list, otherwise, it returns the original
;; ********************************************************************************************************
(defun HPS_xcopy ( fromlist )
  	(if (listp fromlist)
		(setf tolist (copy-list fromlist))
		(setf tolist fromlist)))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this utility performs a safe sort from the cooper book by working on a replica and not the original
;; ********************************************************************************************************
(defun cooper-safe-sort (list predicate)
	(setf new-list (copy-list list))
	(sort new-list predicate))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this utility appends to strings to create one, used to create pathnames, but can be used for anything else
;; ********************************************************************************************************
(defun HPS_pathname ( basedir inputfile )
  	(concatenate 'STRING basedir inputfile))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; ********************************************************************************************************
(defun HPS_substitute_character ( fromchar intochar hps_origstring )
  	(setf fromchar 		(if (stringp fromchar) 		(char fromchar 0) 	nil))
  	(setf intochar 		(if (stringp intochar) 		(char intochar 0) 	nil))
	(setf hps_origstring 	(if (stringp hps_origstring) 	nil 		  	(format nil "~S" hps_origstring)))
	(setf hps_newstring (coerce (mapcar #'(lambda (a) (if (equal a fromchar ) intochar a )) (coerce hps_origstring 'LIST)) 'STRING))
	(return-from HPS_substitute_character hps_newstring))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; a convenience macro for generic application of the above function to strings
;; ********************************************************************************************************
(defun HPS_concat ( a b )
  	(HPS_pathname a b))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this utility determines whether a number is within a range
;; ********************************************************************************************************
(defmacro HPS_is_this_between ( this low high )
	`(if (AND (>= ,this ,low ) (<= ,this ,high)) t nil))
;; ********************************************************************************************************


;; ********************************************************************************************
;; this function simply determines if a value is within the range [low high]
;; ********************************************************************************************
(defun HPS_inbetween (low value high)
  	(if (AND (>= value low) (<= value high)) t nil))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function simply returns the closer value between "high" and "low" to "avalue" 
;; ********************************************************************************************
(defun HPS_theclosest (low avalue high)
  	(if (<= (- avalue low) (- high avalue)) low high))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function determines if a value is (exactly) found within a list and otherwise interpolates a close one 
;; ********************************************************************************************
(defun HPS_nearestvalue (value listofvalues)
  	(setf low (first listofvalues))
  	(dolist (aval listofvalues 'HPSCAN)
	  	(setf high aval) 	
	  	(if (HPS_inbetween low value high) (return-from HPS_nearestvalue (HPS_theclosest low value high)) (setf low aval)))
	(return-from HPS_nearestvalue high))
;; ********************************************************************************************


;; ********************************************************************************************************
;; this utility function simply squares a number 
;; ********************************************************************************************************
(defmacro  HPS_square_of ( anumber )
  	`(* ,anumber ,anumber))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this funtion performs an o(1) lookup over any entry in the original input time series.
;; ********************************************************************************************************
(defmacro HPS_getval_from_input_ts ( at_index )
  	`(aref *HPS_input_array* ,at_index))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this funtion performs an o(1) lookup over any entry in the original input time series.
;; ********************************************************************************************************
(defun HPS_getval ( at_index )
  	(if (< at_index 0)
  		(return-from HPS_getval (HPS_getval_from_input_ts 0 ))
  		(return-from HPS_getval (HPS_getval_from_input_ts (- at_index 1)))))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this general function computes the median for a windowed outlook contained within window
;; ********************************************************************************************************
(defun HPS_median ( window )
  	(when   (NOT (equal window nil))
  		(setq *HPS_temp_window* window)
		(setq *HPS_temp_window_sorted* (coerce (cooper-safe-sort *HPS_temp_window* #'<) 'VECTOR))
		(setq *HPS_temp_midpoint* (round (/ (length window) 2)))
		(setq *HPS_temp_nval*  (aref *HPS_temp_window_sorted* *HPS_temp_midpoint* ))
		(return-from HPS_median *HPS_temp_nval*))

  	(unless (NOT (equal window nil))
		(return-from HPS_median *HPS_TEMP_KLUDGE* )))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this general function computes the mean for a windowed outlook contained within window
;; ********************************************************************************************************
(defun HPS_true_mean ( window )
  	(when   (NOT (equal window nil))
		(setq *HPS_temp_sum* (reduce #'+ window))
		(setq *HPS_temp_wsize* (length window))
		(if (> *HPS_temp_wsize* 0)
			(setq *HPS_temp_nval* (/ *HPS_temp_sum* *HPS_temp_wsize* ))
			(setq *HPS_temp_nval* *HPS_temp_sum*)))

  	(unless (NOT (equal window nil))
		(setq *HPS_temp_nval* *HPS_original_sample*))		;; *HPS_TEMP_KLUDGE*

	(return-from HPS_true_mean *HPS_temp_nval*))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this general function returns the maximum of a windowed outlook
;; ********************************************************************************************************
(defun HPS_maximum (window)
  	(setq *HPS_max_nval* (first window))
  	(dolist (item window 'HPSMAX)
	  	(when (> item *HPS_max_nval*) 
		  	(setq *HPS_max_nval* item)))
	(return-from HPS_maximum (float *HPS_max_nval*)))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this general function returns the minimum of a windowed outlook
;; ********************************************************************************************************
(defun HPS_minimum (window)
  	(setq *HPS_max_nval* (first window))
  	(dolist (item window 'HPSMAX)
	  	(when (< item *HPS_max_nval*) 
		  	(setq *HPS_max_nval* item)))
	(return-from HPS_minimum (float *HPS_max_nval*)))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this general function computes the order 1 trimmed mean for a windowed outlook contained within window
;; ********************************************************************************************************
(defun HPS_trimmed_mean ( window &key ( trimming 2 ))
  	(when   (NOT (equal window nil))
  		(when 	(> *HPS_iteration* (length window))
			(setq *HPS_temp_sum* (reduce #'+ window))
			(setq *HPS_temp_wsize* (length window))

			(when   ( > trimming 2 )
			  	(setq window (cooper-safe-sort window #'>))
				(if (oddp trimming) (incf trimming) nil) 
				(setq window (subseq window (/ trimming 2) (- (length window) (/ trimming 2))))
				(setq *HPS_temp_sum* (reduce #'+ window))
				(setq *HPS_temp_wsize* (length window)))

			(unless ( > trimming 2 )
	  			(setq *HPS_trim_high* (HPS_maximum window))
	  			(setq *HPS_trim_low*  (HPS_minimum window))
				(setq *HPS_temp_sum* (- *HPS_temp_sum* *HPS_trim_high* *HPS_trim_low*))
				(setq *HPS_temp_wsize* (- (length window) 2)))

			(if (> *HPS_temp_wsize* 0)
				(return-from HPS_trimmed_mean (/ *HPS_temp_sum* *HPS_temp_wsize* ))
				(return-from HPS_trimmed_mean (HPS_true_mean window))))

  		(unless (> *HPS_iteration* (length window))
			(return-from HPS_trimmed_mean (HPS_true_mean window))))

  	(unless (NOT (equal window nil))
		(return-from HPS_trimmed_mean (HPS_true_mean window))))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this public function interfaces to the various mean type functions 
;; ********************************************************************************************************
(defun HPS_mean ( window &key ( meantype 'TRUE) (trimming 2))
  	(cond ((equal meantype 'TRIMMED) (return-from HPS_mean (float (HPS_trimmed_mean  window :trimming trimming))))
  	      ((equal meantype 'TRUE)    (return-from HPS_mean (float (HPS_true_mean     window ))))
	      (T 			 (return-from HPS_mean (float (HPS_true_mean     window ))))))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this general function computes the std deviation of a windowed outlook contained within window and having a given mean
;; (setf *HPS_temp_wsize* (- (length window) 1))				;; temp 
;; ********************************************************************************************************
(defun HPS_stddev ( window mean )
  	(when   (NOT (equal window nil))
  		(setq *HPS_temp_SE*           (mapcar #'(lambda (a) (HPS_square_of (- a mean))) window))
		(setq *HPS_temp_SOSE* 	      (float (reduce #'+ *HPS_temp_SE*)))
		(setq *HPS_temp_wsize*        (- (length window) 1))				;; was previously n

		(when   (> *HPS_temp_wsize* 0)
  			(setq *HPS_temp_MSSE* (float (/ *HPS_temp_SOSE* *HPS_temp_wsize*)))
			(setq *HPS_temp_nval* (sqrt *HPS_temp_MSSE*)))

		(unless (> *HPS_temp_wsize* 0)
			(setq *HPS_temp_nval* *HPS_TEMP_KLUDGE* ))

		(return-from HPS_stddev       (float *HPS_temp_nval*)))

  	(unless (NOT (equal window nil))
		(return-from HPS_stddev       *HPS_TEMP_KLUDGE*)))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function performs retrieves the variance associated with a stddev 
;; ********************************************************************************************************
(defun HPS_variance ( window stddev )
  	(when   (NOT (equal window nil))
		(return-from HPS_variance (HPS_square_of stddev )))

  	(unless (NOT (equal window nil))
		(return-from HPS_variance *HPS_TEMP_KLUDGE*)))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; ********************************************************************************************************
(defun HPS_subseq ( alist start end )
  	(when   (> end (length alist))
	  	(if (< start 0)
		  	(return-from HPS_subseq (subseq alist 0     (length alist)))
		  	(return-from HPS_subseq (subseq alist start (length alist)))))

  	(unless (> end (length alist))
	  	(if (< start 0)
		  	(return-from HPS_subseq (subseq alist 0     end ))
		  	(return-from HPS_subseq (subseq alist start end )))))
;; ********************************************************************************************************

;; ********************************************************************************************************
;; this function computes the corresponding windowed outlook.
;; ********************************************************************************************************
(defun HPS_get_movwin_outlook (&key (FROMINDEX 	at_index) 
				    (OFSIZE 	of_size)) 

  	(let* ( (*HPS_frame_end*      FROMINDEX )
  		(*HPS_frame_start* (- FROMINDEX OFSIZE )) )

		(when 	(< *HPS_frame_end* 0) 
			(return-from HPS_get_movwin_outlook nil ))

		(when 	(< *HPS_frame_start* 0)
			(setq *HPS_frame_start* 0))

		(setq *HPS_temp_frame*   (coerce (subseq *HPS_input_array* *HPS_frame_start* *HPS_frame_end* ) 'LIST))

		(return-from HPS_get_movwin_outlook *HPS_temp_frame* )))
;; ********************************************************************************************************


;; ****************************************************************************************************
;; this will become hps_math
;; this function computes the gaussian fit for x given mu and sigma, used to determine the amplitude A
;; ****************************************************************************************************
(defun HPS_gaussian_A1 ( x mu sigma )
  	(when (< (abs sigma) *HPS_EPSILON*) 
	  	(setq sigma *HPS_EPSILON*) 
		nil)

	(setq hps_gaussian_value_for_x 
	      (float (* (/ 1 (* 2 sigma pi)) (exp (* -1 (/ (HPS_square_of (- x mu)) (* 2 (HPS_square_of sigma)))))))))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; this function is a long-way to compute expt.
;; ****************************************************************************************************
(defmacro HPS_power ( a b ) 
  	`(expt ,a ,b))
;; ****************************************************************************************************

