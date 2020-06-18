(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;; ********************************************************************************************************
;; *************************** THE HPS TRANSFORM :   HISTOGRAM MAKER      *********************************
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
;; these functions compute optional histograms used to analyze input and output time series.  Note that 
;; this computation may be time consuming and increase time complexity of the run, in particular, this 
;; implementation increases time complexity, from linear time O( c1 N) to O(c2 N log N).
;; ********************************************************************************************
(setq *HPS_HIST_EPSILON*	0.00000000001)
(defun HPS_histogram_setup ( atimeseries 
			     &key ( NUMBINS 	100 )
			          ( HPS_TERSE 	t ))

  	;; chop initial values of time series if deemed necessary
	;; (setq *HPS_truncated_histogram_series* (HPS_xcopy atimeseries))
	(setq *HPS_truncated_histogram_series* atimeseries)
	
	;; sort the truncated series
	(setq *HPS_sorted_histogram_series* (cooper-safe-sort *HPS_truncated_histogram_series* #'<))

  	(if (HPS_is_this_between numbins 1 1001)
	  	(setq *HPS_histogram_nbins* (round numbins))
	  	(setq *HPS_histogram_nbins* 20 ))

	(setq *HPS_histogram_max* (+ (first (last  *HPS_sorted_histogram_series*)) *HPS_HIST_EPSILON* ))
	(setq *HPS_histogram_min* (- (first *HPS_sorted_histogram_series*) *HPS_HIST_EPSILON* ))

	(when (NOT HPS_TERSE) 
	  (format t "~%[~8:S ~8:S]	[~8:S ~8:S]" (HPS_minimum *HPS_truncated_histogram_series* ) *HPS_histogram_min* 
						     (HPS_maximum *HPS_truncated_histogram_series* ) *HPS_histogram_max*) 
	  nil)

	(setq *HPS_histogram_stepsize* ( - *HPS_histogram_max* *HPS_histogram_min* ))
	(setq *HPS_histogram_stepsize* (/ *HPS_histogram_stepsize* *HPS_histogram_nbins* ))

	(setq *HPS_histogram_stack* nil)
	(setq *HPS_histogram_rangesize_stack* nil)
	(setq *HPS_histogram_rangelow_stack* nil)
	(setq *HPS_histogram_rangehigh_stack* nil)
	(setq *HPS_histogram_rangeelems_stack* nil)

	(when (NOT HPS_TERSE) 
		(setq *HPS_histogram_header* "******   BIN_SIZE	BIN_LOW_	BIN_HIGH	BINELEMS")
		(format t "~S" *HPS_histogram_header*) 
		nil)

	(setq *HPS_histogram_lastitem* (first *HPS_sorted_histogram_series*))
	(dotimes (j *HPS_histogram_nbins* 'BINSORTING)
	  	(setq *HPS_histogram_index_low* (+ *HPS_histogram_min* (- (* j *HPS_histogram_stepsize*) (/ *HPS_HIST_EPSILON* 2))))

		(if (> (+ j 1)  *HPS_histogram_nbins*)
	  		(setq *HPS_histogram_index_high* (- (length *HPS_sorted_histogram_series* ) 1))
	  		(setq *HPS_histogram_index_high* (+ *HPS_histogram_min* (- (* (+ j 1) *HPS_histogram_stepsize*) *HPS_HIST_EPSILON*))))

		(setq *HPS_histogram_rangetemp* (remove-if-not #'(lambda (a) 
							  (HPS_is_this_between a *HPS_histogram_index_low* *HPS_histogram_index_high* ))
							  (member *HPS_histogram_lastitem* *HPS_sorted_histogram_series*)))
							  ;; *HPS_sorted_histogram_series*))
		
		;; this is a tentative fix to the problem of unnecessary linear scanning - 
		;; however it solves nothing yet as member repeats the work attempted to be avoided here
		(setq *HPS_histogram_lastitem* (first *HPS_histogram_rangetemp* ))
		(when (equal *HPS_histogram_lastitem* nil)
			(setq *HPS_histogram_lastitem* (first *HPS_sorted_histogram_series*))
			nil)

		(setq *HPS_histogram_tuple_length* 	(length *HPS_histogram_rangetemp*))
		(setq *HPS_histogram_tuple_low*    	*HPS_histogram_index_low*) 
		(setq *HPS_histogram_tuple_high*    	*HPS_histogram_index_high*)

		(setq *HPS_histogram_tuple* 		(list *HPS_histogram_tuple_length* 
							      *HPS_histogram_tuple_low* 
							      *HPS_histogram_tuple_high* 
							      *HPS_histogram_rangetemp* ))

		(push *HPS_histogram_tuple* 		*HPS_histogram_stack*)

		(push *HPS_histogram_tuple_length* 	*HPS_histogram_rangesize_stack*)
		(push *HPS_histogram_tuple_low*    	*HPS_histogram_rangelow_stack*)
		(push *HPS_histogram_tuple_high*    	*HPS_histogram_rangehigh_stack*)
		(push *HPS_histogram_rangetemp* 	*HPS_histogram_rangeelems_stack*)

		(when (NOT HPS_TERSE) 
			(format t "~%[bin-~3:S] ~8:S	~8:S	~8:S	~60:S" j *HPS_histogram_tuple_length* 
									    		 *HPS_histogram_tuple_low* 
									    		 *HPS_histogram_tuple_high* 
									    		 *HPS_histogram_rangetemp* ) 
			nil))

	(setq *HPS_histogram_dataset* (list *HPS_histogram_rangesize_stack* *HPS_histogram_rangelow_stack* 
					  *HPS_histogram_rangehigh_stack* *HPS_histogram_rangeelems_stack*)))
;; ********************************************************************************************


;; ********************************************************************************************
;; these are external retrieval element functions over the histogram dataset object
;; ********************************************************************************************
(defun HPS_histogram_get_bin_freqcount ( histogram_dataset )
  	(return-from HPS_histogram_get_bin_freqcount (first histogram_dataset)))

(defun HPS_histogram_get_bin_boundaries ( histogram_dataset )
  	(return-from HPS_histogram_get_bin_boundaries (list (second histogram_dataset) (third histogram_dataset))))

(defun HPS_histogram_get_bin_elements ( histogram_dataset )
  	(return-from HPS_histogram_get_bin_elements (fourth histogram_dataset)))
;; ********************************************************************************************


;; ********************************************************************************************
;; CHECK - (POSITION ()) DOES THIS - CHANGE LATER
;; ********************************************************************************************
(defun HPS_histogram_find_binnum_for ( histogram_dataset 
				       binquality_value 
				       &key (binquality_sought 'frequency) 
					    (binquality_given  'range))

  	(setq hist_dataset_temp (mapcar #'(lambda (a b c d ) (list a b c d)) 	(first  histogram_dataset) 
										(second histogram_dataset)
										(third  histogram_dataset)
										(fourth histogram_dataset)))
  	
  	(cond 	((AND (equal binquality_sought 'frequency) (equal binquality_given  'range)) 
			(setq *HPS_search_temp* (mapcar #'(lambda (a) (if (HPS_is_this_between binquality_value 
											 (second a) 
											 (third  a)) 
					      			  	 	a
								  	 	nil)) 
							   	 hist_dataset_temp)))

  	      	((AND (equal binquality_sought 'range) (equal binquality_given  'frequency)) 	;; NOT general CHECK, unimodal only 
			(setq *HPS_search_temp* (mapcar #'(lambda (a) (if (HPS_is_this_between binquality_value 
											 (- (first a) 1) 
											 (+ (first a) 1))
					      			  	 	a
								  	 	nil))
							   	 hist_dataset_temp)))
  	      	(T nil))

	(setq *HPS_hist_iter* 0)
	(dolist (bin_tuple *HPS_search_temp* 'nonnilsearch)
	  	(when   (first bin_tuple)
		  	(return-from HPS_histogram_find_binnum_for bin_tuple))
		(unless (first bin_tuple)
			(incf *HPS_hist_iter*)))
	(return-from HPS_histogram_find_binnum_for nil ))
;; ********************************************************************************************


;; ********************************************************************************************
;; ********************************************************************************************
(defun HPS_histogram_sorter ( bin1 bin2 )
  (if (> (first bin1) (first bin2)) t nil))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function sorts the histogram bins by frequency
;; ********************************************************************************************
(defun HPS_histogram_sort_by_frequencies ( histogram_dataset )
        (setq *HPS_histogram_dataset_sorted_frequencies* nil)

  	(let* 	((hist_dataset_tuples		nil))
  		(setq hist_dataset_tuples	(mapcar #'(lambda (a b c d ) (list a b c d)) 	
						(first  histogram_dataset)  	;; bin-freq
						(second histogram_dataset)	;; bin-start
						(third  histogram_dataset)	;; bin-end
						(fourth histogram_dataset)))	;; bin-elems

		(setq *HPS_histogram_dataset_sorted_frequencies* 
		      				(cooper-safe-sort hist_dataset_tuples	#'HPS_histogram_sorter)))

	(return-from HPS_histogram_sort_by_frequencies *HPS_histogram_dataset_sorted_frequencies* ))
;; ********************************************************************************************


;; ********************************************************************************************
;; finds the FIRST bin tuple associated with a given histogram frequency, whether such is unique or not
;; should be changed to return all instances  (member, remove if
;; ********************************************************************************************
(defun HPS_histogram_find_bin_for_frequency ( frequency_val histogram_dataset 
					      &key ( SORTED_HISTOGRAM_DATASET nil) )

  	(when   (NOT SORTED_HISTOGRAM_DATASET )
		(setq SORTED_HISTOGRAM_DATASET (HPS_histogram_sort_by_frequencies histogram_dataset )))

	(dolist (bin_tuple SORTED_HISTOGRAM_DATASET 'BINSORTLOOKUP)
		(when (equal frequency_val (first bin_tuple))
		  	(return-from HPS_histogram_find_bin_for_frequency bin_tuple)))

	(return-from HPS_histogram_find_bin_for_frequency nil)) 
;; ********************************************************************************************


;; ********************************************************************************************
;; ********************************************************************************************
(defun HPS_histogram_find_frequency_for_bin ( bin_val histogram_dataset 
						      &key ( HIST_DATASET_TUPLES nil ))

  	(when 	(NOT HIST_DATASET_TUPLES)
  		(setq hist_dataset_tuples     (mapcar #'(lambda (a b c d ) (list a b c d)) 	
							(first  histogram_dataset)  	;; bin-freq
							(second histogram_dataset)	;; bin-start
							(third  histogram_dataset)	;; bin-end
							(fourth histogram_dataset))))	;; bin-elems

	(dolist (bin_tuple HIST_DATASET_TUPLES	'BINSORTLOOKUP)
		(when (HPS_is_this_between bin_val (second bin_tuple) (third bin_tuple)) 
		  	(return-from HPS_histogram_find_frequency_for_bin bin_tuple)))

	(return-from HPS_histogram_find_frequency_for_bin nil))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function draws a simple equally-spaced bar-symbol chart representing a histogram of frequencies subject to
;; small display constraints
;; ********************************************************************************************
(defun HPS_histogram_ascii ( xstarts yvalues )
  	(dolist (itempair yvalues 'HISTROWS) 
	  	(setq *HPS_histogram_ascii_temp* (pop xstarts))
	  	(format t "~&[~8S (~10S)]	--> " (round *HPS_histogram_ascii_temp*) *HPS_histogram_ascii_temp* )
	  	(dotimes (j itempair 'HISTDOTS)
		  	(format t "." nil))))
;; ********************************************************************************************


;; ********************************************************************************************
;; simple one histogram per file writer function
;; ********************************************************************************************
(defun HPS_histogram_writer ( intofilename xstarts yvalues ) 
	(with-open-file (output-stream intofilename :direction :output :if-exists :supersede :if-does-not-exist :create) 
	  (with-output-to-string ( HPS-histogram-buffer )
	  	(setq *HPS_hist_sum_y* 0)
		(setq *HPS_hist_sum_y_total* (reduce #'+ yvalues))
	  	(dolist (dataitem yvalues 'HPSHIST_DATASET) 
		  	(setq *HPS_hist_x* (pop xstarts))
		  	(setq *HPS_hist_y* dataitem )
			(incf *HPS_hist_sum_y* *HPS_hist_y*)
			(setq *HPS_hist_sum_y_prob* (/ *HPS_hist_sum_y* *HPS_hist_sum_y_total* ))
			(setq *HPS_hist_y_prob* (/ *HPS_hist_y* *HPS_hist_sum_y_total* ))
	  		(format HPS-histogram-buffer "~&~18,6F	~12,2F	~12,6F	~12,6F" *HPS_hist_x* 	  *HPS_hist_y* 
										        *HPS_hist_y_prob* *HPS_hist_sum_y_prob*))
	  		(format output-stream "~A" (get-output-stream-string HPS-histogram-buffer)))))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function is an interface to the generation of histogram data for a time series, a timeseries is parsed into
;; numbins bins and frequency data for each such bin is generated. Additionally, if necessary, membership information
;; of timeseries elements into each bin is provided.
;; ********************************************************************************************
(defun HPS_histogram ( atimeseries intofilename &key (numbins 100))
  	(setq *HPS_histogram_dataset* 	(HPS_histogram_setup atimeseries :numbins numbins))
	(setq *HPS_histogram_intstarts* (reverse (first *HPS_histogram_dataset*)))
	(setq *HPS_histogram_intvalues* (reverse (second *HPS_histogram_dataset*)))
	(HPS_histogram_writer intofilename *HPS_histogram_intvalues* *HPS_histogram_intstarts* ) 
	(return-from HPS_histogram *HPS_histogram_dataset* ))
;; ********************************************************************************************










