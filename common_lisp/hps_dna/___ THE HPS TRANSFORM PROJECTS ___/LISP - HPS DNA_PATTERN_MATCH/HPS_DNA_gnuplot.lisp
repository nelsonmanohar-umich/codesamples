(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; ********************************************************************************************
;; 			        GNUPLOT GENERATOR FOR DNA PATTERN MINER
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


;; ***********************************************************************************************************************
;; this creates a gnuplot label for a pattern match at coordinates x y 
;; ***********************************************************************************************************************
(defun HPS_DNA_make_patmatch_label ( patnum p &key (HPS_TERSE t))
  	(setq HPS_numsegs_temp (length (HPS_DNA_get_pattern_mapping_set pattern)))
	(if (NOT HPS_TERSE)
		(setq pat_label (format nil "M~D [~D:~D] (~D/~D)"
					patnum
			                (HPS_DNA_get_pattern_match_start  p)
			                (HPS_DNA_get_pattern_match_end    p)
			       	        (round (* (HPS_DNA_get_pattern_match_rating p) HPS_numsegs_temp))
			       	        HPS_numsegs_temp))
		(setq pat_label (format nil "M~D [~D:~D]"
					patnum
			                (HPS_DNA_get_pattern_match_start  p)
			                (HPS_DNA_get_pattern_match_end    p))))
	(return-from HPS_DNA_make_patmatch_label pat_label))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; create a text label to encode the pattern miner's resultant match quality for the segment
;; this quality is not the quality of the match, as the resultant quality is derived from the 
;; probability of all these three dimensional matches taking place at the same time, for details
;; see HPS_DNA_readme.lisp
;; ***********************************************************************************************************************
(defun HPS_DNA_GNUPLOT_label_segment_quality ( mapping_elem )
  	(setq match_quality (HPS_DNA_get_pattern_mappings_matching_quality mapping_elem))
	(cond	((equal match_quality  'EXACTING_MATCH) (setq *HPS_DNA_format_string* "E1"))
		((equal match_quality  'APPROX32_MATCH) (setq *HPS_DNA_format_string* "E2"))
		((equal match_quality  'APPROX64_MATCH) (setq *HPS_DNA_format_string* "E3"))
		((equal match_quality  'SPANNING_MATCH) (setq *HPS_DNA_format_string* "E4"))
		((equal match_quality  'EXACTING_MISPL) (setq *HPS_DNA_format_string* "A1"))
		((equal match_quality  'APPROX32_MISPL) (setq *HPS_DNA_format_string* "A2"))
		((equal match_quality  'APPROX64_MISPL) (setq *HPS_DNA_format_string* "A3"))
		((equal match_quality  'SPANNING_MISPL) (setq *HPS_DNA_format_string* "A4"))
		((equal match_quality  'NOTFOUND_WARNG) (setq *HPS_DNA_format_string* nil ))
		(T 					(setq *HPS_DNA_format_string* nil)))
	(return-from HPS_DNA_GNUPLOT_label_segment_quality *HPS_DNA_format_string*))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; construct gnuplot labels for selected segments in a pattern match with the specified datatype's value for the segment 
;; ***********************************************************************************************************************
(defun HPS_DNA_GNUPLOT_label_segments ( pattern &key (datatype 'TVALS))
	 (with-output-to-string ( *HPS_internal_buffer* ) 
	 	(setq mapping_set (HPS_DNA_get_pattern_mapping_set pattern))

	 	(dolist (mapping-element  mapping_set  'LABELINGOFSEGMENTS) 
	        	(setq seg_label (HPS_DNA_GNUPLOT_label_segment_quality  mapping-element ))

	        	(when   seg_label 
				(setq seg_start (first (HPS_DNA_get_pattern_mappings_matching_segment mapping-element)))
				(setq seg_tval         (HPS_DNA_get_pattern_mappings_targeting_value  mapping-element ))
				(setq seg_sdur         (HPS_DNA_get_pattern_mappings_lambda_value     mapping-element ))

				(setq plot_x 	       (if (equal datatype 'TVALS) 
							 	(incf seg_start 1) 
								(incf seg_start 1)))

				(setq plot_y 	       (if (equal datatype 'TVALS) 
							 	(incf seg_tval  1.00) 
								(incf seg_sdur  0.25)))

				(format *HPS_internal_buffer* "~%		set label \"~2A\"	at ~D, ~D front" 
								seg_label plot_x plot_y))

	        	(unless seg_label 
		  		nil))

	 	(return-from HPS_DNA_GNUPLOT_label_segments *HPS_internal_buffer* )))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; construct gnuplot labels for the alignment vector of this pattern
;; ***********************************************************************************************************************
(defun HPS_DNA_GNUPLOT_label_alignment_vector ( p )
  	(let* ((alignment_vector (HPS_DNA_get_alignment_vector_for p))
	      (align_tuple_type 	nil)
	      (align_tuple_val 		0)
	      (align_origin 		0)
	      (counter			0)
	      (plot_y			0)
	      (plot_x1			0)
	      (plot_y1			0)
	      (plot_x2			0)
	      (plot_y2			0))

	 (with-output-to-string ( HPS_internal_buffer ) 
	 	(dolist (align_vector_tuple alignment_vector 'LABELINGOFALIGNMENTS) 
	        	(setq align_tuple_type (first  align_vector_tuple))
	        	(setq align_tuple_val  (second align_vector_tuple))

			(setq plot_x1 align_origin)
			(setq plot_x2 plot_x1)
			(setq plot_y  (if (equal (mod counter 4) 0)  9.0
				      (if (equal (mod counter 4) 1)  7.8
				      (if (equal (mod counter 4) 2)  6.6
				      (if (equal (mod counter 4) 3)  5.4)))))
			(incf counter)
			(cond   ((equal align_tuple_type 'PRELIM-ALIGN) 
					(incf plot_x1 	align_tuple_val)
					(setq plot_y1 	plot_y)
					(setq plot_x2   plot_x1) 	
					(setq plot_y2	0) 	    
					(format HPS_internal_buffer "~%	set label \"~2A (~D)\"	at ~D, ~D front" 
									align_tuple_type plot_x1 
									(+ plot_x1 10) plot_y1))
 				((equal align_tuple_type 'RESYNC) 
					(incf plot_x1 	align_tuple_val)
					(setq plot_y1 	plot_y)
					(setq plot_x2   plot_x1) 	
					(setq plot_y2	0) 	    
					(format HPS_internal_buffer "~%	set label \"~2A (~D, ~D)\"	at ~D, ~D front" 
									align_tuple_type plot_x1 3
									(+ plot_x1 10) plot_y1))
 				((equal align_tuple_type 'ALIGN-OFFSET) 
					(setq plot_x1 	last_plot_x2)
					(setq plot_y1 	plot_y)
					(setq plot_x2 	(+ plot_x1 align_tuple_val ))
					(setq plot_y2	plot_y1) 	    
					(format HPS_internal_buffer "~%	set label \"~2A (~D, ~A~D)\"	at ~D, ~D front" 
									align_tuple_type plot_x1 
									(if (< align_tuple_val 0) "-" "+")
									(abs align_tuple_val)
									(+ plot_x1 10) plot_y1))
 				((equal align_tuple_type 'INSERT) 
					(setq plot_x1 	last_plot_x2)
					(setq plot_y1 	plot_y)
					(setq plot_x2 	(+ plot_x1 align_tuple_val ))
					(setq plot_y2	plot_y1) 	    
					(format HPS_internal_buffer "~%	set label \"~2A (~D, ~D)\"	at ~D, ~D front" 
									align_tuple_type plot_x1 align_tuple_val 
									(+ plot_x1 10) plot_y1))
 				((equal align_tuple_type 'DELETE)
					(setq plot_x1 	last_plot_x2)
					(setq plot_y1 	plot_y)
					(setq plot_x2 	(+ plot_x1 align_tuple_val ))
					(setq plot_y2	plot_y1) 	    
					(format HPS_internal_buffer "~%	set label \"~2A (~D, ~D)\"	at ~D, ~D front" 
									align_tuple_type plot_x1 align_tuple_val 
									(+ plot_x1 10) plot_y1)))

			(setq last_plot_x1	plot_x1)
			(setq last_plot_x2	plot_x2)
			(decf plot_y1 		0.5)
			(decf plot_y2 		0.5)

			(incf align_origin     align_tuple_val)

			(format HPS_internal_buffer "~%		set arrow from ~D, ~D to ~D, ~D head lw 3 lc rgb 'black'"
									  plot_x1 plot_y1 
									  plot_x2 plot_y2))
	 	(return-from HPS_DNA_GNUPLOT_label_alignment_vector HPS_internal_buffer ))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; an alignment plot of the HPS DNA signature aligned against an HPS DNA matching pattern, with HPS segment-based analysis
;; ***********************************************************************************************************************
(defun HPS_DNA_GNUPLOT_SEGMENT_ALIGNMENT ( p hps_signature hps_match gnuplot-cmd-stream &key (fileindex nil) 
					    			     			     (rank      nil))

	(setq pat_label (HPS_DNA_make_patmatch_label fileindex p))

	(format gnuplot-cmd-stream (HPS_GNUPLOT_SEGMENT_ALIGNMENT_PLOT_START)  fileindex pat_label)

	;; the upper (previously LEFT) panels - alignment time plots
	(setq hyp_start (first  hps_match))
	(setq hyp_end   (second hps_match))
	(setq pattern   (third  hps_match))
	(setq hps_seglabels_tvals 	(HPS_DNA_GNUPLOT_label_segments pattern :datatype 'TVALS))
        (setq hyp_label_tvals 		(get-output-stream-string hps_seglabels_tvals)) 

	(format gnuplot-cmd-stream (HPS_GNUPLOT_SEGMENT_ALIGNMENT_TOP_PANEL) (first  hps_signature)
									     (second hps_signature)
									             hyp_label_tvals
									             hyp_start
									             hyp_end
									             pat_label
										     pat_label)
  
	;; the lower (previously RIGHT) panel - histograms; seglabels are in effect, and 
	(setq hps_seglabels_sdurs 	(HPS_DNA_GNUPLOT_label_segments pattern :datatype 'SDURS))
	(setq hyp_label_sdurs 		(get-output-stream-string hps_seglabels_sdurs))

	(format gnuplot-cmd-stream (HPS_GNUPLOT_SEGMENT_ALIGNMENT_BOTTOM_PANEL) (first  hps_signature)
									        (second hps_signature)
									      	        hyp_label_sdurs
									      	        hyp_start
									      	        hyp_end
									                pat_label))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_GNUPLOT_CROSSREF_SIG2MATCH ( patnum p gnuplot-stream )
	(setq pat_label (HPS_DNA_make_patmatch_label patnum p))

	(setq pat_arrow (format nil "~%	set arrow from ~D, ~D to ~D, ~D heads lc rgb 'blue'"
			                		(HPS_DNA_get_pattern_match_start  p)   	     35
			                		(HPS_DNA_get_pattern_match_end    p)         35 ))

	(setq pat_arrow (format nil "~%	set label \"~A\" at ~D, ~D front" pat_label
         		(+ (HPS_DNA_get_pattern_match_start p) (/ (- (HPS_DNA_get_pattern_match_end   p) 
							             (HPS_DNA_get_pattern_match_start p)) 1.9))   37))

	(format gnuplot-stream (HPS_GNUPLOT_SIG2MATCH_CROSSREF_DETAILS_PLOT_START) patnum
										   patnum pat_label 
										   patnum pat_label 
										   patnum pat_label 
										   patnum pat_label
										   pat_arrow
										   patnum pat_label
										   patnum  ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; return a gnuplot command that places labels corresponding to the start of each pattern interval in any time plot
;; ***********************************************************************************************************************
(defun HPS_DNA_GNUPLOT_label_pattern_matches ( HPS_matching_patterns_by_time &key (labeltype 'RATING)) 
	(setq iter 0)
	(setq *HPS_internal_buffer* nil) 
	(setq *HPS_DNA_num_uniq_matches* 0)

	(with-output-to-string ( *HPS_internal_buffer* ) 
		(setq previous_p nil)	

		(dolist (p HPS_matching_patterns_by_time 'LABELING)
		  	(setq *HPS_DNA_num_uniq_matches* 
			      (HPS_DNA_determine_if_unique_pattern p previous_p *HPS_DNA_num_uniq_matches* ))
			(setq previous_p p)

			(when   (equal labeltype 'RATING)
		  		(setq plot_x	(HPS_DNA_get_pattern_match_start  p))
		  		(setq plot_y 	(if (equal (mod iter 5) 0) 0.40 
						(if (equal (mod iter 5) 1) 0.50 
						(if (equal (mod iter 5) 2) 0.60 
						(if (equal (mod iter 5) 3) 0.70 
								   	   0.80)))))


				(format *HPS_internal_buffer* "~%	set label \"~A\" at ~4,2F, ~4,2F front"
								(HPS_DNA_make_patmatch_label iter p )
								plot_x
								plot_y ))

			(unless (equal labeltype 'RATING)
		  		(setq plot_x  (HPS_DNA_get_pattern_match_start  p))
		  		(setq plot_y 	(if (equal (mod iter 5) 0) -0.5
						(if (equal (mod iter 5) 1)  0.3
						(if (equal (mod iter 5) 2)  1.1
						(if (equal (mod iter 5) 3)  1.9
								   	    2.7 )))))

				(format *HPS_internal_buffer* "~%	set label \"~A\" at ~4,2F, ~4,2F front"
								(HPS_DNA_make_patmatch_label iter p )
								(+ plot_x (/ (- (HPS_DNA_get_pattern_match_end    p) 
										(HPS_DNA_get_pattern_match_start  p)) 10))
								(+ plot_y 0.40))

				(format *HPS_internal_buffer* "~%	set arrow from ~4,2F, ~4,2F 	to ~4,2F, ~4,2F heads lw 2 front"
						                (HPS_DNA_get_pattern_match_start  p) plot_y
						                (HPS_DNA_get_pattern_match_end    p) plot_y))

	  		(incf iter))
		(return-from HPS_DNA_GNUPLOT_label_pattern_matches *HPS_internal_buffer* )))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function creates gnuplot labels for a histogram of match ratings
;; ***********************************************************************************************************************
(defun HPS_DNA_label_histogram_matches ( HPS_matching_patterns_by_time HPS_matching_patterns_by_rank)
	(setq old_p_rating (floor (* (HPS_DNA_get_pattern_match_rating (first HPS_matching_patterns_by_rank)) *HPS_DNA_nbins*)))
	(setq nitemsbin 1)
	(with-output-to-string ( *HPS_internal_buffer* ) 
		(dolist (p HPS_matching_patterns_by_rank 'LABELING)
			(setq p_rating (floor (* (HPS_DNA_get_pattern_match_rating p) *HPS_DNA_nbins*)))
			(when   (HPS_is_this_between p_rating old_p_rating (+ old_p_rating 1)) 
		  		(incf nitemsbin))

			(unless (HPS_is_this_between p_rating old_p_rating (+ old_p_rating 1)) 
				(setq nitemsbin 1))

			(setq plot_x (HPS_DNA_get_pattern_match_rating p))
			(setq plot_y (* nitemsbin hps_sp ))
			(format *HPS_internal_buffer* "~%	set label \".M~D\" at ~4,2F, ~4,2F front"
								(HPS_DNA_rank_of_pattern_in  p HPS_matching_patterns_by_time) plot_x plot_y)
			(setq old_p_rating p_rating))
		(setq hist_labels (get-output-stream-string *HPS_internal_buffer*)))
	(return-from HPS_DNA_label_histogram_matches hist_labels))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_match_histogram_apply_gaussian_fit ()
	(setq hps_mu    	  (HPS_mean     		*HPS_DNA_match_rating_signal* ))
	(setq hps_sigma   	  (HPS_stddev   		*HPS_DNA_match_rating_signal* hps_mu ))

	(setq hps_match_rating_wo (remove-if-not #'(lambda (a) (< a *HPS_DNA_matching_goal*)) *HPS_DNA_match_rating_signal* ))
	(setq hps_mu_wo    	  (HPS_mean			hps_match_rating_wo)) 			     		
	(setq hps_sigma_wo   	  (HPS_stddev   		hps_match_rating_wo hps_mu_wo ))

	(setq hps_match_mean_y_wo (HPS_maximum 			(HPS_histogram_get_bin_freqcount  *HPS_DNA_match_rating_hist_data*)))
	(setq hps_bin_tuple_wo 	  (HPS_histogram_find_binnum_for *HPS_DNA_match_rating_hist_data*  
								 hps_match_mean_y_wo 
				      				:binquality_sought 'range
				      				:binquality_given  'frequency))
	(setq hps_match_mean_x_wo (+ (second hps_bin_tuple_wo) (/ (- (second hps_bin_tuple_wo) (third hps_bin_tuple_wo)) 2)))

	(setq hps_match_mean_x     hps_mu)
	;; CHECK THIS CAN BE FIXED WITH THE POSITION FUNCTION
	(setq hps_bin_tuple 	  (HPS_histogram_find_binnum_for *HPS_DNA_match_rating_hist_data*  
								 hps_match_mean_x 
				      				:binquality_sought 'frequency
				      				:binquality_given  'range ))
	(setq hps_match_mean_y 	  (first hps_bin_tuple))

	(setq hps_amp_wo (/ hps_match_mean_y_wo (HPS_gaussian_A1 hps_match_mean_x_wo hps_mu_wo hps_sigma_wo )))
	(setq hps_amp    (/ hps_match_mean_y_wo (HPS_gaussian_A1 hps_match_mean_x_wo hps_mu    hps_sigma )))

	(setq *hps_fit_commands* (format nil 
		"f(x) = ~5,3F * 1/(2 * ~12,8F * ~12,8F) * exp( -1 * (x - ~12,8F )**2 / (2 * ~12,8F ** 2))
 		 g(x) = ~5,3F * 1/(2 * ~12,8F * ~12,8F) * exp( -1 * (x - ~12,8F )**2 / (2 * ~12,8F ** 2))"
					  			hps_amp
								pi 
								hps_sigma 
								hps_mu 
								hps_sigma
					  			hps_amp_wo
								pi 
								hps_sigma_wo 
								hps_mu_wo 
								hps_sigma_wo))

	(setq hps_sp  (ceiling (* hps_amp 0.026)))
	(setq *hps_fit_labels* (format nil "
		set arrow from ~4,2F, ~4,2F to ~4,2F, ~4,2F heads lc rgb 'light-blue' front
		set arrow from ~4,2F, ~4,2F to ~4,2F, ~4,2F heads lc rgb 'light-blue' front
		set arrow from ~4,2F, ~4,2F to ~4,2F, ~4,2F heads lc rgb 'blue' front
		set arrow from ~4,2F, ~4,2F to ~4,2F, ~4,2F heads lc rgb 'gold' front
		set label \"SPREAD   OF ~4,2F SIGMA LEVELS OF DETECTION W. NOISE FROM MEAN\" at ~4,2F, ~4,2F front
		set label \"SPREAD   OF ~4,2F SIGMA LEVELS OF DETECTION W. NOISE FROM MEAN\" at ~4,2F, ~4,2F front
		set label \"SELECTED THRESHOLD IS AT ~4,2F SIGMA LEVELS FROM W. NOISE MEAN\" at ~4,2F, ~4,2F front
		set label \"N(Z>~4,2F)% SEARCH REGION\"          		     	     at ~4,2F, ~4,2F front
		set label \"GIVEN LIM=~4,2F \"            		     	     	     at ~4,2F, ~4,2F front"
	    						hps_mu        	    		 (- hps_amp (* hps_sp 3)) 	
							(+ hps_mu hps_sigma )   	 (- hps_amp (* hps_sp 3))
 	    						hps_mu  	        	 (- hps_amp (* hps_sp 6)) 	
							(+ hps_mu (* hps_sigma 3)) 	 (- hps_amp (* hps_sp 6))
	    						hps_mu                  	 (- hps_amp (* hps_sp 9))       
							*HPS_DNA_matching_goal*  	 (- hps_amp (* hps_sp 9))
	    						*HPS_DNA_matching_goal* 	 (- hps_amp (* hps_sp 12))      
							1.01 			   	 (- hps_amp (* hps_sp 12))
 		1  					(+ hps_mu 0.02)    		 (- hps_amp (* hps_sp 2))
 	    	3 					(+ hps_mu 0.02) 	         (- hps_amp (* hps_sp 5))
		(HPS_compute_perf_z_value_for *HPS_DNA_matching_goal* hps_mu hps_sigma ) 
							(+ hps_mu 0.02)          	 (- hps_amp (* hps_sp 8))
		(HPS_compute_perf_z_value_for *HPS_DNA_matching_goal* hps_mu hps_sigma ) 
	                        			(+ *HPS_DNA_matching_goal* 0.02) (- hps_amp (* hps_sp 11))
		*HPS_DNA_matching_goal* 	 	(+ *HPS_DNA_matching_goal* 0.02) (- hps_amp (* hps_sp 13))))

	(return-from HPS_DNA_match_histogram_apply_gaussian_fit *hps_fit_labels*))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_match_histogram_get_fit_labels   ()
  	*hps_fit_labels*)

(defun HPS_DNA_match_histogram_get_fit_commands () 
  	*hps_fit_commands*)
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; the MATCH_BEHAVIOR plot with labels for each best matching pattern in the test sequence 
;; ***********************************************************************************************************************
(defun HPS_DNA_MATCH_BEHAVIOUR_PLOT ( HPS_matching_patterns_by_time 
				      HPS_matching_patterns_by_rank 
				      gnuplot-stream )
	(setq HYP_MATCH_LABELS    (get-output-stream-string (HPS_DNA_GNUPLOT_label_pattern_matches  
							   	HPS_matching_patterns_by_time )))

	(setq HYP_BASE_LABELS     (get-output-stream-string (HPS_DNA_GNUPLOT_label_pattern_matches  
							   	HPS_matching_patterns_by_time 
								:labeltype 'DNABASES)))

	(setq HPS_SIGNATURE_LABEL (format nil 
"set arrow from ~4,2F, ~4,2F 	to ~4,2F, ~4,2F heads front lc rgb 'blue'
 set label \"~A\"	at ~4,2F, ~4,2F 
 set label \"~A\"	at ~4,2F, ~4,2F" 
					 *HPS_1DNA_endpoint_signature_start* 				3.75
					 *HPS_1DNA_endpoint_signature_end*   				3.75 
					 (format nil "~A [~D:~D]" 		"DNA signature section" 
										 0
										*HPS_1DNA_endpoint_signature_end*)
						(+ *HPS_1DNA_endpoint_signature_start* 100) 		4.3 
					 (format nil "[~A]" *HPS_1DNA_signature_series_headername* )
						(+ *HPS_1DNA_endpoint_signature_start* 100) 		3.3 ))
 								

	(setq HPS_TESTSEQ_LABEL   (format nil 
"set arrow from ~4,2F, ~4,2F 	to ~4,2F, ~4,2F heads front lc rgb 'brown'
 set label \"~A\"	at ~4,2F, ~4,2F front
 set label \"~A\"	at ~4,2F, ~4,2F front" 
					(+ *HPS_1DNA_endpoint_signature_end* 10) 			2.25
					(length *HPS_DNA_approx_tupleset*)				2.25
 				   	(format nil "~A [~D:~D](indexes offset by [~A])" "DNA test sequence section"
										*HPS_1DNA_endpoint_signature_end*
										(length *HPS_1DNA_actual_input_bases*)
									         *HPS_1DNA_endpoint_signature_end*)
						(+ *HPS_1DNA_endpoint_signature_end* 100 ) 		2.75 
 				   	(format nil "[~A]" *HPS_1DNA_testsequence_series_headername* )
						(+ *HPS_1DNA_endpoint_signature_end* 100 ) 		1.75 ))

	(setq HPS_MATCH_TITLE 	  (format nil 
"HPS DNA SEQUENCE ALIGNER - ~D MATCHES (~D UNIQUE) UNEARTHED (AT LIM=~4,2F AND SEGDUR-THRESHOLD=~D)"
					(length HPS_matching_patterns_by_time)
					*HPS_DNA_num_uniq_matches* 
					*HPS_DNA_matching_goal* 
					*hps_dna_seg_mergedur* ))

	;; apply normal curve fits based on the actual parameters, with and without pattern matches 
	(HPS_DNA_match_histogram_apply_gaussian_fit)
		(setq HPS_FIT_LABELS 	        (HPS_DNA_match_histogram_get_fit_labels))
		(setq HPS_FIT_COMMANDS    	(HPS_DNA_match_histogram_get_fit_commands))

	(setq HPS_HIST_LABELS 	  (HPS_DNA_label_histogram_matches HPS_matching_patterns_by_time 
								   HPS_matching_patterns_by_rank))

	(setq hps_testseq_len (length *HPS_1DNA_input_timeseries*))
	(format gnuplot-stream    (HPS_GNUPLOT_MATCH_BEHAVIOR_PLOT_START) HPS_MATCH_TITLE
									  HPS_TESTSEQ_LABEL 
									  HPS_SIGNATURE_LABEL 
									  HYP_BASE_LABELS 
				                                          (* 0 (/ hps_testseq_len 4)) 		(* 1 (/ hps_testseq_len 4))
				                                          (* 1 (/ hps_testseq_len 4)) 		(* 2 (/ hps_testseq_len 4))
				                                          (* 2 (/ hps_testseq_len 4)) 		(* 3 (/ hps_testseq_len 4))
				                                          (* 3 (/ hps_testseq_len 4)) 		(* 4 (/ hps_testseq_len 4))
									  )
	(format gnuplot-stream    (HPS_GNUPLOT_MATCH_BEHAVIOR_PLOT_END)   HPS_MATCH_TITLE
									  HPS_FIT_COMMANDS 
									  HPS_FIT_LABELS
									 *HPS_DNA_matching_goal* 
									  HPS_HIST_LABELS 
									 *HPS_DNA_matching_goal*
									  HYP_MATCH_LABELS
									  ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; builts a convenient memory of matching_patterns endpoints used to print out user feedback in graphs or displays 
;; ***********************************************************************************************************************
(defun HPS_DNA_get_matching_patterns_endpoints ( matching_patterns_by_time matching_patterns_by_rank )
	(setq patnum 0)
	(setq *HPS_endpoints_matches_in_time* nil)
	(dolist (p matching_patterns_by_time 'BEST_PATTERN_MATCHES)
		(setq HPS_match_hyp_start      (HPS_DNA_get_pattern_match_start p))
		(setq HPS_match_hyp_end   (min (HPS_DNA_get_pattern_match_end   p) (+ HPS_match_hyp_start *HPS_1DNA_endpoint_signature_end* )))
		(setq HPS_match_p               p)
		(setq HPS_timeindex_p	        patnum)
		(setq HPS_rank_p 	       (HPS_DNA_rank_of_pattern_in  p matching_patterns_by_rank ))
		(setq HPS_tuple_p 	  (list HPS_match_hyp_start 
						HPS_match_hyp_end 
						HPS_match_p 
						HPS_timeindex_p 
						HPS_rank_p ))
	  	(push HPS_tuple_p 	       *HPS_endpoints_matches_in_time*)
		(incf patnum))

	(setq *HPS_endpoints_matches_in_time* (reverse  *HPS_endpoints_matches_in_time*))

	(return-from HPS_DNA_get_matching_patterns_endpoints *HPS_endpoints_matches_in_time* ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; the XY DOT MATRIX SIMILARITY PLOT MATRIX
;; ***********************************************************************************************************************
(setq HPS_DNA_XY_PLOT_MATRIX_DATA '( ( 0.3	0.25	0.00	0.08 )
	   			     ( 0.3	0.25	0.33	0.08 )
	   			     ( 0.3	0.25	0.67	0.08 )
	   			     ( 0.3	0.25	0.00	0.40 )
	   			     ( 0.3	0.25	0.33	0.40 )
	   			     ( 0.3	0.25	0.67	0.40 )
	   			     ( 0.3	0.25	0.00	0.70 )
	   			     ( 0.3	0.25	0.33	0.70 )
	   			     ( 0.3	0.25	0.67	0.70 )))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; generates a 3x3 array of dot matrix plots between the patterns and the signature for quick visual identification of the match
;; ***********************************************************************************************************************
(defun HPS_DNA_FINEGRAIN_XY_PLOTSHEET ( HPS_matching_patterns_by_time gnuplot-stream )
  	(setq hps_numpats  (length HPS_matching_patterns_by_time))
  	(setq hps_numiters (ceiling (/ hps_numpats 9)))
  	(dotimes (i hps_numiters)
	  	(let*   ((low_index     (* i 9))
			 (high_index 	(min (- (* (+ i 1) 9) 1) hps_numpats))
		  	 (plot_coord	nil)
			 (num_times	(- high_index low_index)))
		  	 
			 (with-output-to-string ( xy-plot-buffer )
			     	(dotimes (j num_times)
		  	 		(setf plot_coord (nth j HPS_DNA_XY_PLOT_MATRIX_DATA))
  					(format xy-plot-buffer (HPS_GNUPLOT_SIG2MATCH_FINEGRAIN_CROSSREF_PLOT_INSTANCE)
						(first plot_coord) (second plot_coord) (third plot_coord) (fourth plot_coord) 
						(+ (* i 9) j)
						(+ (* i 9) j)
						(+ (* i 9) j)
						(+ (* i 9) j)))

  			     	(format gnuplot-stream (HPS_GNUPLOT_SIG2MATCH_FINEGRAIN_CROSSREF_PLOT)  
			     				low_index 
							high_index
							(get-output-stream-string xy-plot-buffer))))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_ALIGNMENT_ADJUST_PLOT (hps_patterns_by_time gnuplot-buffer)
  	(let 	((patnum	0))
		(dolist (p hps_patterns_by_time 'ALIGNMENT)
			(format gnuplot-buffer (HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT) 
				patnum 
				(format nil "	set xrange [~D:~D]" (HPS_DNA_pattern_alignment_lookup_start  p) 
								    (HPS_DNA_pattern_alignment_lookup_end    p))
				(get-output-stream-string (HPS_DNA_GNUPLOT_label_alignment_vector p ))
				patnum 
				patnum 
				patnum )
			(incf patnum))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this temporary KLUDGE (CHECK) is used to cleanup the gnuplot pipe, as the last plot never finishes up and remains open
;; ***********************************************************************************************************************
(defun HPS_DNA_NIL_FINAL_PLOT ( gnuplot-stream )
  	(format gnuplot-stream (HPS_DNA_DUMMY_FINAL_PLOT) nil))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_treepair_sort ( t1 t2 )
  	(if (< (second t1) (second t2)) t nil))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(defun HPS_DNA_editvector_similarity ( c_edit n_edit )
  	(when (OR (NOT c_edit) (NOT n_edit))
	  nil)

	(unless (OR (NOT c_edit) (NOT n_edit))
  	    (if (equal (first c_edit) (first n_edit)) 
	  	(cond ((equal (first c_edit) 'RESYNC)
	  		(if (HPS_is_this_between (second n_edit) (- (second c_edit) 30) (+ (second c_edit) 30)) t nil))
	  	      ((equal (first c_edit) 'DELETE)
	  		(if (HPS_is_this_between (second n_edit) (- (second c_edit) 10) (+ (second c_edit) 10)) t nil))
	  	      ((equal (first c_edit) 'INSERT)
	  		(if (HPS_is_this_between (second n_edit) (- (second c_edit) 10) (+ (second c_edit) 10)) t nil)))
		nil)))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function creates an in-house phylogenic tree to relate the resultant matching instances
;; ***********************************************************************************************************************
(defun HPS_DNA_make_phylogenic_tree ( HPS_patterns_by_time )
  	(setq HPS_DNA_phylo_tree 	nil)

	;; construct a first order tree of the matching instance relevant metrics for phylogenic differencing
	(let  ((align_vector		nil)
	       (edit_subvector		nil)
	       (patnum			0))

		(dolist (p HPS_patterns_by_time 'LIST1)
	  		(setq align_vector (HPS_DNA_get_alignment_vector_for p))
			(setq edit_subvector (subseq align_vector 2))
			(push (list p
				    (HPS_DNA_get_comparator_table_rating p)
				    (length edit_subvector)
				    align_vector 
				    edit_subvector 
		  		    (if edit_subvector 'BRANCH 'SERIAL)
				    'DNA-SIGNATURE
				    patnum) 
			       			HPS_DNA_phylo_tree)
			(incf patnum))

		(setq HPS_DNA_phylo_tree (reverse HPS_DNA_phylo_tree)))

	;; sort the branches
	(setq HPS_DNA_phylo_tree 	(cooper-safe-sort HPS_DNA_phylo_tree #'HPS_DNA_treepair_sort))

	;; construct the tree plot labels for the branches 
	(let  ((num_branches		(length HPS_DNA_phylo_tree)))

	  	(with-output-to-string (HPS-phylo-buffer)
	  	    (dotimes (i num_branches)
		  	(let* ((c_branch 	(nth i HPS_DNA_phylo_tree))
			       (p	 	(first  c_branch)) 
			       (x-val	 	(second c_branch))					;; value between -1 and 150 - threshold
			       (y-val    	(+ (third  c_branch) (* (mod i 3) 0.5))) 		;; value between -1 25 - num indels
			       (z-val 	 	(- 100 (* (HPS_DNA_get_pattern_match_rating p) 100)))   ;; value between -1 100 - match ratio
			       (branch_label 	(format nil "~A" (HPS_DNA_make_patmatch_label (eighth c_branch) p))))

		  	    (format HPS-phylo-buffer "~%set label \"~A\" at ~D, ~D, ~D front" branch_label x-val y-val z-val )))

		    ;; the signature start point
		    (format HPS-phylo-buffer "~%set label \"~A\" at ~D, ~D, ~D front"     
			   (format nil "SIGNATURE [~D:~D]" *HPS_1DNA_endpoint_signature_start* *HPS_1DNA_endpoint_signature_end*) -1.5 -1.5 1.5 )

		    (setq HPS_DNA_phylo_tree (append (list (list nil 0 0 nil nil 'SERIAL 'DNA-SIGNATURE -1)) HPS_DNA_phylo_tree))

		    (return-from HPS_DNA_make_phylogenic_tree (get-output-stream-string HPS-phylo-buffer)))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this computes an in-house phylogenic tree 
;; ***********************************************************************************************************************
(defun HPS_DNA_PRINT_PHYLOGENIC_TREE ( HPS_patterns_by_time bound-stream )
  	(format bound-stream (HPS_DNA_PHYLOGENIC_TREEPLOT) (HPS_DNA_make_phylogenic_tree HPS_patterns_by_time ))

	(with-open-file (splot-stream (format nil "~A~A" *HPS_OUTPUT_PATH* "HPS_DNA_PHYLOTREE.DAT" ) 
				      :direction :output :if-exists :supersede :if-does-not-exist :create)

	    (let ((i 	0))

	  	(dolist (c_branch HPS_DNA_phylo_tree 'PHYLOPRINT)

		  	(let* ((x-val	(second c_branch))					;; value between -1 and 150 - threshold
			       (y-val   (+ (third  c_branch) (* (mod i 3) 0.5))) 		;; value between -1 25 - num indels
			       (p 	(first c_branch)) 
			       (z-val 	(if p (- 100 (* (HPS_DNA_get_pattern_match_rating p) 100)) 0))   ;; value between -1 100 - match ratio
			       (c 	1.5))	
		  		(format splot-stream "~A	~A	~A~%" (+ x-val 0)  (+ y-val 0)  z-val)
		  		(format splot-stream "~A	~A	~A~%" (+ x-val c)  (+ y-val 0)  z-val)
		  		(format splot-stream "~A	~A	~A~%" (+ x-val 0)  (+ y-val c)  z-val)
		  		(format splot-stream "~A	~A	~A~%" (+ x-val c)  (+ y-val c)  z-val)
		  		(format splot-stream "~%" nil ))

			(incf i)))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function will return a bit vector of the positions for the coded motif
;; ***********************************************************************************************************************
(defun HPS_NDNA_get_motif_bitvector ( HPS_NDNA_codes &key (MOTIF_CODE 0))
  	(let ((HPS_NDNA_bitvector nil))
		(setq HPS_NDNA_bitvector	(mapcar #'(lambda (a) (if (equal a MOTIF_CODE) 1 0)) HPS_NDNA_codes))
		(return-from HPS_NDNA_get_motif_bitvector HPS_NDNA_bitvector)))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function parses the motif regular expression into a sequence of motifs and repeats: {DNA}[*]{AA}[1]{*}[*]
;; ***********************************************************************************************************************
(defun HPS_DNA_parse_motif_regexpr ( motif_regexpr )
	(let  ( ( remainder_motif 		( STENO_CONVERT_TO_LIST motif_regexpr ))
	        ( motif_regexpr_parsed 		nil)
	  	( motif_start			nil)
		( motif_end			nil)
		( motif_code			nil)
		( token_counter			0))

	  (dolist (token remainder_motif 'PARSER)
	    	(cond 	((equal token '#\()
		  		(setq motif_start token_counter))
	    	  	((equal token '#\))
		  		(setq motif_end   token_counter))		
	    	  	((equal token '#\[)
		  		(setq motif_start token_counter))
	    	  	((equal token '#\])
		  		(setq motif_end   token_counter))
		  	(T nil))

		(when (AND motif_start motif_end)
			(setq motif_code 	(coerce (subseq remainder_motif (1+ motif_start) motif_end) 'STRING))
			(push motif_code 	motif_regexpr_parsed)
			(setq motif_start 	nil)
			(setq motif_end   	nil))

		(incf token_counter))

	  (setq motif_regexpr_parsed (reverse motif_regexpr_parsed))  
	  (return-from HPS_DNA_parse_motif_regexpr motif_regexpr_parsed)))
;; ***********************************************************************************************************************
	  	

;; ***********************************************************************************************************************
;; this function parses the motif regular expression into a sequence of motif-repeat commands (motif code, motif repeat)
;; ***********************************************************************************************************************
(defun HPS_DNA_get_motif_regexpr ( motif_regexpr )
  	(let ( ( parsed_motif 			nil)
  	       ( motif_command_set 		nil)
  	       ( motif_code 			nil)
  	       ( motif_repeats			nil)
  	       ( token_counter 			0))

		(setq parsed_motif 		(HPS_DNA_parse_motif_regexpr motif_regexpr ))

		(dolist (token parsed_motif 'PARSER)
		  	(when (equal (mod token_counter 2) 0) 
			  	(setq motif_code    token))

		  	(when (equal (mod token_counter 2) 1) 
			  	(setq motif_repeats token)
			  	(push (list motif_code motif_repeats) motif_command_set)) 
			(incf token_counter))
		(setq motif_command_set (reverse motif_command_set))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function simply generates a bit vector table for each of the motif terms to match
;; ***********************************************************************************************************************
(defun HPS_DNA_makesearch_struct_for_motif_regexpr ( HPS_1DNA_bases &key (MOTIF_REGEXPR nil)) 
  	(let ( ( HPS_1DNA_codes			nil)
	       ( HPS_2DNA_codes			nil)
	       ( HPS_3DNA_codes			nil)
	       ( HPS_4DNA_codes			nil)
	       ( HPS_NDNA_bitvectors		nil)
	       ( motif_code			nil)
	       ( motif_bases 			nil)
	       ( motif_bitvector 		nil)
	       ( motif_commands			nil))

	 	(setq HPS_1DNA_codes 		(HPS_specialized_conditioning HPS_1DNA_bases :COND_TYPE '1DNA))
	 	(setq HPS_2DNA_codes 		(HPS_specialized_conditioning HPS_1DNA_bases :COND_TYPE '2DNA))
	 	(setq HPS_3DNA_codes 		(HPS_specialized_conditioning HPS_1DNA_bases :COND_TYPE '3DNA))
	 	(setq HPS_4DNA_codes 		(HPS_specialized_conditioning HPS_1DNA_bases :COND_TYPE '4DNA))

		(setq motif_commands		(HPS_DNA_get_motif_regexpr MOTIF_REGEXPR ))

		(dolist (motif_command	motif_commands 'BITVECTORS)
		  	(setq motif_code 	(first  motif_command))
			       	(setq motif_bases 		(mapcar #'(lambda (a) (STENO_mapascii a)) (STENO_convert_to_list  motif_code)))

		  		(cond   ((equal (length motif_code) 1)
						(setq motif_bitvector   (HPS_NDNA_get_motif_bitvector HPS_1DNA_codes 
										:MOTIF_CODE (HPS_1DNA_encoder (nth 0 motif_bases)))))

		  	      		((equal (length motif_code) 2)
						(setq motif_bitvector   (HPS_NDNA_get_motif_bitvector HPS_2DNA_codes 
										:MOTIF_CODE (HPS_2DNA_encoder (nth 0 motif_bases)
											  		      (nth 1 motif_bases)))))

		  	      		((equal (length motif_code) 3)
						(setq motif_bitvector   (HPS_NDNA_get_motif_bitvector HPS_3DNA_codes 
									        :MOTIF_CODE (HPS_3DNA_encoder (nth 0 motif_bases)
											  		      (nth 1 motif_bases) 
											  		      (nth 2 motif_bases)))))

		  	      		((equal (length motif_code) 4)
						(setq motif_bitvector  	(HPS_NDNA_get_motif_bitvector HPS_4DNA_codes 
										:MOTIF_CODE (HPS_4DNA_encoder (nth 0 motif_bases) 
											  		      (nth 1 motif_bases) 
											  		      (nth 2 motif_bases) 
											  		      (nth 3 motif_bases))))))
				(push motif_bitvector			HPS_NDNA_bitvectors))

		(setq HPS_NDNA_bitvectors (reverse HPS_NDNA_bitvectors))
		(return-from HPS_DNA_makesearch_struct_for_motif_regexpr HPS_NDNA_bitvectors)))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this handles just the very first ONE try match but without repeats > 1 or infinite
;; ***********************************************************************************************************************
(defun HPS_DNA_search_for_motif_regexpr ( HPS_1DNA_bases &key (MOTIF_REGEXPR nil)) 
  	(let* (( hps_bitvectors 		nil)
	       ( bitvector			nil)
	       ( constraint_met			1)
	       ( HPS_DNA_motif_regexpr_match 	nil)
	       ( last_position 			-1)
	       ( motif_iter 			0)
	       ( motif_commands 		nil)
	       ( motif_repeats 			nil))

		(setq hps_bitvectors		(HPS_DNA_makesearch_struct_for_motif_regexpr HPS_1DNA_bases 
											     :MOTIF_REGEXPR MOTIF_REGEXPR  ))
		(setq motif_commands		(HPS_DNA_get_motif_regexpr MOTIF_REGEXPR ))

		;; CHECK - there was a -1 somewhere affecting a sum with respect to bitvectors?

		(dolist (command motif_commands 'SEARCHING)
		  	(setq motif_repeats 	(read-from-string (second command)))

	  		(setq bitvector		(nth motif_iter hps_bitvectors)) 
	
			;; this function does not constraint the gap nor handle ANY repeats which requires a forward reference to the next command
			(dotimes (i motif_repeats)
				(setq last_position	(position constraint_met bitvector :start (1+ last_position)))
	
				(when (NOT last_position) 
		  			(return-from HPS_DNA_search_for_motif_regexpr nil))
	
				(push (list last_position command) HPS_DNA_motif_regexpr_match))
	
			(incf motif_iter))

		(return-from HPS_DNA_search_for_motif_regexpr (reverse HPS_DNA_motif_regexpr_match))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; prints a plot of the matching regular expression within the given subsequence bases
;; ***********************************************************************************************************************
(defun HPS_DNA_print_regexpr_match ( HPS_1DNA_bases gnuplot-stream 
						    &key (FROM		nil)
						    	 (TO		nil)
							 (MOTIF_REGEXPR nil)) 

  	(let  ( ( motif_regexpr_match 		nil)
		( HPS_regexpr_labels 		nil)
	       	( motif 			nil)
	       	( dna_subseq			nil)
		( plx_stack			nil)
		( ply_stack			nil)
		( temp_list			nil)
		( motif_len			0)
	       	( repeats 			0)
		( print_iter			0)
	       	( next_position 		0))

		(setq motif_regexpr_match 	(HPS_DNA_search_for_motif_regexpr HPS_1DNA_bases :MOTIF_REGEXPR MOTIF_REGEXPR ))
		(setq FROM 			(- (first (first motif_regexpr_match))        5))
		(setq TO   			(+ (first (first (last motif_regexpr_match))) 5))
		(setq dna_subseq 		(subseq HPS_1DNA_bases FROM TO ))
		(setq ply_stack '( 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
				   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ))
		(setq plx_stack nil) (dotimes (i (- TO FROM )) (push (+ FROM i 1) plx_stack)) (setq plx_stack (reverse plx_stack))
		(dolist (base dna_subseq 'TEMP) (push base temp_list) (push (pop plx_stack) temp_list) (push (pop ply_stack) temp_list))
		(setq temp_list (reverse temp_list))

		(with-output-to-string (HPS-regexpr-buffer)
		  	(format HPS-regexpr-buffer "~{~%set label \"~A\" at ~D, ~D~}" temp_list)

			(dolist (match_elem motif_regexpr_match 'PRINTING)
			  	(setq motif 	    (first  (second match_elem)))
			  	(setq repeats 	    (read-from-string (second (second match_elem))))
			  	(setq motif_len	    (length motif))

			  	(setq next_position (first match_elem))
		  		(format HPS-regexpr-buffer "~%set label \"AT:~A~A[~A]\" at ~D, ~D" 
					(1+ (- next_position motif_len -1))
					(subseq HPS_1DNA_bases (- next_position motif_len -1) (+ next_position 1)) 
					(second (second match_elem)) 
					(1+ (- next_position motif_len -1))
					(if (equal (mod print_iter 3) 0) 8 (if (equal (mod print_iter 3) 1) 7 6)))
				(incf print_iter))

			(setq HPS_regexpr_labels (get-output-stream-string HPS-regexpr-buffer)))

 		(format gnuplot-stream (HPS_DNA_REGEXPR_PLOT) 
			(format nil "REGULAR-EXPRESSION: ~A"  MOTIF_REGEXPR) 
			HPS_regexpr_labels 
			FROM 		(floor   (* TO 0.25))   "DNA INPUT SEQUENCE" 
			(* TO 0.25) 	(floor   (* TO 0.50))   "DNA INPUT SEQUENCE"
			(* TO 0.50) 	(floor   (* TO 0.75))   "DNA INPUT SEQUENCE"
			(* TO 0.75) 	(ceiling (* TO 1.00))   "DNA INPUT SEQUENCE")))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; print the labels and arrows identifying the top motifs present in the subsequence being analyzed
;; ***********************************************************************************************************************
(defun HPS_DNA_MOTIF_FINDER ( HPS_1DNA_bases HPS-NDNA-analyzer-buffer &key ( MOTIFTYPE 			 '2DNA)
			      	   					   ( NBINS			 16)
				   					   ( NPOWER_MINUS1		 1)
				   					   ( HOWMANY			 5) 
				   					   ( FROM			 nil)
				   					   ( TO 			 nil)
				   					   ( MOTIF_COLORS		 nil)
		 	           					   ( LABEL_PLACEMENTS		 nil)
		 	           					   ( ARROW_PLACEMENTS		 nil))

	(let  ( ( hps_dna_histogram_file	(format nil "~AHPS_~A_SEQUENCE_ANALYSIS.DNA" *HPS_OUTPUT_PATH* MOTIFTYPE))
	 	( HPS_NDNA_codes 		nil)
	 	( hps_NDNA_histogram_dataset	nil)
	 	( top_NDNA_motifs_dataset 	nil)
	 	( arrow_coord			nil)
	 	( label_coord			nil)
		( motif_color			nil)
		( motif_code			nil)
	 	( bin_elems			nil)
	 	( last_position 		nil)
	 	( position_from 		0)
	 	( overlap_counter 		0) )

	 	(setq HPS_NDNA_codes 		 (HPS_specialized_conditioning HPS_1DNA_bases :COND_TYPE MOTIFTYPE ))
	 	(setq hps_NDNA_histogram_dataset (HPS_histogram HPS_NDNA_codes hps_dna_histogram_file :NUMBINS NBINS))
	 	(setq top_NDNA_motifs_dataset 	 (subseq (HPS_histogram_sort_by_frequencies hps_NDNA_histogram_dataset) 0 HOWMANY))
					
	    	(dolist (motif top_NDNA_motifs_dataset 'MOTIFSUBSTITUTION)
		  	(setq bin_elems 	(fourth motif))
			(setq motif_color	(pop motif_colors))

			(setq last_position   	0)
			(setq overlap_counter 	0)
			(setq position_from   	0)

			(setq arrow_coord (pop arrow_placements))
		  	(dolist (bin_elem bin_elems 'MOTIFSPOSITIONS) 
			  	(setq last_position 	(position bin_elem HPS_NDNA_codes :start (1+ last_position))) 
				(if (> last_position (+ NPOWER_MINUS1 position_from))
					(setq position_from last_position)
					(incf overlap_counter)) 

				;; only display the top motif for now - for 2 is last for 3 is something and 4 is somethign else
				(when (equal motif (first top_NDNA_motifs_dataset))
			  		(format HPS-NDNA-analyzer-buffer "~%	set arrow from ~D, ~D to ~D, ~D head lw 2 lc rgb '~A'" 
						last_position 	(first  arrow_coord)
						last_position 	(second arrow_coord)
						motif_color )))

			(setq label_coord (pop label_placements))
			(setq motif_code 
			      (cond 	((equal MOTIFTYPE '2DNA)
			      		(HPS_2DNA_to_1DNA (round (second motif))))
			            	((equal MOTIFTYPE '3DNA)
			      		(HPS_3DNA_to_1DNA (round (second motif))))
			            	((equal MOTIFTYPE '4DNA)
			      		(HPS_4DNA_to_1DNA (round (second motif))))))

	  		(format HPS-NDNA-analyzer-buffer "~%	set label \"~A\" at ~D, ~D front" 
				(format nil "~3D <  |~A|  < ~3D (~A)" (- (first motif) overlap_counter) motif_code  (first motif) motif_color)  
				(round (* (- 1 (second label_coord)) (* TO 0.25)))
				(first label_coord)))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; print the labels and arrows identifying the top motifs present in the subsequence being analyzed
;; ***********************************************************************************************************************
(defun HPS_DNA_SEQUENCE_ANALYZER ( HPS_1DNA_bases gnuplot-stream &key (FROM nil) (TO nil))
  	(with-output-to-string ( HPS-analyzer-buffer )
	  (let ( ( hps_num_as			(length (remove-if-not #'(lambda (a) (equal a 'A)) HPS_1DNA_bases)))
		 ( hps_num_cs			(length (remove-if-not #'(lambda (a) (equal a 'C)) HPS_1DNA_bases)))
		 ( hps_num_gs			(length (remove-if-not #'(lambda (a) (equal a 'G)) HPS_1DNA_bases)))
		 ( hps_num_ts			(length (remove-if-not #'(lambda (a) (equal a 'T)) HPS_1DNA_bases))))

		;; ************************************************************************************************************
		(HPS_DNA_MOTIF_FINDER HPS_1DNA_bases HPS-analyzer-buffer
			      	:MOTIFTYPE 	        '2DNA
			        :NBINS			 16
				:NPOWER_MINUS1		 1
				:FROM			 FROM
				:TO			 TO
				:HOWMANY		 5
				:MOTIF_COLORS		'( "black" "green" "brown" "yellow" "red" 
							   "green" "brown" "gray" "blue" "red" "brown" 
							   "orange" "salmon" "red" "black" "brown" 
						   	   "yellow" "red" "gray" "orange" "light-blue" 
							   "salmon" "light-red" "green" "brown" "gray" )
		 	        :LABEL_PLACEMENTS	'((13 0.99) (12 0.99) (11 0.99) (10 0.99) (9 0.99))
		 	        :ARROW_PLACEMENTS	'( (8 7.2) (7 6.2) (6 5.2) (5 4.2) (8 7.2) (7 6.2) (6 5.2) (5 4.2)))
		;; ************************************************************************************************************
		(HPS_DNA_MOTIF_FINDER HPS_1DNA_bases HPS-analyzer-buffer
			      	:MOTIFTYPE 	        '3DNA
			        :NBINS			 64
				:NPOWER_MINUS1		 2
				:FROM			 FROM
				:TO			 TO
				:HOWMANY		 5
				:MOTIF_COLORS		'( "green" "brown" "gray" "blue" "red" "brown" 
							   "orange" "salmon" "red" "black" "brown" 
						   	   "yellow" "red" "gray" "orange" "light-blue" 
							   "salmon" "light-red" "green" "brown" "gray" )
		 		:LABEL_PLACEMENTS	'((13 0.72) (12 0.72) (11 0.72) (10 0.72) (9 0.72))
		 		:ARROW_PLACEMENTS	'( (7 6.2) (6 5.2) (5 4.2) (8 7.2) (7 6.2) (6 5.2) (5 4.2)))
		;; ************************************************************************************************************
		(HPS_DNA_MOTIF_FINDER HPS_1DNA_bases HPS-analyzer-buffer
			      	:MOTIFTYPE 	        '4DNA
			        :NBINS			 256
				:NPOWER_MINUS1		 3
				:FROM			 FROM
				:TO			 TO
				:HOWMANY		 5
				:MOTIF_COLORS		'( "orange" "salmon" "red" "black" "brown" 
						   	   "yellow" "red" "gray" "orange" "light-blue" 
							   "salmon" "light-red" "green" "brown" "gray" )
		 		:LABEL_PLACEMENTS	'((13 0.43) (12 0.43) (11 0.43) (10 0.43) (9 0.43))
		 		:ARROW_PLACEMENTS	'( (6 5.2) (5 4.2) (8 7.2) (7 6.2) (6 5.2) (5 4.2)))
		;; ************************************************************************************************************

		;; ************************************************************************************************************
	  	(format HPS-analyzer-buffer "~%	set label \"~A\" at ~D, ~D front" (format nil "|A|=~D" hps_num_as) (* (* TO 0.25) 0.9) 13)
	  	(format HPS-analyzer-buffer "~%	set label \"~A\" at ~D, ~D front" (format nil "|C|=~D" hps_num_cs) (* (* TO 0.25) 0.9) 12)
	  	(format HPS-analyzer-buffer "~%	set label \"~A\" at ~D, ~D front" (format nil "|G|=~D" hps_num_gs) (* (* TO 0.25) 0.9) 11)
	  	(format HPS-analyzer-buffer "~%	set label \"~A\" at ~D, ~D front" (format nil "|T|=~D" hps_num_ts) (* (* TO 0.25) 0.9) 10))
		
		;; (let ((plot_x   from) (plot_y   4))
			;; (dolist (base HPS_1DNA_bases 'HPS_1DNA)
				;; (format HPS-analyzer-buffer "~% set label \"~A\" at ~D, ~D" base plot_x plot_y)
		                ;; (incf plot_x)
				;; (setq plot_y (if (equal (mod plot_x 3) 0) 4 (if (equal (mod plot_x 3) 1) 5 6)))))
		;; ************************************************************************************************************

	  (setq *HPS_DNA_sequence_analyzer_report_summary* (get-output-stream-string HPS-analyzer-buffer )))

	(format gnuplot-stream (HPS_DNA_GNUPLOT_SEQUENCE_ANALYZER_PLOT) *HPS_DNA_sequence_analyzer_report_summary* 5 5 5 FROM TO 
			FROM 		(floor (* TO 0.25))   
			(* TO 0.25) 	(floor (* TO 0.50))  
			(* TO 0.50) 	(floor (* TO 0.75))  
			(* TO 0.75) 	(ceiling (* TO 1.00)))

	(return-from HPS_DNA_SEQUENCE_ANALYZER *HPS_DNA_sequence_analyzer_report* ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; create the gnuplot command file in the basedirectory as needed for execution
;; ***********************************************************************************************************************
(defun HPS_DNA_print_gnuplot_generator ( HPS_matching_patterns_by_time 
					 HPS_matching_patterns_by_rank 
					 &key (HPS_TERSE t ))

  	;; the number of matching patterns
	(setq num_matching_patterns (length HPS_matching_patterns_by_time))

	;; a convenient memory of matching_patterns endpoints 
	(setq *HPS_endpoints_matches_in_time* (HPS_DNA_get_matching_patterns_endpoints HPS_matching_patterns_by_time 
										       HPS_matching_patterns_by_rank ))

	;; print a GNUPLOT command file with instruction to generate various plots for all computed matching patterns 
	(setq *HPS_DNA_filename* (HPS_concat *HPS_DNA_basedir* "PNG-HPS-DNA-PLOTS.PLT")) 
	(format t "~% GENERATING GNUPLOT COMMANDS TO BE INVOKED: ~A" *HPS_DNA_filename* )

	(with-open-file (gnuplot-cmd-stream *HPS_DNA_filename* :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (with-output-to-string ( HPS-gnuplot-buffer )
		;; ***********************************************************************************************************
	  	;; GNUPLOT initialization and credits
		;; ***********************************************************************************************************
		(format HPS-gnuplot-buffer (HPS_GNUPLOT_INIT) *HPS_DNA_basedir* )
		;; ***********************************************************************************************************
	

		;; ***********************************************************************************************************
		(when (NOT HPS_TERSE)
			;; the HPS DNA TIME SERIES ALIGN  plots
			(dolist (p HPS_matching_patterns_by_time 'ALIGNMENT)
				(setq HPS_endpoints_match  (pop   *HPS_endpoints_matches_in_time*))
				(setq HPS_matching_p_start (first  HPS_endpoints_match))
				(setq HPS_matching_p_end   (second HPS_endpoints_match))
				(setq HPS_matching_p       (third  HPS_endpoints_match))
				(setq HPS_timeindex_p 	   (fourth HPS_endpoints_match))
				(setq HPS_rank_p 	   (fifth  HPS_endpoints_match))
	
				;; for the segment alignment plots, the file index is that of time, but the sequence has the proper rank
				(HPS_DNA_GNUPLOT_SEGMENT_ALIGNMENT  HPS_matching_p 
								   *HPS_DNA_endpoints_signature* 
								    HPS_endpoints_match 
								    HPS-gnuplot-buffer 
								   :RANK      HPS_rank_p 
								   :FILEINDEX HPS_timeindex_p )
	
				;; write pending string output to file
				(format gnuplot-cmd-stream "~A" (get-output-stream-string HPS-gnuplot-buffer )))
	
			;; the HPS SEGMENT_ALIGN  CROSSREF HISTOGRAMS plots - awith respect to time order
			(setq patnum 0)
			(dolist (p HPS_matching_patterns_by_time 'MATCH__PATTERNS_CLOSEUPS)
				(HPS_DNA_GNUPLOT_CROSSREF_SIG2MATCH 	patnum 
									p
									HPS-gnuplot-buffer )
				
				;; write pending string output to file
				(format gnuplot-cmd-stream "~A" (get-output-stream-string HPS-gnuplot-buffer ))
	
				(incf patnum)))
		;; ***********************************************************************************************************
	

		;; ***********************************************************************************************************
		;; the MATCH_BEHAVIOR plot with labels for each best matching pattern in the test sequence 
		;; ***********************************************************************************************************
		(HPS_DNA_MATCH_BEHAVIOUR_PLOT   HPS_matching_patterns_by_time 
				        	HPS_matching_patterns_by_rank
				        	HPS-gnuplot-buffer )
		
		(HPS_DNA_ALIGNMENT_ADJUST_PLOT  HPS_matching_patterns_by_time
					        HPS-gnuplot-buffer)

		(HPS_DNA_FINEGRAIN_XY_PLOTSHEET HPS_matching_patterns_by_time
						HPS-gnuplot-buffer )


		(push (list 'GNUCMDS (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)


		(HPS_DNA_PRINT_PHYLOGENIC_TREE  HPS_matching_patterns_by_time 
						HPS-gnuplot-buffer )

		(HPS_DNA_SEQUENCE_ANALYZER 	(subseq *HPS_1DNA_actual_input_bases*   *HPS_1DNA_endpoint_signature_start* 
											*HPS_1DNA_endpoint_signature_end*) 
						HPS-gnuplot-buffer 
						:FROM *HPS_1DNA_endpoint_signature_start* 
						:TO   *HPS_1DNA_endpoint_signature_end* )

		(HPS_DNA_print_regexpr_match 	(subseq *HPS_1DNA_actual_input_bases*   *HPS_1DNA_endpoint_signature_start* 
											*HPS_1DNA_endpoint_signature_end*) 
						HPS-gnuplot-buffer 
						:FROM *HPS_1DNA_endpoint_signature_start* 
						:TO   *HPS_1DNA_endpoint_signature_end* 
						:MOTIF_REGEXPR 
			"(TTG)[1](T)[2](AT)[1](AA)[2](AT)[1](GCT)[1](ACT)[1](GC)[1](AC)[1](GG)[4](CAC)[1](GGA)[1](TAT)[2](GTA)[1]" )

		(when (NOT HPS_TERSE)
			(format HPS-gnuplot-buffer (HPS_GNUPLOT_MATCH_BEHAVIOR_PLOT_3DNA) HPS_MATCH_TITLE)) 

		(push (list 'DNATOOLS (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)

		;; write pending string output to file
		(format gnuplot-cmd-stream "~A" (get-output-stream-string HPS-gnuplot-buffer ))
		;; ***********************************************************************************************************

		nil)))
;; ***********************************************************************************************************************


