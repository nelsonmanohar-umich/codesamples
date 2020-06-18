(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;;(proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))

;; ********************************************************************************************
;;                                     HPS DNA PATTERN MINER 
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
;; ********************************************************************************************


;; ***********************************************************************************************************************
;; coarsegrain to finegrain comparator table accessor functions 
;; ***********************************************************************************************************************
(defun HPS_DNA_get_comparator_table ()
  	(return-from HPS_DNA_get_comparator_table      HPS_DNA_comparator_table ))

(defun HPS_DNA_get_comparison_threshold ()
  	(return-from HPS_DNA_get_comparison_threshold  *HPS_DNA_comparison_threshold*))

(defun HPS_DNA_get_comparator_table_rating ( p )
  	(setq HPS_DNA_match_rating_xyz                (first (first (remove-if-not #'(lambda (a) (equal p (second a)))  HPS_DNA_comparator_table ))))
  	(return-from HPS_DNA_get_comparator_table_rating HPS_DNA_match_rating_xyz ))

(defun HPS_DNA_get_comparator_table_adjusted_sequence ( p )
  	(setq HPS_matching_sequence_temp_xyz          (third (first (remove-if-not #'(lambda (a) (equal p (second a)))  HPS_DNA_comparator_table ))))
  	(return-from HPS_DNA_get_comparator_table_adjusted_sequence HPS_matching_sequence_temp_xyz))

(defun HPS_DNA_get_comparator_table_alignment_index ( p )
  	(setq HPS_DNA_match_alignment_index_xyz       (fourth (first (remove-if-not #'(lambda (a) (equal p (second a))) HPS_DNA_comparator_table ))))
  	;; (incf HPS_DNA_match_alignment_index_xyz       *HPS_DNA_SHIFT_AMOUNT*)
  	(return-from HPS_DNA_get_comparator_table_alignment_index HPS_DNA_match_alignment_index_xyz ))

(defun HPS_DNA_get_alignment_vector_for (p)
	(setq hps_alignment_vector 		      (fifth (first (remove-if-not #'(lambda (a) (equal p (second a)))  HPS_DNA_comparator_table ))))
  	(return-from HPS_DNA_get_alignment_vector_for hps_alignment_vector ))

(defun HPS_DNA_get_pattern_id_for (p) 
  	(return-from HPS_DNA_get_pattern_id_for  (first (first p))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_predetermine_if_approx_unique_pattern (old_p new_p)
	(setq hps_dna_similar_pattern_flag nil)

  	(when   (AND old_p 
		     new_p)

  		(setq old_start  (HPS_DNA_get_pattern_match_start  old_p ))
  		(setq old_end    (HPS_DNA_get_pattern_match_end    old_p ))
  		(setq old_rating (HPS_DNA_get_pattern_match_rating old_p ))

  		(setq new_start  (HPS_DNA_get_pattern_match_start  new_p ))
  		(setq new_end    (HPS_DNA_get_pattern_match_end    new_p ))
  		(setq new_rating (HPS_DNA_get_pattern_match_rating new_p ))

		(when   (AND (> new_rating  *HPS_DNA_matching_goal*) (> new_rating  *HPS_DNA_matching_goal*))
			(when (AND (HPS_is_this_between new_start  (HPS_DNA_pattern_alignment_lookup_start old_p) (+ old_start *HPS_DNA_SHIFT_AMOUNT*)) 
			 	   (HPS_is_this_between new_end    (- old_end  *HPS_DNA_SHIFT_AMOUNT*) (HPS_DNA_pattern_alignment_lookup_end   old_p)))
					(setq hps_dna_similar_pattern_flag t))))

  	(unless (AND old_p new_p)
		(setq hps_dna_similar_pattern_flag t))

	(return-from HPS_DNA_predetermine_if_approx_unique_pattern hps_dna_similar_pattern_flag ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; substitue for macro for consistency
;; ***********************************************************************************************************************
(setq *HPS_DNA_num_uniq_matches* 0)
(defun HPS_DNA_determine_if_approx_unique_pattern (old_p new_p)
	(setq hps_dna_similar_pattern_flag nil)

  	(when   (AND old_p 
		     new_p)
  		(setq old_start (HPS_DNA_get_pattern_match_start old_p ))
  		(setq old_end   (HPS_DNA_get_pattern_match_end   old_p ))

  		(setq new_start (HPS_DNA_get_pattern_match_start new_p))
  		(setq new_end   (HPS_DNA_get_pattern_match_end   new_p))

		(when (AND (HPS_is_this_between new_start  (HPS_DNA_pattern_alignment_lookup_start old_p) (+ old_start *HPS_DNA_SHIFT_AMOUNT*)) 
			   (HPS_is_this_between new_end    (- old_end  *HPS_DNA_SHIFT_AMOUNT*) (HPS_DNA_pattern_alignment_lookup_end   old_p)))
				(setq hps_dna_similar_pattern_flag t))

		(when hps_dna_similar_pattern_flag
	  		(incf *HPS_DNA_num_uniq_matches*)))

  	(unless (AND old_p 
		     new_p)
		(setq hps_dna_similar_pattern_flag t)
	  	(incf *HPS_DNA_num_uniq_matches*))

	(return-from HPS_DNA_determine_if_approx_unique_pattern hps_dna_similar_pattern_flag ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this implements a temporary test for pattern match uniqueness, to be refined later, uniqueness only based on 
;; approximate endpoints
;; ***********************************************************************************************************************
(defun HPS_DNA_determine_if_unique_pattern (p previous_p num_unique_matches )

  	(when   previous_p
  		(setq prev_start (HPS_DNA_get_pattern_match_start previous_p))
  		(setq prev_end   (HPS_DNA_get_pattern_match_end   previous_p))

  		(setq pres_start (HPS_DNA_get_pattern_match_start p))
  		(setq pres_end   (HPS_DNA_get_pattern_match_end   p))

		(setq diff_enough t)
		(when (HPS_is_this_between pres_start prev_start 
					 	   (+ prev_start     (* (- *HPS_1DNA_endpoint_signature_end* *HPS_1DNA_endpoint_signature_start*) 0.05)))
			(when (HPS_is_this_between pres_end prev_end
					                 (+ prev_end (* (- *HPS_1DNA_endpoint_signature_end* *HPS_1DNA_endpoint_signature_start*) 0.05)))
				(setq diff_enough nil)))

		(when diff_enough 
	  		(incf *HPS_DNA_num_uniq_matches*)))

  	(unless previous_p
	  	(incf *HPS_DNA_num_uniq_matches*))

	(return-from HPS_DNA_determine_if_unique_pattern *HPS_DNA_num_uniq_matches*))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; records ALL paired cost differences between ALL segments in the HPS_DNA_ signature and ALL segments in the test sequence 
;; creates a record of the computed comparison metrics between two HPS_DNA_/ATS segments.
;; ***********************************************************************************************************************
(defun HPS_DNA_compute_allpairs_costs ( hps_hypothesis_starting_point 
					hps_signature_segtable
					hps_remainder_testseq_segtable ) 

    	(dolist (x hps_signature_segtable  nil) 

		(setq *HPS_DNA_allpairs_forx* nil)

		(dolist (y hps_remainder_testseq_segtable 'ALLPAIRSCOSTS) 

			;; (setq *HPS_3DNA_hash_lookup_key* 	(list x y))
			;; (setq *HPS_3DNA_hash_lookup_val*	(gethash *HPS_3DNA_hash_lookup_key* *HPS_3DNA_segtable_hashtable* ))

			(setq *HPS_3DNA_hash_lookup_val*	nil)

			;; when the hash table entry exists, this segment pair has previously been examined, so use the retrieved value value instead 
			(when   *HPS_3DNA_hash_lookup_val*
			  	(setq *HPS_DNA_distance_metrics_tuple*	 *HPS_3DNA_hash_lookup_val*)
			 	(push *HPS_DNA_distance_metrics_tuple* 	 *HPS_DNA_allpairs_forx*))

			(unless *HPS_3DNA_hash_lookup_val*
			  (let ((*HPS_3DNA_diff_val* 	(abs (- (HPS_DNA_get_segment_match_tval x) 	
								        (HPS_DNA_get_segment_match_tval y))))
  		  	 	(*HPS_3DNA_diff_dur* 	(abs (- (HPS_DNA_get_segment_match_sdur x) 	
								        (HPS_DNA_get_segment_match_sdur y))))
  		  	 	(*HPS_3DNA_diff_idx* 	(abs (- (HPS_DNA_get_segment_match_start x)  
								        (- (HPS_DNA_get_segment_match_start y) hps_hypothesis_starting_point )))))
		
  		  	 	(setq *HPS_SEG_diff* 	(+ (* *HPS_3DNA_diff_val*  16) 	 ;; penalize heavily  hps segment targeting differences
							   (* *HPS_3DNA_diff_dur*   4) 	 ;; penalize somewhat hps segment duration differences
							   (* *HPS_3DNA_diff_idx*   1))) ;; penalize a little hps time index differences
		
				;; CHECK (remove hardcoding) these for prunning
		  	 	(if (> *HPS_3DNA_diff_val* 		    	4)    
				  	(setq *HPS_SEG_diff* *HPS_DNA_LARGE_DIFF*)

		  	 		(if (> *HPS_3DNA_diff_dur* 	   	32)   
					  	(setq *HPS_SEG_diff* *HPS_DNA_LARGE_DIFF*)

		  	 			(if (> *HPS_3DNA_diff_idx* 	128)  
						  	(setq *HPS_SEG_diff* *HPS_DNA_LARGE_DIFF*) 

							nil)))
		
  		  	 	(setq *HPS_DNA_distance_metrics_tuple_part_b* (list *HPS_3DNA_diff_val* 
										    *HPS_3DNA_diff_dur* 
										    *HPS_3DNA_diff_idx* 
										    "EVAL_AT" 
										     hps_hypothesis_starting_point ))

  		  	 	(setq *HPS_DNA_distance_metrics_tuple*        (list y 
										    *HPS_SEG_diff* 
										    *HPS_DNA_distance_metrics_tuple_part_b* )))

				;; CHECK - need to optimize this, these three lines incur in the bulk (of the memory cons) overhead in the entire 
				;; program (30% overhead approx) and has nothing to do with the algorithm but with storage representation

				;; (setq *HPS_3DNA_hash_lookup_key* 		(list x y))
			  	;; (setq *HPS_3DNA_hash_lookup_val*		*HPS_DNA_distance_metrics_tuple*)
				;; (setq (gethash *HPS_3DNA_hash_lookup_key*	*HPS_3DNA_segtable_hashtable* ) 	*HPS_3DNA_hash_lookup_val*)
			 	(push *HPS_DNA_distance_metrics_tuple* 	 	*HPS_DNA_allpairs_forx*)))
	
		(setq *HPS_DNA_allpairs_tuple* 		(list   x (reverse *HPS_DNA_allpairs_forx*)))

		(push *HPS_DNA_allpairs_tuple* 		*HPS_3DNA_all_sig2subseq_pairings* ))

    	(return-from HPS_DNA_compute_allpairs_costs *HPS_3DNA_all_sig2subseq_pairings* ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; Finds the set of minimum cost matches between segments in the HPS_DNA_ signature and segments in the test sequence, 
;; constrained wrt similar duration, similar value, similar relative order, and similar relative positioning.
;;
;; Two HPS_DNA_ segments are similar enough if they have similar targeting value, similar duration, and take place at similar time indexes
;; note that partial order semantics are inherently accounted for as the segments are taken and compared from ordered segment lists
;;
;; If a minimum cost match does not exist for an HPS_DNA_ segment, generates an association tuple relating such 
;;
;; Output is a table list of matching segments --- given a hypothesis start point since all cost indexes 
;; were computed previously wrt to that hypothesis start value.
;; ***********************************************************************************************************************
(defun HPS_DNA_mincost_patternmatch_at ( hps_hypothesis_start 
					 hps_signature_segtable 
					 hps_exhaustive_allpairs_search_results ) 

  	;; CHECK here exists an error of mincost which instead of choosing the next (future segment) match
	;; chooses the current best match, it seems that it needs to do something with boundaries, iteration
	;; or limits as it is just the item chosen for the next item

    (dolist (x hps_signature_segtable  'HPS_DNA_SIGNATURE_MATCHING) 

	(setq *HPS_DNA_relevant_segments* 	(second (assoc x hps_exhaustive_allpairs_search_results )))

	(setq *minimum_dist*   			999999)
	(setq *HPS_DNA_matching_segments_and_metrics* nil)
	(dolist (s *HPS_DNA_relevant_segments* 'MIN_COST) 
	  	(when (< (second s) *minimum_dist*) 
			(setq *minimum_dist* (second s)) 
			(setq *HPS_DNA_matching_segments_and_metrics* s))

	  	(unless (< (second s) *minimum_dist*) 
			nil )) 

	(when (equal *HPS_DNA_matching_segments_and_metrics* nil)
		(setq *this_matching_seg* 	nil)
		(setq *HPS_SEG_diff*      	99999)
	  	(setq *HPS_3DNA_diff_dur* 	11111)
		(setq *HPS_3DNA_diff_val* 	11111)
		(setq *HPS_3DNA_diff_idx* 	11111))

	(unless (equal *HPS_DNA_matching_segments_and_metrics* nil)
		(setq *this_matching_seg*  	(first  *HPS_DNA_matching_segments_and_metrics* ))
		(setq *HPS_SEG_diff*       	(second *HPS_DNA_matching_segments_and_metrics* ))
	  	(setq *HPS_3DNA_diff_dur*  	(second (third *HPS_DNA_matching_segments_and_metrics* )))
		(setq *HPS_3DNA_diff_val*  	(first  (third *HPS_DNA_matching_segments_and_metrics* )))
		(setq *HPS_3DNA_diff_idx*  	(third  (third *HPS_DNA_matching_segments_and_metrics* ))))

	(setq *HPS_DNA_sig_seg*  x) 
	(cond 	((AND (is_between *HPS_3DNA_diff_val* 0 0) (is_between *HPS_3DNA_diff_dur* 0 2)  (is_between *HPS_3DNA_diff_idx* 0 4))    		     
	 		(setq *matching_tuple* (list *HPS_DNA_sig_seg* *HPS_DNA_matching_segments_and_metrics*  'EXACTING_MATCH)))
	  	((AND (is_between *HPS_3DNA_diff_val* 0 0) (is_between *HPS_3DNA_diff_dur* 0 4)  (is_between *HPS_3DNA_diff_idx* 0 8))    		     
	 		(setq *matching_tuple* (list *HPS_DNA_sig_seg* *HPS_DNA_matching_segments_and_metrics*  'APPROX32_MATCH)))
	  	((AND (is_between *HPS_3DNA_diff_val* 0 0) (is_between *HPS_3DNA_diff_dur* 0 8)  (is_between *HPS_3DNA_diff_idx* 0 16))    		     
	 		(setq *matching_tuple* (list *HPS_DNA_sig_seg* *HPS_DNA_matching_segments_and_metrics*  'APPROX64_MATCH)))
	  	((AND (is_between *HPS_3DNA_diff_val* 0 0) (is_between *HPS_3DNA_diff_dur* 0 16) (is_between *HPS_3DNA_diff_idx* 0 16))    		     
	 		(setq *matching_tuple* (list *HPS_DNA_sig_seg* *HPS_DNA_matching_segments_and_metrics*  'SPANNING_MATCH)))
	  	((AND (is_between *HPS_3DNA_diff_val* 1 1) (is_between *HPS_3DNA_diff_dur* 0 2)  (is_between *HPS_3DNA_diff_idx* 0 4))    		     
	 		(setq *matching_tuple* (list *HPS_DNA_sig_seg* *HPS_DNA_matching_segments_and_metrics*  'EXACTING_MISPL)))
	  	((AND (is_between *HPS_3DNA_diff_val* 1 1) (is_between *HPS_3DNA_diff_dur* 0 4)  (is_between *HPS_3DNA_diff_idx* 0 8))    		     
	 		(setq *matching_tuple* (list *HPS_DNA_sig_seg* *HPS_DNA_matching_segments_and_metrics*  'APPROX32_MISPL)))
	  	((AND (is_between *HPS_3DNA_diff_val* 1 1) (is_between *HPS_3DNA_diff_dur* 0 8)  (is_between *HPS_3DNA_diff_idx* 0 16))    		     
	 		(setq *matching_tuple* (list *HPS_DNA_sig_seg* *HPS_DNA_matching_segments_and_metrics*  'APPROX64_MISPL)))
	  	((AND (is_between *HPS_3DNA_diff_val* 1 1) (is_between *HPS_3DNA_diff_dur* 0 16) (is_between *HPS_3DNA_diff_idx* 0 16))    		     
	 		(setq *matching_tuple* (list *HPS_DNA_sig_seg* *HPS_DNA_matching_segments_and_metrics*  'SPANNING_MISPL)))
	  	(T
	 		(setq *matching_tuple* (list *HPS_DNA_sig_seg* *HPS_DNA_matching_segments_and_metrics*  'NOTFOUND_WARNG))))

	(setq *last_match* (first *HPS_DNA_matching_segments*))
	(when (NOT (equal *last_match* nil))
		(setq *last_signature_segment* 		  (first *last_match*))
		(setq *last_matching_segment_and_metrics* (second *last_match*))
		(setq *last_matching_segment_quality*     (third *last_match*))

		(when (NOT (equal *last_matching_segment_quality* 'NOTFOUND_WARNG))
			(setq *last_matching_seg*  (first *last_matching_segment_and_metrics*))
			(setq *lastHPS_SEG_diff*       (second *last_matching_segment_and_metrics*))

			(when (equal *last_matching_seg* *this_matching_seg*)
				(when   (< *lastHPS_SEG_diff* *HPS_SEG_diff*)
			  		(if *HPS_DNA_debug* (print 'DUPLICATE_REMOVED) nil))

				(unless (< *lastHPS_SEG_diff* *HPS_SEG_diff*)
			  		(pop *HPS_DNA_matching_segments*)
					(push (list *last_signature_segment* nil 'NOTFOUND_WARNG) *HPS_DNA_matching_segments*)
					(push *matching_tuple* *HPS_DNA_matching_segments*)))

			(unless (equal *last_matching_seg* *this_matching_seg*)
				(push *matching_tuple* *HPS_DNA_matching_segments*)))

		(unless (NOT (equal *last_matching_segment_quality* 'NOTFOUND_WARNG))
			(push *matching_tuple* *HPS_DNA_matching_segments*)))

	(unless (NOT (equal *last_match* nil))
		(push *matching_tuple* *HPS_DNA_matching_segments*)))

    (return-from HPS_DNA_mincost_patternmatch_at *HPS_DNA_matching_segments* ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; Initialization of per-iteration global items
;; ***********************************************************************************************************************
(defun HPS_DNA_iteration_initialization ()
	;; recall that cost metric is of the form (segment2 distance_metric (dur_diff val_diff ind_diff 'EVALUATED_AT starting_index_test_sequence))
	(setq *HPS_DNA_relevant_segments*     nil) ; "tracks the subset of relevant pairwise comparisons to analyze")
	(setq *HPS_DNA_matching_segments_and_metrics*  
	      				      nil) ; "tracks the minimal cost tuple, best match among the set of relevant pairwise comparisons")
	(setq *HPS_DNA_matching_segments*     nil) ; "a minimum cost pairings HPS_DNA_ signature and test sequence segmetns give a hypothesis start point")

	;; These are the components of the difference metric which detemines how similar are two segments to each other
	;; the distance cost metric used to determine if two HPS_DNA_ segments (x and y) are similar enough 
	;; Note that (currently) cost components are each weighted by a factor of 1.
	(setq *diff_dur* 			0) ; "difference (x-y) in the duration of the two segments") 
	(setq *diff_val* 			0) ; "difference (x-y) in the targeting value of the two segments") 
	(setq *diff_idx* 			0) ; "difference (x-y) in the relative time index start of the two segments") 
	(setq *HPS_SEG_diff*  			0) ; "total weighted difference (x-y) between the two segments") 

	;; global variable that stores all paired comparisons and the results (difference/cost metrics) of such
	(setq *HPS_DNA_allpairs_forx* 	      nil) ; "comparisons with a given signature tuple; relates the approximate presence within test sequence")
	(setq *HPS_DNA_allpairs_tuple* 	      nil) ; "format is of the form: ( signature_segx ( res_tuple1 res_tuple2 ... ))")
	(setq *HPS_DNA_distance_metrics_tuple* 
	      				      nil) ; "format is of the form: ( testseg_y diff_x-y ( val_diff dur_diff ind_diff ))") 
	(setq *HPS_3DNA_all_sig2subseq_pairings*      
	      				      nil) ; "all comparisons given a hypothesis start; relates an approximate pattern match")

	;; global intermediate output variable that stores an approximate pattern match between the HPS_DNA_ signature and an instance 
	;; of the test sequence given a specified hypothesis starting point.  stores all paired comparisons and their cost metrics
	(setq *HPS_3DNA_allpairs_search_result* 
	      				      nil) ; "memory of all comparisons in table format indexed wrt each segment of the HPS_DNA_ signature")
	
	;; the start of the approximate pattern matching region being evaluated. it is given by the application of the hypothesis start,
	;; which value is effectively the start-point of a segment in the test sequence. Note that this value is different than the value
	;; of the first segment matched, as the first segment may not be found but the others could.
	(setq *HPS_DNA_hypothesis_start* 	0)

	;; the end of the approximate pattern matching region being evaluated. it is given by the end-point of the last segment 
	;; matched, which could be either an exact or approximate or mispelled matching.
	(setq *HPS_DNA_hypothesis_end* 		0)

	;; percentage of matched segments between the signature and the particular test sequence region, 
	;; it incorporates only exact and approximate segment matchings without mispelled segments.
	(setq *HPS_DNA_matching_percentage* 	0.0)		

	;; fitness score for a particular pattern match of segments between the signature and a particular test sequence region, 
	;; it incorporates metrics that accounts for all types of matchings (i.e., exact, approximate, mispelling, and not-found).
	(setq *HPS_DNA_fitness_score* 		0.0)

	;; stores various metrics for the fitness of the approximate HPS_DNA_ shape pattern match of a HPS_DNA_ signature within the region 
	;; contained within *HPS_DNA_hypothesis_start* and *HPS_DNA_hypothesis_end*
  	(setq *HPS_DNA_fitness_metrics_tuple*   nil)

	;; accounting information wrt segment tracking
	(setq *num_exactmatch* 			0) 
	(setq *num_apprxmatch* 			0) 
	(setq *num_loosematch* 			0) 
	(setq *num_spannmatch* 			0) 
	(setq *num_exactmispl* 			0) 
	(setq *num_apprxmispl* 			0) 
	(setq *num_loosemispl* 			0) 
	(setq *num_spannmispl* 			0) 
	(setq *num_nomatchfnd*  		0))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_compute_matchfitness ( hps_hypothesis_start 
				      hps_matching_segments )

	(setq *HPS_DNA_fitness_score* 		0.0)
	(setq *HPS_DNA_matching_percentage* 	0.0)		
	(dolist (x hps_matching_segments 'HPS_DNA_PATTERNMATCHTABLE) 

		(when (NOT (equal (second x) nil))
	  		(setq *match_quality*                            (third   x))
	  		(setq *HPS_DNA_matching_segments_and_metrics*    (second  x))
	  		(setq *match_fitness*                            (second *HPS_DNA_matching_segments_and_metrics*))
			(setq *HPS_DNA_fitness_score*       		 (+ *match_fitness* 
									    *HPS_DNA_fitness_score* ))
	  		(setq *match_segment* 			         (first  *HPS_DNA_matching_segments_and_metrics*))
			(if *match_segment* 
				(setq *HPS_DNA_hypothesis_end* 	    	(second *match_segment*))
				(setq *HPS_DNA_hypothesis_end* 	    	 hps_hypothesis_start))
	
	        	(cond 	((equal *match_quality* 'EXACTING_MATCH) (incf *HPS_DNA_matching_percentage*) (incf *num_exactmatch*))
	 			((equal *match_quality* 'APPROX32_MATCH) (incf *HPS_DNA_matching_percentage*) (incf *num_apprxmatch*))
	 			((equal *match_quality* 'APPROX64_MATCH) (incf *HPS_DNA_matching_percentage*) (incf *num_loosematch*))
	 			((equal *match_quality* 'SPANNING_MATCH) (incf *HPS_DNA_matching_percentage*) (incf *num_spannmatch*))
	 			((equal *match_quality* 'EXACTING_MISPL) (incf *HPS_DNA_matching_percentage*) (incf *num_exactmispl*))
	 			((equal *match_quality* 'APPROX32_MISPL) (incf *HPS_DNA_matching_percentage*) (incf *num_apprxmispl*))
	 			((equal *match_quality* 'APPROX64_MISPL) (incf *HPS_DNA_matching_percentage*) (incf *num_loosemispl*))
	 			((equal *match_quality* 'SPANNING_MISPL) (incf *HPS_DNA_matching_percentage*) (incf *num_spannmispl*))
				((equal *match_quality* 'NOTFOUND_WARNG) (incf *num_nomatchfnd*))
				( T 					  nil)))

	  	(unless (NOT (equal (second x) nil)) 
		  	nil))

	(when   hps_matching_segments
	  	(if (> (length hps_matching_segments) 0)
	  		(setq *HPS_DNA_matching_percentage* (/ *HPS_DNA_matching_percentage* (length hps_matching_segments)))
	  		(setq *HPS_DNA_matching_percentage* 0))
  		(setq *HPS_DNA_fitness_metrics_tuple* 
		      		(list   *HPS_DNA_hypothesis_start* 
			    		*HPS_DNA_hypothesis_end*    
			    		*HPS_DNA_matching_percentage*   
			    		*HPS_DNA_fitness_score* 
			    		(list 	*num_exactmatch* 	  *num_apprxmatch* 	   *num_loosematch* 	   *num_spannmatch* 
				  	      	*num_exactmispl* 	  *num_apprxmispl* 	   *num_loosemispl* 	   *num_spannmispl* ))))

	(unless hps_matching_segments
	  	(setq *HPS_DNA_matching_percentage* 0.0)
  		(setq *HPS_DNA_fitness_metrics_tuple* 
		      		(list 	*HPS_DNA_hypothesis_start* 
			    		*HPS_DNA_hypothesis_start*  
			    		*HPS_DNA_matching_percentage*   
			    		*HPS_DNA_HIGHCOST_FLAG0*
			    		(list 	*HPS_DNA_HIGHCOST_FLAG1* *HPS_DNA_HIGHCOST_FLAG1* *HPS_DNA_HIGHCOST_FLAG1* *HPS_DNA_HIGHCOST_FLAG1* 
			          		*HPS_DNA_HIGHCOST_FLAG1* *HPS_DNA_HIGHCOST_FLAG1* *HPS_DNA_HIGHCOST_FLAG1* *HPS_DNA_HIGHCOST_FLAG1* ))))

	(return-from HPS_DNA_compute_matchfitness *HPS_DNA_fitness_metrics_tuple*))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_get_ALL_matching_patterns ()
  	(return-from HPS_DNA_get_all_matching_patterns (HPS_DNA_get_best_matches :sortby 'ALLP)))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; retrieves memory of matching patterns, usually best matching patterns either sorted by rank or by time, but also 
;; retrieves the memory of ALL possible patterns that were examined
;; ***********************************************************************************************************************
(defun HPS_DNA_get_best_matches ( &key (sortby 'RANK))
  	(cond   ((equal sortby 'RANK) (return-from HPS_DNA_get_best_matches *HPS_DNA_best_matching_patterns_sorted_by_rank*))
  	        ((equal sortby 'ORIG) (return-from HPS_DNA_get_best_matches *HPS_DNA_best_matching_patterns_sorted_by_rank_old*))
  	        ((equal sortby 'TIME) (return-from HPS_DNA_get_best_matches *HPS_DNA_best_matching_patterns_sorted_by_time*))
  	        ((equal sortby 'ALLP) (return-from HPS_DNA_get_best_matches *HPS_DNA_all_patterns_found*))
		(T 		      (return-from HPS_DNA_get_best_matches *HPS_DNA_best_matching_patterns_sorted_by_time*))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_best_pattern_of ( p1 p2 )
  	(if (>= (HPS_DNA_get_pattern_match_rating p1) (HPS_DNA_get_pattern_match_rating p2)) 
	  	(return-from HPS_DNA_best_pattern_of p1) 
	  	(return-from HPS_DNA_best_pattern_of p2))) 
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function returns the position of the particular pattenr within a set of patterns, 
;; when the pattern-set is ordered by rank, it returns the i-th best pattern of the set
;; where 0-th is the best and n-th is said to be the least fit of the best.
;; ***********************************************************************************************************************
(defun HPS_DNA_rank_of_pattern_in (pattern pattern_set)
  	(setq pat_rank_temp 0)
  	(dolist (p pattern_set 'RANKINGPATTERNS)
	  	(if (equal (first pattern) (first p)) 
		  	(return-from HPS_DNA_rank_of_pattern_in pat_rank_temp)
			(incf pat_rank_temp)))
	(return-from HPS_DNA_rank_of_pattern_in pat_rank_temp))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function sorts a subset of the best matching patterns by a cost criteria, this being the rating or the normalized cost 
;; ***********************************************************************************************************************
(defun HPS_DNA_best_pattern_in ( remainder &key (criteria 'RATING))
  	(if (not remainder) 
	  	(return-from HPS_DNA_best_pattern_in nil) 
		nil)

	(if (equal criteria 'RATING)
		(setq bestval      (HPS_DNA_get_pattern_match_rating   (first remainder)))
		(setq bestval (/ 1 (HPS_DNA_get_pattern_match_normcost (first remainder)))))
	
	(setq bestpat (first remainder))

	(dolist (p remainder 'SORTINGOFMATCHES)
		(if (equal criteria 'RATING)
			(setq currentval      (HPS_DNA_get_pattern_match_rating   p )) 
			(setq currentval (/ 1 (HPS_DNA_get_pattern_match_normcost p ))))

  		(when   (< bestval currentval )
	  		(setq bestpat p)
	  		(setq bestval currentval))

		(unless (< bestval currentval )
			nil))

  	(return-from HPS_DNA_best_pattern_in bestpat))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function sorts the best matching patterns by a cost criteria
;; ***********************************************************************************************************************
(defun HPS_DNA_sort_best_patterns ( best_matching_patterns_by_time &key (criteria 'RATING))
	(setq remainder best_matching_patterns_by_time)
	(setq numpats (length remainder))
  	(setq *HPS_DNA_best_matching_patterns_sorted_by_rank* nil)

	;; an inefficient o(|{p}|2) loop
	(dotimes (j numpats 'MAINSORTING)
		(setq best_pattern_in_remainder (HPS_DNA_best_pattern_in  remainder 
									 :criteria criteria))
		(when best_pattern_in_remainder 
			(push best_pattern_in_remainder *HPS_DNA_best_matching_patterns_sorted_by_rank*)
			nil)
		(setq remainder (delete best_pattern_in_remainder remainder)))

	(setq *HPS_DNA_best_matching_patterns_sorted_by_rank* (reverse *HPS_DNA_best_matching_patterns_sorted_by_rank*))

	(return-from HPS_DNA_sort_best_patterns *HPS_DNA_best_matching_patterns_sorted_by_rank* ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; HPS_DNA_ state table I/O: actual decoded set of unearthed HPS_DNA_/ATS segments evaluated at the provided minimum durations 
;; ***********************************************************************************************************************
(defun HPS_DNA_print_mincost_patternmatch ( these_HPS_DNA_matching_segments 
					    hps_segtable_filename )

	(with-open-file (output-stream hps_segtable_filename :direction :output :if-exists :append :if-does-not-exist :create)
		(format output-stream "~&" )
		(format output-stream "~&~S" '____________________________________________________________ )

		(dolist (x these_HPS_DNA_matching_segments 'HPS_DNA_PATTERNMATCHTABLE) 
  			(setq *signature_segment*               (first x))
  			(setq *matching_segment_and_metrics*    (second x))
  			(setq *match_quality*                   (third x))
  			(setq *testseq_segment*   		(first *matching_segment_and_metrics*))
  			(setq *match_details*     		(rest  *matching_segment_and_metrics*))

			(when (equal *match_quality* 'NOTFOUND_WARNG)
      				(format output-stream "~&" )
           		     	(format output-stream "~{~18A	~}	" *signature_segment* )
           		     	(format output-stream "~S	" 'NOT_APPLICABLE )
           		     	(format output-stream "~S	" *match_quality* )
           		     	(format output-stream "~S	" 'NOT_APPLICABLE )) 

			(unless (equal *match_quality* 'NOTFOUND_WARNG)
      				(format output-stream "~&")
           		     	(format output-stream "~{~18A	~}	" *signature_segment* )
      	   		     	(format output-stream "~S	" *testseq_segment* )
      	   		     	(format output-stream "~S	" *match_quality* )
      	   		     	(format output-stream "~S	" *match_details*)))

		(format output-stream "~&~S" '____________________________________________________________ )
		(format output-stream "~&" )))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; produce a table of all matching patterns found to be feasible solutions from within the set of all possible patterns
;; ***********************************************************************************************************************
(defun HPS_DNA_choose_best_patternmatch ( all_possible_hps_matches 
					  hps_matching_goal 
					  hps_match_behavior_filename
					  hps_segtable_filename )

  	(setq *HPS_DNA_fitness_metrics_tuple* 		nil)
        (setq *HPS_DNA_best_matching_patterns* 		nil)

	(with-open-file (output-stream hps_match_behavior_filename :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (with-output-to-string ( HPS-pattern-buffer )
		(format HPS-pattern-buffer "~&~{~12A	~}" '( INTERV_START 
						       INTERVAL_END 
						       MATCHPERCENT 
						       DIST_CMETRIC 
						       NORML_METRIC 
						       MATCHSPAN    
						       MCOST_VECTOR ))

		(dolist (x all_possible_hps_matches 'BESTMATCHES)
	    		(when x
  				(setq *HPS_DNA_fitness_metrics_tuple* (first  x))
	        		(setq *HPS_DNA_matching_segments*     (first  (second x)))
  				(setq *HPS_DNA_hypothesis_start*      (first  (first *HPS_DNA_fitness_metrics_tuple*))) 
  				(setq *HPS_DNA_hypothesis_end*        (second (first *HPS_DNA_fitness_metrics_tuple*))) 
  				(setq *HPS_DNA_matching_percentage*   (third  (first *HPS_DNA_fitness_metrics_tuple*))) 
  				(setq *HPS_DNA_fitness_score*         (fourth (first *HPS_DNA_fitness_metrics_tuple*))) 

				(setq *HPS_DNA_segment_duration*      (- *HPS_DNA_hypothesis_end* 
									 *HPS_DNA_hypothesis_start*))
				(setq *HPS_DNA_normalized_score*      (/ *HPS_DNA_fitness_score* 
									 (if (< *HPS_DNA_matching_percentage* *HPS_DNA_MATCH_EPSILON* ) 
									   	*HPS_DNA_MATCH_EPSILON* 
										*HPS_DNA_matching_percentage* )))

				(format HPS-pattern-buffer "~&~{~12,3F  ~}	~12,3F	~12D	~18A" 
							(HPS_subseq (first *HPS_DNA_fitness_metrics_tuple*)        0 4) 
							*HPS_DNA_normalized_score* 
							*HPS_DNA_segment_duration*
							(first (HPS_subseq (first *HPS_DNA_fitness_metrics_tuple*) 4 5)))

				(when   (>= *HPS_DNA_matching_percentage* hps_matching_goal)
       					(push x *HPS_DNA_best_matching_patterns*)

					;; dump printout of the best matching patterns in one single file, for debugging 
					(when *HPSdebug*
						(HPS_DNA_print_mincost_patternmatch *HPS_DNA_matching_segments* hps_segtable_filename ))
					nil)

				(unless (>= *HPS_DNA_matching_percentage* hps_matching_goal) 
					nil))
		
	    		(unless x
	      			nil))

		(format output-stream "~A" (get-output-stream-string HPS-pattern-buffer))))

	;; sorted by default, because of the traversal implementation
	(setq *HPS_DNA_best_matching_patterns_sorted_by_time* 	(HPS_xcopy 	 (reverse   *HPS_DNA_best_matching_patterns*)))

	;; needs to be sorted in order of accuracy
	(setq *HPS_DNA_best_matching_patterns_sorted_by_rank* 	(HPS_DNA_sort_best_patterns *HPS_DNA_best_matching_patterns*))

	(return-from HPS_DNA_choose_best_patternmatch *HPS_DNA_best_matching_patterns_sorted_by_rank* ))
;; ***********************************************************************************************************************

  	
;; ***********************************************************************************************************************
;; this function setups the next segment in the test sequence for analysis. it places the focus of the current search 
;; into a short frame of interest extracted from the true DNA test sequence represented as 3DNA segments This function 
;; is INVOKED TO setup the next iteration of the allpairs approximate pattern match.
;; ***********************************************************************************************************************
(defun HPS_DNA_setup_next_iteration ( hps_testseq_segment 
				      hps_testsequence_segtable 
				      hps_1DNA_signature_size )

  	;; CHECK here we update the boundaries of the test pattern segtable to include a little more to the end and beginnning
	;; in order to avoid boundary problems
	(setq hps_1DNA_index_from   (- (HPS_DNA_get_segment_match_start hps_testseq_segment ) *HPS_DNA_SHIFT_AMOUNT*))	;; MINUS 32 CHECK
	(setq hps_1DNA_index_to     (+ *HPS_DNA_hypothesis_start* hps_1DNA_signature_size     *HPS_DNA_SHIFT_AMOUNT*))	;; PLUS 32 CHECK

	;; choose those segments which are within the hypothesis start interval
	;; (setq *HPS_DNA_test_sequence_remainder*
	      ;; (remove-if-not #'(lambda (x) (AND (HPS_DNA_is_between (first  x)  hps_1DNA_index_from  hps_1DNA_index_to) 
					        ;; (HPS_DNA_is_between (second x)  hps_1DNA_index_from  hps_1DNA_index_to))) 
			     ;; hps_testsequence_segtable ))

	(setq *HPS_DNA_test_sequence_remainder*
		(STENO_get_segment_table :SEG_TABLE 	hps_testsequence_segtable 
				 	 :FROMINDEX 	hps_1DNA_index_from
     			     	 	 :TOINDEX 	hps_1DNA_index_to))

	(return-from HPS_DNA_setup_next_iteration *HPS_DNA_test_sequence_remainder* ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; Initialization of one-time setup of global items
;; ***********************************************************************************************************************
(defun HPS_DNA_global_initialization ( ) 
	(setq *HPS_DNA_debug*           nil)	; debug-flag which prints at the termination of major routines.
	(setq *HPS_DNA_HIGHCOST_FLAG1*  99)
	(setq *HPS_DNA_HIGHCOST_FLAG0*  999999)
	(setq *HPS_DNA_LARGE_DIFF*      999999) ; "a large value used to force a matching tuple to be discarded")
	(setq *HPS_DNA_SMALL_DIFF*      64)  	; "define what is small enough; maximum tolerance for equating two HPS_DNA_ segments as similar")
	(setq *HPS_DNA_MATCH_EPSILON*   0.005)
	(setq *HPS_DNA_EPSILON* 	0.001))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_finepick_best_patternsmatches ( best_matching_patterns_sorted_by_time 
					       &key (HPS_TERSE nil))


	(when (NOT best_matching_patterns_sorted_by_time)  
	  	(return-from HPS_DNA_finepick_best_patternsmatches  nil))

	(setq patnum_local 0)
	(setq HPS_DNA_comparator_table 		nil)

	;; *********************************************************************************************************
	(dolist (p  best_matching_patterns_sorted_by_time  'FINEGRAINSORTING)
  		(setf *HPS_DNA_pattern_alignment_vector* nil)

	  	(when (NOT HPS_TERSE)
			(format t "~% M[~5A:~5A]	@(~3,2F): "   		 (HPS_DNA_get_pattern_match_start  p) 
										 (HPS_DNA_get_pattern_match_end    p) 
										 (HPS_DNA_get_pattern_match_rating p)))

		(push (list 'PRELIM-ALIGN (HPS_DNA_get_pattern_match_start p)) *HPS_DNA_pattern_alignment_vector*)

		(setq hps_alignment_dataset_local 	
		      				(HPS_DNA_finegrain_alignment 	  p 
									     	  patnum_local 
									     	  t 
									     	  :GENERATE_REPORT nil ))

		(setq hps_alignment_index_local	(HPS_get_alignment_dataset_field hps_alignment_dataset_local
										 :FIELD 'INDEX))

		(push (list 'ALIGN-OFFSET (- hps_alignment_index_local *HPS_DNA_SHIFT_AMOUNT*)) *HPS_DNA_pattern_alignment_vector*)

		(setq HPS_DNA_magnified_coded_comparison 
		      				(HPS_get_alignment_dataset_field hps_alignment_dataset_local 
										 :FIELD 'COMP_MAGNIFIED_CODES))

		(setq HPS_DNA_comparison_rating	(abs (HPS_1DNA_compute_magnified_differences 
						       HPS_1DNA_magnified_coded_comparison 
						       				 :COMPARATOR_TYPE 'BASECOUNT
										 :HPS_TERSE HPS_TERSE)))
						       				 
		;; CHECK, don't want HPS_Xcopy unless necessary
		(setq HPS_DNA_comparison_rating_tuple (list (HPS_xcopy HPS_DNA_comparison_rating)
						      	    (HPS_xcopy p) 
						      	    (HPS_xcopy (HPS_DNA_get_final_alignment))
						      	    (HPS_xcopy hps_alignment_index_local)
						      	    (HPS_xcopy (reverse *HPS_DNA_pattern_alignment_vector*))))

		(when (NOT HPS_TERSE)
			(pprint (reverse *HPS_DNA_pattern_alignment_vector*)))

		(push HPS_DNA_comparison_rating_tuple  
		      				HPS_DNA_comparator_table)

		(incf patnum_local))
	;; *********************************************************************************************************

	;; *********************************************************************************************************
	(setq HPS_DNA_comparator_table 		(reverse HPS_DNA_comparator_table))

	(setq *HPS_DNA_comparison_threshold* 	(HPS_1DNA_comparator_threshold HPS_DNA_comparator_table ))

	(when (NOT HPS_TERSE)
		(format t "~%PATTERN MINING THRESHOLD APPLIED WAS: [~A]" hps_comparator_threshold))
	;; *********************************************************************************************************

	;; *********************************************************************************************************
	(setq HPS_DNA_bestmatches_finegrained    (remove-if-not #'(lambda (a) 
						  	(HPS_DNA_is_between (first a) 0 *HPS_DNA_comparison_threshold*))
							 HPS_DNA_comparator_table ))

	(setq *HPS_DNA_bestmatches_finegrained_with_duplicates* 
	      					(mapcar #'(lambda (a) (second a)) HPS_DNA_bestmatches_finegrained))
	;; *********************************************************************************************************

	;; *********************************************************************************************************
	;; THIS SHOULD BE INSTEAD 2SIGMA LIMITS --- CHECK
	;; *********************************************************************************************************
	(let*  ((tail_events			(remove-if-not #'(lambda (a) 
						  	(HPS_DNA_is_between (first a)   *HPS_DNA_comparison_threshold* 
									    		(* *HPS_DNA_comparison_threshold* 1.50)))
							 HPS_DNA_comparator_table ))
		(short_tails 			(mapcar #'(lambda (a) (second a)) tail_events ))
		(num_events			(length short_tails))
		(tail_flag 			(if (> num_events 0) t nil)))
		(when tail_flag 		
		  (format t "~% [~A] STRONG TAIL EVENTS DETECTED          : [~S]" 
			  num_events (mapcar #'(lambda (a) (list (first  (first (first a))) 
					    (second (first (first a))) 
					    (format nil "~3,2F" (* (third  (first (first a))) 100)))) short_tails))))
	;; *********************************************************************************************************


	(when (NOT HPS_TERSE)
	  	(pprint (mapcar #'(lambda (a) (list (first  (first (first a))) 
					    (second (first (first a))) 
					    (format nil "~3,2F" (* (third  (first (first a))) 100))))
						*HPS_DNA_bestmatches_finegrained_with_duplicates*)))

	(setq *HPS_DNA_bestmatches_finegrained* (remove-duplicates *HPS_DNA_bestmatches_finegrained_with_duplicates*
								   :test #'HPS_DNA_determine_if_approx_unique_pattern))

	(when (NOT HPS_TERSE)
		(pprint (mapcar #'(lambda (a) (list (first  (first (first a))) 
					    (second (first (first a))) 
					    (format nil "~3,2F" (* (third  (first (first a))) 100))))
						*HPS_DNA_bestmatches_finegrained*)))

	(setq *HPS_DNA_best_matching_patterns_sorted_by_time* 	  (HPS_xcopy 	            *HPS_DNA_bestmatches_finegrained* ))

	(setq *HPS_DNA_best_matching_patterns_sorted_by_rank_old* (HPS_xcopy *HPS_DNA_best_matching_patterns_sorted_by_rank*  ))

	(setq *HPS_DNA_best_matching_patterns_sorted_by_rank* 	  (HPS_DNA_sort_best_patterns *HPS_DNA_bestmatches_finegrained* ))

	(return-from HPS_DNA_finepick_best_patternsmatches *HPS_DNA_best_matching_patterns_sorted_by_time* )) 
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_get_bestmatches_finegrained_for ( hps_dna_selection_threshold 
						 &key (FROM_MINIMUM_VALUE 0))

	(setq HPS_DNA_bestmatches_finegrained       (remove-if-not #'(lambda (a) (HPS_DNA_is_between (first a) 
											   	from_minimum_value 
											   	hps_dna_selection_threshold )) 
								   (HPS_DNA_get_comparator_table)))

	(setq *HPS_DNA_bestmatches_finegrained_xyz* (mapcar #'(lambda (a) (second a)) HPS_DNA_bestmatches_finegrained))

  	(return-from HPS_DNA_get_bestmatches_finegrained_for *HPS_DNA_bestmatches_finegrained_xyz* ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_get_next_bestmatches_finegrained_for ()
  	(setq *HPS_DNA_comparison_threshold_old* 	     (HPS_DNA_get_comparison_threshold))

  	(setq *HPS_DNA_comparison_threshold_new* 	(* 2 (HPS_DNA_get_comparison_threshold)))

	(setq *HPS_DNA_bestmatches_finegrained_xyz* 	(HPS_DNA_get_bestmatches_finegrained_for                
							  			     *HPS_DNA_comparison_threshold_new* 	
						 		:FROM_MINIMUM_VALUE  *HPS_DNA_comparison_threshold_old* ))

  	(return-from HPS_DNA_get_next_bestmatches_finegrained_for *HPS_DNA_bestmatches_finegrained_xyz* ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; THE HPS PATTERN MINER, it examines EVERY single possibility of a match but in terms of HPS segments, whose total number is 
;; determined by the fractality of the HPS approximation and the minimum threshold duration used in the stenographic decoder
;; The run time complexity cost of this exhaustive search is actually sub-linear wrt the size of the input sequence
;; ***********************************************************************************************************************
(defun HPS_DNA_pattern_match ( hps_full_segtable
			       hps_signature_segtable
			       hps_signature_size
			       hps_matching_goal 
			       &key (HPS_TERSE nil)) 

	(HPS_DNA_global_initialization ) 

	(setq *HPS_DNA_best_matching_patterns*  	      nil) 	;; the output of the approximate shape pattern matching 
									;; between a HPS_DNA_ signature and all its occurance in a test sequence
	(setq *HPS_DNA_all_patterns_found*		      nil) 	;; this table tracks the fitness of the best match 
									;; found at a given starting point if any


        ;; the hash table used to memoize cost metrics values for sig2seg pairs in the allpairs computation
	;; (setq *HPS_3DNA_segtable_hashtable*		      (make-hash-table 
								;; :test #'equal 
								;; :size 10 ))
								;; (length (HPS_DNA_get_actual_sequence :ENCODING_TYPE '1DNA_BASES))))

	;; a sequential pattern mining, finds ALL matches in the 3DNA HPS segtable domain
	(setq HPS_DNA_iteration_temp 0)
	(setq HPS_DNA_ITERATION_SKIP 2)			;; this forces skipping every nth segment consecutive segment, not very 
							;; good idea in general but increases speed significantly and the likelihood of loss
							;; is reduced by extending the limits of the interval being examined, most segments
							;; are redundantly examined and rarely the coarsegrain alignment is sensitive to precise start
	(dolist ( hps_testseq_segment  hps_full_segtable  'HPS_DNA_MAINLOOP ) 
	  (when (NOT (equal (mod HPS_DNA_iteration_temp HPS_DNA_ITERATION_SKIP ) 0)) 

		;; perform a sequential search over the 3DNA HPS segtable, computing distance vectors for across all possible combinations
		(HPS_DNA_iteration_initialization)

		;; setup the hypothesis start to the start index of the current segment of the remainder of the test sequence  
		(setq *HPS_DNA_hypothesis_start*  	     (HPS_DNA_get_segment_match_start hps_testseq_segment ))

		(setq *HPS_DNA_remainder_testseq_window*     (HPS_DNA_setup_next_iteration    hps_testseq_segment 
											      hps_full_segtable 
											      hps_signature_size ))

		;; compute all distance vectors
		(setq *HPS_3DNA_allpairs_within_window*      (HPS_DNA_compute_allpairs_costs  *HPS_DNA_hypothesis_start* 
											       hps_signature_segtable
											      *HPS_DNA_remainder_testseq_window* ))

		(setq *HPS_3DNA_allpairs_within_window*      (reverse *HPS_3DNA_allpairs_within_window* ))
	
		;; find the minimum cost distance vectors with respect to each matching segment
		(setq *HPS_DNA_matching_segments*  	     (HPS_DNA_mincost_patternmatch_at *HPS_DNA_hypothesis_start* 
										    	       hps_signature_segtable 
										              *HPS_3DNA_allpairs_within_window* ))

		(setq *HPS_DNA_matching_segments* 	     (reverse *HPS_DNA_matching_segments*))

		;; put together minimum cost matching segments in the 3DNA search space
		(when 	*HPS_DNA_matching_segments*
			(setq *HPS_DNA_fitness_metrics_tuple* (HPS_DNA_compute_matchfitness   *HPS_DNA_hypothesis_start* 
											      *HPS_DNA_matching_segments* ))

			(setq *HPS_DNA_temp_tuple*            (list 	(list *HPS_DNA_fitness_metrics_tuple* ) 
							      		(list *HPS_DNA_matching_segments* )))

			(push *HPS_DNA_temp_tuple* 	      *HPS_DNA_all_patterns_found* )

			(when (equal (mod HPS_DNA_iteration_temp 512) 511)
				(setq *HPS_DNA_all_patterns_found*    
				      			      (remove-duplicates *HPS_DNA_all_patterns_found*
								      :test #'HPS_DNA_predetermine_if_approx_unique_pattern))))

		(unless *HPS_DNA_matching_segments*
			nil))

	  (incf HPS_DNA_iteration_temp))

	(setq *HPS_DNA_all_patterns_found* 	   (reverse *HPS_DNA_all_patterns_found*))

	;; the output of the approximate HPS_DNA_ shape matching algorithm is written to the file, 
	(setq *HPS_DNA_match_behavior_filename*    (format nil "~A~A" *HPS_DNA_outputdir* "HPS_DNA_BEHAVIOR_PATTERN_DISCOVERY.DNA" ))
	(setq *HPS_DNA_segtables_filename* 	   (format nil "~A~A" *HPS_DNA_outputdir* "HPS_DNA_MATCHING_PATTERNS.DNA"))
	(with-open-file (output-stream *HPS_DNA_segtables_filename* :direction :output :if-exists :supersede :if-does-not-exist :create) 
	  	(format output-stream "" nil))

	(setq *HPS_DNA_bestmatches_coarsegrain_with_duplicates*    
	      				 	   (HPS_DNA_choose_best_patternmatch 	  *HPS_DNA_all_patterns_found* 
										      	  hps_matching_goal 
										     	  *HPS_DNA_match_behavior_filename* 
										     	  *HPS_DNA_segtables_filename* ))

	(setq *HPS_DNA_bestmatches_coarsegrain*	   (remove-duplicates *HPS_DNA_bestmatches_coarsegrain_with_duplicates*
								      :test #'HPS_DNA_determine_if_approx_unique_pattern))

	(setq *HPS_DNA_bestmatches_finegrained*    (HPS_DNA_finepick_best_patternsmatches *HPS_DNA_bestmatches_coarsegrain* 
											  :HPS_TERSE HPS_TERSE))

	(return-from HPS_DNA_pattern_match *HPS_DNA_bestmatches_finegrained*  ))
;; ***********************************************************************************************************************


