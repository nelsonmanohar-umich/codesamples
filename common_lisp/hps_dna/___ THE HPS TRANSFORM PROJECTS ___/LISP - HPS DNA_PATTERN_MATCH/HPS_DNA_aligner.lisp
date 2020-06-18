(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
(proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;; ********************************************************************************************
;;                           SEG TABLE SHARED OVERLAP ALIGNER
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
;; a number that determines how far right and left from the coarsegrain match, the algorithm looks to find a finegrain match
;; ***********************************************************************************************************************
(setq HPS_COMPARATOR_SIGMA_LEVELS     		3)	;; 1. true pattern matches once aligned have low scores that
							;;    are statistically significant different from scores from 
							;;    random non-matching instances. Sets the number of noise
							;;    sigma levels to filter out to candidates for (costly) 
							;;    finegrain alignment. Each sigma level increases/decreates
							;;    significantly the filtering along gaussian probabilities.
(setq HPS_DNA_MAXIMUM_DISPLACENT_FROM_START	91)	;; 1. determines how far from the reported coarsegrain start, the
							;;    final finegrain alignment for a matching instance could be
							;;    in order to be considered a faithful match to coarsegrain 
							;;    matching instance.
							;; 2. limits the number of consecutively skipped misaligned bases 
							;;    that are tolerated due to deletion/insertion resynching 
							;;    during finegrain alignement.
(setq *HPS_DNA_NUM_PAIREDBASES_REQD*		13)	;; 1. how many consecutive matched bases must be detected
							;;    during finegrain alignment of a coarsegrain matching 
							;;    instance in order to considered preliminary aligned.
							;; 2. controls chance alignment probability that a correctness 
							;;    burst exists out-of-place in the coarsegrain alignment; such
							;;    then handler by a renewed invocation of the finegrain alignment's
							;;    resyncher once mismatches re-accumulate.
							;; 3. danger that if a mutation happens to be periodically changing
							;;    more than every other 1 out of (this X) bases, coarsegrain 
							;;    matching instance may not properly align with signature
(setq HPS_DNA_MISMATCH_TOLERANCE_PERCENTAGE 	0.15) 	;; 1. USED WHEN OUTLIERS OUT, IN ORDER TO PREDETERMINE
							;;    WHAT IS CONSIDERED AN OUTLIERS AND COMPUTE LIMITS WITHOUT THOSE.
                                                	;; 2. triggers the call of the finegrain deletion/insertion 
							;;    handler for coarsegrain matching instances that SOMEHOW 
							;;    failed the preliminary finegrain alignment and thus likely 
							;;    containing deletions/insertions that must be sorted out
(setq *HPS_DNA_SHIFT_AMOUNT* 	   		61)	;; 1. USED TO DETERMINE DUPLICATE COARSEGRAIN PATTERNs
							;; 2. how many bases back and forth to look up for a finegrain resync
							;; 3. Limits the number of deletes/inserts that can be tolerated.
(setq *HPS_DNA_MAX_RESYNCS_LIMIT*		96)	;; 1. how many times should finegrain intra-alignment be done 
							;;    for any matching instance
							;; 2. limits the number of separate error bursts that can be corrected
							;; 3. prevents that random chance alignment bursts be inaccurately 
							;;    accumulated
(setq HPS_DNA_RESYNC_RESTART_OFFSET 		10)     ;; 1. determines the number of bases to skip (resulting in FORCED mismatches)
							;;    during a realingment resynch. If the number is small, more bases are
							;;    examined for a match, whereas too large forced large penalties for
							;;    small gaps. effectively limits the number of bursts by
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; these values are used to encode an XY dot plot of the alignment between the DNA signature and a possible pattern match
;; ***********************************************************************************************************************
(setq DNA_MAGNIFY_CODE_A			-16) 
(setq DNA_MAGNIFY_CODE_C			-8)
(setq DNA_MAGNIFY_CODE_G			 8) 
(setq DNA_MAGNIFY_CODE_T			 16)
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; these are computation accessor macros to maintain semantic consistency on the definition of the starts and ends of a 
;; coarse-grain pattern match and its differentiating meaning as a lookup hint point for the finegrain alignment
;; ***********************************************************************************************************************
(defun HPS_DNA_pattern_alignment_lookup_start ( p )
	(return-from HPS_DNA_pattern_alignment_lookup_start 	(max (- (HPS_DNA_get_pattern_match_start p) *HPS_DNA_SHIFT_AMOUNT* ) 
								     0)))

(defun HPS_DNA_pattern_alignment_lookup_end   ( p )
	(return-from HPS_DNA_pattern_alignment_lookup_end   	(min (+  (HPS_DNA_get_pattern_match_end  p) *HPS_DNA_SHIFT_AMOUNT* ) 
								     (length *HPS_1DNA_input_bases_temp*))))

;; CHECK FOR MIN AND MAX HERE BELOW
(defun HPS_DNA_pattern_alignment_start_for ( p hps_finegrain_alignment_offset ) 	
  	(return-from  HPS_DNA_pattern_alignment_start_for 	(+ (HPS_DNA_pattern_alignment_lookup_start  p) 
								    hps_finegrain_alignment_offset )))

(defun HPS_DNA_pattern_alignment_end_for   ( p hps_finegrain_alignment_offset ) 
  	(return-from  HPS_DNA_pattern_alignment_end_for 	(+ (HPS_DNA_pattern_alignment_lookup_start  p) 
								    hps_finegrain_alignment_offset
								   *HPS_1DNA_endpoint_signature_end*)))

(defun HPS_DNA_get_final_alignment ()
  	(return-from HPS_DNA_get_final_alignment *HPS_DNA_final_alignment_dataset*))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this does a fine grain alignment at the DNA base level, the two sequences are approximately aligned at a coarse level
;; so this, simply seeks the identify the exacting alignment by applying a simple start equivalency test with low probability
;; of error, to do this, it seeks to align the seven bases at a previously selected reliable common_index in the sequence
;; ***********************************************************************************************************************
(defun HPS_find_a_shared_subseq_between ( hps_signature_temp 
					  hps_testseq_remainder_temp 
					  &key  (HPS_SHIFT_OFFSET *HPS_DNA_SHIFT_AMOUNT*)
					  	(HPS_WHICH_OPERAT 'NEUTRO)
					  	(HPS_MATCH_LENGTH *HPS_DNA_NUM_PAIREDBASES_REQD*))

  	;; ***************************************************************************************************************
  	;; A note about relative indexes:
		;; The testseq_subseq_temp is shifted backward from the current position by X=HPS_DNA_SHIFT_AMOUNT (i.e., i-X) 
		;; meanwhile the signature is set to be at the current position i
  	;; ***************************************************************************************************************

  	;; ***************************************************************************************************************
  	;; boundary case - when the signature being matched has less than the number of leading bases needed to subsearch 
	;; in such case, return the original OFFSET so that it points to the true current index in the test sequence
  	;; ***************************************************************************************************************
  	(when (< (length hps_signature_temp) 		(* 2 HPS_MATCH_LENGTH) )
		(setq *HPS_alignment_success_flag* nil)
		(return-from HPS_find_a_shared_subseq_between HPS_SHIFT_OFFSET))

  	(when (< (length hps_testseq_remainder_temp ) 	(* 2 HPS_MATCH_LENGTH) )
		(setq *HPS_alignment_success_flag* nil)
		(return-from HPS_find_a_shared_subseq_between HPS_SHIFT_OFFSET))
  	;; ***************************************************************************************************************

  	;; ***************************************************************************************************************
  	;; boundary case - when either the signature or the test sequence are nil
	;; in such case, return the original OFFSET so that it points to the true current index in the test sequence
  	;; ***************************************************************************************************************
  	(when (OR (NOT hps_signature_temp)  
		  (NOT hps_testseq_remainder_temp ))
		(setq *HPS_alignment_success_flag* nil)
		(return-from HPS_find_a_shared_subseq_between HPS_SHIFT_OFFSET))
  	;; ***************************************************************************************************************

  	;; ***************************************************************************************************************
  	;; compute the finegrain alignment LEAD (the first *HPS_DNA_NUM_PAIREDBASES_REQD* bases) to find within hps_testseq_remainder_temp
  	;; ***************************************************************************************************************
	(setq *HPS_alignment_success_flag* nil)
	(cond 	;; ***************************************************************************************************************
		;; determine an alignment when bases from the signature are suspected to be deleted from the matching instance
  		;; ***************************************************************************************************************
	  	((equal HPS_WHICH_OPERAT 'DELETE)
		 	(let* ((hps_offset	HPS_DNA_RESYNC_RESTART_OFFSET)
			       (sig_len		(length hps_signature_temp))
			       (sig_index_from 	hps_offset)
			       (sig_index_to	(+ sig_index_from HPS_MATCH_LENGTH))
			       (hps_maxtimes 	(floor (/ (* HPS_DNA_MISMATCH_TOLERANCE_PERCENTAGE sig_len)  hps_offset)))
			       (hps_done	nil))

			  (do ((hps_iter	0))

			       ((equal hps_done t) t)

			       (setq sig_index_from 		        (min (* hps_iter hps_offset) 
									     (- sig_len HPS_MATCH_LENGTH)))
			       (setq sig_index_to		   	(+ sig_index_from HPS_MATCH_LENGTH))
			       (setq hps_signature_subseq_temp     	(subseq hps_signature_temp         sig_index_from sig_index_to ))
			       (setq hps_testseque_subseq_temp     	(subseq hps_testseq_remainder_temp 0 ))
			       (setq hps_position_within_signatu_subseq sig_index_from)
			       (setq hps_position_within_testseq_subseq (search hps_signature_subseq_temp hps_testseque_subseq_temp ))

			       (when hps_position_within_testseq_subseq 
					(setq *HPS_alignment_success_flag* t)
				 	(setq hps_done t))

			       (if (OR (> hps_iter hps_maxtimes) (> sig_index_to sig_len))
				 	(setq hps_done t)
			       		(incf hps_iter)))) 
			nil)
  		;; ***************************************************************************************************************

  		;; ***************************************************************************************************************
		;; determine an alignment when extra bases in the matching instance are present, skip them and seek to realign
  		;; ***************************************************************************************************************
	      	((equal HPS_WHICH_OPERAT 'INSERT)
		 	(let* ((hps_offset	HPS_DNA_RESYNC_RESTART_OFFSET)
			       (seg_len		(length hps_testseq_remainder_temp ))
			       (hps_maxtimes 	(floor (/ (* HPS_DNA_MISMATCH_TOLERANCE_PERCENTAGE seg_len)  hps_offset)))
			       (seg_index_from 	hps_offset)
			       (seg_index_to	(+ seg_index_from HPS_MATCH_LENGTH))
			       (hps_done	nil))

			  (do ((hps_iter	0))

			       ((equal hps_done t) t)

			       (setq seg_index_from 			(min (* hps_iter hps_offset) 
									     (- seg_len HPS_MATCH_LENGTH)))
			       (setq seg_index_to			(+ seg_index_from HPS_MATCH_LENGTH))
			       (setq hps_signature_subseq_temp     	(subseq hps_signature_temp         0 ))
			       (setq hps_testseque_subseq_temp     	(subseq hps_testseq_remainder_temp seg_index_from seg_index_to ))
			       (setq hps_position_within_signatu_subseq (search hps_testseque_subseq_temp hps_signature_subseq_temp ))
			       (setq hps_position_within_testseq_subseq seg_index_from)

			       (when hps_position_within_signatu_subseq
					(setq *HPS_alignment_success_flag* t)
				 	(setq hps_done t))

			       (if (OR (> hps_iter hps_maxtimes) (> seg_index_to seg_len))
				 	(setq hps_done t)
			       		(incf hps_iter)))) 
			nil)
  		;; ***************************************************************************************************************

  		;; ***************************************************************************************************************
		;; reset the alignment problem to a different common start point, (currently chance finding) should repeat as above, but not yet
  		;; ***************************************************************************************************************
	      	((equal HPS_WHICH_OPERAT 'NEUTRO)
			(setq hps_signature_subseq_temp     	 (subseq hps_signature_temp         0 (ceiling (/ HPS_MATCH_LENGTH 2))))
			(setq hps_testseque_subseq_temp     	 (subseq hps_testseq_remainder_temp 0 ))
			(setq hps_position_within_signatu_subseq 0)
			(setq hps_position_within_testseq_subseq (search hps_signature_subseq_temp  hps_testseque_subseq_temp ))

			(when hps_position_within_testseq_subseq
				(setq *HPS_alignment_success_flag* t))
			nil))
  	;; ***************************************************************************************************************

  	;; ***************************************************************************************************************
	;; if the finegrain alignment index is NIL, the signature and remainder test sequence CANNOT be aligned anymore
	;; in such case, return the original OFFSET so that it points to the true current index in the test sequence
  	;; ***************************************************************************************************************
	(when (NOT hps_position_within_testseq_subseq)
		(setq *HPS_alignment_success_flag* nil)
		(return-from HPS_find_a_shared_subseq_between HPS_SHIFT_OFFSET))

	(when (NOT hps_position_within_signatu_subseq)
		(setq *HPS_alignment_success_flag* nil)
		(return-from HPS_find_a_shared_subseq_between HPS_SHIFT_OFFSET))
  	;; ***************************************************************************************************************

  	;; ***************************************************************************************************************
	;; determine whether the finegrain alignment index for this matching instance seems TOOOOOO far from the its start 
	;; this way, the finegrain alignment index will not traverse very far away into the sequence -- CHECK this is troublesome
  	;; ***************************************************************************************************************
	(when (> hps_position_within_signatu_subseq HPS_DNA_MAXIMUM_DISPLACENT_FROM_START)
		(setq *HPS_alignment_success_flag* nil)
		(return-from HPS_find_a_shared_subseq_between HPS_SHIFT_OFFSET ))

	(when (> hps_position_within_testseq_subseq HPS_DNA_MAXIMUM_DISPLACENT_FROM_START)
		(setq *HPS_alignment_success_flag* nil)
		(return-from HPS_find_a_shared_subseq_between HPS_SHIFT_OFFSET ))
  	;; ***************************************************************************************************************

	(return-from HPS_find_a_shared_subseq_between hps_position_within_testseq_subseq ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function remaps the DNA basecode to a code more suitable for plain eye examination and discrimination of valid
;; properly aligned sequences along an in-house attempt to simulate or enhance an XY dot matrix DNA plot
;; ***********************************************************************************************************************
(defun HPS_DNA_remap_code ( basecode )
  	(cond 	((equal basecode 0) (setq hps_unbiased_code  DNA_MAGNIFY_CODE_A)) 
	  	((equal basecode 1) (setq hps_unbiased_code  DNA_MAGNIFY_CODE_C)) 
	  	((equal basecode 2) (setq hps_unbiased_code  DNA_MAGNIFY_CODE_G)) 
	  	((equal basecode 3) (setq hps_unbiased_code  DNA_MAGNIFY_CODE_T)))
	(return-from HPS_DNA_remap_code hps_unbiased_code))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; computes the true alignment index within the original test sequence (and not the actual input sequence) 
;; ***********************************************************************************************************************
(defun HPS_DNA_true_alignment_sig2pat ( p hps_finegrain_alignment_offset )
	(setq *HPS_1DNA_alignment_index_within_testseq* 	(max (- (HPS_DNA_pattern_alignment_start_for p 
													     hps_finegrain_alignment_offset ) 
								   	*HPS_1DNA_endpoint_signature_end* 
								   	-2) 
								     0))
	(return-from HPS_DNA_true_alignment_sig2pat *HPS_1DNA_alignment_index_within_testseq*))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; magnifies the difference between two magnified DNA base encodings 
;; ***********************************************************************************************************************
(defun HPS_1DNA_base_comparator_valuedist ( a_magnified b_magnified )
	(let ((hps_comparator_diffvalue 	(HPS_power (- a_magnified b_magnified ) 2)))
		(return-from HPS_1DNA_base_comparator_valuedist hps_comparator_diffvalue )))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function given two sequences it computes the paired differences sequence corresponding to their aligned comparison
;; this function returns a table which contains the comparison elements and elements that enable plotting an XY DNA dot plot.
;; ***********************************************************************************************************************
(defun HPS_1DNA_get_magnified_coded_comparison_for ( hps_signature_sequence hps_matching_pattern_sequence )
	(setq hps_dna_iter_xyz 0)

	(setq HPS_1DNA_magnified_coded_comparison 		(mapcar #'(lambda (a b) 
									    (list (HPS_DNA_base a) 
										  (HPS_DNA_base b) 
										  (+ hps_dna_iter_xyz 
										     (HPS_power (HPS_DNA_remap_code (HPS_DNA_base a)) 2)) 
										  (+ hps_dna_iter_xyz 
										     (HPS_power (HPS_DNA_remap_code (HPS_DNA_base b)) 2)) 
										  (incf hps_dna_iter_xyz ))) 
									hps_signature_sequence 
									hps_matching_pattern_sequence ))

	(return-from HPS_1DNA_get_magnified_coded_comparison_for HPS_1DNA_magnified_coded_comparison))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function determines if a coarsegrain comparison sequence represents a true finegrain alignment by determining the
;; number of basepairs that correspond in time and place between the signature and the coarsegrain pattern match. this 
;; function accounts for a limited number of deletions on either the signature or the pattern, so it is possible that the
;; resultant basepair count be low but a subsegment of the pattern match appears not to be corresponding to the signature
;; when in reality corresponds when time-shifted due to the deletion.
;; ***********************************************************************************************************************
(defun HPS_DNA_count_differences_when_deletions_present ( hps_comparison_sequence 
							  &key        (HPS_TERSE nil))
	;; ***************************************************************************************************************
	;; counters of match and mismatch bases, with global and local tracking outlooks
	;; ***************************************************************************************************************
  	(setq HPS_total_match_count          		0)
	(setq HPS_local_mismatch_count 			0)
  	(setq HPS_total_mismatch_count 			0)
	(setq HPS_DNA_num_resyncs 			0)

	;; ***************************************************************************************************************
	;; the sequences being compared
	;; ***************************************************************************************************************
	(setq hps_signature_local 	   		(mapcar #'(lambda (a) (first  a)) hps_comparison_sequence))
	(setq hps_testseq_local   	   		(mapcar #'(lambda (a) (second a)) hps_comparison_sequence))
	(setq hps_comparison_sequence_reset 		nil)    

	;; ***************************************************************************************************************
	;; the outputs of the function, in addition to the total mismatch counter 
	;; ***************************************************************************************************************
	(setq *HPS_DNA_final_alignment_dataset* 	nil) 
	(setq HPS_unmatch_remainder_seq       		nil)
	(setq HPS_position_index_in_signature       	0)

	;; ***************************************************************************************************************
	;; compare the bases in both sequence one by one, unless realignments are needed, then realign and continue 
	;; ***************************************************************************************************************
  	(dolist ( dna_comparison_tuple hps_comparison_sequence 'DELETIONHANDLER)
	  	;; handle redefinitions of the sequence, apparently unnecessary but CHECK
	  	(if hps_comparison_sequence_reset 
		  	(setq dna_comparison_tuple_in_use (pop hps_comparison_sequence_reset))
		  	(setq dna_comparison_tuple_in_use dna_comparison_tuple))

	  	(setq   hps_signature_local_base    	(first  dna_comparison_tuple_in_use))
	  	(setq   hps_testseq_local_base      	(second dna_comparison_tuple_in_use))

		;; *******************************************************************************************************
		;; when a match takes place
		;; *******************************************************************************************************
	  	(when   (equal hps_signature_local_base hps_testseq_local_base) 

		  	;; push the matched base pair into the memory of alignments
			(setq *HPS_DNA_final_alignment_dataset* (append *HPS_DNA_final_alignment_dataset* 
									(list (list hps_signature_local_base hps_testseq_local_base))))

		  	;; increment the count of observed DNA base pair matches
		  	(incf  HPS_total_match_count))


		;; *******************************************************************************************************
		;; when a mismatch takes place
		;; *******************************************************************************************************
	  	(unless (equal hps_signature_local_base hps_testseq_local_base) 

			;; push the mismatched base pair into the memory of the alignment
			(setq *HPS_DNA_final_alignment_dataset* (append *HPS_DNA_final_alignment_dataset* 
									(list (list hps_signature_local_base hps_testseq_local_base))))

			;; increment the mismatch count
			(incf HPS_local_mismatch_count )
			(incf HPS_total_mismatch_count ))


		;; *******************************************************************************************************
		;; determine if a realignment is needed and analyze the three different cases in realignment between two 
		;; sequences --- THIS CONTROLS HOW OFTEN TO TRIGGER EXPENSIVE REALIGNMENT
		;; *******************************************************************************************************
	  	(when (AND (NOT (equal hps_signature_local_base hps_testseq_local_base)) 
			   (> HPS_local_mismatch_count 1))			;; WAS 2 - CHECK

		  	;; track and limit the number of intra-sequence resyncs that are possible per pattern
		  	(incf HPS_DNA_num_resyncs)

			(when (NOT HPS_TERSE)
				(format t "	RESYNCH INTRA-ALIGNMENT NUM - [~3A]" HPS_DNA_num_resyncs))

			;; ************************************************************************************************
		  	;; adjust the outlook of the sequences being aligned to be corresponding to the local index in 
			;; signature through an ATTEMPT to align these sub-sequences with a short alignment LEAD
			;; ************************************************************************************************
	  		(setq hps_intra_alignment_index_local        0) 	
			(setq *HPS_alignment_success_flag* 	     nil)

			(when (NOT *HPS_alignment_success_flag*)
	  			(setq hps_intra_alignment_index_local 	
			      			  (HPS_find_a_shared_subseq_between (subseq HPS_signature_local HPS_position_index_in_signature)
			      							    (subseq HPS_testseq_local   HPS_position_index_in_signature)
					  					    :HPS_WHICH_OPERAT 'DELETE
										    :HPS_SHIFT_OFFSET 0)) 
				(if *HPS_alignment_success_flag*  (setq hps_alignment_success	'DELETE))
				nil)

			(when (NOT *HPS_alignment_success_flag*)
	  			(setq hps_intra_alignment_index_local 	
			      			  (HPS_find_a_shared_subseq_between (subseq HPS_signature_local HPS_position_index_in_signature)
			      							    (subseq HPS_testseq_local   HPS_position_index_in_signature)
					  					    :HPS_WHICH_OPERAT 'INSERT
										    :HPS_SHIFT_OFFSET 0)) 
				(if *HPS_alignment_success_flag*  (setq hps_alignment_success	'INSERT))
				nil)


			(when (NOT *HPS_alignment_success_flag*)
	  		        (setq hps_intra_alignment_index_local 	
			      			  (HPS_find_a_shared_subseq_between (subseq HPS_signature_local HPS_position_index_in_signature)
			      							    (subseq HPS_testseq_local   HPS_position_index_in_signature)
					  					    :HPS_WHICH_OPERAT 'NEUTRO
										    :HPS_SHIFT_OFFSET 0)) 
				(if *HPS_alignment_success_flag*  (setq hps_alignment_success	'NEUTRO))
				nil)
			;; ************************************************************************************************


			;; ************************************************************************************************
			;; when no alignment exist between the two subsequences -- if the value returned was the trigger being 
			;; passed, effectively pointing back to the genomic address that is known to have the mismatches
			;; ************************************************************************************************
			(when (NOT *HPS_alignment_success_flag*)
			  	;; increment the total mismatch count by the local count PLUS the rest of the sequence
			  	(incf HPS_total_mismatch_count  (OR (length (subseq HPS_signature_local HPS_position_index_in_signature)) 0))

				;; fill the remainder with the unmatched base pairs
				(setq HPS_unmatch_remainder_seq (mapcar #'(lambda (a b) (list a b))
			      			  				    (subseq HPS_signature_local HPS_position_index_in_signature)
			      			  				    (subseq HPS_testseq_local   HPS_position_index_in_signature)))

				;; break out of the loop and finish the counting process with the resulting mismatch count
				(return HPS_total_mismatch_count))
			;; ***************************************************************************************************************


			;; ***************************************************************************************************************
			;; when an alignment actually exists, then realign the sequences to continue with the alignment process
			;; reset the realigned sub-sequences to the actual alignment subsequences - shown for clarity
			;; ***************************************************************************************************************
			(when *HPS_alignment_success_flag*
				(setq HPS_local_mismatch_count 0)

				(when (NOT HPS_TERSE)
					(format t " 	OPERATION TYPE  [~A]" hps_alignment_success )
					(format t "	ALIGNMENT INDEX [~A]" hps_intra_alignment_index_local))

				(push (list 'RESYNC HPS_position_index_in_signature) *HPS_DNA_pattern_alignment_vector*)
					
				(push (list hps_alignment_success (max hps_position_within_signatu_subseq 
								       hps_position_within_testseq_subseq )) *HPS_DNA_pattern_alignment_vector*)


				(cond   ;; ***********************************************************************************************
					;; when the finegrain alignment index (i*) is greater than the initial offset X, the alignment falls 
					;; into FUTURE genomic addresses within the test sequence (i* > (i-X)+X). The genomic addresses 
					;; (i .. i*) are therefore without matching base counterparts within the HPS signature and 
					;; correspond to inserted bases within the test sequence
					;; ***********************************************************************************************
				  	((equal hps_alignment_success 'DELETE)			;; NAMES NOT LOGIC ARE REVERSED
						(let*  ((idx_fr     HPS_position_index_in_signature ) 
					  	   	(idx_to  (+ HPS_position_index_in_signature 
								    (max hps_position_within_signatu_subseq 
									 hps_position_within_testseq_subseq) ))
					           	(tlist  (subseq hps_testseq_local idx_fr 
									                  idx_to)) 
						   	(slist '(4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
							    	 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
							    	 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
							    	 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
							    	 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
							    	 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
							    	 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
							    	 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
							    	 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
							    	 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
							    	 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4))
					  	   	(mlist  (mapcar #'(lambda (a b) (list a b)) slist tlist)))

						   	(setq   *HPS_DNA_final_alignment_dataset* (append *HPS_DNA_final_alignment_dataset* mlist ))
							nil)

						(setq hps_signature_local      (subseq HPS_signature_local (+    hps_position_within_signatu_subseq
														 HPS_position_index_in_signature )))
						(setq hps_testseq_local        (subseq hps_testseq_local   (+    hps_position_within_testseq_subseq 
														 HPS_position_index_in_signature )))
			  			(incf HPS_total_mismatch_count (+ hps_position_within_signatu_subseq 
										  hps_position_within_testseq_subseq ))
						nil)
					;; ***********************************************************************************************


					;; ***********************************************************************************************
					;; when the finegrain alignment index (i*) is smaller than the initial offset X, the alignment falls 
					;; into PAST genomic addresses within the test sequence (i* < (i-X)+X). The genomic addresses 
					;; (i-X .. (i*<i)) are therefore without matching base counterparts within the HPS signature 
					;; (we knew that, as such misses got us here) but indeed these indicate that genomic addresses 
					;; (i-x .. i*) are inserted bases within the DNA signature and deleted from the test sequence
					;; ***********************************************************************************************
				  	((equal hps_alignment_success 'INSERT)
						(let*  ((idx_fr     HPS_position_index_in_signature ) 
					  	   	(idx_to  (+ HPS_position_index_in_signature 
								    (max hps_position_within_signatu_subseq 
									 hps_position_within_testseq_subseq) ))
						   	(slist  (subseq hps_signature_local idx_fr 
											    idx_to))
						   	(tlist '(5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
							    	 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
							    	 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
							    	 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
							    	 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
							    	 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
							    	 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
							    	 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
							    	 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
							    	 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5))
					  	   	(mlist  (mapcar #'(lambda (a b) (list a b)) slist tlist)))

						   	(setq *HPS_DNA_final_alignment_dataset* (append *HPS_DNA_final_alignment_dataset* mlist ))
							nil)

						(setq hps_signature_local      (subseq HPS_signature_local (+    hps_position_within_signatu_subseq
														 HPS_position_index_in_signature )))
						(setq hps_testseq_local        (subseq hps_testseq_local   (+    hps_position_within_testseq_subseq 
														 HPS_position_index_in_signature )))
			  			(incf HPS_total_mismatch_count (+ hps_position_within_signatu_subseq 
										  hps_position_within_testseq_subseq ))
						nil)
					;; ***********************************************************************************************


					;; ***********************************************************************************************
					;; if the alignment is lost at this point, attempt to realign with a smaller LEAD (HPS_MATCH_LEN)
					;; this is due when single point mutations are found within the larger LEAD (a chance probability)
					;; this attempts to uncover a match, but only once - CHECK could be done more, but it has an
					;; interaction effect with the other caller (in coarse grain alignment).
					;; ***********************************************************************************************
				  	((equal hps_alignment_success 'NEUTRO)
						(setq hps_signature_local      (subseq HPS_signature_local    HPS_position_index_in_signature ))
						(setq hps_testseq_local        (subseq hps_testseq_local   (+ HPS_position_index_in_signature 
													      hps_position_within_testseq_subseq)))
			  			(incf HPS_total_mismatch_count (+ 0 
										  hps_position_within_testseq_subseq ))
						nil))
					;; ***********************************************************************************************

				;; reset the position index to the start of the new subsequence	
				(setq HPS_position_index_in_signature 0)

				(when (NOT HPS_TERSE)
					(format t "~% 			N_SIGNATURE    [~A]" (HPS_subseq hps_signature_local 0 30))
					(format t "~% 			N_TESTSEQ      [~A]" (HPS_subseq hps_testseq_local   0 30)))

			  	;; construct the new comparison sequence and continue the counting process
				(setq hps_comparison_sequence_reset    (mapcar #'(lambda (a b) (list a b)) hps_signature_local 
									       			           hps_testseq_local)))
			;; ************************************************************************************************

			;; ************************************************************************************************
			;; when no alignment exist between the two subsequences -- if the value returned was the trigger being 
			;; passed, effectively pointing back to the genomic address that is known to have the mismatches
			;; ************************************************************************************************
			(when   (> HPS_DNA_num_resyncs *HPS_DNA_MAX_RESYNCS_LIMIT*)
			  	(pprint 'NUM_RESYNCS_EXCEEDED)

			  	;; increment the total mismatch count by the local count PLUS the rest of the sequence
			  	(incf HPS_total_mismatch_count  (length hps_signature_local)) 

				;; fill the remainder with the unmatched base pairs
				(setq hps_unmatch_remainder_seq (mapcar #'(lambda (a b) (list a b))     hps_signature_local 
													hps_testseq_local ))

				;; break out of the loop and finish the counting process with the resulting mismatch count
				(return HPS_total_mismatch_count ))
			;; ***************************************************************************************************************

			nil)

		(incf HPS_position_index_in_signature))

	(setq *HPS_DNA_final_alignment_dataset* 	(append *HPS_DNA_final_alignment_dataset* hps_unmatch_remainder_seq))
		  	
	(return-from HPS_DNA_count_differences_when_deletions_present HPS_total_mismatch_count ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; computes an in-house comparator distance value between two DNA subsequences - multiple entry point for mining and preselection 
;; ***********************************************************************************************************************
(defun HPS_1DNA_compute_magnified_differences ( HPS_1DNA_magnified_coded_comparison 
							&key (COMPARATOR_TYPE 'MAGNIFIED)
							     (HPS_TERSE        nil))

	;; ***********************************************************************************************************************
  	(cond   ((equal comparator_type 'MAGNIFIED)
			(setq HPS_1DNA_magnified_comparison_differences		
			      				 (mapcar #'(lambda (a) (HPS_1DNA_base_comparator_valuedist  
											(first  a) (second a))) 
								HPS_1DNA_magnified_coded_comparison ))
			(setq HPS_1DNA_comparator_value	 (reduce #'+ HPS_1DNA_magnified_comparison_differences )))
	;; ***********************************************************************************************************************


	;; ***********************************************************************************************************************
  	        ((equal comparator_type 'SAMPLING_BASECOUNT)
			(setq HPS_1DNA_magnified_comparison_sampling_points 
			      				 (mapcar #'(lambda (a) (random 2)) 
								HPS_1DNA_magnified_coded_comparison ))

			(setq HPS_1DNA_comparison_vector (mapcar #'(lambda (a b) (if (* b (equal (first  a) (second a))) 0 1))
								HPS_1DNA_magnified_coded_comparison 
								HPS_1DNA_magnified_comparison_sampling_points ))

			(setq HPS_1DNA_comparator_value	 (reduce #'+ HPS_1DNA_comparison_vector 	)))
	;; ***********************************************************************************************************************


	;; ***********************************************************************************************************************
  	        ((equal comparator_type 'DELETION_HANDLER)
			(setq HPS_1DNA_comparator_value	 (HPS_DNA_count_differences_when_deletions_present 
							   HPS_1DNA_magnified_coded_comparison)))

  	        ((AND T (equal comparator_type 'BASECOUNT))
			(setq HPS_1DNA_comparison_vector (mapcar #'(lambda (a) (if (equal (first  a) (second a)) 0 1))
								HPS_1DNA_magnified_coded_comparison ))

			(setq HPS_1DNA_comparator_value	 (reduce #'+ HPS_1DNA_comparison_vector ))))
	;; ***********************************************************************************************************************

	(when (NOT HPS_TERSE)
		(format t "~%	NORMAL_BASED_COMPARATOR	~A" HPS_1DNA_comparator_value))

	;; ***********************************************************************************************************************
	;; determine if expensive delete/insertion checks should be performed, do so only if BP mismatch count is sufficiently high
	;; even when the matching instance has already qualified - as it could contain a few deletions that are going unnoticed as
	;; be considered as an even better match than it is currently reported.
	;; ***********************************************************************************************************************
	(if (> HPS_1DNA_comparator_value (* (- *HPS_1DNA_endpoint_signature_end* *HPS_1DNA_endpoint_signature_start* ) 
					    0.75 HPS_DNA_MISMATCH_TOLERANCE_PERCENTAGE ))
		(setq HPS_1DNA_comparator_value	 (let ((HPS_temp_comparator_value (HPS_DNA_count_differences_when_deletions_present 
							   			   HPS_1DNA_magnified_coded_comparison
										   :HPS_TERSE HPS_TERSE )))
						       (if (< HPS_temp_comparator_value 
							      HPS_1DNA_comparator_value) 
							 HPS_temp_comparator_value
							 HPS_1DNA_comparator_value)))
	  	(setq *HPS_DNA_final_alignment_dataset* HPS_1DNA_magnified_coded_comparison ))
	;; ***********************************************************************************************************************

	(when (NOT HPS_TERSE)
		(format t "	FINAL_BASED_COMPARATOR	~A" HPS_1DNA_comparator_value))

	(return-from HPS_1DNA_compute_magnified_differences HPS_1DNA_comparator_value ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this function determines a threshold value wrt valid matching patterns exist, it works at extremely high sigma values
;; because differences are magnified quite significantly and sufficient patterns are chosen at a low threshold to generate
;; a noise background to determine from those, which are true matches
;; ***********************************************************************************************************************
(defun HPS_1DNA_comparator_threshold ( hps_comparator_table &key ( THRESHOLD_TYPE 'OUTLIERS_IN ))

	(setq HPS_COMPARATOR_OUTLIERS_THRESHOLD		(* (- *HPS_1DNA_endpoint_signature_end* 
							      *HPS_1DNA_endpoint_signature_start* ) 
							   HPS_DNA_MISMATCH_TOLERANCE_PERCENTAGE ))

	(cond ((equal threshold_type 'OUTLIERS_IN)
		(setq HPS_COMPARATOR_SIGMA_LEVELS     		HPS_COMPARATOR_SIGMA_LEVELS )

		(setq hps_matched_comparison_totals 	(mapcar #'(lambda (a) (abs (first a))) hps_comparator_table )) 
	
  		(setq hps_comparator_threshold_mean 	(HPS_mean   	hps_matched_comparison_totals ))
	
  		(setq hps_comparator_threshold_sigma   	(HPS_stddev 	hps_matched_comparison_totals 
							    		hps_comparator_threshold_mean ))

  		(setq hps_comparator_threshold 		(- hps_comparator_threshold_mean
						   	(* hps_comparator_threshold_sigma  HPS_COMPARATOR_SIGMA_LEVELS )))
		nil)


	      ((AND T (equal threshold_type 'OUTLIERS_OUT))
		(setq HPS_COMPARATOR_SIGMA_LEVELS     		(* 2 HPS_COMPARATOR_SIGMA_LEVELS))

		(setq hps_matched_comparison_totals_wo	(remove-if #'(lambda (a) 
							       	(HPS_DNA_is_between a 0 
										   HPS_COMPARATOR_OUTLIERS_THRESHOLD)) 
							   	hps_matched_comparison_totals))

		(setq hps_mct_wo_mean 			(HPS_mean 	hps_matched_comparison_totals_wo))
	
		(setq hps_mct_wo_sigma 			(HPS_stddev 	hps_matched_comparison_totals_wo 
							    		hps_mct_wo_mean))
  		(setq hps_comparator_threshold 		(- hps_mct_wo_mean
						   	(* hps_mct_wo_sigma HPS_COMPARATOR_SIGMA_LEVELS )))
		nil))
	
	;; final comparator value has to be within an acceptable range, as three sigma levels could force it to be negative
	(when 	(NOT (HPS_DNA_is_between hps_comparator_threshold  0  HPS_COMPARATOR_OUTLIERS_THRESHOLD ))
	  	(setq hps_comparator_threshold HPS_COMPARATOR_OUTLIERS_THRESHOLD ))

  	(return-from HPS_1DNA_comparator_threshold hps_comparator_threshold))
;; ***********************************************************************************************************************



;; **************************************************************************************************************
;; this function given a set of segtable ranges, determines whether the given timeindex belongs to one of them
;; **************************************************************************************************************
(defun HPS_DNA_valid_index_within_segtable ( timeindex segtable_rangeset )
	(dolist ( segment_interval segtable_rangeset 'HPS_RANGECHECK)
		(setq hps_valid_index 	(HPS_valid_iteration  timeindex 	
					      			:FROM 	(first  segment_interval) 
							    	:TO   	(second segment_interval)))

		(when   hps_valid_index
	  		(return-from HPS_DNA_valid_index_within_segtable segment_interval))

		(unless hps_valid_index
		 	 nil))

	(return-from HPS_DNA_valid_index_within_segtable nil))
;; **************************************************************************************************************


;; **************************************************************************************************************
;; **************************************************************************************************************
(defun HPS_DNA_dot_product ( hps_pat_segtable hps_sig_segtable )
	(setq *HPS_DNA_dot_product_timeseries* nil)

	(setq hps_sig_rangeset   	hps_sig_segtable )
	(setq hps_pat_rangeset   	hps_pat_segtable )

	(setq hps_pat_start		(HPS_DNA_get_segment_match_start (first hps_pat_segtable)))
	(setq hps_realtime_index 	(+ hps_pat_start 1))

  	(setq hps_size_signature 	(- *HPS_1DNA_endpoint_signature_end* *HPS_1DNA_endpoint_signature_start*))

  	(dotimes (j hps_size_signature 'DOT_PRODUCT_TS_BUILDER)
		(setq hps_sig_seg 	(HPS_DNA_valid_index_within_segtable j hps_sig_rangeset ))
		(setq hps_pat_seg 	(HPS_DNA_valid_index_within_segtable (+ j hps_pat_start) hps_pat_rangeset ))
		(setq hps_intersection 	(AND hps_sig_seg hps_pat_seg))

	  	(when   hps_intersection 
		 	(push (list j 	 hps_realtime_index 
				        (HPS_DNA_get_segment_match_tval hps_sig_seg)
				      	(HPS_DNA_get_segment_match_tval hps_pat_seg))
			      		*HPS_DNA_dot_product_timeseries*))
	  	(unless hps_intersection
		 	(push (list j 	 hps_realtime_index  
				    	 0 
				    	 0) 
					*HPS_DNA_dot_product_timeseries*))

		(incf hps_realtime_index))

	(return-from HPS_DNA_dot_product (reverse *HPS_DNA_dot_product_timeseries*)))
;; **************************************************************************************************************


;; **************************************************************************************************************
;; this function generates output that can be read by an spreadsheet to generate a similarity plot between
;; HPS_DNA_ signature and the uncovered matching patterns to the HPS_DNA_ signature. Each series needs a name.
;; **************************************************************************************************************
(defun HPS_DNA_coarsegrain_alignment ( best_matching_patterns_by_time )
	(setq *HPS_DNA_pnum* 0)
	(dolist (p best_matching_patterns_by_time 'DOTPRODUCTS)
	  	(setq hps_pat_segtable (STENO_get_segment_table :SEG_table 	(STENO_get_segment_table) 
				      		 		:fromindex 	(HPS_DNA_get_pattern_match_start p) 
     			     	      		 		:toindex 	(HPS_DNA_get_pattern_match_end   p)))

	  	(setq hps_sig_segtable (STENO_get_segment_table :SEG_table 	(STENO_get_segment_table)
				      		 		:fromindex 	*HPS_1DNA_endpoint_signature_start* 
     			     	      		 		:toindex 	*HPS_1DNA_endpoint_signature_end*))

		(setq *HPS_DNA_xref_coarse_tracker_timeseries* (HPS_DNA_dot_product hps_pat_segtable hps_sig_segtable ))

		(setq *HPS_subseq_filename* (format nil "~A~A_~D.DNA" 
						    	*HPS_DNA_outputdir* 
						    	"HPS_DNA_SEQUENCE_SIG2PAT_OVERLAPS_PLOT" 
							*HPS_DNA_pnum* )) 

		(HPS_timeseries_printer 	*HPS_DNA_xref_coarse_tracker_timeseries* 
							*HPS_subseq_filename*
							:xy_headerline "i	sig-j	pat-j	sig_tval	seg_tval")

		(incf *HPS_DNA_pnum*)))
;; **************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_finegrain_alignment ( p patnum bound-stream  &key ( GENERATE_REPORT t ))
 	(setq *HPS_1DNA_input_bases_temp* 	(HPS_DNA_get_input_dataset :seriestype '1DNABASES))

	(setq *HPS_1DNA_input_bases_signature* 	(subseq *HPS_1DNA_input_bases_temp* 0
						       			*HPS_1DNA_endpoint_signature_end* ))

	(setq *HPS_1DNA_input_bases_testsubseq* (subseq *HPS_1DNA_input_bases_temp* 
						    			(HPS_DNA_pattern_alignment_lookup_start p) 
						    			(HPS_DNA_pattern_alignment_lookup_end   p))) 

	(setq HPS_DNA_finegrain_align_index 	(HPS_find_a_shared_subseq_between 
									*HPS_1DNA_input_bases_signature*  
						  		        *HPS_1DNA_input_bases_testsubseq* ))

	(setq hps_sigseq_temp 			*HPS_1DNA_input_bases_signature* )
	(setq hps_subseq_temp 			(subseq *HPS_1DNA_input_bases_testsubseq*    (+ HPS_DNA_finegrain_align_index  0)))  

	(setq *HPS_1DNA_matching_bases_comparison_temp* (mapcar #'(lambda (a b) (list a b))  hps_sigseq_temp 
											     hps_subseq_temp ))

	(setq *HPS_1DNA_matching_coded_comparison_temp* (HPS_1DNA_get_magnified_coded_comparison_for hps_sigseq_temp   
												     hps_subseq_temp ))

	;; ***************************************************************************************************************
	;; report generator for normal comparison and adjusted comparison if different
	;; ***************************************************************************************************************
	(when GENERATE_REPORT
		(HPS_DNA_finegrain_alignment_report 	 p 
						 	 patnum 
						 	 HPS_DNA_finegrain_align_index 
							*HPS_1DNA_matching_bases_comparison_temp*
							*HPS_1DNA_matching_coded_comparison_temp*
							 bound-stream )

		(let* ((HPS_DNA_match_with_adjustments 	(third (first (remove-if-not #'(lambda (a) (equal p (second a))) 
										     HPS_DNA_comparator_table ))))
		       (HPS_DNA_match_with_adjustments  (mapcar #'(lambda (a) (list (HPS_DNA_maptobase (first a)) 
										     (HPS_DNA_maptobase (second a))))
								HPS_DNA_match_with_adjustments)))
			
		       (if (NOT (equal HPS_DNA_match_with_adjustments *HPS_1DNA_matching_bases_comparison_temp*))
		           (HPS_DNA_finegrain_alignment_report 
			     				 p 
						 	 patnum 
						 	 HPS_DNA_finegrain_align_index 
							 HPS_DNA_match_with_adjustments 
							*HPS_1DNA_matching_coded_comparison_temp*
							 bound-stream
							 :ADJUSTED_SEQUENCE_FLAG t ))


			   (HPS_timeseries_printer 	(HPS_1DNA_get_magnified_coded_comparison_for 
							  	(mapcar #'(lambda (a) (first  a)) HPS_DNA_match_with_adjustments)
							  	(mapcar #'(lambda (a) (second a)) HPS_DNA_match_with_adjustments))
	                   				(format nil "~A~A~D.DNA" 
								*HPS_DNA_outputdir* 
								"HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M" 
								patnum )
							:TRUE_INDEXING 
								(+ (HPS_DNA_pattern_alignment_lookup_start p ) HPS_DNA_finegrain_align_index)
							:XY_HEADERLINE 	
							"i	sig-base	pat-base	sig_idx_base	seg_idx_base	rel_index	true_indx")

		       nil)

		nil)
	;; ***************************************************************************************************************

	;; ***************************************************************************************************************
	(setq HPS_DNA_alignment_dataset 	 	(list HPS_DNA_finegrain_align_index 
							      p 
							      patnum 
							      *HPS_1DNA_matching_bases_comparison_temp* 
							      *HPS_1DNA_matching_coded_comparison_temp*))
	;; ***************************************************************************************************************

	(return-from HPS_DNA_finegrain_alignment HPS_DNA_alignment_dataset ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_get_alignment_dataset_field ( alignment_dataset &key (field 'INDEX))
  	(cond 	((equal field 'INDEX)
			(return-from HPS_get_alignment_dataset_field (first  alignment_dataset)))

	  	((equal field 'PATTERN)
			(return-from HPS_get_alignment_dataset_field (second alignment_dataset)))

	  	((equal field 'PATNUM)
			(return-from HPS_get_alignment_dataset_field (third  alignment_dataset)))

	  	((equal field 'COMP_BASES)
			(return-from HPS_get_alignment_dataset_field (fourth alignment_dataset)))

	  	((equal field 'COMP_MAGNIFIED_CODES)
			(return-from HPS_get_alignment_dataset_field (fifth  alignment_dataset)))

	  	(T
			(return-from HPS_get_alignment_dataset_field (first  alignment_dataset)))))
;; ***********************************************************************************************************************



