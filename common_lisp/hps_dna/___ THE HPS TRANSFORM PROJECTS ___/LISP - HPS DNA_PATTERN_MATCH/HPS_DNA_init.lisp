;; ********************************************************************************************
;;                                     HPS DNA INITIALIZATION
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
;; these macro gets various relevant fields such as match rating from a known-to-be valid pattern
;; ***********************************************************************************************************************
(defmacro HPS_DNA_get_pattern_match_rating     ( pattern )
	`(third  (first (first ,pattern)))) 

(defmacro HPS_DNA_get_pattern_match_start      ( pattern )
	`(first  (first (first ,pattern)))) 

(defmacro HPS_DNA_get_pattern_match_end        ( pattern )
	`(second (first (first ,pattern)))) 

(defmacro HPS_DNA_get_pattern_match_span       ( pattern )
	`(+ (- (first  (first (first ,pattern)))) 
	       (second (first (first ,pattern))) 
	       1))

(defmacro HPS_DNA_get_pattern_match_costweigth ( pattern )
	`(fourth (first (first ,pattern)))) 

(defmacro HPS_DNA_get_pattern_match_costvector ( pattern )
	`(fifth  (first (first ,pattern))))

(defun HPS_DNA_get_pattern_match_normcost   ( pattern )
  	(let* 	((costweigth   (HPS_DNA_get_pattern_match_costweigth          pattern))
  		 (matchquality (HPS_DNA_get_pattern_mappings_matching_quality pattern))
	         (costweigth   (if (< costweigth   *HPS_DNA_EPSILON*) (/ 1 *HPS_DNA_EPSILON*) costweigth))
		 (matchquality (if (< matchquality *HPS_DNA_EPSILON*)      *HPS_DNA_EPSILON*  matchquality )))
		(return-from HPS_DNA_get_pattern_match_normcost  (/ costweigth matchquality))))

(defmacro HPS_DNA_get_pattern_mapping_set      ( pattern )
	`(first (second ,pattern)))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(defmacro HPS_DNA_get_pattern_mapping_element  		    ( mapping-set mapnumber )
	`(nth ,mapnumber mapping-set ))

(defmacro HPS_DNA_get_pattern_mapping_element_from_pattern  ( pattern mapnumber )
	`(nth ,mapnumber (first (second ,pattern))))

(defmacro HPS_DNA_get_pattern_mappings_signature_segment    ( mapping-element )
	`(first ,mapping-element ))

(defmacro HPS_DNA_get_pattern_mappings_matching_segment     ( mapping-element )
	`(first (second ,mapping-element )))

(defmacro HPS_DNA_get_pattern_mappings_cost_weigth          ( mapping-element )
  	`(second (second ,mapping-element)))

(defmacro HPS_DNA_get_pattern_mappings_matching_quality     ( mapping-element )
	`(third ,mapping-element ))

(defmacro HPS_DNA_get_pattern_mappings_targeting_value      ( mapping-element )
  	`(fourth (first (second ,mapping-element ))))

(defmacro HPS_DNA_get_pattern_mappings_lambda_value         ( mapping-element )
  	`(third (first (second ,mapping-element ))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(defmacro HPS_DNA_get_segment_match_start (seg_elem)
  	`(first ,seg_elem))

(defmacro HPS_DNA_get_segment_match_end (seg_elem)
  	`(second ,seg_elem))

(defmacro HPS_DNA_get_segment_match_sdur (seg_elem)
  	`(third ,seg_elem))

(defmacro HPS_DNA_get_segment_match_tval (seg_elem)
  	`(fourth ,seg_elem))
;; ***********************************************************************************************************************


;; **************************************************************************************************************************
;; **************************************************************************************************************************
(defun HPS_DNA_load_1DNA_signature    ( hps_inputdir hps_1DNA_bases_fromfile hps_1DNA_coded_tofile hps_3DNA_coded_tofile )
	;; apply (trade-secret) specialized conditioning into the sequence of DNA bases, 
	;; to obtain a well conditioned series to be processed by the HPS transform
	(setf *HPS_1DNA_signature_input_dataset_xyz* 
	      				 (HPS_DNA_reader 		hps_inputdir 
									hps_1DNA_bases_fromfile 
									hps_1DNA_coded_tofile 
									hps_3DNA_coded_tofile ))

  	(setf *HPS_1DNA_signature_bases* (HPS_DNA_get_input_dataset 	:hps_input_dataset *HPS_1DNA_signature_input_dataset_xyz* 
				   					:seriestype '1DNABASES ))

  	(setf *HPS_1DNA_signature_coded* (HPS_DNA_get_input_dataset 	:hps_input_dataset *HPS_1DNA_signature_input_dataset_xyz* 
				   					:seriestype '1DNACODED ))

	(return-from HPS_DNA_load_1DNA_signature *HPS_1DNA_signature_input_dataset_xyz* ))
;; **************************************************************************************************************************


;; **************************************************************************************************************************
;; **************************************************************************************************************************
(defun HPS_DNA_load_1DNA_testsequence ( hps_inputdir hps_1DNA_bases_fromfile hps_1DNA_coded_tofile hps_3DNA_coded_tofile )
	;; apply (trade-secret) specialized conditioning into the sequence of DNA bases, 
	;; to obtain a well conditioned series to be processed by the HPS transform
	(setf *HPS_1DNA_testsequence_input_dataset_xyz* 
	      				    (HPS_DNA_reader 		hps_inputdir 
									hps_1DNA_bases_fromfile 
									hps_1DNA_coded_tofile 
									hps_3DNA_coded_tofile ))

  	(setf *HPS_1DNA_testsequence_bases* (HPS_DNA_get_input_dataset 	:hps_input_dataset *HPS_1DNA_testsequence_input_dataset_xyz* 
				   					:seriestype '1DNABASES ))

  	(setf *HPS_1DNA_testsequence_coded* (HPS_DNA_get_input_dataset 	:hps_input_dataset *HPS_1DNA_testsequence_input_dataset_xyz* 
				   					:seriestype '1DNACODED ))

	(return-from HPS_DNA_load_1DNA_testsequence *HPS_1DNA_testsequence_input_dataset_xyz* ))
;; **************************************************************************************************************************


;; **************************************************************************************************************************
;; **************************************************************************************************************************
(defun HPS_DNA_get_stabilizer_filler ()
	(setf *HPS_1DNA_series_stabilizer_filler* '( )))
;; **************************************************************************************************************************


;; **************************************************************************************************************************
;; **************************************************************************************************************************
(defun HPS_DNA_prepend_signature_to_testseq ( hps_signature hps_testsequence )
  	(setf *HPS_1DNA_full_input_sequence_xyz* (append (HPS_DNA_get_stabilizer_filler) 
							  hps_signature 
							  hps_testsequence ))

	;; displacements and indexings into the actual HPS DNA test sequence
	(setf *HPS_1DNA_filler_bases_len*        (length (HPS_DNA_get_stabilizer_filler)))
	(setf *HPS_1DNA_signature_bases_len*     (length hps_signature ))

	(setf *HPS_index_signature_start*        *HPS_1DNA_filler_bases_len*)
	(setf *HPS_index_signature_end*          (+ *HPS_1DNA_signature_bases_len* *HPS_1DNA_filler_bases_len* ))
	(setf *HPS_index_testsequence_start*     *HPS_index_signature_end* )
	(setf *HPS_index_testsequence_end*       (length *HPS_1DNA_full_input_sequence_xyz* ))

	(return-from HPS_DNA_prepend_signature_to_testseq *HPS_1DNA_full_input_sequence_xyz* ))
;; **************************************************************************************************************************


;; **************************************************************************************************************************
;; The HPS_DNA_ signature is embedded within the HPS test sequence. This code simply extracts the HPS_DNA_ signature from it. 
;; (setf *HPS_DNA_pivot_timeindex*   	  (setf *HPS_DNA_hypothesis_start*   	   hps_1DNA_index_to))
;; **************************************************************************************************************************
(defun HPS_DNA_define_hps_signature 	( hps_segment_table 
					  &key 	(hps_1DNA_index_from 0 ) 
					  	(hps_1DNA_index_to   1100) )

	;; the DNA signature in 3DNA segments --- CHECK whether this was broken by changes
	(setf *HPS_3DNA_signature_coded*  (remove-if-not #'(lambda (x) 
						(AND (HPS_DNA_is_between (first  x)  hps_1DNA_index_from  hps_1DNA_index_to) 
						     (HPS_DNA_is_between (second x)  hps_1DNA_index_from  hps_1DNA_index_to))) 
							 hps_segment_table ))

	;; important record keeping to be accessed through accessor functions
	(setf *HPS_DNA_signature_last_tuple*      (first (last *HPS_3DNA_signature_coded*)))
	(setf *HPS_DNA_signature_first_tuple*     (first       *HPS_3DNA_signature_coded*))
	(setf *HPS_DNA_endpoint_signature_end* 	  (HPS_DNA_get_segment_match_end    *HPS_DNA_signature_last_tuple* ))
	(setf *HPS_DNA_endpoint_signature_start*  (HPS_DNA_get_segment_match_start  *HPS_DNA_signature_first_tuple*))
	(setf *HPS_DNA_endpoint_signature_startp* 0)

	(setf *HPS_DNA_signature_size* 	  	  (- *HPS_DNA_endpoint_signature_end*  
						     *HPS_DNA_endpoint_signature_start*))
	(setf *HPS_DNA_endpoints_signature* 	  (list *HPS_DNA_endpoint_signature_start* 
							*HPS_DNA_endpoint_signature_end* ))
	(setf *HPS_DNA_signature_dataset_xyz*     (list *HPS_DNA_signature_size* 
							*HPS_DNA_endpoints_signature* 
							*HPS_3DNA_signature_coded*))

	(return-from HPS_DNA_define_hps_signature *HPS_DNA_signature_dataset_xyz*))
;; **************************************************************************************************************************


;; **************************************************************************************************************************
;; The HPS_DNA_ signature is currently embedded within the test sequence. This code simply extracts the HPS_DNA_ signature from it. 
;; (setf *HPS_DNA_hypothesis_start* 0) 			;; this should later be set to *HPS_DNA_signature_size*
;; **************************************************************************************************************************
(defun HPS_DNA_define_hps_testsequence ( hps_segment_table &key (hps_1DNA_index_from *HPS_DNA_signature_size* ))
	;; the DNA test sequence in 3DNA segments, the test sequence contains the signature prepended into it 
	(setf hps_1DNA_index_from 		  (+ *HPS_DNA_signature_size* 1))
	(setf hps_1DNA_index_to 		  (first (first (last hps_segment_table))))
	(setf *HPS_3DNA_testsequence_coded*  	  (remove-if-not #'(lambda (x) 
							(AND (HPS_DNA_is_between (first  x)  hps_1DNA_index_from  hps_1DNA_index_to) 
							     (HPS_DNA_is_between (second x)  hps_1DNA_index_from  hps_1DNA_index_to)))
							     	hps_segment_table ))

	;; important record keeping to be accessed through accessor functions
	(setf *HPS_DNA_testsequence_last_tuple*      (first (last *HPS_3DNA_testsequence_coded*)))
	(setf *HPS_DNA_testsequence_first_tuple*     (first       *HPS_3DNA_testsequence_coded*))
	(setf *HPS_DNA_endpoint_testsequence_end*    (HPS_DNA_get_segment_match_end    *HPS_DNA_testsequence_last_tuple* ))
	(setf *HPS_DNA_endpoint_testsequence_start*  (HPS_DNA_get_segment_match_start  *HPS_DNA_testsequence_first_tuple*))
	(setf *HPS_DNA_endpoint_testsequence_startp* (+ *HPS_DNA_endpoint_signature_end* 1))

	(setf *HPS_DNA_testsequence_size* 	     (- *HPS_DNA_endpoint_testsequence_end*  
							*HPS_DNA_endpoint_testsequence_start*))
	(setf *HPS_DNA_endpoints_testsequence* 	     (list *HPS_DNA_endpoint_testsequence_start* 
							   *HPS_DNA_endpoint_testsequence_end* ))
	(setf *HPS_DNA_testsequence_dataset_xyz*     (list *HPS_DNA_testsequence_size* 
							   *HPS_DNA_endpoints_testsequence* 
							   *HPS_3DNA_testsequence_coded*))

	(return-from HPS_DNA_define_hps_testsequence *HPS_DNA_testsequence_dataset_xyz* ))
;; **************************************************************************************************************************


;; ***********************************************************************************************************************
(defun HPS_DNA_get_signature ( &key (ENCODING_TYPE '3DNA_CODED) )
  	(cond 	((equal ENCODING_TYPE '3DNA_CODED)
  			(return-from HPS_DNA_get_signature     *HPS_3DNA_signature_coded*))
  	      	((equal ENCODING_TYPE '3DNA_BASES)
  			(return-from HPS_DNA_get_signature     *HPS_3DNA_signature_bases*))
  	      	((equal ENCODING_TYPE '1DNA_CODED)
  			(return-from HPS_DNA_get_signature     *HPS_1DNA_signature_coded*))
  	      	((equal ENCODING_TYPE '1DNA_BASES)
  			(return-from HPS_DNA_get_signature     *HPS_1DNA_signature_bases*))
  	      	(T
  			(return-from HPS_DNA_get_signature     *HPS_3DNA_signature_coded*))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(defun HPS_DNA_get_test_sequence ( &key (ENCODING_TYPE '3DNA_CODED) )
  	(cond 	((equal ENCODING_TYPE '3DNA_CODED)
  			(return-from HPS_DNA_get_test_sequence *HPS_3DNA_testsequence_coded*))
  	      	((equal ENCODING_TYPE '3DNA_BASES)
  			(return-from HPS_DNA_get_test_sequence *HPS_3DNA_testsequence_bases*))
  	      	((equal ENCODING_TYPE '1DNA_CODED)
  			(return-from HPS_DNA_get_test_sequence *HPS_1DNA_testsequence_coded*))
  	      	((equal ENCODING_TYPE '1DNA_BASES)
  			(return-from HPS_DNA_get_test_sequence *HPS_1DNA_testsequence_bases*))
  	      	(T
  			(return-from HPS_DNA_get_test_sequence *HPS_3DNA_testsequence_coded*))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(defun HPS_DNA_get_actual_sequence ( &key (ENCODING_TYPE '3DNA_CODED) )
  	(cond 	((equal ENCODING_TYPE '3DNA_CODED)
  			(return-from HPS_DNA_get_actual_sequence (append *HPS_3DNA_signature_coded* *HPS_3DNA_testsequence_coded*)))
  	      	((equal ENCODING_TYPE '3DNA_BASES)
  			(return-from HPS_DNA_get_actual_sequence (append *HPS_3DNA_signature_bases* *HPS_3DNA_testsequence_bases*)))
  	      	((equal ENCODING_TYPE '1DNA_CODED)
  			(return-from HPS_DNA_get_actual_sequence (append *HPS_1DNA_signature_coded* *HPS_1DNA_testsequence_coded*)))
  	      	((equal ENCODING_TYPE '1DNA_BASES)
  			(return-from HPS_DNA_get_actual_sequence (append *HPS_1DNA_signature_bases* *HPS_1DNA_testsequence_bases*)))
  	      	(T
  			(return-from HPS_DNA_get_actual_sequence (append *HPS_3DNA_signature_coded* *HPS_3DNA_testsequence_coded*)))))
;; ***********************************************************************************************************************




