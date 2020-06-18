(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; ********************************************************************************************
;;                           INVOCATION EXAMPLE OF THE DNA PATTERN MINER
;; ********************************************************************************************
;; this illustrates how to use the HPS DNA PATTERN MATCHER by creating a sample data mining 
;; application for finding all the matches of a DNA sequence (referred to as the signature)
;; within another DNA sequence  (referred to as the test sequence).
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


;; ********************************************************************************************
;; ********************************************************************************************
(defvar *HPS_DNA_mapalphabet* 	'( (a a a) (a a c) (a a g) (a a t) (a c a) (a c c) (a c g) (a c t) (a g a) (a g c) (a g g) (a g t) (a t a) (a t c) (a t g) (a t t) (c a a) (c a c) (c a g) (c a t) (c c a) (c c c) (c c g) (c c t) (c g a) (c g c) (c g g) (c g t) (c t a) (c t c) (c t g) (c t t) (g a a) (g a c) (g a g) (g a t) (g c a) (g c c) (g c g) (g c t) (g g a) (g g c) (g g g) (g g t) (g t a) (g t c) (g t g) (g t t) (t a a) (t a c) (t a g) (t a t) (t c a) (t c c) (t c g) (t c t) (t g a) (t g c) (t g g) (t g t) (t t a) (t t c) (t t g) (t t t) ) "the set of alphabet elements")
;; ********************************************************************************************


;; ********************************************************************************************
;; ********************************************************************************************
(defvar *HPS_DNA_mapcode* 	'( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 ) "coding value for each corresponding alphabet element")
;; ********************************************************************************************


;; **********************************************************************************************************
;; this function verifies that a produced pattern match is not trivial. a pattern match set is trivial if 
;; it ONLY contains instances of the signature which was embedded within the actual input sequence
;; **********************************************************************************************************
(defun HPS_DNA_verify_non_trivial_match ( hps_pattern_match_dataset_by_time )
  	(when (NOT hps_pattern_match_dataset_by_time ) 
		(setq hps_found_nontrivial_match nil )
		(return-from HPS_DNA_verify_non_trivial_match hps_found_nontrivial_match))

	(setq hps_found_nontrivial_match         t)

	(let*  ((hps_num_patmatches 		(length  HPS_pattern_match_dataset_by_time ))
	  	(p       			(first   HPS_pattern_match_dataset_by_time ))
  		(p_start       			(HPS_DNA_get_pattern_match_start p))
  		(p_end         			(HPS_DNA_get_pattern_match_end   p)))
		(when (equal hps_num_patmatches 1) 
			 (if (AND (HPS_is_this_between p_start *HPS_1DNA_endpoint_signature_start* (+ *HPS_1DNA_endpoint_signature_start* 100 ))
		  	 	  (HPS_is_this_between p_end   (- *HPS_1DNA_endpoint_signature_end* 100 ) 
						       	       (+ *HPS_1DNA_endpoint_signature_end* 100 )))
					(setq hps_found_nontrivial_match 	      nil)
					(setq hps_found_nontrivial_match 	      t))))

	(return-from HPS_DNA_verify_non_trivial_match hps_found_nontrivial_match))
;; **********************************************************************************************************


;; ********************************************************************************************
;; ********************************************************************************************
(defun HPS_DNA_main_init ( &key (alphabet    nil)
			        (mapcode     nil)
				(printthem   nil))

  	(if alphabet (setq *HPS_DNA_mapalphabet* alphabet) nil)
  	(if mapcode  (setq *HPS_DNA_mapcode*     mapcode ) nil)

	(when  printthem
		(HPS_pretty_printer *HPS_DNA_mapalphabet* t :nchars 8 :headermsg  "APHABET CURRENTLY IN USE	:")
		(HPS_pretty_printer *HPS_DNA_mapcode*     t :nchars 8 :headermsg  "MAPPING CODE CURRENTLY IN USE	:"))

	(unless printthem
	  	nil)

  	;; if your alphabet has numerical gaps, you must have care on its construction and set specialized fields
	;; that describe within the high (:maphigh), low (:maplow) and middle (:maphalf) points of your coding alphabet
	;; otherwise, these values are computed automatically for you
	(setq *STENO_mapcode*     (STENO_set_cypher 	*HPS_DNA_mapalphabet*
							*HPS_DNA_mapcode*
				    			:alphabet_description "UNDISCLOSED ALPHABET"
				  			:cypher_description   "UNDISCLOSED CODE" )))
;; ********************************************************************************************


;; ********************************************************************************************
;; ********************************************************************************************
(defun HPS_DNA_main ( &key (ALPHABET    	 nil)
			   (MAPCODE     	 nil)
		      	   (GOALDUR 		 30 ) 
			   (MINDUR 		 5 )
			   (INPUT_DATASET	 nil)
			   (SIGNATURE_START      (length (HPS_DNA_get_stabilizer_filler)))
			   (SIGNATURE_END 	 1200 )
			   (MATCH_GOAL  	 0.65 )
			   (HPS_TERSE            nil )
			   (INPUT_FILE  	"HPS_3DNA_APPROXIMATION_SERIES.DNA" ) 
			   (BASE_DIR    	"C:/HPS_DATA")
			   (OUTPUT_DIR  	"C:/HPS_DATA/HPS_OUTPUTS/" )
			   (REPORT_FILE 	"HPS_DNA.HTML" ))

  	;; initialize the 3DNA alphabet cyphers
	(HPS_DNA_main_init 		:ALPHABET    alphabet 
					:MAPCODE     mapcode)

	;; the true indexes of the HPS signature series of DNA base in the actual input sequence 
	(setq *HPS_1DNA_endpoint_signature_start*    signature_start)
	(setq *HPS_1DNA_endpoint_signature_end*	     signature_end)

	;; this is done by using rules related to stability and minimum segment duration.
	(setq *HPS_DNA_SEG_mindur*       	     goaldur)	; "minimum duration of a true HPS_DNA_ state represented as an ATS segment")
	(setq *HPS_DNA_SEG_mergedur* 	 	     mindur) 	; "minimum duration of a potential HPS_DNA_ state")

	;; at least X=match_goal of all HPS ATS segments must be present in the resultant match, 
	(setq *HPS_DNA_matching_goal*    	     match_goal)		

	;; an approximate upper bound of the probability of such matches is (1 - P(Z)^(X*<n>)) 

	(setq *HPS_DNA_inputfile* 	(format nil "~A"    input_file ))
	(setq *HPS_DNA_basedir*   	 base_dir)
	(setq *HPS_DNA_outputdir* 	 output_dir)
	(setq *HPS_DNA_reportfile* 	(format nil "~A~A"  base_dir            report_file ))
	(setq *HPS_DNA_segtable_file*   (format nil "~A~A" *HPS_DNA_outputdir* "HPS_DNA_SEGMENT_TABLE.DNA" ))
	(setq *STENO_reportfile* 	(format nil "~A~A"  base_dir 	       "HPS_REPORT_STENO.HTM" ))

	(when (NOT HPS_TERSE)
		(format t "~% ANALYSIS PATTERN MATCH GOAL OF      : ~S" *HPS_DNA_matching_goal* )
		(format t "~% ANALYSIS RUN FROM DIRECTORY FOUND ON: ~S" *HPS_DNA_basedir* )
		(format t "~% ANALYSIS DATA FILES WILL BE FOUND ON: ~S" *HPS_DNA_outputdir* )
		(format t "~% STENO ANALYSIS WILL BE PLACED ON    : ~S" *STENO_reportfile* )
		(format t "~% DNA ANALYSIS WILL BE PLACED ON      : ~S" *HPS_DNA_reportfile* )
		(format t "~% CONSTRUCTING HPS SEGMENT TABLE FOR  : ~S" *HPS_DNA_inputfile* )
		(format t "~% DNA SEGMENT TABLE WILL BE PLACED IN : ~S" *HPS_DNA_segtable_file* )
		(format t "~% VERIFYING PATH TO OUTPUT DIRECTORY  : ~S" *HPS_DNA_outputdir* )
		nil)

	;; ensure that said output directory exists or otherwise created it
	(ensure-directories-exist 		     *HPS_DNA_outputdir* )

	;; apply (trade-secret) specialized conditioning into the sequence of DNA bases, 
	;; to obtain a well conditioned series to be processed by the HPS transform
	(when (NOT input_dataset)
		(setq *HPS_1DNA_sequence_filename*     "HPS_1DNA_INPUT_SERIES_BASES.DNA" )
		(setq *HPS_1DNA_encoded_sequence_file* "HPS_1DNA_INPUT_SERIES_ENCODED.DNA" )
 		(setq *HPS_3DNA_encoded_seq_file*      "HPS_3DNA_INPUT_SERIES_ENCODED.DNA" )
		(setq *HPS_1DNA_actual_input_dataset*  (HPS_DNA_reader 	(format nil "~A~A" *HPS_DNA_basedir* "HPS_INPUTS/" )
							       		*HPS_1DNA_sequence_filename* 
									*HPS_1DNA_encoded_sequence_file*
									*HPS_3DNA_encoded_seq_file* ))

  		(setq *HPS_1DNA_input_timeseries*      (HPS_DNA_get_input_dataset      
							 		:hps_input_dataset *HPS_1DNA_actual_input_dataset*
				   				        :seriestype 	   '1DNACODED)))

	(unless (NOT input_dataset)
  		(setq *HPS_1DNA_input_timeseries*      (HPS_DNA_get_input_dataset      
							 		:hps_input_dataset  input_dataset
				   					:seriestype 	   '1DNACODED)))

	;; retrieve the in-memory input tuple set for use in the alignment and report modes
	(setq *HPS_DNA_approx_tupleset* 	(STENO_get_input_tupleset :mode 'EXACT))

	;; retrieve the HPS segment table from the STENO decoder
	(setq *HPS_3DNA_STENO_SEGTABLE* 	(STENO_get_segment_table))

	;; define the DNA signature in terms of HPS ATS segments
	(setq *HPS_3DNA_signature_dataset*  	(HPS_DNA_define_hps_signature    *HPS_3DNA_STENO_SEGTABLE* 
									 	 :hps_1DNA_index_from 		signature_start 
									 	 :hps_1DNA_index_to   		signature_end ))

	(setq *HPS_3DNA_signature_coded*     		(third  *HPS_3DNA_signature_dataset* ))
	(setq *HPS_3DNA_endpoints_signature*  		(second *HPS_3DNA_signature_dataset* ))
	(setq *HPS_3DNA_signature_size*       		(first  *HPS_3DNA_signature_dataset* ))
	(setq *HPS_3DNA_endpoint_signature_start*  	(first  *HPS_3DNA_endpoints_signature* ))
	(setq *HPS_3DNA_endpoint_signature_end*    	(second *HPS_3DNA_endpoints_signature* ))

	;; define the DNA test sequence in terms of HPS ATS segments
	(setq *HPS_3DNA_testsequence_dataset* 	(HPS_DNA_define_hps_testsequence *HPS_3DNA_STENO_SEGTABLE* 
									 	 :hps_1DNA_index_from 		signature_end ))

	(setq *HPS_3DNA_testsequence_coded*     	(third  *HPS_3DNA_testsequence_dataset* ))
	(setq *HPS_3DNA_endpoints_testsequence*  	(second *HPS_3DNA_testsequence_dataset* ))
	(setq *HPS_3DNA_testsequence_size*       	(first  *HPS_3DNA_testsequence_dataset* ))
	(setq *HPS_3DNA_endpoint_testsequence_start*  	(first  *HPS_3DNA_endpoints_testsequence* ))
	(setq *HPS_3DNA_endpoint_testsequence_end*    	(second *HPS_3DNA_endpoints_testsequence* ))

	;; write the signature in terms of the original input sequence (a DNA base sequence) to a file
	(HPS_timeseries_printer 	*HPS_1DNA_input_timeseries* 
	      				(format nil "~A~A" *HPS_DNA_outputdir* "HPS_DNA_TIMESERIES_SIGNATURE.DNA" )
							     :from *HPS_1DNA_endpoint_signature_start*
							     :to   *HPS_DNA_endpoint_signature_end* 
							     :xy_headerline "i	x	y	DNA")

	;; write the test sequence in terms of the original input sequence (a DNA base sequence) to a file
	(HPS_timeseries_printer 	*HPS_1DNA_input_timeseries* 
					(format nil "~A~A" *HPS_DNA_outputdir* "HPS_DNA_TIMESERIES_TESTSEQUENCE.DNA" )
							     :from *HPS_DNA_endpoint_signature_end*
							     :xy_headerline "i	x	y	DNA")

	;; print the HPS segment table to the specified file
	(STENO_decoder_writer_segtable 	*HPS_3DNA_STENO_SEGTABLE* 
					*HPS_DNA_segtable_file* 
					(setq ext_call_source t) )

	;; the output of the approximate HPS_DNA_ shape matching algorithm is written to the file, 
	;; normally it is appended to, so it's created here
	(setq *HPS_DNA_filename* (format nil "~A~A" *HPS_DNA_outputdir* "HPS_DNA_MATCHING_PATTERNS.DNA"))
	(with-open-file (output-stream *HPS_DNA_filename* :direction :output :if-exists :supersede :if-does-not-exist :create) 
	  	(format output-stream "" nil))


	;; timestamp
	(push (list 'DNASETUP (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)


	;; the pattern matcher, examines all possible fits and returns the best matching sequences
	(format t "~% FINDING ALL FEASIBLE MATCHING INSTANCES  : " )
	(setq *HPS_DNA_bestmatches_ordered_by_rank* 	(HPS_DNA_pattern_match 	 *HPS_3DNA_STENO_SEGTABLE* 
										 *HPS_3DNA_signature_coded* 
										 *HPS_DNA_signature_size* 
										 *HPS_DNA_matching_goal* 
										 :HPS_TERSE HPS_TERSE))

	;; retrieve the same patterns but actually ordered by time
	(setq *HPS_DNA_bestmatches_ordered_by_time* 	(HPS_DNA_get_best_matches :sortby 'TIME))


	;; timestamp
	(push (list 'DATAMINING (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)


	(when   (HPS_DNA_verify_non_trivial_match *HPS_DNA_bestmatches_ordered_by_time* )
		;; the sequence aligner, converts the HPS ATS sequence data into the original domain 
		;; and readies it (aligns and fits it) to be printed into the specified report
		(when (NOT HPS_TERSE)
			(format t "~% GENERATING ALIGNMENT DATA MAPS           : " )
			(HPS_DNA_coarsegrain_alignment 	*HPS_DNA_bestmatches_ordered_by_time* )
			nil)
	
		;; the pattern matcher, examines all possible fits and returns the best matching sequences
		(format t "~% REPORTING BEST MATCHING INSTANCES        : " )
		(HPS_DNA_print_report 		*HPS_DNA_bestmatches_ordered_by_rank*  
						*HPS_DNA_bestmatches_ordered_by_time* 
						:HPS_TERSE HPS_TERSE)

		;; timestamp
		(push (list 'DNAREPORT (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)

		;; create a gnuplot cmd file to be used in generating the plots
		(format t "~% GENERATING DATA FOR PLOTTING ANALYSIS    :" )
		(HPS_DNA_print_gnuplot_generator 		*HPS_DNA_bestmatches_ordered_by_time* 
								*HPS_DNA_bestmatches_ordered_by_rank* 
								:HPS_TERSE HPS_TERSE)

		;; build a convenient memory containing the results, if needed elsewhere
		(setq *HPS_DNA_pattern_matches_dataset* 
	     		(list *HPS_DNA_bestmatches_ordered_by_rank* 
		      	      *HPS_DNA_bestmatches_ordered_by_time*)))

	(unless (HPS_DNA_verify_non_trivial_match *HPS_DNA_bestmatches_ordered_by_time* )
		(setq *HPS_DNA_pattern_matches_dataset* nil))

	
	(return-from HPS_DNA_main *HPS_DNA_pattern_matches_dataset* ))
;; ********************************************************************************************





