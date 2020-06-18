;; ****************************************************************************************************************
;;                           *****     INVOCATION OF THE MAIN ENTRY POINT         ******
;;      THIS FILE CONTAINS EXAMPLES OF THE APPLICATION AND USE OF THE HPS TRANSFORM AND THE PROVIDED ROUTINES
;; ****************************************************************************************************************


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


;; ****************************************************************************************************************
;; ****************************************************************************************************************
(defun HPS_DNA_APPLICATION ( &key (OUTPUT_MODE		    nil) 
				  (HPS_DNA_WWW_ACCESS	    nil)
				  (HPS_DNA_LDB_ACCESS	    nil)
				  (HPS_TESTSEQUENCE_NUMBER  1786520)
				  (HPS_SIGNSEQUENCE_NUMBER  1786520)
				  (HPS_SIGNSEQUENCE_FROM    5000)
				  (HPS_SIGNSEQUENCE_TO      6000)
				  (HPS_MUTATE_SIGNATURE	    nil)
				  (HPS_TERSE		    nil)
				  (HPS_ARCHIVAL_MODE	    nil))

 	;; set to a detailed report with graphics and analyses in addition to the HPS transform results
  	(setf *HPS_FULL_OUTPUT_MODE*			 output_mode)

	(setf *HPS_UNIV_TIME_STACK* 			 nil)
	(push (list 'START (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)
	
	;; **********************************************************************************************************
	;; retrieve the DNA signature directly from the DNA database if so instructed, being specified as the subsequence
	;; of the DNA fragment pointed to by sequence number and contained between the genomic addresses specified 
	;; **********************************************************************************************************
	(when   HPS_DNA_WWW_ACCESS
		(format t "~% RETRIEVING SIGNATURE FROM ENTREZ DB : ~A" 	  HPS_SIGNSEQUENCE_NUMBER )
		(setf *HPS_1DNA_signature_bases*  
		      (HPS_DNA_WWW_get_sequence			     HPS_SIGNSEQUENCE_NUMBER 
			   :FROM 				     HPS_SIGNSEQUENCE_FROM 
			   :TO   				     HPS_SIGNSEQUENCE_TO
			   :GENOME_NAME 			     "E_COLI_K12"
			   :OUTPUT_DIR				     "E:/HPS_DATA/HPS_INPUTS/"
			   :WRITE_1DNA_BASES_SEQUENCE_INTO_THIS_FILE "E:/HPS_DATA/HPS_INPUTS/HPS_1DNA_SIGNATURE_BASES.DNA"))) 
	;; **********************************************************************************************************


	;; **********************************************************************************************************
	;; the reading of the DNA signature in DNA bases (A C G T) and generation of an input dataset
	;; **********************************************************************************************************
	(setf *HPS_1DNA_sequence_filename*     "HPS_1DNA_SIGNATURE_BASES.DNA" )
	(setf *HPS_1DNA_encoded_sequence_file* "HPS_1DNA_SIGNATURE_ENCODED.DNA" )
 	(setf *HPS_3DNA_encoded_seq_file*      "HPS_3DNA_SIGNATURE_ENCODED.DNA" )
	(setf *HPS_DNA_signature_dataset*      (HPS_DNA_load_1DNA_signature    *HPS_INPUT_PATH* 
									       *HPS_1DNA_sequence_filename* 
									       *HPS_1DNA_encoded_sequence_file* 
									       *HPS_3DNA_encoded_seq_file* ))

  	(setf *HPS_1DNA_signature_bases*       (HPS_DNA_get_input_dataset      :hps_input_dataset *HPS_DNA_signature_dataset*
				   					       :seriestype '1DNABASES ))
	;; **********************************************************************************************************


	;; **********************************************************************************************************
	;; for experimentation purposes, handle within the mutation of the DNA signature being searched within the test sequence
	;; **********************************************************************************************************
	(when   HPS_MUTATE_SIGNATURE
		(format t "~% MUTATING SIGNATURE FOR EXPERIMENT   : ~A" 	  HPS_SIGNSEQUENCE_NUMBER )

		;; **********************************************************************************************************
		;; mutate the sequence with a simple DNA substitution scheme of damaged DNA of a low frequency (~0.10* 1/4) 
		;; **********************************************************************************************************
		(setf *HPS_1DNA_mutated_sequence*   (HPS_DNA_mutate_sequence 	  *HPS_1DNA_signature_bases*	
										  :HOWTO	  'BY_NOISING
										  :HOWMANY   	   0.10
										  :FROMWHAT       'A
										  :TOWHAT	  'T ))

		;; **********************************************************************************************************
		;; print the actual sequence (filler signature testsequence) so it can be read at any time for hps processing
		;; **********************************************************************************************************
		(HPS_timeseries_printer		    *HPS_1DNA_mutated_sequence* 
						    (format nil "~A~A" *HPS_INPUT_PATH* *HPS_1DNA_sequence_filename* )    
						    :xy_headerline (format nil "i	1DNA_BASES_SEQ~A_MUTATED" HPS_SIGNSEQUENCE_NUMBER ))

		;; **********************************************************************************************************
		;; re-reading the mutated DNA signature in DNA bases (A C G T) and re-generate the input dataset
		;; **********************************************************************************************************
		(setf *HPS_1DNA_sequence_filename*      "HPS_1DNA_SIGNATURE_BASES.DNA" )
		(setf *HPS_1DNA_encoded_sequence_file*  "HPS_1DNA_SIGNATURE_ENCODED.DNA" )
		(setf *HPS_3DNA_encoded_seq_file*       "HPS_3DNA_SIGNATURE_ENCODED.DNA" ) 
		(setf *HPS_DNA_signature_dataset*       (HPS_DNA_load_1DNA_signature *HPS_INPUT_PATH* 
										     *HPS_1DNA_sequence_filename* 
		       								     *HPS_1DNA_encoded_sequence_file* 
										     *HPS_3DNA_encoded_seq_file* ))

		(setf *HPS_1DNA_signature_bases*        (HPS_DNA_get_input_dataset   :hps_input_dataset *HPS_DNA_signature_dataset*
										     :seriestype '1DNABASES )))
	;; **********************************************************************************************************


	;; **********************************************************************************************************
	(setf *HPS_1DNA_signature_series_headername* 	(HPS_substitute_character "_" "-" 	
								(second (HPS_reader_get_file_column  *HPS_HEADERROW* (HPS_get_timeseries_dataset)))))
	(setf *HPS_1DNA_signature_series_filename* 	(first (last (HPS_get_timeseries_dataset))))
	(format t "~& DNA SIGNATURE	: ~A	[~A]" *HPS_1DNA_signature_series_filename* 	*HPS_1DNA_signature_series_headername*) 	
	;; **********************************************************************************************************


	;; **********************************************************************************************************
	;; retrieve the DNA test sequence directly from the DNA database if so instructed, being specified as the subsequence
	;; of the DNA fragment pointed to by sequence number  (usually this being between 10000 and 100000 bp
	;; **********************************************************************************************************
	(when   HPS_DNA_WWW_ACCESS
		(HPS_DNA_WWW_get_sequence 	 
			 HPS_TESTSEQUENCE_NUMBER
			:GENOME_NAME 				  "E_COLI_K12"
			:OUTPUT_DIR				  "E:/HPS_DATA/HPS_INPUTS/"
			:WRITE_1DNA_BASES_SEQUENCE_INTO_THIS_FILE "E:/HPS_DATA/HPS_INPUTS/HPS_1DNA_TESTSEQUENCE_BASES.DNA"))
	;; **********************************************************************************************************


	;; **********************************************************************************************************
	;; retrieve the DNA test sequence directly from the local replica of the DNA database if so instructed, 
	;; this innefficient version reads and writes back to the input directory for the time being.
	;; **********************************************************************************************************
	(when  HPS_DNA_LDB_ACCESS 
		(setf *HPS_1DNA_sequence_filename*     (format nil "~ASEQUENCE_~A.DNA" HPS_DNA_LDB_ACCESS HPS_TESTSEQUENCE_NUMBER ))
		(setf *HPS_1DNA_encoded_sequence_file* "HPS_1DNA_TESTSEQUENCE_ENCODED.DNA" )
 		(setf *HPS_3DNA_encoded_seq_file*      "HPS_3DNA_TESTSEQUENCE_ENCODED.DNA" )

		(setf *HPS_1DNA_testsequence_input_bases* 
		      					(HPS_read_series *HPS_1DNA_sequence_filename*  ))

		;;; temp CHECK KLUDGE
		(HPS_timeseries_printer 	        (mapcar #'(lambda (a) (second a)) *HPS_1DNA_testsequence_input_bases*)
	                        	       		(format nil "~A~A" *HPS_INPUT_PATH* "HPS_1DNA_TESTSEQUENCE_BASES.DNA" ) 
						        :xy_headerline	(format nil "i	1DNA_BASES_SEQ~A" HPS_TESTSEQUENCE_NUMBER ))
	  	nil)
	;; **********************************************************************************************************


	;; **********************************************************************************************************
	;; the reading of the DNA test sequence in DNA bases (A C G T) and generation of an input dataset
	;; **********************************************************************************************************
	(setf *HPS_1DNA_sequence_filename*     "HPS_1DNA_TESTSEQUENCE_BASES.DNA" )
	(setf *HPS_1DNA_encoded_sequence_file* "HPS_1DNA_TESTSEQUENCE_ENCODED.DNA" )
 	(setf *HPS_3DNA_encoded_seq_file*      "HPS_3DNA_TESTSEQUENCE_ENCODED.DNA" )
	(setf *HPS_DNA_testsequence_dataset*   (HPS_DNA_load_1DNA_testsequence *HPS_INPUT_PATH* 
									       *HPS_1DNA_sequence_filename* 
									       *HPS_1DNA_encoded_sequence_file* 
									       *HPS_3DNA_encoded_seq_file* ))
	
  	(setf *HPS_1DNA_testsequence_bases*    (HPS_DNA_get_input_dataset  :hps_input_dataset *HPS_DNA_testsequence_dataset*
				   					   :seriestype '1DNABASES ))
	;; **********************************************************************************************************


	;; **********************************************************************************************************
	(setf *HPS_1DNA_testsequence_series_headername* (HPS_substitute_character "_" "-"
								(second (HPS_reader_get_file_column  *HPS_HEADERROW* (HPS_get_timeseries_dataset)))))
	(setf *HPS_1DNA_testsequence_series_filename* 	(first (last (HPS_get_timeseries_dataset))))
	(format t "~& DNA TESTSEQUENCE	: ~A	[~A]" *HPS_1DNA_testsequence_series_filename* 	*HPS_1DNA_testsequence_series_headername*) 	
	;; **********************************************************************************************************


	;; **********************************************************************************************************
	;; construct the actual sequence format being used by the HPS DNA pattern miner prepends the signature 
	;; to the test sequence (plus an initial data filler used to stabilize the HPS transient region 
	;; **********************************************************************************************************
	(setf *HPS_1DNA_actual_input_bases*    (HPS_DNA_prepend_signature_to_testseq *HPS_1DNA_signature_bases* 
										     *HPS_1DNA_testsequence_bases* ))

	;; print the actual sequence (filler signature testsequence) so it can be read at any time for hps processing
	(HPS_timeseries_printer 	       *HPS_1DNA_actual_input_bases* 
	                        	       (format nil "~A~A" *HPS_INPUT_PATH* "HPS_1DNA_INPUT_SERIES_BASES.DNA" ) 
					       :xy_headerline (format nil "i	1DNA_BASES_SIG~A_TSEQ_~A" HPS_SIGNSEQUENCE_NUMBER 
								      					  HPS_TESTSEQUENCE_NUMBER ))

	;; apply (trade-secret) specialized conditioning into the sequence of DNA bases, 
	;; to obtain a well conditioned series to be processed by the HPS transform
	(setf *HPS_1DNA_sequence_filename*     "HPS_1DNA_INPUT_SERIES_BASES.DNA" )
	(setf *HPS_1DNA_encoded_sequence_file* "HPS_1DNA_INPUT_SERIES_ENCODED.DNA" )
 	(setf *HPS_3DNA_encoded_seq_file*      "HPS_3DNA_INPUT_SERIES_ENCODED.DNA" )
	(setf *HPS_1DNA_actual_input_dataset*  (HPS_DNA_reader 	*HPS_INPUT_PATH* 
							       	*HPS_1DNA_sequence_filename* 
								*HPS_1DNA_encoded_sequence_file*
								*HPS_3DNA_encoded_seq_file* ))
	;; **********************************************************************************************************

	(push (list 'DBACCESS (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)

	;; **********************************************************************************************************
	;; invoke the main entry point of the HPS transform - this represents preprocessing not part of the online
	;; DNA pattern miner. Instead, a databank of HPS transformed sequences (small in size, as it is represented 
	;; in terms of SEGTABLES as represented and used by the STENOGRAPHIC DECODER which size is typically a significant
	;; small fraction of the size of the input sequence. For example, for a sequence of 14000 bps, a segtable of
	;; approximately 650 was generated. The size of this <n> wrt N can also be controlled by the parameters specified
	;; below and through other parameters resulting in a further control of the tradeoff between accuracy and speed
	;; **********************************************************************************************************
	(setq *HPS_transformed_inputseries*
   		(HPS_driver     :INPUTFILE             *HPS_3DNA_encoded_seq_file*
	 			:BASEDIR               *HPS_BASE_PATH*  
	 			:OUTPUTFILE            "HPS_3DNA_APPROXIMATION_SERIES.DNA"
							:M          	  	60 			       
							:MP 	    	  	30
					        	:TIMESHIFT  	  	30
					        	:SEGLIMIT   	  	90
					        	:ALPHALEVEL       	0.01				;; was 0.001,005 	       
							:MSERELAX   	  	(sqrt 0.5)  			;; was 2
							:FORECAST_WINSIZE 	60				;; was 60
				:K          	  	3 			
				:SEGTRIVIAL 	  	1 
				:MSEDELAY   	  	1 	
				:PRINTDUR   	  	3 
	 			:OUTPUTDIR             *HPS_OUTPUT_PATH*
	 			:DATASETFILE           "HPS_FULLDATA.DAT" 
	 			:REPORTFILE            "RESULTANT_HPS_APPROXIMATION.HTM" 
				:HPS_TRANSIENT_FILLER	t
				:HPS_TERSE		HPS_TERSE
	 			:HPS_FULL_OUTPUT_MODE  *HPS_FULL_OUTPUT_MODE* ))
	;; **********************************************************************************************************


	(push (list 'HPSTRANSFORM (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)


	;; **********************************************************************************************************
	;; need to fix in here the fact that the steno looks at (i f(i)) tuples, so needs to dummy it. also need to 
	;; check whether 3dna printout is needed, otherwise, need to hps-terse it to only write thn. Note that passing the
	;; time series in memory overrides the need for file reading in the STENO functions
	;; **********************************************************************************************************
	(setq *HPS_iter* 0)
	(setq *HPS_transformed_inputseries* (mapcar #'(lambda (a) (list (incf *HPS_iter* ) a)) *HPS_transformed_inputseries*))
	;; **********************************************************************************************************


	;; **********************************************************************************************************
	;; the invocation of the STENO decoder, it takes a series of ATS segments hidden behind an HPS-removable noise cloud
	;; and extract the HPS fundamental frequencies of these ATS segments
	;; **********************************************************************************************************
		(setf *HPS_3DNA_inputfile* 	"E:/HPS_DATA/HPS_OUTPUTS/HPS_3DNA_APPROXIMATION_SERIES.DNA" )
		(setf *HPS_3DNA_segtable_file*  "E:/HPS_DATA/HPS_OUTPUTS/SEQ/HPS_DNA_SEGMENT_TABLE.DNA" )

		(when (NOT HPS_TERSE)
			(format t "~% CONSTRUCTING HPS SEGMENT TABLE FOR  : ~A" 	*HPS_3DNA_inputfile* )
			(format t "~% DNA SEGMENT TABLE WILL BE PLACED IN : ~A" 	*HPS_3DNA_segtable_file* ))

		;; initialize the 3DNA alphabet cyphers
		(HPS_DNA_main_init 	    :ALPHABET   	     nil  
		   			    :MAPCODE    	     nil)

		;; decoder duration values - evaluated until HPS compressability is (1 - 0.04) about 1/25
		(setf HPS_STENO_MINSEGDUR_VALS   		   '( 6 5 4 3 2))
		(setf HPS_STENO_SEGRATIO_GOAL 			    0.040)						        ;; was 0.05
      		(setf LARGESEG_FRAGMENTER_ON      		    t)						       	        ;; was nil

		;; apply the HPS decoder
		(dolist (hps_steno_minsegdur HPS_STENO_MINSEGDUR_VALS  HPS_STENO_MINSEGDUR_VALS)

			(STENO_decoder_main :HPS_INPUTFILE  	     "E:/HPS_DATA/HPS_OUTPUTS/HPS_3DNA_APPROXIMATION_SERIES.DNA"
		                   	    :HPS_DEBUGFILE  	     "E:/HPS_DATA/HPS_OUTPUTS/HPS_3DNA_ROUNDED_APPROXIMATION_ENCODED.DNA"
	  				    :MEANDUR_GUESS 	     (setf *HPS_DNA_SEG_goaldur*       30)    			;; was 30
		        		    :MINDUR_THRESHOLD 	     (setf *HPS_DNA_SEG_thresholddur*  hps_steno_minsegdur) 	;; was 5
		        		    :LARGESEG_FRAGMENTER_ON   LARGESEG_FRAGMENTER_ON      				;; was nil
		                            :HPS_OUTPUTFILE 	     "E:/HPS_DATA/HPS_OUTPUTS/HPS_DNA_SEGMENT_TABLE.DNA"
					    :HPS_TIMESERIES_ARG	      *HPS_transformed_inputseries*
		                            :SYMBOLS_PER_LINE 	      14
		                            :HPS_EXTERNAL_CALL        t
		                            :HPS_TERSE 		      t )

			;; define the DNA signature in terms of HPS ATS segments
			(setf *HPS_3DNA_signature_segtable*
		      		(HPS_DNA_define_hps_signature    (STENO_get_segment_table) 
								      :HPS_1DNA_INDEX_FROM            0
								      :HPS_1DNA_INDEX_TO             (length *HPS_1DNA_signature_bases*)))  

			(setf hps_steno_siglen_to_input_ratio (/ (length (third *HPS_3DNA_signature_segtable*) ) 
			      					 (length *HPS_1DNA_signature_bases*)))


			(when   (>= hps_steno_siglen_to_input_ratio HPS_STENO_SEGRATIO_GOAL )  
				(return hps_steno_siglen_to_input_ratio))

			(unless (>= hps_steno_siglen_to_input_ratio HPS_STENO_SEGRATIO_GOAL )  
				 nil))
	
		;; here we still need dto check that the segtable is not nil, a bug is causing the segtable to be nil
		;; because somehow the output of the transform gets stuck into a value for the duration (seems like
		;; a malfesance breakpoint control though as it is impossible for it to be stuck to a value away from
		;; the mean as 42)
	;; **********************************************************************************************************


	(push (list 'HPSSTENOAPP (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)

	;; **********************************************************************************************************
	;; **********************************************************************************************************
	(HPS_cleanup t)
	;; **********************************************************************************************************

	(push (list 'HPS_GC      (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)


	;; **********************************************************************************************************
	;; apply the HPS DNA pattern miner
	;; **********************************************************************************************************
		;; **************************************************************************************************
		;; the operating regions of the HPS DNA pattern miner
		;; **************************************************************************************************
		(setf HPS_DNA_NUMBER_RETRY_LABELS		'( (0.90 	"MATCH - VIRTUAL CERTAINTY REGION")   
								   (0.80 	"MATCH - VERY HIGH CONFIDENCE REGION")   
								   (0.70 	"MATCH - HIGH CONFIDENCE REGION")   
								   (0.60 	"MATCH - SOUND CONFIDENCE REGION")   
								   (0.50 	"MATCH - VERY LIKELY REGION")
								   (0.35 	"----- - SEARCH OPERATING REGION") ))
		;; **************************************************************************************************


		;; **************************************************************************************************
		;; here a simple call to the pattern miner will be perrformed at the level of the broad search
		;; this level corresponds to roughly a level at which the bulk of the findings are typically dump as noise
		;; when this level is lowered, more such coarsegrain potential matches must be examined at the finegrain level
		;; increasing considerably the execution time, on a grid computer one could farm out the particular
		;; variations of parameters as any one of them could come with the true answer OR none but at differnet 
		;; times
		;; **************************************************************************************************
		(setf HPS_DNA_NUMBER_RETRY_LEVELS		'( (0.3750 	"----- - SEARCH OPERATING REGION") ))
		;; **************************************************************************************************


		;; **************************************************************************************************
		;; the region at which the broad search will be made, in order to find ALL possible matches
		;; **************************************************************************************************
		(setf HPS_DNA_BROAD_SEARCH_LEVEL		(first (last HPS_DNA_NUMBER_RETRY_LEVELS )))
		;; **************************************************************************************************


		;; **************************************************************************************************
		;; this call will return a coarsegrain preselection of the best choices at said threshold value
		;; this means that all matches at this level contain at least X% matching segments with respect
		;; to the DNA signature. Finegrain alignment is then applied over these to choose the real 
		;; matches at a localized run-time complexity cost. 
		;; **************************************************************************************************
		(dolist (hps_match_goal HPS_DNA_NUMBER_RETRY_LEVELS HPS_DNA_NUMBER_RETRY_LEVELS )
	            (format t "~% " nil)
	            (format t "~% __________________________________________________________________ : " nil)
	            (format t "~% PATTERN MINING FOR MATCHES AT COARSEGRAIN PATTERN MATCH GOAL OF .. : [~A] " (first  hps_match_goal))
	            (format t "~% PATTERN MINER RESULTS AT THIS LEVEL ARE LOCATED WITHIN THE ....... : [~A] " (second hps_match_goal))

		    (setf *HPS_pattern_match_dataset* 
	     		  (HPS_DNA_main   :ALPHABET    		nil
		  			  :MAPCODE     		nil
					  :INPUT_DATASET 	*HPS_1DNA_actual_input_dataset*
	  	   			  :INPUT_FILE  		"HPS_3DNA_APPROXIMATION_SERIES.DNA"  
					  :SIGNATURE_START       0
					  :SIGNATURE_END        (length *HPS_1DNA_signature_bases*)
	   	   			  :BASE_DIR    		"E:/HPS_DATA/"
	     	        					 :GOALDUR     	*HPS_DNA_SEG_goaldur*		
	  	   						 :MINDUR      	*HPS_DNA_SEG_thresholddur*	
	   	   						 :MATCH_GOAL  	(first hps_match_goal)
	   	   			  :OUTPUT_DIR  		"E:/HPS_DATA/HPS_OUTPUTS/" 
					  :HPS_TERSE		HPS_TERSE
	   	   			  :REPORT_FILE 		"HPS_DNA.HTM" ))

		     (setf *HPS_DNA_FINDINGS* 	(HPS_DNA_verify_non_trivial_match (first *HPS_pattern_match_dataset*)))

		     (when *HPS_DNA_FINDINGS*
			  	(return *HPS_pattern_match_dataset*)))
		;; **************************************************************************************************


		;; **************************************************************************************************
		;; generate the automatically selected DNA data plots for the autogenerated DNA analyses
		;; **************************************************************************************************
		(when *HPS_DNA_FINDINGS*
			(HPS_gnuplot_interface	:COMMAND 	'PLOT
	   	       	       			:OUTPUT_DIR 	*HPS_OUTPUT_PATH*
	   	       	       			:WITH_CMD_FILE 	(format nil "~A~A" *HPS_BASE_PATH* "HPS_DNA_WINDOWS_CMDS.BAT")))
		;; **************************************************************************************************


	;; **********************************************************************************************************
	;; prepare the data of the autogenerated analyses for the HPS Transform
	;; **********************************************************************************************************
	(when *HPS_DNA_FINDINGS*
	    (when *HPS_FULL_OUTPUT_MODE*
		(HPS_gnuplot_interface  :COMMAND 	'PLOT
		   	       		:OUTPUT_DIR 	*HPS_OUTPUT_PATH*
		   	       		:WITH_CMD_FILE 	(format nil "~A~A" *HPS_BASE_PATH* "HPS_TRANSFORM_WINDOWS_CMDS.BAT" ))))
	;; **********************************************************************************************************


	(push (list 'DNAPLOT (get-universal-time)) 	*HPS_UNIV_TIME_STACK*)


	;; **********************************************************************************************************
  	;; attempt to cleanup previous (HPS TRANSFORM) lingered state
	;; **********************************************************************************************************
	(when (NOT HPS_TERSE)
		(format t "~% Marking lingered memory state for clean-up ....................... : " nil )
  		(HPS_cleanup *HPS_FULL_OUTPUT_MODE* )
		nil)
	;; **********************************************************************************************************

	(push (list 'HPS_GC (get-universal-time)) 		*HPS_UNIV_TIME_STACK*)
	(push (list 'END (get-universal-time)) 		*HPS_UNIV_TIME_STACK*)

	(return-from HPS_DNA_APPLICATION *HPS_DNA_FINDINGS*))
;; ****************************************************************************************************************











