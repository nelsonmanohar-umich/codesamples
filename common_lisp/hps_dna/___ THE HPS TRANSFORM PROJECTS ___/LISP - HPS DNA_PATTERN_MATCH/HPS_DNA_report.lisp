(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; ********************************************************************************************
;; 			REPORT GENERATOR FOR DNA PATTERN MINER
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
;; ***********************************************************************************************************************
(setq HPS_SEGTABLE_HEADER    			'( 	SIGNATURE_SEG 
						   	OF_VALUE 
						   	MATCHES_TO 
						   	PATTERN_SEG 
						   	WITH_VALUE 
						   	WITH_RATING_OF 
						   	WITH_COSTMETRICS))
(setq HPS_HTML_FIXEDSIZE_MAPDESC_HEADER 	'(	MATCH_STARTS 
						   	AND_ENDS_AT 
							MATCH_RATING 
							MATCH_SPANS 
							MATCH_WEIGHT 
							NONMATCHBP
							))
							;; COST_VECTOR 
							;; COST_METRIC
(setq HPS_HTML_FIXEDSIZE_MAPPING_HEADER0 	'(	ORIG-SEG 
						   	OTVAL 
							MAPS?
							MATCH-SEG 
							MTVAL 
							MATCH-QUALITY 
							WEIGTH ))
(setq HPS_HTML_FIXEDSIZE_MAPPING_HEADER1 	'(	METRICS 
						   	EVAL-AT 
							START ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; print summary statistics about the mapping
;; ***********************************************************************************************************************
(defun HPS_DNA_HTML_print_specific_matching_pattern_stats ( p bound-stream &key (HPS_DNA_print_to_screen nil))
	;; print summary statistics about the mapping
	(when (not HPS_DNA_print_to_screen)
		(format bound-stream "~&<TABLE ALIGN=CENTER>"    nil)
		(format bound-stream "~&<TR>"    	 nil)
		(format bound-stream "~{~%<TD WIDTH=120 VALIGN=TOP ALIGN=CENTER> <P CLASS=MSONORMAL> ~A </P> </TD> ~} ~%" 
							 HPS_HTML_FIXEDSIZE_MAPDESC_HEADER )
		(format bound-stream "~&</TR>"   	 nil)
		(format bound-stream "~&<TR>"    	 nil)
		(format bound-stream "~{~%<TD WIDTH=120 VALIGN=TOP ALIGN=CENTER> <P CLASS=MSONORMAL> ~A </P> </TD> ~} ~%" 
		                                    	(list (HPS_DNA_get_pattern_match_start      p)
							      (HPS_DNA_get_pattern_match_end        p)
							      (HPS_DNA_get_pattern_match_rating     p)
							      (HPS_DNA_get_pattern_match_span       p)
							      (HPS_DNA_get_pattern_match_costweigth p)
							      ;; (HPS_DNA_get_pattern_match_normcost   p)
							      (HPS_DNA_get_comparator_table_rating  p)
							      ;; (HPS_DNA_get_pattern_match_costvector p)
							      ))
		(format bound-stream "~&</TR>" 		 nil)
		(format bound-stream "~&</TABLE>"    	 nil)
		(format bound-stream "~&<BR>"        	 nil))

	(unless (not HPS_DNA_print_to_screen)
		(format bound-stream "~%~{~16S  ~}" 	HPS_HTML_FIXEDSIZE_MAPDESC_HEADER )
		(format bound-stream "~%~{~16A  ~}" 	(list (HPS_DNA_get_pattern_match_start      p)
							      (HPS_DNA_get_pattern_match_end        p)
							      (HPS_DNA_get_pattern_match_rating     p)
							      (HPS_DNA_get_pattern_match_span       p)
							      (HPS_DNA_get_pattern_match_costweigth p)
							      ;; CHECK NIL INSTEAD OF REAL ON NORMCOST FROM PATTERN MINING
							      ;; (HPS_DNA_get_pattern_match_normcost   p)
							      (HPS_DNA_get_comparator_table_rating  p)
							      ;; (HPS_DNA_get_pattern_match_costvector p)
							      ))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; generate a mapping table between signature and matching segment tables for each of the best patterns
;; prints a table containing a color-coded qualified mapping between a signature and a matching pattern within a test sequence  
;; ***********************************************************************************************************************
(defun HPS_DNA_HTML_print_specific_matching_pattern_maptable ( p bound-stream  &key (HPS_DNA_terse t))
	;; print the header for the mapping table
	(format bound-stream "~%~A~%" _TABLE0)
	(format bound-stream "~A~%" _TROW0)
		;; basic report with single datum cost metrics
		(dolist (header_item HPS_HTML_FIXEDSIZE_MAPPING_HEADER0 'TABLEHEADERS)
	      		(format bound-stream "~A~%" _TCOL0a)
				(format bound-stream "~A~%" header_item)
	      		(format bound-stream "~A~%" _TCOL1))

		;; report with cost metrics vector
		(when   (NOT HPS_DNA_terse) 
			(dolist (header_item HPS_HTML_FIXEDSIZE_MAPPING_HEADER1 'TABLEHEADERS)
	      			(format bound-stream "~A~%" _TCOL0a)
					(format bound-stream "~A~%" header_item)
	      			(format bound-stream "~A~%" _TCOL1)))

	(format bound-stream "~A~%" _TROW1)

	;; print each matching-element into a table row with color-coded cell codified with respect to the match quality
	(setq hps_mapping_set  (HPS_DNA_get_pattern_mapping_set p))
	(dolist ( mapping-elem hps_mapping_set 'ITEMPROCESSING )
		(format bound-stream "~A~%" _TROW0)
		      	(format bound-stream "~A~%" _TCOL0c)
				(format bound-stream "~20A	" 	  (HPS_DNA_get_pattern_mappings_signature_segment mapping-elem))
		      	(format bound-stream "~A~%" _TCOL1)
		      	(format bound-stream "~A~%" _TCOL0a)
				(format bound-stream "~8A"                (STENO_getmapletter 
									    (fourth (HPS_DNA_get_pattern_mappings_signature_segment mapping-elem))))
		      	(format bound-stream "~A~%" _TCOL1)
	
			;; handling of color-coded match information
			(when   (NOT (equal (third  mapping-elem) 'NOTFOUND_WARNG ))
		      		(format bound-stream "~A~%" _TCOL0a)
					(format bound-stream " -->  " 	  nil) 
		      		(format bound-stream "~A~%" _TCOL1)
		      		(format bound-stream "~A~%" _TCOL0c)
					(format bound-stream "~20A	" (HPS_DNA_get_pattern_mappings_matching_segment mapping-elem))
		      		(format bound-stream "~A~%" _TCOL1)
		      		(format bound-stream "~A~%" _TCOL0a)
					(format bound-stream "~8A  "      (STENO_getmapletter 
									    (fourth (HPS_DNA_get_pattern_mappings_matching_segment mapping-elem))))
		      		(format bound-stream "~A~%" _TCOL1)
		      		(format bound-stream "~A~%" _TCOL0c)
					(format bound-stream "~16A"       (HPS_DNA_get_pattern_mappings_matching_quality mapping-elem))
		      		(format bound-stream "~A~%" _TCOL1))
	
			(unless (NOT (equal (third  mapping-elem) 'NOTFOUND_WARNG ))
		      		(format bound-stream "~A~%" _TCOL0a)
					(format bound-stream " ***  " 	   nil)
		      		(format bound-stream "~A~%" _TCOL1)
		      		(format bound-stream "~A~%" _TCOL0d)
					(format bound-stream "~20A	"  "-------- n/a -------")
		      		(format bound-stream "~A~%" _TCOL1)
		      		(format bound-stream "~A~%" _TCOL0a)
					(format bound-stream "~8A  "       "- n/a - ")
		      		(format bound-stream "~A~%" _TCOL1)
		      		(format bound-stream "~A~%" _TCOL0d)
					(format bound-stream "~16A" 	   (HPS_DNA_get_pattern_mappings_matching_quality mapping-elem))
		      		(format bound-stream "~A~%" _TCOL1))

		      	(format bound-stream "~A~%" _TCOL0a)
				(format bound-stream "~4D" 	           (HPS_DNA_get_pattern_mappings_cost_weigth mapping-elem))
		      	(format bound-stream "~A~%" _TCOL1)

			;; optional cost metrics vector information
			(when   (NOT HPS_DNA_terse) 
		      		(format bound-stream "~A~%" _TCOL0a)
					(format bound-stream "[~{~3D ~}] " (HPS_subseq (third  (second mapping-elem)) 0 3))
		      		(format bound-stream "~A~%" _TCOL1)
		      		(format bound-stream "~A~%" _TCOL0b)
					(format bound-stream "~12A " 	   (fourth (third  (second mapping-elem))))
		      		(format bound-stream "~A~%" _TCOL1)
		      		(format bound-stream "~A~%" _TCOL0a)
					(format bound-stream "~D" 	   (fifth  (third  (second mapping-elem))))
		      		(format bound-stream "~A~%" _TCOL1)))

		    (format bound-stream "~A~%" _TROW0)

		(format bound-stream "~%~A~%" _TABLE1) 
	    (format bound-stream "~%~A~%" _BREAK))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; prints a particular (sub) segment table extract from a full segment table from a time series, the segment table 
;; comprises ALL the segments found to lie within the inclusive range [from to]
;; ***********************************************************************************************************************
(defun HPS_DNA_print_specific_pattern_segtable (pattern segtable intofile)
	(with-open-file (segtable-stream intofile :direction :output :if-exists :supersede :if-does-not-exist :create)
	  	(when   pattern ;; print the specific HPS DNA pattern match provided
  			(STENO_print_segtable  segtable segtable-stream (setq external_call_flag t) 
					      			:relative_indexing 	(HPS_DNA_get_pattern_match_start pattern)
								:fromindex 		(HPS_DNA_get_pattern_match_start pattern)
     			     					:toindex 		(HPS_DNA_get_pattern_match_end   pattern)))

	  	(unless pattern ;; if there is no pattern, then it is a request to print the HPS DNA signature
  			(STENO_print_segtable segtable segtable-stream (setq external_call_flag t) 
					      			:relative_indexing 	0
								:fromindex 		0
     			     					:toindex 		*HPS_1DNA_endpoint_signature_end*))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; computes histograms for both HPS segment duration and HPS targeting value for each pattern in the best patterns set 
;; ***********************************************************************************************************************
(setq *HPS_DNA_nbins* 20)
(defun HPS_DNA_generate_histograms_for ( HPS_matching_patterns_by_time )
	;; compute and write histogram setup data for the specified pattern match, being the signature
	(setq *HPS_DNA_histogram_filename* (HPS_concat *HPS_DNA_outputdir* "HPS_DNA_SIGNATURE_HISTOGRAM_LAMBDAS.DNA" )) 
	(setq *HPS_histogram_ts* (mapcar #'(lambda (segelem) (third segelem)) 
					 (STENO_get_segment_table  :SEG_table 	(STENO_get_segment_table)
				      				   :fromindex 	*HPS_1DNA_endpoint_signature_start*
     			     	      				   :toindex	*HPS_1DNA_endpoint_signature_end*)))
	(HPS_histogram *HPS_histogram_ts* *HPS_DNA_histogram_filename* :numbins *HPS_DNA_nbins* )

	;; compute and write histogram setup data for the specified pattern match, being the signature
	(setq *HPS_DNA_histogram_filename* (HPS_concat *HPS_DNA_outputdir* "HPS_DNA_SIGNATURE_HISTOGRAM_TVALS.DNA" )) 
	(setq *HPS_histogram_ts* (mapcar #'(lambda (segelem) (fourth segelem)) 
					 (STENO_get_segment_table  :SEG_table 	(STENO_get_segment_table)
				      				   :fromindex 	*HPS_1DNA_endpoint_signature_start*
     			     	      				   :toindex	*HPS_1DNA_endpoint_signature_end*)))
	(HPS_histogram *HPS_histogram_ts* *HPS_DNA_histogram_filename* :numbins *HPS_DNA_nbins* )

	;; write the the histograms for each of the patterns
	(dolist (p HPS_matching_patterns_by_time 'BEST_PATTERN_MATCHES)
	  	;; retrieve the index of the selected pattern from the time sorted patterns  
	  	(setq p_idx (HPS_DNA_rank_of_pattern_in p HPS_matching_patterns_by_time ))

		;; compute and write histogram setup data for the specified pattern match
		(setq *HPS_DNA_histogram_filename* (HPS_concat *HPS_DNA_outputdir* 
							(format nil "~A~D.DNA" "HPS_DNA_PATTERN_MATCH_HISTOGRAM_TVALS_"     p_idx))) 
		(setq *HPS_histogram_ts* (mapcar #'(lambda (segelem) (fourth segelem)) 
						 (STENO_get_segment_table  :SEG_table 	(STENO_get_segment_table)
				      				 	   :fromindex 	(HPS_DNA_get_pattern_match_start    p)
     			     	      					   :toindex	(HPS_DNA_get_pattern_match_end      p))))
		(HPS_histogram *HPS_histogram_ts* *HPS_DNA_histogram_filename* :numbins *HPS_DNA_nbins* )
		
		;; compute and write histogram setup data for the specified pattern match
		(setq *HPS_DNA_histogram_filename* (HPS_concat *HPS_DNA_outputdir* 
							(format nil "~A~D.DNA" "HPS_DNA_PATTERN_MATCH_HISTOGRAM_LAMBDAS_"   p_idx))) 
		(setq *HPS_histogram_ts* (mapcar #'(lambda (segelem) (third segelem)) 
						 (STENO_get_segment_table  :SEG_table 	(STENO_get_segment_table)
				      				 	   :fromindex 	(HPS_DNA_get_pattern_match_start    p)
     			     	      					   :toindex	(HPS_DNA_get_pattern_match_end      p))))
		(HPS_histogram *HPS_histogram_ts* *HPS_DNA_histogram_filename* :numbins *HPS_DNA_nbins* )))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; print the segtable for each specific pattern match as well as the signature into a file
;; ***********************************************************************************************************************
(defun HPS_DNA_print_ALL_segtables ( HPS_matching_patterns_by_time )
	(setq *HPS_DNA_histogram_filename* (HPS_concat *HPS_DNA_outputdir* "HPS_DNA_SIGNATURE_SEGTABLE.DNA" )) 
	(HPS_DNA_print_specific_pattern_segtable nil (STENO_get_segment_table) *HPS_DNA_histogram_filename* )

	(setq patnum 0)
	(dolist (p HPS_matching_patterns_by_time 'BEST_PATTERN_MATCHES)
		(setq *HPS_DNA_histogram_filename* (HPS_concat *HPS_DNA_outputdir* (format nil "HPS_DNA_PATTERN_MATCH_SEGTABLE_~D.DNA" patnum )))
		(HPS_DNA_print_specific_pattern_segtable p (STENO_get_segment_table) *HPS_DNA_histogram_filename* )
		(incf patnum)))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; creates a histogram of the pattern matching strength signal to understand the discrimination power of the results
;; ***********************************************************************************************************************
(defun HPS_ifnilp_thenzero (a) (if (equal a nil) 0 a))
(defun HPS_DNA_match_strength_analysis ( MATCHSTRENGTHLABEL )
	(setq *HPS_histogram_ts* 		(HPS_DNA_get_all_matching_patterns))
	(setq *HPS_DNA_match_rating_signal* 	(mapcar #'(lambda (p) 
							    (HPS_ifnilp_thenzero (HPS_DNA_get_pattern_match_rating   p))) 
							*HPS_histogram_ts* ))

	;; (setq *HPS_DNA_match_metric_signal* 	(mapcar #'(lambda (p) 
							    ;; (HPS_ifnilp_thenzero (HPS_DNA_get_pattern_match_normcost p))) 
							;; *HPS_histogram_ts* ))
	(setq *HPS_DNA_match_metric_signal* 	'(0 1 2 3 4))

	(format t "~% GENERATING MATCH BEHAVIOR ANALYSIS DATA  : [~A]" MATCHSTRENGTHLABEL)
	(setq *HPS_DNA_filename* 		(HPS_concat *HPS_DNA_outputdir* "HPS_DNA_MATCHBEHAVIOR_HISTOGRAM.DNA")) 
	(setq *HPS_DNA_match_rating_hist_data*  (HPS_histogram *HPS_DNA_match_rating_signal* *HPS_DNA_filename* :numbins 100 )))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; print summary statistics about the matching pattern
;; ***********************************************************************************************************************
(defun HPS_DNA_match_strength_summary_stats ( bound-stream )

  	(setq _TCOL0 _TCOL0b)
	(with-output-to-string (HPS-stats-buffer)
		(format HPS-stats-buffer "<H3> BEST MATCHES BETWEEN SIGNATURE AND DNA TEST SEQUENCE </H3>" nil)

		(format HPS-stats-buffer "~%<BR>" nil )
		(format HPS-stats-buffer "~%~A" _TABLE0)
		(format HPS-stats-buffer "~%~A" _TROW0)
		(format HPS-stats-buffer "~%~A" _TCOL0)

		(format HPS-stats-buffer "MINIMUM  ~A ~A"  	_TCOL1 _TCOL0)
		(format HPS-stats-buffer "~6,2F ~A ~A "   	(HPS_minimum *HPS_DNA_match_rating_signal* ) _TCOL1 _TCOL0)
		(format HPS-stats-buffer "~A ~A "   	(HPS_minimum *HPS_DNA_match_metric_signal* ) _TCOL1 )
		(format HPS-stats-buffer "~A ~A ~A "   	_TCOL0e "minimum value observed across all HPS sampling points" _TCOL1 )
		(format HPS-stats-buffer "~%~A~%~A" 	_TROW1 _TROW0)

		(format HPS-stats-buffer "~%~A" 		_TCOL0)
		(format HPS-stats-buffer "MAXIMUM  ~A ~A"  	_TCOL1 _TCOL0)
		(format HPS-stats-buffer "~6,2F ~A ~A "   	(HPS_maximum *HPS_DNA_match_rating_signal* ) _TCOL1 _TCOL0)
		(format HPS-stats-buffer "~A ~A "   	(HPS_maximum *HPS_DNA_match_metric_signal* ) _TCOL1 )
		(format HPS-stats-buffer "~A ~A ~A "   	_TCOL0e "maximum value observed across all HPS sampling points" _TCOL1 )
		(format HPS-stats-buffer "~%~A~%~A" 	_TROW1 _TROW0)

		(format HPS-stats-buffer "~%~A" 		_TCOL0)
		(format HPS-stats-buffer "MEAN     ~A ~A"  	_TCOL1 _TCOL0)
		(format HPS-stats-buffer "~6,2F ~A ~A "   	(setq m_r (HPS_mean    *HPS_DNA_match_rating_signal* )) _TCOL1 _TCOL0)
		(format HPS-stats-buffer "~A ~A "   	(setq m_m (HPS_mean    *HPS_DNA_match_metric_signal* )) _TCOL1 )
		(format HPS-stats-buffer "~A ~A ~A "   	_TCOL0e "mean value taken across all HPS sampling points" _TCOL1 )
		(format HPS-stats-buffer "~%~A~%~A" 	_TROW1 _TROW0)

		(format HPS-stats-buffer "~%~A" 		_TCOL0)
		(format HPS-stats-buffer "MEDIAN   ~A ~A"  	_TCOL1 _TCOL0)
		(format HPS-stats-buffer "~6,2F ~A ~A "   	(HPS_median  *HPS_DNA_match_rating_signal* ) _TCOL1 _TCOL0)
		(format HPS-stats-buffer "~A ~A "   	(HPS_median  *HPS_DNA_match_metric_signal* ) _TCOL1 )
		(format HPS-stats-buffer "~A ~A ~A "   	_TCOL0e "median value taken across all HPS sampling points" _TCOL1 )
		(format HPS-stats-buffer "~%~A~%~A" 	_TROW1 _TROW0)

		(format HPS-stats-buffer "~%~A" 		_TCOL0)
		(format HPS-stats-buffer "STDDEV   ~A ~A"  	_TCOL1 _TCOL0)
		(format HPS-stats-buffer "~6,2F ~A ~A "   	(setq s_r (HPS_stddev  *HPS_DNA_match_rating_signal* m_r )) _TCOL1 _TCOL0)
		(format HPS-stats-buffer "~A ~A "   	(setq s_m (HPS_stddev  *HPS_DNA_match_metric_signal* m_m )) _TCOL1 )
		(format HPS-stats-buffer "~A ~A ~A "   	_TCOL0e "observed standard deviation wrt said mean value" _TCOL1 )
		(format HPS-stats-buffer "~%~A~%~A" 	_TROW1 _TROW0)

		(format HPS-stats-buffer "~%~A" 		_TCOL0)
		(format HPS-stats-buffer "THRESHOLD~A ~A"  	_TCOL1 _TCOL0)
		(format HPS-stats-buffer "~6,2F ~A ~A "   	*HPS_DNA_matching_goal* _TCOL1 _TCOL0)
		(format HPS-stats-buffer "~A ~A "   	"--- n/a ---" _TCOL1 )
		(format HPS-stats-buffer "~A ~A ~A "   	_TCOL0e "LIM: pivot point, used to select best pattern matches" _TCOL1 )
		(format HPS-stats-buffer "~%~A~%~A" 	_TROW1 _TROW0)

		(format HPS-stats-buffer "~%~A" 		_TCOL0)
		(format HPS-stats-buffer "Z-SCORE  ~A ~A"  	_TCOL1 _TCOL0)
		(format HPS-stats-buffer "~6,2F ~A ~A "   	(HPS_compute_perf_z_value_for *HPS_DNA_matching_goal* m_r s_r ) _TCOL1 _TCOL0)
		(format HPS-stats-buffer "~A ~A "   	(HPS_compute_perf_z_value_for *HPS_DNA_matching_goal* m_r s_r ) _TCOL1 )
		(format HPS-stats-buffer "~A ~A ~A "   	_TCOL0e "LIM's z-score corresponding to observed mean and std-dev" _TCOL1 )
		(format HPS-stats-buffer "~%~A" 		_TROW1 )

		(format HPS-stats-buffer "~%~A" _TABLE0)

		(format bound-stream "~A" (get-output-stream-string HPS-stats-buffer ))))
;; ***********************************************************************************************************************
			

;; ***********************************************************************************************************************
;; prints the cost vector metrics associated with a pattern to the specified file
;; ***********************************************************************************************************************
(defun HPS_DNA_print_costvector_metrics_for ( p patnum bound-stream )
  	(setq _TCOL0 _TCOL0b)
	(setq costvector (HPS_DNA_get_pattern_match_costvector 	p))

	;; (format bound-stream "<H4> ASSOCIATED COST METRICS FOR PATTERN MATCH M~D </H4>" patnum )
	(format bound-stream "~%~A" _TABLE0)
	(format bound-stream "~%~A" _TROW0)
	(format bound-stream "~%~A" _TCOL0)
  	(format bound-stream "~%~4A ~A ~A TOTAL NUM: ~D ~A ~A (EXACTING_MATCH) ~A ~A ~A" 
		"E1" _TCOL1 _TCOL0 (pop costvector) _TCOL1 _TCOL0c _TCOL1 _TCOL0e "EXACTING SEG-DUR, 	SAME SEG-VAL MATCHES" )   
	(format bound-stream "~%~A" _TCOL1)
	(format bound-stream "~%~A" _TROW1)
	(format bound-stream "~%~A" _TROW0)
	(format bound-stream "~%~A" _TCOL0)
  	(format bound-stream "~%~4A ~A ~A TOTAL NUM: ~D ~A ~A (APPROX32_MATCH) ~A ~A ~A" 
		"E2" _TCOL1 _TCOL0 (pop costvector) _TCOL1 _TCOL0c _TCOL1 _TCOL0e "APPROX. ~SEG-DUR, 	SAME SEG-VAL MATCHES" )   
	(format bound-stream "~%~A" _TCOL1)
	(format bound-stream "~%~A" _TROW1)
	(format bound-stream "~%~A" _TROW0)
	(format bound-stream "~%~A" _TCOL0)
  	(format bound-stream "~%~4A ~A ~A TOTAL NUM: ~D ~A ~A (APPROX64_MATCH) ~A ~A ~A" 
		"E3" _TCOL1 _TCOL0 (pop costvector) _TCOL1 _TCOL0c _TCOL1 _TCOL0e "LOOSELY ~SEG-DUR, 	SAME SEG-VAL MATCHES" )   
	(format bound-stream "~%~A" _TCOL1)
	(format bound-stream "~%~A" _TROW1)
	(format bound-stream "~%~A" _TROW0)
	(format bound-stream "~%~A" _TCOL0)
  	(format bound-stream "~%~4A ~A ~A TOTAL NUM: ~D ~A ~A (SPANNING_MATCH) ~A ~A ~A" 
		"E4" _TCOL1 _TCOL0 (pop costvector) _TCOL1 _TCOL0c _TCOL1 _TCOL0e "SPANNING SEG-DUR, 	SAME SEG-VAL MATCHES" )   
	(format bound-stream "~%~A" _TCOL1)
	(format bound-stream "~%~A" _TROW1)
	(format bound-stream "~%~A" _TROW0)
	(format bound-stream "~%~A" _TCOL0)
  	(format bound-stream "~%~4A ~A ~A TOTAL NUM: ~D ~A ~A (EXACTING_MISPL) ~A ~A ~A" 
		"A1" _TCOL1 _TCOL0 (pop costvector) _TCOL1 _TCOL0c _TCOL1 _TCOL0e "EXACTING SEG-DUR, 	+-1MISPELLED SEG-VAL" )   
	(format bound-stream "~%~A" _TCOL1)
	(format bound-stream "~%~A" _TROW1)
	(format bound-stream "~%~A" _TROW0)
	(format bound-stream "~%~A" _TCOL0)
  	(format bound-stream "~%~4A ~A ~A TOTAL NUM: ~D ~A ~A (APPROX32_MISPL) ~A ~A ~A" 
		"A2" _TCOL1 _TCOL0 (pop costvector) _TCOL1 _TCOL0c _TCOL1 _TCOL0e "APPROX. ~SEG-DUR, 	+-1MISPELLED SEG-VAL" )   
	(format bound-stream "~%~A" _TCOL1)
	(format bound-stream "~%~A" _TROW1)
	(format bound-stream "~%~A" _TROW0)
	(format bound-stream "~%~A" _TCOL0)
  	(format bound-stream "~%~4A ~A ~A TOTAL NUM: ~D ~A ~A (APPROX64_MISPL) ~A ~A ~A" 
		"A3" _TCOL1 _TCOL0 (pop costvector) _TCOL1 _TCOL0c _TCOL1 _TCOL0e "LOOSELY ~SEG-DUR, 	+-1MISPELLED SEG-VAL" )   
	(format bound-stream "~%~A" _TCOL1)
	(format bound-stream "~%~A" _TROW1)
	(format bound-stream "~%~A" _TROW0)
	(format bound-stream "~%~A" _TCOL0)
	(format bound-stream "~%~4A ~A ~A TOTAL NUM: ~D ~A ~A (SPANNING_MISPL) ~A ~A ~A" 
		"A4" _TCOL1 _TCOL0 (pop costvector) _TCOL1 _TCOL0c _TCOL1 _TCOL0e "SPANNING SEG-DUR, 	+-1MISPELLED SEG-VAL" )   
	(format bound-stream "~%~A" _TCOL1)
	(format bound-stream "~%~A" _TROW1)
	(format bound-stream "~%~A" _TABLE1)
	(format bound-stream "~%<BR>" nil))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_1DNA_base_comparator_prettyprint ( a b )
	(if (equal a b)
		(setq hps_tuple (list a b ))
		(setq hps_tuple (list "<FONT COLOR=#00CFFF> " a "</FONT>" 
				      "<FONT COLOR=#FF0000> " b "</FONT>" )))

	(return-from HPS_1DNA_base_comparator_prettyprint hps_tuple))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_1DNA_base_prettyprint ( hps_1DNA_comparison_sequence )
	(setq *HPS_1DNA_matching_coded_comparison_pretty_print_xyz* 	
	      (mapcar #'(lambda (a) (HPS_1DNA_base_comparator_prettyprint (first a) 
									  (second a))) 
		      hps_1DNA_comparison_sequence))

	(return-from HPS_1DNA_base_prettyprint *HPS_1DNA_matching_coded_comparison_pretty_print_xyz* ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(setq *HPS_DNA_SIZE_1DNA_PRINTOUT* 20)
(defun HPS_DNA_finegrain_alignment_report ( p patnum hps_alignment_index 
					             hps_matched_1DNA_bases_sequence 
					             hps_matched_1DNA_coded_sequence 
					             bound-stream 
						     &key (ADJUSTED_SEQUENCE_FLAG nil))

  	(when (NOT ADJUSTED_SEQUENCE_FLAG)
		(format bound-stream "~%<H3> FINEGRAIN ALIGNMENT WITHIN HPS INPUT SEQUENCE </H3>" nil))

  	(when (NOT ADJUSTED_SEQUENCE_FLAG)
	    (with-output-to-string ( *HPS_internal_buffer* ) 
	      	(let 	((alist '( DNA_SEQUENCE DESCRIPTION SIGNATURE_STARTS MATCHING_STARTS_AT SIGNATURE_ENDS MATCH_ENDS_AT ))
			 (blist  (list	(format nil "HPS INPUT SEQUENCE"  		nil) 
					(format nil "(SIGNATURE + TESTSEQUENCE)"  	nil) 
					(format nil "SIGNATURE BP=<B>~D</B>" 		*HPS_1DNA_endpoint_signature_start* )
					(format nil "TESTSEQUENCE BP=<B>~D</B>)" 	(HPS_DNA_pattern_alignment_start_for p hps_alignment_index))       
					(format nil "SIGNATURE BP=<B>~D</B>)" 		*HPS_1DNA_endpoint_signature_end*   )
					(format nil "TESTSEQUENCE=<B>~D</B>)" 		(HPS_DNA_pattern_alignment_end_for   p hps_alignment_index))))
	     	      	 (clist	 (list  (format nil "DNA SIGNATURE"   			nil)
					(format nil "    ---      "			nil) 
					(format nil "BP-INDEX=<B>~D</B>" 		*HPS_1DNA_endpoint_signature_start* )
					(format nil "    ---      "			nil) 
					(format nil "BP-INDEX=<B>~D</B>" 		*HPS_1DNA_endpoint_signature_end* )
					(format nil "    ---      "			nil)))
			 (dlist  (list  (format nil "DNA TEST SEQUENCE"		        nil)
					(format nil "SUBSEQUENCE M~D"   		patnum )
					(format nil "      ----      "                	nil )
					(format nil "BP-INDEX=<B>~D</B>" 		(- (HPS_DNA_pattern_alignment_start_for p hps_alignment_index)
										   	*HPS_1DNA_endpoint_signature_end* ))       
					(format nil "      ----      "                	nil )
					(format nil "BP-INDEX=<B>~D</B>" 		(- (HPS_DNA_pattern_alignment_end_for   p hps_alignment_index)
										   	*HPS_1DNA_endpoint_signature_end* )))))

			(setq HPS_table_print_format (format nil "~%~A ~A ~A ~A ~A" "~{" _TCOL0a "~A"  _TCOL1 "~}"))

			;; print out the table rows
			(format *HPS_internal_buffer* "~%~A" _TABLE0)
			(format *HPS_internal_buffer* "~%~A" _TROW0)
				(format *HPS_internal_buffer* HPS_table_print_format alist )
			(format *HPS_internal_buffer* "~%~A" _TROW1)
			(format *HPS_internal_buffer* "~%~A" _TROW0)
				(format *HPS_internal_buffer* HPS_table_print_format blist )
			(format *HPS_internal_buffer* "~%~A" _TROW1)
			(format *HPS_internal_buffer* "~%~A" _TROW0)
				(format *HPS_internal_buffer* HPS_table_print_format clist )
			(format *HPS_internal_buffer* "~%~A" _TROW1)
			(format *HPS_internal_buffer* "~%~A" _TROW0)
				(format *HPS_internal_buffer* HPS_table_print_format dlist )
			(format *HPS_internal_buffer* "~%~A" _TROW1)
			(format *HPS_internal_buffer* "~%~A" _TABLE1)

	        	(setq *HPS_DNA_internal_description_inputs* (get-output-stream-string *HPS_internal_buffer*))))

		(format bound-stream "~%~A~%" *HPS_DNA_internal_description_inputs* ))



	(setq *HPS_1DNA_matching_coded_comparison_pretty_print* 	(HPS_1DNA_base_prettyprint hps_matched_1DNA_bases_sequence ))

	(setq *HPS_1DNA_alignment_index_within_testseq* 		(HPS_DNA_true_alignment_sig2pat p hps_alignment_index))

	(let* ((titlemsg_adj 	(format nil "~A M~A ~A"	"ADJUSTED ALIGNMENT BETWEEN DNA SIGNATURE AND" patnum
							"<BR>DUE TO DELETED (D) OR INSERTED (I) BURST DNA DAMAGE" ))
	       (titlemsg_org    (format nil "M~D ~A [~A] ~A [~A]" patnum "ALIGNMENT AT GENOMIC ADDRESS"
									  *HPS_1DNA_alignment_index_within_testseq*
									  "<BR>FOR TEST SEQUENCE"
			        					  *HPS_1DNA_testsequence_series_headername* ))
  	       (titlemsg 	(if ADJUSTED_SEQUENCE_FLAG titlemsg_adj titlemsg_org))
	       (hps_headermsg   (format nil "(SIG:M~D): " patnum)))

	       (HPS_pretty_printer 	*HPS_1DNA_matching_coded_comparison_pretty_print* 
			     		bound-stream 
			    		:NCHARS 		*HPS_DNA_SIZE_1DNA_PRINTOUT* 
			    		:HEADERMSG  	 	 hps_headermsg
					:INDEX_DISPLACEMENT	*HPS_1DNA_alignment_index_within_testseq*
					:TITLE			titlemsg
			    		:HTMLMODE	 	t ))


	(HPS_timeseries_printer 	hps_matched_1DNA_coded_sequence 
					(format nil "~A~A~D.DNA" *HPS_DNA_outputdir* "HPS_1DNA_CODED_FINEGRAIN_XY_ALIGNMENT_M" patnum )
					:XY_HEADERLINE 	"i	sig-base	pat-base	sig_idx_base	seg_idx_base	rel_index")

	(return-from HPS_DNA_finegrain_alignment_report  hps_alignment_index ))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_HTML_print_specific_matching_pattern_base_alignment ( p patnum bound-stream  )
	(setq hps_alignment_dataset	(HPS_DNA_finegrain_alignment p 
								     patnum 
								     bound-stream 
								    :GENERATE_REPORT t ))

	(setq hps_alignment_index 	(HPS_get_alignment_dataset_field hps_alignment_dataset :field 'INDEX))

	(return-from HPS_DNA_HTML_print_specific_matching_pattern_base_alignment  hps_alignment_index))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; writes a short summary report of the pattern mining effort based on the HPS approximate shape matching
;; ***********************************************************************************************************************
(defun HPS_DNA_generate_pattern_mining_report ( HPS_matching_patterns_by_rank 
						HPS_matching_patterns_by_time 
						HPS_DNA_main_report_filename 
						&key (HPS_TERSE 		t)
						     (HPS_DNA_TABBED_REPORT     t))

    ;; a very general description of the input DNA test sequence and DNA signature
    (with-output-to-string ( HPS-main-report-buffer ) 
      	(let 	((alist (list "DNA_INPUT_SEQUENCE" "NUMBER_OF_BASES" 
			      "DESCRIPTION_OF_THE_DNA_INPUT_SEQUENCE" 
			      "FILENAME_CONTAINING_THE_INPUT_SEQUENCE"))
		 (blist (list "DNA_SIGNATURE" (length *HPS_1DNA_signature_bases*) 
		        *HPS_1DNA_signature_series_headername* 
		        *HPS_1DNA_signature_series_filename*))
     	      	 (clist (list "DNA TEST SEQUENCE" (length *HPS_1DNA_testsequence_bases*) 
		        *HPS_1DNA_testsequence_series_headername* 
		        *HPS_1DNA_testsequence_series_filename*)))

		(setq HPS_table_print_format (format nil "~%~A ~A ~A ~A ~A" "~{" _TCOL0c "~A"  _TCOL1 "~}"))

		(format HPS-main-report-buffer "~%~A" _TABLE0)
		(format HPS-main-report-buffer "~%~A" _TROW0)
			(format HPS-main-report-buffer HPS_table_print_format alist )
		(format HPS-main-report-buffer "~%~A" _TROW1)
		(format HPS-main-report-buffer "~%~A" _TROW0)
			(format HPS-main-report-buffer HPS_table_print_format blist )
		(format HPS-main-report-buffer "~%~A" _TROW1)
		(format HPS-main-report-buffer "~%~A" _TROW0)
			(format HPS-main-report-buffer HPS_table_print_format clist )
		(format HPS-main-report-buffer "~%~A" _TROW1)
		(format HPS-main-report-buffer "~%~A" _TABLE1)
        	(setq *HPS_DNA_internal_description_inputs* (get-output-stream-string HPS-main-report-buffer))))

    ;; the contents of the TOP LEVEL (MAIN / SUMMARY) report
    (with-output-to-string ( HPS-main-report-buffer ) 
	;; print a header for the HTML report
	(format HPS-main-report-buffer "~%~A~%" *HPS_DNA_HTML_MAIN_HEADER_TOPLEVEL* )

		;; (dolist (p HPS_matching_patterns_by_rank 'BEST_PATTERN_MATCHES)
		(dolist (p HPS_matching_patterns_by_time 'BEST_PATTERN_MATCHES)
			;; ******************************************************************************************************************
			;; print header
			;; ******************************************************************************************************************
			(setq hps_patnum_temp   (HPS_DNA_rank_of_pattern_in 	p 
										HPS_matching_patterns_by_time ))

			(setq *HPS_DNA_detailed_filename* (format nil "~A_M~D.HTM" 
								      "HPS_DNA_BESTMATCHING_PATTERNS_DETAILED" 
								       hps_patnum_temp )) 
			;; ******************************************************************************************************************


			;; ******************************************************************************************************************
	  		;; forward reference to the detailed report for this pattern
			;; ******************************************************************************************************************
			(format HPS-main-report-buffer 
				"~%<BR> ~%<H3> <A HREF=~A TARGET=_BLANK> M~D:  DETAILED REPORT FOR MATCHING INSTANCE </A> </H3>"
									       *HPS_DNA_detailed_filename* 
										hps_patnum_temp	)
			;; ******************************************************************************************************************


			;; ******************************************************************************************************************
			;; print general stats about the specified pattern
			;; ******************************************************************************************************************
  			(HPS_DNA_HTML_print_specific_matching_pattern_stats     p 
							        		HPS-main-report-buffer )
			;; ******************************************************************************************************************


			;; *****************************************************************************************************
			;; print the alignment adjustment vector
			;; *****************************************************************************************************
			(format HPS-main-report-buffer "~%<IMG SRC=HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M~D.PNG> <BR><BR>" 
				hps_patnum_temp   )
			;; *****************************************************************************************************


			(when (NOT HPS_TERSE)
				;; **********************************************************************************************************
				;; prints cost vector metrics for this pattern
				;; **********************************************************************************************************
				(HPS_DNA_print_costvector_metrics_for p hps_patnum_temp HPS-main-report-buffer )))
			;; **********************************************************************************************************


		;; ******************************************************************************************************************
  		;; print summary statistics about the WHOLE matching process - CHECK !!!!!!!!
		;; ******************************************************************************************************************
		;; (HPS_DNA_match_strength_summary_stats HPS-main-report-buffer )
		;; ******************************************************************************************************************

	    ;; print a footer for the HTML report
	    (format HPS-main-report-buffer "~%~A~%" *HPS_DNA_HTML_MAIN_FOOTER* )

    	    (setq *HPS_DNA_internal_summary_report* (get-output-stream-string HPS-main-report-buffer)))

    (when HPS_DNA_TABBED_REPORT
	(HPS_DNA_HTML_tab_printer "HPS_FOREWORD.HTM"			0 	
				  :TABARG1 (format nil (HPS_HTML_TAB_FOREWORD) nil))
	(HPS_DNA_HTML_tab_printer "HPS_INTRODUCTION.HTM"		1 	
				  :TABARG1 (format nil (HPS_HTML_TAB_INTRODUCTION) nil)) 
	(HPS_DNA_HTML_tab_printer "HPS_INPUTS.HTM"			2 	
				  :TABARG1 (format nil (HPS_HTML_TAB_INPUTS) *HPS_DNA_internal_description_inputs*)) 
	(HPS_DNA_HTML_tab_printer "HPS_DATA_DESCRIPTION.HTM"		3 	
				  :TABARG1 (format nil (HPS_HTML_TAB_OPTIONAL) nil))
	(HPS_DNA_HTML_tab_printer "HPS_BEHAVIOR.HTM"			4 	
				  :TABARG1 (format nil (HPS_HTML_TAB_BEHAVIOR) (HPS_DNA_MATCH_BEHAVIOR_COMPONENT)))
	(HPS_DNA_HTML_tab_printer "HPS_MATCHING_INSTANCES.HTM"		5 	
				  :TABARG1 (format nil (HPS_HTML_TAB_MATCHES) nil))
	(HPS_DNA_HTML_tab_printer "HPS_RECONSTRUCTION_OPS.HTM"		6 	
				  :TABARG1 (format nil (HPS_HTML_TAB_RECONSTRUCTION) *HPS_DNA_internal_summary_report*))
	(HPS_DNA_HTML_tab_printer "HPS_SIMILARITY_PLOTS.HTM"		7 	
				  :TABARG1 (format nil (HPS_HTML_TAB_SIMILARITY) nil))
	(HPS_DNA_HTML_tab_printer "HPS_PHILOGRAM_PLOT.HTM"		8 	
				  :TABARG1 (format nil (HPS_HTML_TAB_PHILOGRAM) nil))
	(HPS_DNA_HTML_tab_printer "HPS_MOTIF_SEARCH.HTM"		9 	
				  :TABARG1 (format nil (HPS_HTML_TAB_MOTIF_ID) nil))
	(HPS_DNA_HTML_tab_printer "HPS_MOTIF_REGEXPR.HTM"		10 	
				  :TABARG1 (format nil (HPS_HTML_TAB_MOTIF_MINER) nil))
	(HPS_DNA_HTML_tab_printer "HPS_ABOUT_US.HTM"			11 	
				  :TABARG1 (format nil (HPS_HTML_TAB_ABOUT_US) nil))
	(HPS_DNA_HTML_tab_printer "HPS_DISCLAIMER.HTM"			12 	
				  :TABARG1 (format nil (HPS_HTML_TAB_DISCLAIMER) nil)))

    (when (NOT HPS_DNA_TABBED_REPORT)
	    (with-open-file (report-stream HPS_DNA_main_report_filename :direction :output :if-exists :supersede :if-does-not-exist :create)
			(format report-stream (HPS_HTML_DNA_MAIN_REPORT) *HPS_DNA_internal_description_inputs* 
									 (HPS_DNA_MATCH_BEHAVIOR_COMPONENT)
									 *HPS_DNA_internal_summary_report*))))
						

;; ***********************************************************************************************************************
(defun HPS_DNA_HTML_tab_printer ( filename tabindex &key (TABARG1 nil) 
					   		 (TABARG2 nil)) 

  	(let (( fullpath_filename 	 (format nil "~A~A" *HPS_DNA_outputdir* filename ))
  	      ( tabindex_array	 	'( nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)))

	    (setq tabindex_array 	(fill tabindex_array '"class=unselected")) 
	    (setf (nth tabindex tabindex_array) "class=selected")
	      
	    (with-open-file (report-stream fullpath_filename :direction :output :if-exists :supersede :if-does-not-exist :create)
			(format report-stream (HPS_DNA_TABBED_REPORT_NEWTAB)  
				(nth 0 tabindex_array)   (nth 1 tabindex_array)  (nth 2 tabindex_array)   (nth 3 tabindex_array)  
				(nth 4 tabindex_array)   (nth 5 tabindex_array)  (nth 6 tabindex_array)   (nth 7 tabindex_array)  
				(nth 8 tabindex_array)   (nth 9 tabindex_array)  (nth 10 tabindex_array)  (nth 11 tabindex_array)  
				(nth 12 tabindex_array) 
				TABARG1))))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defun HPS_DNA_print_report ( HPS_matching_patterns_by_rank 
			      HPS_matching_patterns_by_time 
			      &key (HPS_TERSE nil))

	(when (NOT HPS_TERSE) 
		;; print all the segtables, these are needed? for subsequent sequence alignment plots?
		(format t "~% (OPTIONAL) WRITING ALL HPS SEGMENT TABLES: [~A] type files" "HPS_*_SEGTABLE.DNA")
		(HPS_DNA_print_all_segtables 		 HPS_matching_patterns_by_time )

		;; generate histograms datafiles for all matching patterns as well as the signature,  to the specified stream
		(format t "~% GENERATING LAMBDA, TVAL HISTOGRAMS       : [~A] type files" "HPS_DNA_*_HISTOGRAM_*.DNA" )
		(HPS_DNA_generate_histograms_for  	 HPS_matching_patterns_by_time ))


	;; creates a histogram of the pattern matching strength signal
	(format t "~% GENERATING MATCH BEHAVIOR ANALYSIS DATA  : [~A]" "HPS_DNA_MATCHBEHAVIOR_HISTOGRAM.DNA" )
	(HPS_DNA_match_strength_analysis "across ALL (not just the best) patterns" )
  	
	;; filenames for a brief summary report as well as the basename of the detailed reports generated for each pattern match
	(setq *HPS_DNA_main_report_filename*     (HPS_concat *HPS_DNA_outputdir* "HPS_DNA_BESTMATCHING_PATTERNS.HTM")) 
	(setq *HPS_DNA_detailed_report_filename* (HPS_concat *HPS_DNA_outputdir* "HPS_DNA_BESTMATCHING_PATTERNS_DETAILED" )) 

	;; print a HTML detailed reports for each matching pattern of the pattern discovery data mining analysis
	(format t "~% GENERATING REPORT FOR HPS PATTERN MINING : [~A]" *HPS_DNA_main_report_filename* )
	(HPS_DNA_generate_pattern_mining_report  HPS_matching_patterns_by_rank 
						 HPS_matching_patterns_by_time 
						 *HPS_DNA_main_report_filename* 
						 :HPS_TERSE		    HPS_TERSE
						 :HPS_DNA_TABBED_REPORT     t)

	;; print a detailed pattern mining reports for the ranked pattern matches
	(format t "~% GENERATING DETAILED REPORT FOR P-MATCHES : [~A]-type files" *HPS_DNA_detailed_report_filename* )

	(setq hps_patnum 0)
	(dolist (p HPS_matching_patterns_by_time 'BEST_PATTERN_MATCHES)

		(setq *HPS_DNA_detailed_filename* (format nil "~A_M~D.HTM" *HPS_DNA_detailed_report_filename* hps_patnum )) 
		(with-open-file (report-stream *HPS_DNA_detailed_filename* :direction :output :if-exists :supersede :if-does-not-exist :create)
		  (with-output-to-string ( HPS-report-buffer )
			;; *****************************************************************************************************
			;; print a header for the HTML report
			;; *****************************************************************************************************
			(format HPS-report-buffer "~%~A~%" *HPS_DNA_HTML_MAIN_HEADER_TOPLEVEL* )
			(format HPS-report-buffer "~%<BR> ~%<H3> <A HREF=~A_M~D.HTM> M~D: FINEGRAIN ALIGNMENT DETAILS </A> </H3>"
									        "HPS_DNA_BESTMATCHING_PATTERNS_DETAILED"
										hps_patnum 
										hps_patnum )

			;; (princ (get-output-stream-string HPS-report-buffer) report-stream)
			(write (get-output-stream-string HPS-report-buffer) :stream report-stream :escape nil :readably nil)
			;; *****************************************************************************************************


			;; *****************************************************************************************************
			;; print the alignment adjustment vector
			;; *****************************************************************************************************
			(format HPS-report-buffer "~%<IMG SRC=HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M~D.PNG> <BR><BR>" hps_patnum)

			;; (princ (get-output-stream-string HPS-report-buffer) report-stream)
			(write (get-output-stream-string HPS-report-buffer) :stream report-stream :escape nil :readably nil)
			;; *****************************************************************************************************


			;; *****************************************************************************************************
  			;; print summary statistics about the matching pattern
			;; *****************************************************************************************************
  			(HPS_DNA_HTML_print_specific_matching_pattern_stats     p 
								        	HPS-report-buffer)

			;; (princ (get-output-stream-string HPS-report-buffer) report-stream)
			(write (get-output-stream-string HPS-report-buffer) :stream report-stream :escape nil :readably nil)
			;; *****************************************************************************************************


			;; *****************************************************************************************************
			;; print a cross comparison of the actual bases report
			;; *****************************************************************************************************
			(setq hps_alignment_index 	
			      (HPS_DNA_HTML_print_specific_matching_pattern_base_alignment 	
			  							p 
										hps_patnum 
										HPS-report-buffer))

			;; (princ (get-output-stream-string HPS-report-buffer) report-stream)
			(write (get-output-stream-string HPS-report-buffer) :stream report-stream :escape nil :readably nil)
			;; *****************************************************************************************************


			;; *****************************************************************************************************
			;; control the release of HPS disclosure-sensitive content
			;; *****************************************************************************************************
			(when (NOT HPS_TERSE)
				;; *****************************************************************************************************
				;; print a formatted color-coded table for the matching pattern
				;; *****************************************************************************************************
				(format HPS-report-buffer "~%<BR> ~%<H3> DETAILED REPORT FOR MATCHING INSTANCE M~D </H3>"
											hps_patnum )
	
				(HPS_DNA_HTML_print_specific_matching_pattern_maptable  p 
									    		HPS-report-buffer
									        	:HPS_DNA_terse t )
	
				;; (princ (get-output-stream-string HPS-report-buffer) report-stream)
				(write (get-output-stream-string HPS-report-buffer) :stream report-stream :escape nil :readably nil)
				;; *****************************************************************************************************
	
	
				;; *****************************************************************************************************
				;; insert the sequence alignment labeled comparison plots
				;; *****************************************************************************************************
				(setq *HPS_DNA_detailed_filename_plot_1* (format nil "~A_M~D.PNG" 
										 	"HPS_DNA_BESTMATCHING_PATTERNS_DETAILED_LABELED" 
										  	hps_patnum )) 
	
				(format HPS-report-buffer (HPS_DNA_DETAILED_PLOT) 	hps_patnum
											"DETAILED SEQUENCE ALIGNMENT COMPARISON PLOT"
											*HPS_DNA_detailed_filename_plot_1*  )
	
				;; (princ (get-output-stream-string HPS-report-buffer) report-stream)
				(write (get-output-stream-string HPS-report-buffer) :stream report-stream :escape nil :readably nil)
				;; *****************************************************************************************************
	
	
				;; *****************************************************************************************************
				;; insert the sequence alignment histograms and pairwise cross reference plots
				;; *****************************************************************************************************
				;; (format HPS-report-buffer (HPS_HTML_VERTICAL_WHITESPACE) 150 150)
				(setq *HPS_DNA_detailed_filename_plot_2* (format nil "~A_M~D.PNG" 
										 	"HPS_DNA_BESTMATCHING_PATTERNS_DETAILED_PAIRWISE" 
										  	hps_patnum )) 
											  
				(format HPS-report-buffer (HPS_DNA_DETAILED_PLOT) 	hps_patnum
											"PAIRWISE SEQUENCE ALIGNMENT PLOT AND HISTOGRAMS"
											*HPS_DNA_detailed_filename_plot_2*  )
	
				;; (princ (get-output-stream-string HPS-report-buffer) report-stream)
				(write (get-output-stream-string HPS-report-buffer) :stream report-stream :escape nil :readably nil))
				;; *****************************************************************************************************


			(unless (NOT HPS_TERSE)
				(format HPS-report-buffer "~%<BR> ~%<H3> SECTIONS BELOW CENSURED FROM DISCLOSURE AT THIS TIME </H3>" nil)

				;; (princ (get-output-stream-string HPS-report-buffer) report-stream)
				(write (get-output-stream-string HPS-report-buffer) :stream report-stream :escape nil :readably nil) )
			;; *****************************************************************************************************
	

			;; *****************************************************************************************************
			;; print a footer for the HTML report
			;; *****************************************************************************************************
			(format HPS-report-buffer "~%~A~%" *HPS_DNA_HTML_MAIN_FOOTER* )

			;; (princ (get-output-stream-string HPS-report-buffer) report-stream)))
			(write (get-output-stream-string HPS-report-buffer) :stream report-stream :escape nil :readably nil)))
			;; *****************************************************************************************************

		(incf hps_patnum))

	;; feedback a brief summary of the data mining analysis to the user
	(format t "~&~A" "__________________________________________________________________________________")
	(format t "~&~A" "MATCHING INSTANCES BETWEEN SIGNATURE AND TEST SEQUENCE" )
	;; (dolist (p HPS_matching_patterns_by_rank 'BEST_PATTERN_MATCHES)
	(dolist (p HPS_matching_patterns_by_time 'BEST_PATTERN_MATCHES)
  			(HPS_DNA_HTML_print_specific_matching_pattern_stats     p 	
										t 
										:HPS_DNA_print_to_screen t))
	(format t "~&~A" "__________________________________________________________________________________"))
;; ***********************************************************************************************************************



