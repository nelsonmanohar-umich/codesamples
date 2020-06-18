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
(defun HPS_DNA_SEQUENCE_ANALYZER ( HPS_SEQUENCE &key (HPS_SEQUENCE_NUMBER   1786520)
			  		    	     (HPS_SIGNSEQUENCE_FROM 5000)
				  		     (HPS_SIGNSEQUENCE_TO   6000)
				  		     (HPS_TERSE		    t))
	     
  	(with-output-to-string ( HPS-analyzer-buffer )
	  (let* (( HPS_1DNA_bases 		(mapcar #'(lambda (a) (second a)) HPS_SEQUENCE))
	         ( HPS_1DNA_codes 		(mapcar #'(lambda (a) (HPS_DNA_mapcode a)) HPS_SEQUENCE))
		 ( HPS_3DNA_codes 		(HPS_specialized_conditioning HPS_1DNA_bases* :COND_TYPE '3DNA ))
		 ( hps_num_as			(length (remove-if-not #'(lambda (a) (equal a 'A)) HPS_1DNA_bases)))
		 ( hps_num_cs			(length (remove-if-not #'(lambda (a) (equal a 'C)) HPS_1DNA_bases)))
		 ( hps_num_gs			(length (remove-if-not #'(lambda (a) (equal a 'G)) HPS_1DNA_bases)))
		 ( hps_num_ts			(length (remove-if-not #'(lambda (a) (equal a 'T)) HPS_1DNA_bases)))
		 ( motif_colors			'( light-blue light-red light-green ))  
		 ( sequence_len			(length HPS_1DNA_bases))
		 ( bin_elems			 nil)
		 ( hps_dna_histogram_file	(format nil "~A~A" *HPS_OUTPUT_PATH* "HPS_3DNA_SEQUENCE_ANALYSIS.DNA"))
		 ( hps_3DNA_histogram_dataset	(HPS_histogram HPS_3DNA_codes hps_dna_histogram_file :NUMBINS 64))
		 ( top_3DNA_motifs_dataset 	(subseq (HPS_histogram_sort_by_frequencies hps_3DNA_histogram_dataset 0 4))))

	    	(dolist (motif top_3DNA_motifs_dataset 'MOTIFSUBSTITUTION)
		  	(setq bin_elems 	(fourth motif))
			(setq motif_color	(pop motif_colors))

		  	(dolist (bin_elem bin_elems 'MOTIFSPOSITIONS) 
			  	(format HPS-analyzer-buffer "~%	set arrow from ~D, ~D to ~D, ~D lc rgb '~A'" 
					bin_elem 9 bin_elem 0 motif-color)))

	  	(format HPS-analyzer-buffer "set label \"~A\" at ~D, ~D" 
			(format nil "|~A|=~D" STENO_getmapletter (round (second (pop top_3DNA_motifs_dataset))) 10 100))
	  	(format HPS-analyzer-buffer "set label \"~A\" at ~D, ~D" 
			(format nil "|~A|=~D" STENO_getmapletter (round (second (pop top_3DNA_motifs_dataset))) 9  100))
	  	(format HPS-analyzer-buffer "set label \"~A\" at ~D, ~D" 
			(format nil "|~A|=~D" STENO_getmapletter (round (second (pop top_3DNA_motifs_dataset))) 8  100))
	  	(format HPS-analyzer-buffer "set label \"~A\" at ~D, ~D" 
			(format nil "|~A|=~D" STENO_getmapletter (round (second (pop top_3DNA_motifs_dataset))) 7  100))
	  	(format HPS-analyzer-buffer "set label \"~A\" at ~D, ~D" (format nil "|A|=~D" hps_num_as) 10 (- sequence_len 100))
	  	(format HPS-analyzer-buffer "set label \"~A\" at ~D, ~D" (format nil "|C|=~D" hps_num_cs) 9  (- sequence_len 100))
	  	(format HPS-analyzer-buffer "set label \"~A\" at ~D, ~D" (format nil "|G|=~D" hps_num_gs) 8  (- sequence_len 100))
	  	(format HPS-analyzer-buffer "set label \"~A\" at ~D, ~D" (format nil "|T|=~D" hps_num_ts) 7  (- sequence_len 100)))

	  (setq *HPS_DNA_sequence_analyzer_report* (get-output-stream-string HPS-analyzer-buffer ))) 

	(return-from HPS_DNA_SEQUENCE_ANALYZER *HPS_DNA_sequence_analyzer_report* ))
;; ****************************************************************************************************************











