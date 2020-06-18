(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; ********************************************************************************************************
;; *****************************   DNA TIME SERIES HPS-CONDITIONING ENCODER   *****************************
;;     functions within apply various specialized conditioning transform to an input time series
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


;; ********************************************************************************************************
;; this function codifies a DNA base into a fixed code in the range 0-3 (the particular encoding has to be checked)
;; ********************************************************************************************************
(defmacro HPS_DNA_base ( base )
	`(cond  ((equal ,base 'a) 0 )
	  	((equal ,base 'c) 1 )
	  	((equal ,base 'g) 2 )
	  	((equal ,base 't) 3 )
	  	((equal ,base 'd) 4 )
	  	((equal ,base 'i) 5 )
		(t                nil) ))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function codifies three consecutive DNA bases (b3 b2 b1) 
;; 	where b1 represents the current time index i and 
;; 	      b2 the previous time index (i-1) and 
;; 	      b3 the previous to that (i-2), 
;; into a codified number using base 4. As a result, all 64 possible triplets are codified into range 0-63.
;; ********************************************************************************************************
(defmacro HPS_1DNA_encoder ( b1 )
        `(let*  ((nb1 (HPS_DNA_base ,b1)))
		 (setq *HPS_nval_3DNA_coding* (* nb1 1))))
(defmacro HPS_2DNA_encoder ( b2 b1 )
        `(let*  ((nb2 (HPS_DNA_base ,b2)) 
		 (nb1 (HPS_DNA_base ,b1)))
		 (setq *HPS_nval_3DNA_coding* (+ (* nb2 4) (* nb1 1)))))
; ;; ********************************************************************************************************
(defmacro HPS_3DNA_encoder ( b3 b2 b1 )
        `(let*  ((nb3 (HPS_DNA_base ,b3)) 
		 (nb2 (HPS_DNA_base ,b2)) 
		 (nb1 (HPS_DNA_base ,b1)))
		 (setq *HPS_nval_3DNA_coding* (+ (* nb3 16) (* nb2 4) (* nb1 1)))))
; ;; ********************************************************************************************************
(defmacro HPS_4DNA_encoder ( b4 b3 b2 b1 )
        `(let*  ((nb4 (HPS_DNA_base ,b4)) 
		 (nb3 (HPS_DNA_base ,b3)) 
		 (nb2 (HPS_DNA_base ,b2)) 
		 (nb1 (HPS_DNA_base ,b1)))
		 (setq *HPS_nval_3DNA_coding* (+ (* nb4 64) (* nb3 16) (* nb2 4) (* nb1 1)))))
; ;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function takes a current sequential DNA memory tuple, in this case fixed to be a ternary tuple, and 
;; returns the corresponding base 4 encoding for it
;; ********************************************************************************************************
(defmacro HPS_2DNA_encode ( DNA_memory_tuple )
  	`(HPS_2DNA_encoder (second ,DNA_memory_tuple) (first ,DNA_memory_tuple)))
(defmacro HPS_3DNA_encode ( DNA_memory_tuple )
  	`(HPS_3DNA_encoder (third ,DNA_memory_tuple) (second ,DNA_memory_tuple) (first ,DNA_memory_tuple)))
(defmacro HPS_4DNA_encode ( DNA_memory_tuple )
  	`(HPS_4DNA_encoder (fourth ,DNA_memory_tuple) (third ,DNA_memory_tuple) (second ,DNA_memory_tuple) (first ,DNA_memory_tuple)))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function applies various specialized conditioning transform to an input time series
;; ********************************************************************************************************
(defun HPS_specialized_conditioning ( atimeseries 
				      &key (COND_TYPE '3DNA) )
	(setq *HPS_conditioned_ts* nil)

	(setq *iter* 0)
	(cond   ((equal COND_TYPE '1DNA)
			(setq *HPS_conditioned_ts* (mapcar #'(lambda (a) (HPS_DNA_base a)) atimeseries)))

	  	((equal COND_TYPE '2DNA)
		 	(dolist (datum atimeseries '2DNAWORD_ENCODER)
				(incf *iter*)
				(when (< *iter* 2)
				  	(cond 	((equal *iter* 1) 
				  			(setq *HPS_3DNA* (list datum datum datum))
							(setq *HPS_nval* (HPS_2DNA_encode *HPS_3DNA*)))
					  	(t nil)))

				(unless (< *iter* 2)
				  	(setq *HPS_3DNA* (list datum (first *HPS_3DNA*)))
					(setq *HPS_nval* (HPS_2DNA_encode *HPS_3DNA*)))

				(push *HPS_nval* *HPS_conditioned_ts*))

			(setq *HPS_conditioned_ts* (reverse *HPS_conditioned_ts*)))

	  	((equal COND_TYPE '3DNA)
		 	(dolist (datum atimeseries '3DNAWORD_ENCODER)
				(incf *iter*)
				(when (< *iter* 3)
				  	(cond 	((equal *iter* 1) 
				  			(setq *HPS_3DNA* (list datum datum datum))
							(setq *HPS_nval* (HPS_3DNA_encode *HPS_3DNA*)))
					  	((equal *iter* 2)
				  			(setq *HPS_3DNA* (list datum (first *HPS_3DNA*) (second *HPS_3DNA*)))
							(setq *HPS_nval* (HPS_3DNA_encode *HPS_3DNA*)))
					  	(t nil)))

				(unless (< *iter* 3)
				  	(setq *HPS_3DNA* (list datum (first *HPS_3DNA*) (second *HPS_3DNA*)))
					(setq *HPS_nval* (HPS_3DNA_encode *HPS_3DNA*)))

				(push *HPS_nval* *HPS_conditioned_ts*))

			(setq *HPS_conditioned_ts* (reverse *HPS_conditioned_ts*)))

	       ((equal COND_TYPE '4DNA)
		 	(dolist (datum atimeseries '4DNAWORD_ENCODER)
				(incf *iter*)
				(when (< *iter* 4)
				  	(cond 	((equal *iter* 1) 
				  			(setq *HPS_4DNA* (list datum datum datum datum))
							(setq *HPS_nval* (HPS_4DNA_encode *HPS_4DNA*)))
					  	((equal *iter* 2)
				  			(setq *HPS_4DNA* (list datum datum (first *HPS_4DNA*) (second *HPS_4DNA*)))
							(setq *HPS_nval* (HPS_4DNA_encode *HPS_4DNA*)))
					  	((equal *iter* 3)
				  			(setq *HPS_4DNA* (list datum (first *HPS_4DNA*) (second *HPS_4DNA*) (third *HPS_4DNA*)))
							(setq *HPS_nval* (HPS_4DNA_encode *HPS_4DNA*)))
					  	(t nil)))

				(unless (< *iter* 4)
				  	(setq *HPS_4DNA* (list datum (first *HPS_4DNA*) (second *HPS_4DNA*) (third *HPS_4DNA*)))
					(setq *HPS_nval* (HPS_4DNA_encode *HPS_4DNA*)))

				(push *HPS_nval* *HPS_conditioned_ts*))

			(setq *HPS_conditioned_ts* (nreverse *HPS_conditioned_ts*)))

	       ((equal COND_TYPE 'SQRT)
			(setq *HPS_conditioned_ts* (mapcar #'sqrt atimeseries)))

	       ((equal COND_TYPE 'LOG)
			(setq *HPS_conditioned_ts* (mapcar #'log atimeseries)))

	       (t 
			(setq *HPS_conditioned_ts* atimeseries)))

  	(return-from HPS_specialized_conditioning *HPS_conditioned_ts* ))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; ********************************************************************************************************
(defmacro HPS_DNA_maptobase ( base ) 
  `(cond ((equal ,base 0) 'a)
	 ((equal ,base 1) 'c)
	 ((equal ,base 2) 'g)
	 ((equal ,base 3) 't)
	 ((equal ,base 4) 'd)
	 ((equal ,base 5) 'i)))
;; ********************************************************************************************************
(defmacro HPS_2DNA_to_1DNA ( HPS_2DNA )
  `(let* ((dig1     (floor (/ ,HPS_2DNA 4)))
	 (rem1     (rem   ,HPS_2DNA 4))
	 (dig0     (floor rem1 1)))
    (list    (HPS_DNA_maptobase dig1) (HPS_DNA_maptobase dig0))))
;; ********************************************************************************************************
(defmacro HPS_3DNA_to_1DNA ( HPS_3DNA )
  `(let* ((dig2     (floor (/ ,HPS_3DNA 16)))
	 (rem2     (rem   ,HPS_3DNA 16))
	 (dig1     (floor rem2 4))
	 (rem1     (rem   rem2 4))
	 (dig0     (floor rem1 1)))
    (list   (HPS_DNA_maptobase dig2) (HPS_DNA_maptobase dig1) (HPS_DNA_maptobase dig0))))
;; ********************************************************************************************************
(defmacro HPS_4DNA_to_1DNA ( HPS_4DNA )
  `(let* ((dig3     (floor (/ ,HPS_4DNA 64)))
	 (rem3     (rem   ,HPS_4DNA 64))
	 (dig2     (floor rem3 16))
	 (rem2     (rem   rem3 16))
	 (dig1     (floor rem2 4))
	 (rem1     (rem   rem2 4))
	 (dig0     (floor rem1 1)))
    (list  (HPS_DNA_maptobase dig3) (HPS_DNA_maptobase dig2) (HPS_DNA_maptobase dig1) (HPS_DNA_maptobase dig0))))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function reads a DNA sequence from the specified file and writes a numerically suited encoded 
;; representation into the specified file
;; ********************************************************************************************************
(defun HPS_DNA_get_input_dataset ( &key ( HPS_INPUT_DATASET *HPS_DNA_input_dataset*) 
					( SERIESTYPE 'ALL ))
  	(cond 	((equal seriestype '1DNA_XY) 	(return-from HPS_DNA_get_input_dataset (first  hps_input_dataset)))
	  	((equal seriestype '1DNABASES) 	(return-from HPS_DNA_get_input_dataset (second hps_input_dataset)))
	  	((equal seriestype '1DNACODED) 	(return-from HPS_DNA_get_input_dataset (third  hps_input_dataset)))
	  	((equal seriestype '3DNACODED) 	(return-from HPS_DNA_get_input_dataset (fourth hps_input_dataset)))
	  	((equal seriestype '3DNABASES) 	(return-from HPS_DNA_get_input_dataset  nil))
	  	((equal seriestype 'ALL) 	(return-from HPS_DNA_get_input_dataset *HPS_DNA_input_dataset*))
	  	(T 				(return-from HPS_DNA_get_input_dataset *HPS_DNA_input_dataset*))))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; ********************************************************************************************************
(defun HPS_DNA_reader ( hps_input_dir hps_1dna_bases_sequence_file
				      hps_1dna_coded_sequence_file
				      hps_3dna_coded_sequence_file )

	;; read the DNA sequence from the specified file (in xy format)
  	(setq *HPS_1DNA_bases_xy_timeseries* (HPS_read_series 		   (format nil "~A~A" hps_input_dir hps_1dna_bases_sequence_file)))
	(setq *HPS_1DNA_bases_only* 	     (HPS_reader_get_file_column   *HPS_YCOLUMN* (HPS_get_timeseries_dataset)))
	(setq *HPS_3DNA_bases_coded* 	     (HPS_specialized_conditioning *HPS_1DNA_bases_only* :COND_TYPE '3DNA ))

	;; print the triplet DNA sequence numerically encoded into the specified file (in xy format)
	(HPS_timeseries_printer 	    *HPS_3DNA_bases_coded* 
	                        	    (format nil "~A~A" hps_input_dir hps_3dna_coded_sequence_file) 
					    :xy_headerline "i	ENCODED_3DNA")

	(setq *HPS_1DNA_input_timeseries_coded* (mapcar #'(lambda (a) (HPS_DNA_base a)) *HPS_1DNA_bases_only*))

	(HPS_timeseries_printer 	    *HPS_1DNA_input_timeseries_coded* 
	                        	    (format nil "~A~A" hps_input_dir hps_1dna_coded_sequence_file) 
					    :xy_headerline "i	ENCODED_1DNA")

	(setq *HPS_DNA_input_dataset* 	    (list 	*HPS_1DNA_bases_xy_timeseries* 
					    		*HPS_1DNA_bases_only* 
					    		*HPS_1DNA_input_timeseries_coded* 
					    		*HPS_3DNA_bases_coded* ))

	(return-from HPS_DNA_reader *HPS_DNA_input_dataset* ))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function conditions the input time series for windowed operators, this function is somewhat unnecessary and 
;; will be enhanced/removed later
;; ********************************************************************************************************
(defun HPS_conditioning (atimeseries)
	;; take a reduced look (we choose the initial (transient) stage) to derive an indication for the distortion
        (setq *HPS_small_fraction* 0.000001)
	(setq *HPS_initial_rmean*  (/ (reduce #'+ (subseq atimeseries 0 *HPS_FILLER_SIZE* )) *HPS_FILLER_SIZE* ))

	;; the variance degenerate case is handled through an addititve very small noise function, this decreases the
	;; likelihood of encountering a windowed outlook whose variance happens to be zero (i.e., the degenerate case)
	(if (zerop *HPS_initial_rmean*) 
	  	(setq *HPS_distortion* *HPS_small_fraction*)
        	(setq *HPS_distortion* (* *HPS_initial_rmean* *HPS_small_fraction* ))) 

	;; additive conditioning is applied to the entire time series (offline which has to be changed to online)
        (return-from HPS_conditioning (mapcar #'(lambda (a) (+ a *HPS_distortion*)) atimeseries)))
;; ********************************************************************************************************



;; ********************************************************************************************************
;; this function determines how much mutation has a base sequence suffered in terms of base differences
;; ********************************************************************************************************
(defun HPS_DNA_how_much_mutation ( from_sequence to_sequence )
  	(setq hps_dna_differences 	(mapcar #'(lambda (a b) (if (equal a b) 0 1)) 
						       from_sequence 
						       to_sequence))

  	(setq hps_dna_number_diff 	(reduce #'+ hps_dna_differences))

	(return-from HPS_DNA_how_much_mutation hps_dna_number_diff))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function determines whether a mutation is to be inflicted based on a probability of a DNA collision
;; a DNA collision occurs when the every X base equals the targetted DNA base to mutate 
;; ********************************************************************************************************
(defun HPS_DNA_mutation_selectivity ( from_base target_base one_out_of_every)
	(setq HPS_DNA_mutation_retval nil)
	(when 	(equal (mod *HPS_iteration_temp* one_out_of_every) 0)
		(if (equal  from_base  target_base)
			(setq HPS_DNA_mutation_retval t)
			nil))

	(unless (equal (mod *HPS_iteration_temp* one_out_of_every) 0)
	  	nil)

	(incf *HPS_iteration_temp*)

	(return-from HPS_DNA_mutation_selectivity HPS_DNA_mutation_retval))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function selectively mutates a DNA sequence based on the specified controls  
;; ********************************************************************************************************
(defun HPS_DNA_mutate_sequence 	( 	hps_1DNA_input_sequence 
					&key 	 (HOWTO		                           'BY_NOISING)
						 (HOWMANY 				    0.10)
						 (FROMWHAT				    'A)
						 (TOWHAT				    'G))

	(setq *HPS_iteration_temp* 	0)

	(setq *HPS_mutation_ratio* 	(round (* HOWMANY 100)))

	(cond 	((equal HOWTO	'BY_DELETIONS) 		nil)
	      	((equal HOWTO	'BY_TRANSPOSITIONS) 	nil)
	      	((equal HOWTO	'BY_INSERTIONS) 	nil)
	      	((equal HOWTO	'BY_NOISING)
			(setq *HPS_1DNA_mutated_bases* (mapcar #'(lambda (a) 
							(if (HPS_DNA_mutation_selectivity a FROMWHAT *HPS_mutation_ratio*)
								towhat
								a))
							hps_1DNA_input_sequence)))

		(T       				nil))
	
	(return-from HPS_DNA_mutate_sequence *HPS_1DNA_mutated_bases*))
;; ********************************************************************************************************
