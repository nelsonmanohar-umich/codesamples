;;; ******************************************************************************************************
;;;                            FROM TEXT TO HPS SERIES - ENCODER
;;; ******************************************************************************************************
;;; function map string to steno time series, this function takes a string and generates a time series 
;;; that contains variable length states that represent a coding of the input string. furthermore,
;;; the variable lenght are disguised by hidding them within a relatively large scale noise function. 
;;; The small magnitude of the HPS states compared to the amplitude of the inserted noise combined with
;;; their variable STENO_SEG_duration makes the states hidden, even when the translation code has been 
;;; compromised.  Ed Note: Paper has to show that extracting the HPS states without knowledge of the 
;;; HPS transform is not possible.
;;; ******************************************************************************************************


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


;; ******************************************************************************************************
;; ******************************************************************************************************
(defun STENO_get_input ( inputtext )
  	(when (equal inputtext nil)  
		(setf *temp_STENO_inputtext_1* " begin the hps transform allows unearthing a form xxx of timescale information from an input signal although" )
		(setf *temp_STENO_inputtext_2* " it is suited for adaptive process control applications it has vast implications for other domains such as" )
		(setf *temp_STENO_inputtext_3* " measurements stenography state estimation dna sequence alignment and pattern mining end" )
		(setf *temp_STENO_inputtext* (format nil "~A~A~A" *temp_STENO_inputtext_1* *temp_STENO_inputtext_2* *temp_STENO_inputtext_3* ))
		(return-from STENO_get_input *temp_STENO_inputtext*))

  	(unless (equal inputtext nil)  
		(return-from STENO_get_input inputtext )))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; this function computes an even random duration for inserted gaps that exhibits some variability
;; ******************************************************************************************************
(setf *STENO_EVEN_GAP_MODE*	t)
(defun STENO_SEG_avgdur () 
  	(setf *STENO_SEG_durnval* (HPS_UNIFORM (- *STENO_SEG_avgdur* *STENO_SEG_avgdurvar*) *STENO_SEG_avgdur*))

	(when   *STENO_EVEN_GAP_MODE*
		(if (equal *STENO_SEG_durnval* 0)     (incf *STENO_SEG_durnval* 2) nil)  
  		(if (oddp  *STENO_SEG_durnval*)
	  		(return-from STENO_SEG_avgdur (incf *STENO_SEG_durnval*))
	  		(return-from STENO_SEG_avgdur       *STENO_SEG_durnval*)))

	(unless *STENO_EVEN_GAP_MODE*
		(return-from STENO_SEG_avgdur *STENO_SEG_durnval*)))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; this function computes a short and even random duration for inserted gaps
;; ******************************************************************************************************
(defun STENO_SEG_choose_gapdur () 
  	(setf *STENO_SEG_gapnval* (HPS_UNIFORM 0 *STENO_SEG_gapdur*))

	(when   *STENO_EVEN_GAP_MODE*
		(if (equal *STENO_SEG_gapnval* 0)            (incf *STENO_SEG_gapnval* 2) nil)  
  		(if (oddp  *STENO_SEG_gapnval*)
	  		(return-from STENO_SEG_choose_gapdur (incf *STENO_SEG_gapnval*))
	  		(return-from STENO_SEG_choose_gapdur       *STENO_SEG_gapnval*)))

	(unless *STENO_EVEN_GAP_MODE*
		(return-from STENO_SEG_choose_gapdur *STENO_SEG_gapnval*)))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; for each encoded elemnet now generate the HPS segments (intermediary step, not necessary) 
;; segments are encoded by the tuple (start, end, mean), where the STENO_SEG_duration is decided within,
;; and the mean is given by the encoding, and the segments are aligned one after the other.
;; therefore, the start of a segment is given by the end of the previous one.
;; Ed. Note, later one ought to add gaps between the segments, these being of variable lenght.
;; ******************************************************************************************************
(defun STENO_encoder_init ( avgdur avgdurvar gapdur noisefactor alphabet mapcode filename )
	(setf *STENO_SEG_avgdur* 	avgdur )
	(setf *STENO_SEG_avgdurvar* 	avgdurvar)
	(setf *STENO_SEG_gapdur* 	gapdur)
	(setf *STENO_HPS_noise_factor* 	noisefactor) 
	(setf *STENO_output_filename* 	filename)

	;; determines the number of segments to an output line in the output
	(setf *STENO_SEG_NITEMS_PER_LINE* 4)

	;; used in random scaling of a gap pulses to increase the discrimination power
	(setf *STENO_maphigh_lowend* (* *STENO_maphigh* 0.00))

	(setf *STENO_SEG_table* nil)
	(setf *STENO_SEG_start* 0)
	(setf *STENO_SEG_end* 0))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; gaps are short burst of noise used to increase HPS discrimination power, their value is dynamically selected
;; ******************************************************************************************************
(defun STENO_choosegapval (fromsegval intosegval)
  	(when   intosegval
		(when   (> fromsegval 0 ) 
	  		(if (< intosegval 0)
		  		(return-from STENO_choosegapval intosegval)
		  		(return-from STENO_choosegapval (- (HPS_UNIFORM *STENO_maphigh_lowend* *STENO_maphigh* )))))
	
		(unless (> fromsegval 0 ) 
			(if (> intosegval 0)
		  		(return-from STENO_choosegapval intosegval)
		  		(return-from STENO_choosegapval (HPS_UNIFORM *STENO_maphigh_lowend* *STENO_maphigh* )))))

  	(unless intosegval
	  	(if (> fromsegval 0)
		  	(return-from STENO_choosegapval (- (HPS_UNIFORM *STENO_maphigh_lowend* *STENO_maphigh* )))
		  	(return-from STENO_choosegapval (HPS_UNIFORM *STENO_maphigh_lowend* *STENO_maphigh* )))))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; the value of *STENO_SEG_avgdur* will be changed to use approxnormal and thus make the STENO_SEG_duration of
;; its underlying segment to be time-variant.
;; ******************************************************************************************************
(defun STENO_encoder_letter_to_segment ( ENCODERLABEL )
  	(setf *STENO_iter* 0) 
	(setf *STENO_num_items* (length *STENO_mapresult*))

	(dolist (x *STENO_mapresult* 'STENO_TABLEBUILDING) 
		(setf *STENO_SEG_end*        (+ *STENO_SEG_start* (STENO_SEG_avgdur)))
		(setf *STENO_SEG_table*      (append *STENO_SEG_table* (list (list *STENO_SEG_start* *STENO_SEG_end* x))))
		(setf *STENO_SEG_start*      (+ *STENO_SEG_end* 1))
	
	        ; insert a random STENO_SEG_duration, biased but random value gap between HPS segments to better discriminate segments
		(setf *STENO_SEG_end*        (+ *STENO_SEG_start* (STENO_SEG_choose_gapdur)))

		(if (< *STENO_iter* *STENO_num_items*)
			(setf *STENO_SEG_next_nval*  (nth (+ *STENO_iter* 1) *STENO_mapresult*))
			(setf *STENO_SEG_next_nval*  x))
			
		(setf *STENO_SEG_gapmean*    (STENO_choosegapval x *STENO_SEG_next_nval* ))
		(setf *STENO_SEG_table*      (append *STENO_SEG_table* (list (list *STENO_SEG_start* *STENO_SEG_end* *STENO_SEG_gapmean*))))
		(setf *STENO_SEG_start*      (+ *STENO_SEG_end* 1))

		(incf *STENO_iter*)))
;; ******************************************************************************************************

	
;; ******************************************************************************************************
;; for each HPS segment, generate the underlying HPS time series
;; ******************************************************************************************************
(defun STENO_encoder_segment_to_timeseries ( ENCODERLABEL )
	(setf *STENO_iter* 	 0)
	(setf *STENO_HPS_series* nil)
	(dolist (x *STENO_SEG_table* 'HPSTIMESERIES) 
		(setf *STENO_SEG_duration* (- (second x) (first x)))
		(setf *STENO_nval* (first (last x)))
		(dotimes (i *STENO_SEG_duration*)
			(incf *STENO_iter*)
			(push *STENO_nval* *STENO_HPS_series*))))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; this function writes the time series to diect to the specified file
;; ******************************************************************************************************
(defun STENO_write_encoded_ts ( output_filename )
	(setf *STENO_iter* 0)
	(setf *STENO_nval* 0)
	(with-open-file (output-stream output_filename :direction :output :if-exists :supersede :if-does-not-exist :create)
		(format output-stream "~&i____	mapcode")
		(dolist (x *STENO_HPS_series* 'NOISEHPSTIMESERIES) 
			(incf *STENO_iter*)
			(setf *STENO_nval* x)
			(format output-stream "~&~D	~12,3F" *STENO_iter* *STENO_nval*))))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; this function generates a cloud of random values around the coded message values, the random cloud has
;; particular properties which interact with the HPS transform's properties.
;; this function should go into the conditioning functions
;; ******************************************************************************************************
(defun STENO_HPS_encoder_noise ( TIMESERIESLABEL noisefactor )
	(setf *STENO_HPS_noise_range* 	noisefactor )				; provided as input *STENO_HPS_noise_factor* 
	(setf *STENO_nval_sign_altern* -1)					; a global alternating sign memory
	(setf *STENO_HPS_rand_nval* 	0)					; a local which contains the HPS noise to be added
	(setf *STENO_rand_iter* 	0)					; a global iteration counter memory

	; the hps recoverable noise being added to the input based on a self-cancelling form
	(setf *STENO_HPS_series_temp* 	nil)
	(dolist (x *STENO_HPS_series* 'NOISEHPSTIMESERIES) 
		(when   (equal (mod *STENO_rand_iter* 2) 0)
  			(setf *STENO_HPS_rand_nval* (HPS_UNIFORM 0 *STENO_HPS_noise_range*)))

		(unless (equal (mod *STENO_rand_iter* 2) 0)
			(setf *STENO_HPS_rand_nval* (- *STENO_HPS_rand_nval* )))
		  	
		;; store the noised input series
		(setf *STENO_HPS_random_nval_plusx* (+ x *STENO_HPS_rand_nval*))
		(push *STENO_HPS_random_nval_plusx* *STENO_HPS_series_temp* )

		(incf *STENO_rand_iter*))

	(setf *STENO_HPS_series* (reverse *STENO_HPS_series_temp*)))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; this is the main for the steno encoder 
;; ******************************************************************************************************
(defun STENO_encoder_main ( &key (avgdur 		120) 
				 (avgdurvar 		30) 
				 (gapdur 		5) 
				 (noisefactor 		100) 
				 (inputtext     	nil)
				 (alphabet      	alphabet)
				 (mapcode		mapcode)
				 (randomvals_file	"C:/HPS_DATA/HPS_INPUTS/HPS_RANDOM_VALUES.DAT")
				 (intofilename  	"C:/HPS_DATA/HPS_INPUTS/HPS_INPUT_SERIES.DAT"))

	(STENO_encoder_init avgdur avgdurvar gapdur noisefactor alphabet mapcode intofilename)

  	(setf *STENO_inputtext* (STENO_get_input inputtext))
	(setf *STENO_inputlist* (STENO_convert_to_list *STENO_inputtext* ))

	;; feedback the input parameters
	(HPS_pretty_printer *STENO_mapalphabet*   t  :nchars 60 :headermsg  "APHABET CURRENTLY IN USE : ")
	(HPS_pretty_printer *STENO_alphabetdescr* t  :nchars 60 :headermsg  "APHABET DESCRIPTION      : ")
	(HPS_pretty_printer *STENO_mapcode*       t  :nchars 13 :headermsg  "MAPPING CODE IN USE      : ")
	(HPS_pretty_printer *STENO_cypherdescr*   t  :nchars 60 :headermsg  "MAPPING CODE DESCRIPTION : ")
	(HPS_pretty_printer *STENO_inputtext*     t  :nchars 72 :headermsg  "INPUT TEXT               : ")
	(HPS_pretty_printer *STENO_inputlist*     t  :nchars 40 :headermsg  "INPUT TO ALGORITHM       : ")
	(HPS_pretty_printer intofilename     	  t  :nchars 60 :headermsg  "OUTPUT WRITTEN TO        : ")
	(format t "~& SIZE OF INPUT TEXT : ~S" 	  (length *STENO_inputtext*))
	(format t "~& APPROX OUTPUT SIZE : ~S" 	  (* (length *STENO_inputtext*) avgdur ))

	;; read an input file containing random values between 0 and 1 using the excel random function
	(HPS_random_init :randomvals_file randomvals_file )

	;; this generates a list containing the tokenized symbols corresponding to the characters found in 
	;; the input ascii message. This list is going to be encoded using the dictionary table.
	(setf *STENO_tokenlist* (mapcar #'STENO_mapascii *STENO_inputlist*))
	(HPS_pretty_printer *STENO_tokenlist*     t  :nchars 40 :headermsg  "TOKENIZED LIST           : ")
		
	;; this generates the encoded list for the tokenized symbols, that is, a list of numbers to hide within
	;; a noise signal in terms of variable lenght states whose means (representative values) are made to be 
	;; these values.
	(setf *STENO_mapresult* (mapcar #'STENO_getmapcode *STENO_tokenlist*))

	;; encode each resulting cyphered letter into segments
	(STENO_encoder_letter_to_segment "GLOBALPARAMETERSBEINGUSED" )

	;; expand and embed each segment into a noise timeseries
	(STENO_encoder_segment_to_timeseries "*STENO_HPS_series*" )

	;; mark some memory as free

	;; correct the series as it is reversed
	(setf *STENO_HPS_series* (reverse *STENO_HPS_series*))

	;; write the resultant segment table to the screen
	(STENO_HPS_writer_segtable_to_screen :gapdurationguess *STENO_SEG_gapdur* )

	;; this hides the encoded time series into a time series embedded with HPS removable noise
	(setf *STENO_HPS_series* (STENO_HPS_encoder_noise "*STENO_HPS_series*" *STENO_HPS_noise_factor* ))

	;; this writes the resulting timeseries to disk
	(STENO_write_encoded_ts *STENO_output_filename* )

	(pprint 'DONE))
;; ******************************************************************************************************







