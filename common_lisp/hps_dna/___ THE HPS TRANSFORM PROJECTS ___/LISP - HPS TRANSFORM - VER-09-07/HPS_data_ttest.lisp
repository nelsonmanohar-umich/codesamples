(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;; ********************************************************************************************
;;                                 TIMESERIES READER
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
;; this reads a single data tuple from the file, make this to read full line all symbols
;; ********************************************************************************************
(defun HPS_read_t_tuple (filestream)
	(setf *HPS_t_df* (read filestream))
	(push *HPS_t_df* *HPS_df_headers*)

  	(setf *HPS_ttable_row* nil)
        (dolist (tailprob (rest *HPS_ttable_header*) 'HPS_TTABLE_ROW)
		(setf *HPS_tval* (read filestream))
		(setf *HPS_tuple* (list *HPS_t_df* tailprob *HPS_tval*)) 
		(push *HPS_tuple* *HPS_ttable_row*))

	(return-from HPS_read_t_tuple *HPS_ttable_row*))
;; ********************************************************************************************


;; ********************************************************************************************
;; this reads all tuples from a file whose name is given below and 
;; concatenates all such data tuples into a big table array
;; ********************************************************************************************
(defun HPS_read_all_t_tuples (filestream)
	(dotimes (row *HPS_ttable_rows* 'HPS_TTABLE_ROWREADER) 
		 (push (HPS_read_t_tuple filestream) *HPS_ttable*))
	(return-from HPS_read_all_t_tuples *HPS_ttable*))
;; ********************************************************************************************


;; ********************************************************************************************
;; reads and produce a table of t values
;; ********************************************************************************************
(defun HPS_read_t_table ()

	;; the name of the file that contains the system file containing the t-table, not an input parameter
	(setf *HPS_ttable_file* (HPS_pathname *HPS_input_directory* "HPS_T_TABLE_GENERATOR.DAT" ))

  	;; speed up the loading of the t-table, if already loaded, don't reload it
  	(when (AND (boundp '*HPS_ttable*) *HPS_ttable*)
		(return-from HPS_read_t_table *HPS_df_headers*))

	;; (format t "~% HPS Transform loading t-table from file: [~S]" *HPS_ttable_file* )

  	(setf *HPS_ttable_rows*  204)				; HARDCODED
  	(setf *HPS_ttable_cols*  20)				; HARDCODED
  	(setf *HPS_ttable* nil)					; where the table for student-t values will be placed stored (38x12 TABLE)
	(setf *HPS_ttable_header* nil)
	(setf *HPS_df_headers* nil)

	(with-open-file (stream *HPS_ttable_file* )
        	(dotimes (j *HPS_ttable_cols*  'HPS_TTABLE_HEADERROW)
			(setf *HPS_tval* (read stream ))
			(push *HPS_tval* *HPS_ttable_header*)))

	(setf *HPS_ttable_header* (reverse *HPS_ttable_header*))

	(with-open-file (stream *HPS_ttable_file* )
		(setf *HPS_ttable* (reverse (HPS_read_all_t_tuples stream))))

	(setf *HPS_df_headers* (reverse *HPS_df_headers*)))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function looks up a tail probability value and returns the t-value associated with it. 
;; lookup the t-table and find an exacting tuple that fits the constraints (df alpha) if one such exists in there
;; ********************************************************************************************
(defun HPS_tdist_approx (alpha m mp)
	(setf oneminusalpha (- 1 alpha ))			; CHECK FOR PROPER VALUE
	(setf df (+ m mp -2))					; CHECK FOR THE PROPER FORMULATION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	(if *HPSdebug* (format t "~%(~S, ~S) 	--> 	" df  oneminusalpha ) nil)

	(setf oneminusalpha (HPS_nearestvalue oneminusalpha (rest *HPS_ttable_header*)))
	(setf df (HPS_nearestvalue df (rest *HPS_df_headers*)))
	(if *HPSdebug* (format t "(~S, ~S)	" df  oneminusalpha )  nil)

	(dolist (tablerow *HPS_ttable* 'HPS_TTABLE_SCAN)
        	(setf *HPS_tuple* (mapcar #'(lambda (a) 
					      (if (AND (equal (first a) df) 
						       (equal (second a) oneminusalpha)) 
						(third a) 
						nil)) 
					  tablerow ))

        	(setf *HPS_tuple* (remove-if-not #'numberp *HPS_tuple*))

		;; if a matching tuple (df, alpha --> tprob) was found, return the tprob
		(if *HPS_tuple* 
		  	(return (setf *HPS_tmax* (first *HPS_tuple*))) 
			nil))

	;; (pprint "FORCING TMAX to 3.21")
	;; (setf *HPS_tmax* 3.21)
	(if *HPSdebug* (pprint *HPS_tmax*) nil)

  	(return-from HPS_tdist_approx *HPS_tmax* ))
;; ********************************************************************************************


;; ********************************************************************************************
;; this function lookups the t-test threshold value corresponding to the sampled populations unknown variance and mean 
;; ********************************************************************************************
(defun HPS_tdist_lookup (alpha m mp )
	(setf *HPS_tmax* (HPS_tdist_approx alpha m mp))
	(return-from HPS_tdist_lookup *HPS_tmax*))
;; ********************************************************************************************








