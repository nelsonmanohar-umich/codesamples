(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;; ********************************************************************************************************
;; *************************** THE HPS TRANSFORM :   TIME SERIES READER   *********************************
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
;; this function reads the input time series and returns a list contained within the global variable
;; *HPS_input_timeseries*.  The input file is expected to have this format:
;; 		INDEX_NAME (e.g., X(i)					DATA_NAME (e.g., Y(i))
;; 		index_number/index_label (e.g., 1 or 10:10)   		datum (e.g., 3.141592)
;; 		index_number/index_label (e.g., 2 or 10:11)   		datum (e.g., 1.000000)
;; 		index_number/index_label (e.g., 3 or 10:12)   		datum (e.g., 2.690000)
;; 		index_number/index_label (e.g., 4 or 10:13)   		datum (e.g., 2.000000)
;; where the x(i) are assumed to be sequentially listed and without indexing gaps.
;;
;; The datums are separated by blank space. The labels are not used except for plotting purposes.
;; The datum is expected as numbers, preferrably floats.
;;
;; The function retains memory of the labels contained in the first row as well as it decomposes
;; the time series into two data columns (without headers) and returns the actual y(i) values which
;; are to be transformed, while remembering its indexing x(i) values for later needs, such as plotting.data
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this reads all tuples and concatenates all such into a big table
;; ********************************************************************************************************
(defun HPS_read_all_ts_tuples (filestream)
  	(setq *HPS_input_ts_tupleset* nil)

	;; the debugger takes care of informing user of the file i/o error when the 
	;; input file has not been setup and placed on the specified directory, as the
	;; error handler is not nil nil
	(let*  ((HPS_index    		(read filestream))
  		(HPS_value    		(read filestream))
		(HPS_tuple 		(list HPS_index HPS_value)))
		(push HPS_tuple 	*HPS_input_ts_tupleset* )

		(dotimes (j *HPS_MAXIMUM_TUPLECAPACITY* 'HPS_TSREADER) 
			(when (NOT (setq HPS_index  (read filestream nil nil)))
				(return-from HPS_read_all_ts_tuples *HPS_input_ts_tupleset*))
		
			(when (NOT (setq HPS_value  (read filestream nil nil)))
				(return-from HPS_read_all_ts_tuples *HPS_input_ts_tupleset*))
	
	  		(push (list HPS_index HPS_value) *HPS_input_ts_tupleset* )))

	(return-from HPS_read_all_ts_tuples *HPS_input_ts_tupleset*))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this produces a table of HPS input time series tuples of the form (x(i) y(i)) 
;; ********************************************************************************************************
(defun HPS_read_timeseries_from ( fromfilename )
	(with-open-file (stream fromfilename )
		(HPS_read_all_ts_tuples stream))
	(return-from HPS_read_timeseries_from (reverse *HPS_input_ts_tupleset*)))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function reads the input time series from a file named as below and returns a list of 
;; the y(i) values in a format over which the HPS transform could be applied.
;; this function is hardcoded to read just two columns of data, an index value column pair
;; ********************************************************************************************************
(defun HPS_timeseries_reader ( filename )
	(setq *HPS_MAXIMUM_TUPLECAPACITY* 1000000)

  	(setq *HPS_timeseries_input_file* filename )
  	(setq *HPS_input_timeseries* nil)

	;; the reading of the input time series from the given file
  	(setq *HPS_input_timeseries* (HPS_read_timeseries_from *HPS_timeseries_input_file* ))

	(setq *HPS_input_timeseries_header*   (first *HPS_input_timeseries* )) 
	(setq *HPS_input_timeseries_tuples*   (rest  *HPS_input_timeseries* )) 

	(setq *HPS_input_timeseries_index*    (mapcar #'(lambda (a) (first  a)) *HPS_input_timeseries_tuples*))
	(setq *HPS_input_timeseries_data*     (mapcar #'(lambda (a) (second a)) *HPS_input_timeseries_tuples*))

	(setq *HPS_timeseries_dataset* (list *HPS_input_timeseries_header* 
					     *HPS_input_timeseries_index* 
					     *HPS_input_timeseries_data* 
					      filename ))

	(return-from HPS_timeseries_reader *HPS_timeseries_dataset* ))
;; ********************************************************************************************************


;; ********************************************************************************************************
;; this function accesss data previously setup after a successful read of an input time series 
;; this function returns the data column referred to by column within a filename 
;; but if column equals -1 it returns the header of the file 
;; this function seems wrong - CHECK
;; ********************************************************************************************************
(setq *HPS_XCOLUMN* 	0)
(setq *HPS_YCOLUMN* 	1)
(setq *HPS_HEADERROW*  -1)
(setq *HPS_FILETITLE*   2)
(defun HPS_reader_get_file_column (column dataset )
	(cond ((equal column -1) (return-from HPS_reader_get_file_column (first  dataset )))  ; header 
	      ((equal column  0) (return-from HPS_reader_get_file_column (second dataset )))  ; indexes
	      ((equal column  1) (return-from HPS_reader_get_file_column (third  dataset )))  ; values
	      ((equal column  2) (return-from HPS_reader_get_file_column (fourth dataset )))  ; filename
	      (t 		 (pprint "NOT YET IMPLEMENTED. THIS VERSION ONLY PROCESSES 1D TIMESERIES"))))	; hardcoded to one index-value pair
;; ********************************************************************************************************


;; ****************************************************************************************************
;; reads the HPS input series from the specified file and produce a table of value-pair tuples (i HPSval)
;; ****************************************************************************************************
(defun HPS_read_series ( input_filename )
	(setq *HPS_dataset*    (HPS_timeseries_reader input_filename ))
	(setq *HPS_xvals*      (HPS_reader_get_file_column *HPS_XCOLUMN* *HPS_dataset* ))
	(setq *HPS_yvals*      (HPS_reader_get_file_column *HPS_YCOLUMN* *HPS_dataset* ))
	(setq *HPS_timeseries* (mapcar #'(lambda (a b) (list a b)) *HPS_xvals* *HPS_yvals*)))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; ****************************************************************************************************
(defun HPS_get_timeseries_dataset ()
	(return-from HPS_get_timeseries_dataset *HPS_timeseries_dataset* )) 
;; ****************************************************************************************************




