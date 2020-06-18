(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;; ********************************************************************************************
;;                            TIME SERIES WRITER FUNCTIONS OF THE HPS TRANSFORM                   
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


;; ********************************************************************************************************
;; this is a generic function that prints one or more time series to the given output file having a given
;; time index as the starting point
;; ********************************************************************************************************
(defun HPS_timeseries_printer ( xy_timeseries xy_filename &key  (FROM 		0) 
					      			(TO 		(length xy_timeseries)) 
								(XY_HEADERLINE  "x	y")
								(TRUE_INDEXING  nil))
	(WHEN TRUE_INDEXING
		(setq *HPS_index*      TRUE_INDEXING))

	(setq *HPS_print_iter* FROM)

	(with-open-file (output-stream xy_filename :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (with-output-to-string (HPS-printer-buffer)
		(format HPS-printer-buffer "~&"          nil)				; newline
		(format HPS-printer-buffer XY_HEADERLINE nil)				; header line

		(dolist (x xy_timeseries 'HPSTIMESERIESPRINTER) 
			(incf *HPS_print_iter*)						; always starts at one plus the from value

			(WHEN TRUE_INDEXING
				(incf *HPS_index*))					; always starts at one plus the from value

			(when (HPS_is_this_between *HPS_print_iter* FROM TO) 
				(when   (listp x)
					(terpri                    HPS-printer-buffer )
					(print-object *HPS_print_iter* HPS-printer-buffer)
					;; (write "	" :stream HPS-printer-buffer :escape nil :readably nil)
					(princ "	" HPS-printer-buffer)
			  		(dolist ( sub_x x 'MULTIPLETIMESERIESPRINTER)
						(print-object sub_x HPS-printer-buffer)
						(princ "	" HPS-printer-buffer))
						;; (write "	" :stream HPS-printer-buffer :escape nil :readably nil))
					(when TRUE_INDEXING
						(write *HPS_index* :stream HPS-printer-buffer :escape nil :readably nil)))

				(unless (listp x)
					(terpri 		   HPS-printer-buffer )
					(print-object *HPS_print_iter* HPS-printer-buffer)
					(princ "	" HPS-printer-buffer)
					;; (write "	" :stream HPS-printer-buffer :escape nil :readably nil)
					(print-object x HPS-printer-buffer)
					(princ "	" HPS-printer-buffer)
					;; (write "	" :stream HPS-printer-buffer :escape nil :readably nil)
					(when TRUE_INDEXING
						(write *HPS_index* :stream HPS-printer-buffer :escape nil :readably nil)))

	  			;; this writes the memory buffered output to the specified file
				(when (equal (mod *HPS_print_iter* 16) 15)
					(write (get-output-stream-string HPS-printer-buffer) :stream output-stream :escape nil :readably nil)))

			(unless (HPS_is_this_between *HPS_print_iter* FROM TO) 
				nil))

		(when HPS-printer-buffer
			(write (get-output-stream-string HPS-printer-buffer) :stream output-stream :escape nil :readably nil))

		nil)))
;; ********************************************************************************************************
	

;; ****************************************************************************************************
;; fits number of characters to print to the screen of a message, write the message to the specified file
;; ****************************************************************************************************
(defun HPS_pretty_printer ( message out-stream  &key 
				    ( nchars    	12 ) 
				    ( htmlmode 		nil)
				    ( cellmode 		"<P CLASS=MSONORMAL> <SPAN STYLE='FONT-SIZE:7.0PT;FONT-FAMILY:\"Lucida Sans Unicode\"'>" )
				    ( index_displacement 0)
				    ( title		nil)
				    ( headermsg "	: "))

    (with-output-to-string ( HPS-pprint-buffer )
  	(setq html_single_cell_descriptor   (format nil   "~%<TD> ~A ~A </SPAN> </P> </TD>" cellmode "~A" ))
  	(setq html_multiple_cell_descriptor (format nil "~A~A~A~A" "~{~%<TD>" cellmode "~A" "</SPAN> </P> </TD>~}" ))

	(if (AND htmlmode title)
	  	(format HPS-pprint-buffer "~%<H3> ~A </H3>" title )
		nil)
	  	
	(if htmlmode (format HPS-pprint-buffer "~%<TABLE>" nil) nil)

		(when htmlmode
	  		(format HPS-pprint-buffer "~%<TR>" nil)
				(setq hps_field_len 		  (min nchars (length message )))
				(format HPS-pprint-buffer         html_single_cell_descriptor (format nil "~A" headermsg )) 
				(dotimes (i  hps_field_len  'TEMP)
					(format HPS-pprint-buffer html_single_cell_descriptor (format nil "_~D_"  i )))
	  		(format HPS-pprint-buffer "~%</TR>" nil))


  		(dotimes (j (ceiling (/ (length message ) nchars )) 'PRINTMSG)
			(setq *HPS_submsg_temp* (subseq message (* j   nchars ) (min (* (+ j 1) nchars ) (length message )))) 

		  	(when htmlmode
	  			(format HPS-pprint-buffer "~%<TR>" nil)
			  	(format HPS-pprint-buffer html_single_cell_descriptor 	(format nil "~A[~4D]:  " headermsg 
											(+ (* j nchars) index_displacement))) 
			  	(format HPS-pprint-buffer html_multiple_cell_descriptor *HPS_submsg_temp* )
	  			(format HPS-pprint-buffer "~%</TR>" nil))

			(unless htmlmode
		  		(format HPS-pprint-buffer "~%~A"  				 headermsg ) 
		  		(format HPS-pprint-buffer "~{~A	~}" 				*HPS_submsg_temp* ))

	  		;; this writes the memory buffered output to the specified file
			(when (equal (mod j 16) 15)
				(write (get-output-stream-string HPS-pprint-buffer) :stream out-stream :escape nil :readably nil)))

	(when htmlmode 
	 	(format HPS-pprint-buffer "~%</TABLE>" nil))

	(format HPS-pprint-buffer "~%" nil)

	(write (get-output-stream-string HPS-pprint-buffer) :stream out-stream :escape nil :readably nil)))
;; ****************************************************************************************************



