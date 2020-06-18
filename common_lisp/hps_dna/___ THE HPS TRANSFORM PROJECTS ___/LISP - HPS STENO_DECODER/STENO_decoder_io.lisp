;; ****************************************************************************************************
;;                             		STENO DECODER FILE IO
;; ****************************************************************************************************
;; function decodes a steno time series looking for the presence of HPS states which are then 
;; mapped into a text string by using a shared knowledge alphabet-code mapping (shown below). 
;; the function reads the HPS time series, which is required to be of 3601 samples, as produced,
;; by the simulator and then generates the corresponding HPS state sequence. These HPS states
;; are of variable length and their mean value represents a representative value coding for a letter.
;; ****************************************************************************************************


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


;; ****************************************************************************************************
;; reads the HPS input series from the specified file and produce a table of value-pair tuples (i HPSval)
;; ****************************************************************************************************
(defun STENO_HPS_read_series ( &key (input_filename "C:/HPS_DATA/HPS_OUTPUTS/HPS_APPROXIMATION.DAT"))
	(HPS_read_series input_filename ))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; this function writes a value-pair time series to the specified file (needs to be reused from elsewhere)
;; ****************************************************************************************************
(defun STENO_decoder_writer_timeseries ( atimeseries intofile )
	(setq *STENO_iter* 0)
	(setq *STENO_nval* 0)
	(with-open-file (output-stream intofile :direction :output :if-exists :supersede :if-does-not-exist :create)
		(format output-stream "~&i	hps(i)	code(hps(i))")
		(dolist (x atimeseries 'NOISEHPSTIMESERIES) 
			(setq *STENO_iter* (first x))
			(setq *STENO_nval* (second x))
			(setq *STENO_code* (STENO_getmapcode *STENO_nval*)) 
			(format output-stream "~&~A	~A	~A" *STENO_iter* *STENO_code* *STENO_nval*))))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; (setq *STENO_SIMILAR_EPSILON* (abs (/ old_nval1 20 )))
;; ****************************************************************************************************
(defun STENO_similar_nval ( old_nval1 new_nval2 )
	(setq *STENO_SIMILAR_EPSILON* 0.0)
	(setq *STENO_nval_diff* (abs (- (abs old_nval1) (abs new_nval2))))
	(if (<= *STENO_nval_diff* *STENO_SIMILAR_EPSILON*)
	  	t
		nil))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; this function prints the full segment table or a set of segments from the full segment table
;; ****************************************************************************************************
(defun STENO_print_segtable ( SEG_table segtable-stream external_call_flag &key (relative_indexing 0)
			     					     	        (fromindex 	   nil) 
     			     					     	        (toindex 	   nil))
	(setq *STENO_header_row_temp* '(SEGSTART SEGEND SEGDUR SEGTVAL RELINDX MAPCODE SEGTYPE))
	(format  segtable-stream "~{~8A	~}" *STENO_header_row_temp* )

	(dolist (a_segment SEG_table 'HPSSTENO_SEG_MENTTABLE) 
		(setq *STENO_SEG_start* (first  a_segment))
		(setq *STENO_SEG_end*   (second a_segment))
		(setq *STENO_SEG_dur*   (third  a_segment))
		(setq *STENO_SEG_nval*  (fourth a_segment))
		(setq *STENO_SEG_code*  (STENO_getmapletter *STENO_SEG_nval*)) 

		(when   (OR (AND (NOT fromindex) (NOT toindex)) 
			    (AND fromindex 
				 (HPS_is_this_between *STENO_SEG_start* fromindex toindex)
				 (HPS_is_this_between *STENO_SEG_end*   fromindex toindex)))

			(when   (NOT external_call_flag) 
				(format  segtable-stream "~%" nil)
				(if (equal (fifth a_segment) *STENO_MISPELLING_CODE* ) 
					(format segtable-stream "~8D	~8D	~8D	~8,3F	~8A	~8A	~A" 
						*STENO_SEG_start* *STENO_SEG_end* *STENO_SEG_dur* *STENO_SEG_nval* 
										(- *STENO_SEG_start* relative_indexing) 
										*STENO_SEG_code* 
										"SHORTSEG")
					(format segtable-stream "~8D	~8D	~8D	~8,3F	~8A	~8A	~A" 
						*STENO_SEG_start* *STENO_SEG_end* *STENO_SEG_dur* *STENO_SEG_nval* 
										(- *STENO_SEG_start* relative_indexing) 
										*STENO_SEG_code* 
										"LONGSEG" )))
	
			(unless (NOT external_call_flag ) 
				(format  segtable-stream "~%" nil)
				(format segtable-stream "~8D	~8D	~8D	~8,3F	~8A	~{~1A~}" 
					*STENO_SEG_start* *STENO_SEG_end* *STENO_SEG_dur* *STENO_SEG_nval* 
										(- *STENO_SEG_start* relative_indexing)
										*STENO_SEG_code*))

			(push *STENO_SEG_code* *STENO_decoded_msg*))

		(unless (OR (AND (NOT fromindex) (NOT toindex)) 
			    (AND fromindex 
				 (HPS_is_this_between *STENO_SEG_start* fromindex toindex)
				 (HPS_is_this_between *STENO_SEG_end*   fromindex toindex)))
		  	nil))

	(return-from STENO_print_segtable  *STENO_decoded_msg* ))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; this function writes the resultant HPS segment table into the specified file
;; ****************************************************************************************************
(defun STENO_decoder_writer_segtable ( SEG_table SEG_table_file external_call_flag )
	(when   (NOT external_call_flag) 
		(with-open-file (segtable-stream SEG_table_file :direction :output :if-exists :append :if-does-not-exist :create)
			(setq *STENO_decoded_msg* (STENO_print_segtable SEG_table segtable-stream external_call_flag ))))

	(unless (NOT external_call_flag) 
		(with-open-file (segtable-stream SEG_table_file :direction :output :if-exists :supersede :if-does-not-exist :create)
			(setq *STENO_decoded_msg* (STENO_print_segtable SEG_table segtable-stream external_call_flag ))))

	(return-from STENO_decoder_writer_segtable (reverse *STENO_decoded_msg*)))
;; ****************************************************************************************************


;; ****************************************************************************************************
;; ****************************************************************************************************
(defun STENO_autodecoder_printer ( decrypted_message
				   segtable_stream  
				   &key ( NCHARS      12 )
				   	( HEADER_MSG  "ATTEMPTED AUTO-DECODING:	"))

  	(HPS_pretty_printer decrypted_message segtable_stream :nchars nchars :headermsg header_msg))
;; ****************************************************************************************************


;; ********************************************************************************************
;; the html headers and footers of the html report to be generated 
;; ********************************************************************************************
(setq STENO_HTML_MAIN_HEADER "<HTML> <HEAD> <TITLE>HPS Stenographic Decoder</TITLE> <STYLE>
<!--
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:8.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:8.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=center> <H2> HPS STENOGRAPHIC DECODER </H2> 
			<P> <STRONG> (c) 2007 <A HREF=http://www3.webng.com/nelsonmanohar/research.htm> 
	         	The HPS Transform <BR> by Nelson R. Manohar, Ph.D.  </A> 
		 	<BR> nelsonmanohar@yahoo.com </STRONG> </P> <BR> </DIV>
<DIV ALIGN=justify> <P ALIGN=JUSTIFY> 	This report is automatically generated by the HPS stenographic decoder.  
		  	Access to this stenographic report is provided only to illustrate the discrimination 
			power achievable by the HPS transform.  </P> " )
(setq STENO_HTML_MAIN_FOOTER  "<P ALIGN=JUSTIFY> 	No permit to use is granted without authorization from the author. 
							Due to malfeasance, the stenographic application can not be made available 
							for anonymous use. Only qualified reviewers and academics may request 
							access to the application by e-mailing your specific request to the author 
							to the e-mail address given.  </P> </DIV> 
							</BODY> 
							</HTML> " )
(setq STENO_HTML_MAIN_ITEM_P0 "  <H3> <A HREF=FILE://" )
(setq STENO_HTML_MAIN_ITEM_P1 " TARGET=_BLANK> ATTEMPTED AUTO DECODING NUM. " )
(setq STENO_HTML_MAIN_ITEM_P2 " </A> </H3> " )
(setq STENO_HTML_MAIN_ITEM_P3 " <PRE CLASS=MSONORMAL> <SPAN STYLE='FONT-SIZE:7.0PT;FONT-FAMILY:VERDANA'> " )
(setq STENO_HTML_MAIN_ITEM_P4 " </SPAN> </PRE> " )
;; ********************************************************************************************








