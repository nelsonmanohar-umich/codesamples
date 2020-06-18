;; ********************************************************************************************
;;                           INTERNET DNA SEQUENCE FILE FILTER AND RETRIEVER 
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


;; ***********************************************************************************************************
;; this function reads an ENTREZ format HTM file from the NIH GENOMIC DATABASE and zooms into the sequence 
;; portion of it, that is, between the ORIGIN label and the end of the sequence of DNA bases, (just before
;; the Disclaimer)
;; ***********************************************************************************************************
(defun HPS_DNA_read_genbank_file ( fromfilename )
	(setq HPS_DNA_LARGE_FILESIZE 10000)
  	(setq *HPS_DNA_genbank_dna_file_contents* nil)
  	    (with-open-file (stream fromfilename )
	 	(dotimes (i HPS_DNA_LARGE_FILESIZE 'READLARGEFILE)
	  	 	(setq dna_file_line (read-line stream nil nil))
				(if (string>= dna_file_line "ORIGIN")
				  	(if (>= (length dna_file_line) 6)
						(if (equal (subseq dna_file_line 0 6) "ORIGIN")
				  			(return t)
							nil)
						nil)
					nil))

 	(dotimes (i HPS_DNA_LARGE_FILESIZE 'READLARGEFILE)
  	 	(setq dna_file_line (read-line stream nil nil))
			(if (equal (subseq dna_file_line 4 7) "pre")
			  	(return t)
				nil)
		(pprint dna_file_line)
		(push dna_file_line *HPS_DNA_genbank_dna_file_contents*)))

  	(return-from HPS_DNA_read_genbank_file (reverse *HPS_DNA_genbank_dna_file_contents*)))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function encoded an ascii letter into the corresponding symbol constant of the DNA BASE SYMBOL ALPHABET
;; ***********************************************************************************************************
(defun HPS_DNA_letter_encoding (letter)
	(cond 	((equal letter '#\a) (setq dna_base 'a))
	  	((equal letter '#\c) (setq dna_base 'c))
	  	((equal letter '#\g) (setq dna_base 'g))
	  	((equal letter '#\t) (setq dna_base 't)))
	(return-from HPS_DNA_letter_encoding dna_base))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function encodes an HTM ENTREZ sequence portion (i.e., the sequence of DNA bases after ORIGIN) into
;; a format that can be handled by the HPS transform and LISP (an in-memory list of DNA bases encoded as letters)
;; ***********************************************************************************************************
(defun HPS_DNA_genbank_get_dna ( dna_file_contents )
  	(setq *HPS_DNA_genbank_sequence_temp* nil)

	(setf (first dna_file_contents) (subseq (first dna_file_contents) 31))

	(setq dna_file_contents_but_lastone (subseq dna_file_contents 0 (- (length dna_file_contents) 1)))

	(with-output-to-string ( *HPS_internal_buffer* )
		(dolist ( dna_line dna_file_contents_but_lastone 'DNAFILESKIMMING ) 
			(setq dna_line (subseq dna_line 9 ))
			(format *HPS_internal_buffer* "~A" dna_line ))
		(setq dna_sequence_temp (get-output-stream-string *HPS_internal_buffer*)))

	(setq dna_sequence (remove-if-not #'(lambda (a) (if (equal a #\Space) nil a)) dna_sequence_temp))

	(setq dna_sequence_list (coerce dna_sequence 'LIST))

	(setq *HPS_DNA_genbank_sequence* (mapcar #'(lambda (a) (HPS_DNA_letter_encoding a)) dna_sequence_list))

	(return-from HPS_DNA_genbank_get_dna *HPS_DNA_genbank_sequence*))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function reads an HTM genomic sequence datafile in the ENTREZ format and retrieves the DNA base 
;; sequence part from it to then write it to the specified file; it returns a copy of the DNA sequence
;; ***********************************************************************************************************
(defun HPS_DNA_filereader_main ( fromfilename 
				 intofilename 
				 &key (SERIES_TITLE '1DNA-BASES))

	;; read an ENTREZ NIH DNA sequence file
	(setq dna_file_contents 	(HPS_DNA_read_genbank_file 	fromfilename ))

	;; select the portion of the HTM file that contains the DNA sequence
	(setq dna_base_sequence		(HPS_DNA_genbank_get_dna 	dna_file_contents ))

	;; write the sequence of DNA bases to the specified file
	(HPS_timeseries_printer 	dna_base_sequence 
	                        	intofilename
					:xy_headerline (format nil "i	~A" series_title))

	;; return the in-memory sequence to the caller, 
	(return-from HPS_DNA_filereader_main dna_base_sequence))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function retrieves a sequence or a subsequence (between from and to genomic addresses) from the 
;; specified ENTREZ NIH databank
;; ***********************************************************************************************************
(defun HPS_DNA_ENTREZ_get_sequence ( DNA_DATABANK_SEQUENCE_NUMBER 
				     &key 	(from 		nil) 
				     		(to   		nil)
						(OUTPUT_DIR	"C:/HPS_DATA/HPS_INPUTS/"))

	;; determine what and how much is to be retrieve, whether the sequence or a subsequence from within
  	(if (AND from to)
		(setq SEQUENCE_HREF	
		      (format nil "http://www.ncbi.nlm.nih.gov/entrez/viewer.fcgi?val=~A&from=~A&to=~A&view=gbwithparts" 
			      DNA_DATABANK_SEQUENCE_NUMBER from to ))
		(setq SEQUENCE_HREF	
		      (format nil "http://www.ncbi.nlm.nih.gov/entrez/viewer.fcgi?val=~A" DNA_DATABANK_SEQUENCE_NUMBER)))

	;; retrieve an ENTREZ NIH DNA sequence file as an HTM file
	(setq *HPS_DNA_ENTREZ_sequence_HTML_file* 
	      (HPS_gnuwget_interface 	:SEQUENCE_DBNUM	 DNA_DATABANK_SEQUENCE_NUMBER
		       		:OUTPUT_DIR 	OUTPUT_DIR
		       		:GNUPATH     	"C:/Program Files/GnuWin32/bin/wget.exe"
				:SEQUENCE_HREF	 SEQUENCE_HREF))

	;; return the full path name to the file where the ENTREZ HTML sequence file was stored
	(return-from HPS_DNA_ENTREZ_get_sequence *HPS_DNA_ENTREZ_sequence_HTML_file*))
;; ***********************************************************************************************************


;; ***********************************************************************************************************
;; this function reads a sequence file from the Internet ENTREZ file and makes its sequence available in both
;; an in-memory version as well as an input sequence file written to the specified filename
;; ***********************************************************************************************************
(defun HPS_DNA_WWW_get_sequence ( DNA_DATABANK_SEQUENCE_NUMBER 
				  &key 	(FROM 					  nil) 
				  	(TO   					  nil)
				  	(GENOME_NAME 				  "E_COLI_K12")
					(OUTPUT_DIR				  "C:/HPS_DATA/HPS_INPUTS/")
				  	(WRITE_1DNA_BASES_SEQUENCE_INTO_THIS_FILE nil)) 

  	;; retrieve the HTML file from the internet
	(setq *HPS_DNA_HTML_sequence_filename* 	(HPS_DNA_ENTREZ_get_sequence 		DNA_DATABANK_SEQUENCE_NUMBER 
											:OUTPUT_DIR	OUTPUT_DIR
											:FROM 		FROM 
											:TO 		TO))

  	;; construct the title name to describe the series being retrieved
	(setq *HPS_1DNA_series_title* 		(format nil "~A_~A_~A" 			"1DNA_BASES" 	
											GENOME_NAME 
										    	DNA_DATABANK_SEQUENCE_NUMBER))


	;; from now on, all output of the HPS transform will be written to this directory
	(setq *DNA_OUTPUT_PATH* 		(format nil "~ADNA_~A/SEQUENCE_~A.DNA" 	OUTPUT_DIR 
											GENOME_NAME 
											DNA_DATABANK_SEQUENCE_NUMBER))


	;; if no output file is specified, then write it to the same directory but as a .DNA file 
	(if (NOT WRITE_1DNA_BASES_SEQUENCE_INTO_THIS_FILE) 
	  	(setq WRITE_1DNA_BASES_SEQUENCE_INTO_THIS_FILE *DNA_OUTPUT_PATH*)
		nil)
		      				
	;; process the retrieved file and extract the DNA sequence from it to then write it to the specified file
	(setq *HPS_1DNA_filereader_bases_sequence* 
		(HPS_DNA_filereader_main 	*HPS_DNA_HTML_sequence_filename* 
						 WRITE_1DNA_BASES_SEQUENCE_INTO_THIS_FILE
						:SERIES_TITLE *HPS_1DNA_series_title*))
	
	(return-from HPS_DNA_WWW_get_sequence *HPS_1DNA_filereader_bases_sequence*))
;; ***********************************************************************************************************



