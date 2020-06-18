;; ****************************************************************************************************************
;;                           *****     SYSTEM AND O/S DEPENDENT FUNCTIONS ******
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
;; this function represents an example of what the user of the HPS libraries would have to do to build applications
;; relying on the HPS transform. Currently, this implementation relies only on the online HPS transform. This 
;; example illustrates the application of the HPS transform to several application domains.
;; ****************************************************************************************************************
(defun HPS_gnuplot_interface (&key (COMMAND 		'PLOT) 
				   (OUTPUT_DIR 		"C:/HPS_DATA/HPS_OUTPUTS/")
				   (GNUPATH     	"/usr/bin/gnuplot")
				   (WITH_CMD_FILE 	"HPS_TRANSFORM_WINDOWS_CMDS.BAT"))
				   ;;(GNUPATH     	"C:/Program Files/gnuplot/bin/wgnuplot.exe")

	;; from now on, all output of the HPS transform will be written to this directory
	(setf *GNU_OUTPUT_PATH* OUTPUT_DIR)

	;; ensure that said output directory exists or otherwise created it
	;; (format t "~% Verifying path to output directory for the GNU plot interfaces ....: [~S]" *GNU_OUTPUT_PATH* )
	(ensure-directories-exist *GNU_OUTPUT_PATH* )

	;; verify the path CHECK otherwise feedback error (assert)
	(setf *GNU_GNUPLOT_PATH* GNUPATH)
	;; (format t "~% PATH to GNUPLOT for win32 binaries has been setup to be ...........: [~S]" *GNU_GNUPLOT_PATH* )

	;; prepare the data of the autogenerated analyses
	(format t "~% Running GNUPLOT interface .........................................: [~S]"  WITH_CMD_FILE )

	(RUN-SHELL-COMMAND WITH_CMD_FILE))
;; ****************************************************************************************************************


;; ****************************************************************************************************************
;; this function represents an example of what the user of the HPS libraries would have to do to build applications
;; relying on the HPS transform. Currently, this implementation relies only on the online HPS transform. This 
;; example illustrates the application of the HPS transform to several application domains.
;; ****************************************************************************************************************
(defun HPS_gnuwget_interface (&key (SEQUENCE_DBNUM	 2367095) 
				   (OUTPUT_DIR 		"C:/HPS_DATA/HPS_INPUTS/")
				   (GENOME_NAME		"E_COLI_K12")
				   (GNUPATH     	"C:/Program Files/GnuWin32/bin/wget.exe")
				   (SEQUENCE_HREF	"http://www.ncbi.nlm.nih.gov/entrez/viewer.fcgi?val=~A&from=8992&to=10140&view=gbwithparts"))

	;; from now on, all output of the HPS transform will be written to this directory
	(setf *GNU_OUTPUT_PATH* (format nil "~ADNA_~A/SEQUENCE_~A.HTM" OUTPUT_DIR GENOME_NAME SEQUENCE_DBNUM))

	;; ensure that said output directory exists or otherwise created it
	;; (format t "~% Verifying path to output directory for the GNU plot interfaces ....: [~S]" *GNU_OUTPUT_PATH* )
	(ensure-directories-exist *GNU_OUTPUT_PATH* )

	;; verify the path CHECK otherwise feedback error (assert)
	(setf *GNU_GNUWGET_PATH* GNUPATH)
	;; (format t "~% PATH to GNUWGET for win32 binaries has been setup to be ...........: [~S]" *GNU_GNUWGET_PATH* )

	;; retrieve the specified genome sequence
	(setf *GNU_GET_HTTP_FILE* (format nil SEQUENCE_HREF SEQUENCE_DBNUM ))
	(setf WITH_CMD_FILE 	  (format nil "~A --output-document=\"~A\"  ~A"  *GNU_GNUWGET_PATH* *GNU_OUTPUT_PATH* *GNU_GET_HTTP_FILE*))
	(format t "~% Running GNUWGET interface .........................................: [~S]"  WITH_CMD_FILE )
	(RUN-SHELL-COMMAND WITH_CMD_FILE)

	(return-from HPS_gnuwget_interface *GNU_OUTPUT_PATH*))
;; ****************************************************************************************************************


;; ****************************************************************************************************************
;; system dependent call to the garbage collector - if one such exists and it is available
;; ****************************************************************************************************************
(defun HPS_commonlisp_interface ( &key (COMMAND		'CLGC))
	(format t "~% Invoking the allegro common lisp's garbage collector ............. : [~A]" COMMAND )
  	(excl:gc t))
;; ****************************************************************************************************************


;; ****************************************************************************************************************
;; system dependent call used to pack the contents of the HPS_OUTPUTS directory, nothing is deleted
;; ****************************************************************************************************************
(defun HPS_ntfilepack_interface ( &key 	(COMMAND	'PACK)
			       		(SEQUENCE_DBNUM	 2367095) 
			       		(BASEDIR 	"C:/HPS_DATA/"))

	;; outputs of the HPS transform will be saved to this directory
	(setf *WIN_OUTPUT_PATH_FULL* 	(format nil "~A~A~A/"  BASEDIR "HPS_OUTPUTS_" SEQUENCE_DBNUM ))
	(setf *WIN_OUTPUT_PATH* 	(format nil "~A"       SEQUENCE_DBNUM ))

	;; ensure that said output directory exists or otherwise created it
	;; (format t "~% Verifying path to output directory for the WIN NTFS interfaces ....: [~S]" *WIN_OUTPUT_PATH_FULL* )
	(ensure-directories-exist *WIN_OUTPUT_PATH_FULL* )

	;; retrieve the specified genome sequence
	(setf *WITH_CMD_FILE* 	  (format nil "~A~A ~A"  BASEDIR "HPS_DNA_ARCHIVAL_WINDOWS_CMDS.BAT" *WIN_OUTPUT_PATH* ))
	;; (setf WITH_CMD_FILE 	  (format nil "~A~A ~A~A ~A"  BASEDIR "HPS_DNA_ARCHIVAL_WINDOWS_CMDS.BAT" 
					  		      ;; BASEDIR "HPS_OUTPUTS" *WIN_OUTPUT_PATH* ))
	(format t "~% Running WIN NTFS interface (archival of previous HPS_OUTPUTS)......: [~S]"  *WITH_CMD_FILE* )
	(RUN-SHELL-COMMAND *WITH_CMD_FILE*)

	(return-from HPS_ntfilepack_interface *WIN_OUTPUT_PATH*))
;; ****************************************************************************************************************


;; ****************************************************************************************************************
;; system dependent call to win32 - never to be used
;; ****************************************************************************************************************
(defun HPS_win32_interface ( &key 	(WITH_CMD_FILE	nil))
	(format t "~% Running WIN32 interface    ........................................: [~S]"  WITH_CMD_FILE )
	;; (RUN-SHELL-COMMAND WITH_CMD_FILE)

	(return-from HPS_win32_interface WITH_CMD_FILE ))
;; ****************************************************************************************************************


;; ****************************************************************************************************************
;; system dependent call used to clean up the contents of the HPS_OUTPUTS directory, nothing is deleted
;; ****************************************************************************************************************
(defun HPS_cleanup_interface 	( &key  (COMMAND	COMMAND)
					(BASEDIR	BASEDIR))

	;; outputs of the HPS transform will be saved to this directory
	(setf *WIN_OUTPUT_PATH* 	(format nil "~A~A"  BASEDIR "HPS_OUTPUTS/"))
	(ensure-directories-exist *WIN_OUTPUT_PATH*)
	(setf *WIN_OUTPUT_PATH* 	(HPS_substitute_character "/" "\\" *WIN_OUTPUT_PATH* ))

	(setf *REN_WIN_OUTPUT_PATH* 	(format nil "~A~A~A" BASEDIR "OLD_" "HPS_OUTPUTS/"))
	(ensure-directories-exist *REN_WIN_OUTPUT_PATH*)
	(setf *REN_WIN_OUTPUT_PATH* 	(HPS_substitute_character "/" "\\" *REN_WIN_OUTPUT_PATH* ))

	;; retrieve the specified genome sequence
	(setf *WITH_CMD_FILE* 	  	(format nil "~A~A ~A ~A"  BASEDIR 
								 "HPS_DNA_CLEANUP_WINDOWS_CMDS.BAT" 
								 *WIN_OUTPUT_PATH* 
								 *REN_WIN_OUTPUT_PATH* ))
	(format t "~% Running WIN NTFS interface (cleanup of previous HPS_OUTPUTS).......: [~S]"  *WITH_CMD_FILE* )
	(RUN-SHELL-COMMAND *WITH_CMD_FILE*)

	(return-from HPS_cleanup_interface *WIN_OUTPUT_PATH*))
;; ****************************************************************************************************************


;; ****************************************************************************************************************
;; interface to system dependent calls, to be replaced with daemon script-based model which is not WIN/ACL dependent
;; ****************************************************************************************************************
(defun HPS_system_interface (&key  (DATUM		'BASIC) 
				   (COMMAND 		'PLOT) 
				   (OUTPUT_DIR 		 nil)
				   (WITH_CMD_FILE 	"HPS_TRANSFORM_WINDOWS_CMDS.BAT"))

	(cond 	((equal COMMAND 'PLOT)
		 (HPS_gnuplot_interface    :OUTPUT_DIR 		(OR OUTPUT_DIR *HPS_OUTPUT_PATH* *HPS_OUTPUTDIR*)
					   :WITH_CMD_FILE 	WITH_CMD_FILE))

	      	((equal COMMAND 'WGET)
		 (HPS_gnuwget_interface    :SEQUENCE_DBNUM 	DATUM
					   :OUTPUT_DIR 		(OR OUTPUT_DIR *HPS_OUTPUT_PATH* *HPS_OUTPUTDIR*)
					   :SEQUENCE_HREF 	WITH_CMD_FILE))

	      	((equal COMMAND 'CLGC)
		 (HPS_commonlisp_interface :COMMAND		COMMAND))

	      	((equal COMMAND 'PACK)
		 (HPS_ntfilepack_interface :COMMAND		COMMAND
			       		   :SEQUENCE_DBNUM	DATUM 
			       		   :BASEDIR 	        (OR *HPS_BASE_PATH* *HPS_BASEDIR*)))

	      	((equal COMMAND 'CLEAN)
		 (HPS_cleanup_interface    :COMMAND		COMMAND
		 			   :BASEDIR		(OR *HPS_BASE_PATH* *HPS_BASEDIR*)))

	      	((equal COMMAND 'NT32)
		 (HPS_win32_interface 	   :WITH_CMD_FILE	WITH_CMD_FILE))

		(T
		 (HPS_gnuplot_interface    :OUTPUT_DIR 		(OR OUTPUT_DIR *HPS_OUTPUT_PATH* *HPS_OUTPUTDIR*)
					   :WITH_CMD_FILE 	WITH_CMD_FILE))))
;; ****************************************************************************************************************



