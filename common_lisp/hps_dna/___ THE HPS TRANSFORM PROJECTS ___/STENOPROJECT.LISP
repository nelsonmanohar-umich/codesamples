(EXPLAIN-COMPILER-SETTINGS)
(proclaim '(optimize (speed 2) (safety 1) (space 0) (debug 0)))
(setf HPS_COMPILE_AND_LOAD_IS_ON nil)

;;; **********************************************************************************************************************
;;; THE HPS TRANSFORM
;;; **********************************************************************************************************************
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_generic_utilities.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_debugger.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_timeseries_reader.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_conditioning_encoder.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_data_ttest.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_histograms.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_peformance_metrics.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_plotter.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_random.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_global_init.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_stationarity.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_mse_handler.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_summary_stats.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_system_dependent.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_Transform-Online-O(m' N).lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_zdriver.lisp" )
;;; **********************************************************************************************************************


;;; **********************************************************************************************************************
;;; THE STENO ENCODER - DEPENDS ON THE HPS TRANSFORM
;;; **********************************************************************************************************************
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_ENCODER/STENO_shared.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_ENCODER/STENO_segprinter.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_ENCODER/STENO_encoder.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_ENCODER/STENO_zdriver_encoder.lisp" )
;;; **********************************************************************************************************************


;;; **********************************************************************************************************************
;;; THE STENO DECODER - DEPENDS ON THE HPS TRANSFORM AND THE STENO ENCODER
;;; **********************************************************************************************************************
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_decoder_io.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_gnuplot.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_report.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_decoder.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_decoder_iterative.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_decoder_zdriver.lisp" )
;;; **********************************************************************************************************************


;;; **********************************************************************************************************************
;;; THE PATTERN MINER - DEPENDS ON THE STENO DECODER (AND SHORTLY) THE STENO ENCODER AND THE HPS TRANSFORM
;;; **********************************************************************************************************************
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_readme.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_init.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_utilities.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_gnuplot_headers.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_gnuplot.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_report.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_pattern_mining.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_aligner.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_main.lisp" )
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_zdriver.lisp" )
;;; **********************************************************************************************************************


;;; **********************************************************************************************************************
;;; **********************************************************************************************************************
(compile-file "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_applications.lisp" )
;;; **********************************************************************************************************************




;;; **********************************************************************************************************************
;;; THE HPS TRANSFORM
;;; **********************************************************************************************************************
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_generic_utilities.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_debugger.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_timeseries_reader.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_conditioning_encoder.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_data_ttest.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_random.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_global_init.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_histograms.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_peformance_metrics.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_plotter.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_stationarity.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_mse_handler.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_summary_stats.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_system_dependent.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_Transform-Online-O(m' N).fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_zdriver.fasl" )
;;; **********************************************************************************************************************


;;; **********************************************************************************************************************
;;; THE STENO ENCODER - DEPENDS ON THE HPS TRANSFORM
;;; **********************************************************************************************************************
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_ENCODER/STENO_shared.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_ENCODER/STENO_segprinter.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_ENCODER/STENO_encoder.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_ENCODER/STENO_zdriver_encoder.fasl" )
;;; **********************************************************************************************************************


;;; **********************************************************************************************************************
;;; THE STENO DECODER - DEPENDS ON THE HPS TRANSFORM AND THE STENO ENCODER
;;; **********************************************************************************************************************
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_decoder_io.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_gnuplot.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_report.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_decoder.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_decoder_iterative.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS STENO_DECODER/STENO_decoder_zdriver.fasl" )
;;; **********************************************************************************************************************


;;; **********************************************************************************************************************
;;; THE PATTERN MINER - DEPENDS ON THE STENO DECODER (AND SHORTLY) THE STENO ENCODER AND THE HPS TRANSFORM
;;; **********************************************************************************************************************
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_readme.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_init.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_utilities.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_gnuplot_headers.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_gnuplot.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_report.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_pattern_mining.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_aligner.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_main.fasl" )
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS DNA_PATTERN_MATCH/HPS_DNA_zdriver.fasl" )
;;; **********************************************************************************************************************


;;; **********************************************************************************************************************
;;; **********************************************************************************************************************
(load "F:/HPS_DATA/___ THE HPS TRANSFORM PROJECTS ___/LISP - HPS TRANSFORM - VER-09-07/HPS_applications.fasl" )
;;; **********************************************************************************************************************




;;; **********************************************************************************************************************
;;;                               THE PARTICULAR INVOCATIONS
;;; **********************************************************************************************************************
(setf HPS_COMPILE_AND_LOAD_IS_ON t) 
			;;; *******************************************************
					    (if 
				   HPS_COMPILE_AND_LOAD_IS_ON 
				     (STENO_encoder_driver)
				             nil)
			;;; *******************************************************

(setf HPS_COMPILE_AND_LOAD_IS_ON t) 

			;;; *******************************************************
				    	    (if 
				   HPS_COMPILE_AND_LOAD_IS_ON 
					   (MAIN 	
					     	:APPMODE  	'STENO
					     	;; :APPMODE  	'DEFAULT
					     	;; :APPMODE  	'TEST
					     	;; :APPMODE  	'DNA
						:BASE_DIR	"F:/HPS_DATA/")
				             nil)
			;;; *******************************************************

(setf HPS_COMPILE_AND_LOAD_IS_ON t) 

			;;; *******************************************************
					    (if 
				   HPS_COMPILE_AND_LOAD_IS_ON 
				     (STENO_decoder_driver)
				             nil)
			;;; *******************************************************

(setf HPS_COMPILE_AND_LOAD_IS_ON t) 

			;;; *******************************************************
				            (when  
				   HPS_COMPILE_AND_LOAD_IS_ON 
					   (print 'HPS_DNA_START)
						(HPS_DNA_main 	:alphabet    nil
			   					:mapcode     nil
		      	   					:goaldur     30	  ; previously was 40 - very coarse tracking - very few segments
			   					:mindur      5	  ; previously was 10 - very coarse tracking - very few segments
			   					:match_goal  0.75
			   					:input_file  "HPS_DNA_SERIES_IN.DNA"  
			   					:base_dir    "F:/HPS_DATA/"
			   					:output_dir  "F:/HPS_DATA/HPS_OUTPUTS/" 
			   					:report_file "HPS_DNA.HTM" )
					   (print 'HPS_DNA_DONE))

				            (unless 
				   HPS_COMPILE_AND_LOAD_IS_ON 
				             nil)
			;;; *******************************************************


;;; **********************************************************************************************************************



