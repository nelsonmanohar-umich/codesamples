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
;; this function represents an example of what the user of the HPS libraries would have to do to build applications
;; relying on the HPS transform. Currently, this implementation relies only on the online HPS transform. This 
;; example illustrates the application of the HPS transform to several application domains.
;; ****************************************************************************************************************
(defun HPS_APP_main (&key 	(APPMODE 	'DNA) 
		  	        (BASE_DIR 	"C:/HPS_DATA/"))

	(HPS_print_separator 2)

	(pprint 'HPS_STARTED)

	;; *******************************************************************************************************
	(setf *HPS_BASE_PATH*   BASE_DIR) 
	(format t "~% The HPS Transform application files expected to be installed here ..: [~S]" *HPS_BASE_PATH* )

	;; input files such as the t-table and the input series are expected to be here, except for applications
	;; which use pipes
	(setf *HPS_INPUT_PATH*  (HPS_concat *HPS_BASE_PATH*   "HPS_INPUTS/" ))
	(format t "~% Input files to the HPS Transform are expected to be placed be here .: [~S]" *HPS_INPUT_PATH* )

	;; from now on, all output of the HPS transform will be written to this directory
	(setf *HPS_OUTPUT_PATH* (HPS_concat *HPS_BASE_PATH*   "HPS_OUTPUTS/"))
	(format t "~% The output of the HPS Transform will be placed be here .............: [~S]" *HPS_OUTPUT_PATH* )

	;; ensure that said output directory exists or otherwise created it
	(format t "~% Verifying path to input  directory for the HPS transform to be  ....: [~S]" *HPS_INPUT_PATH* )
	(ensure-directories-exist *HPS_INPUT_PATH*  )

	(format t "~% Verifying path to output directory for the HPS transform to be  ....: [~S]" *HPS_OUTPUT_PATH* )
	(ensure-directories-exist *HPS_OUTPUT_PATH* )
	;; *******************************************************************************************************

	;; set to a lean output mode as default, no detailed report with graphics and analyses
	(setf *HPS_FULL_OUTPUT_MODE*            		nil)

	;; *******************************************************************************************************
	(cond   ((equal APPMODE 0)
			(HPS_DNA_APPLICATION 	:OUTPUT_MODE		  nil 				;;; OUTPUT
				  		:HPS_DNA_WWW_ACCESS	  *HPS_ACCESS_WWW_SEQS* 	;;; WEB ACCESS
				  		:HPS_DNA_LDB_ACCESS	  *HPS_ACCESS_LDB_SEQS*		;;; LDB
				  		:HPS_TESTSEQUENCE_NUMBER  *HPS_TESTSEQUENCE_NUM* 	;;; TEST SEQUENCE
				  		:HPS_SIGNSEQUENCE_NUMBER  *HPS_SIGNATURESEQ_NUM* 	;;; SIGNATURE
				  		:HPS_SIGNSEQUENCE_FROM    *HPS_SIGNATURE_FROM_INDEX*   	;;; FROM
				  		:HPS_SIGNSEQUENCE_TO      *HPS_SIGNATURE_TO_INDEX*  	;;; TO
				  		:HPS_MUTATE_SIGNATURE	  *HPS_MUTATE_SIGNATURE* )	;;; MUTATION CONTROL

			;; archive the data of the autogenerated analyses for the DNA application of signature against test sequence
			(setf HPS_ARCHIVAL_MODE nil)
			(when HPS_ARCHIVAL_MODE
				(HPS_system_interface 	:COMMAND 	'PACK
			       	        		:DATUM 		(format nil "SIG~A[~D-~D]_SEQ~A" *HPS_SIGNATURESEQ_NUM*
				  							 		 *HPS_SIGNATURE_FROM_INDEX*
				  							 		 *HPS_SIGNATURE_TO_INDEX*
											 		 *HPS_TESTSEQUENCE_NUM*)))
		
			nil)
	        ;; *******************************************************************************************************


	        ;; *******************************************************************************************************
	  	((equal APPMODE 1) 
			;; initialization of random state used in random generators, if any - previously this was above
			(HPS_random_init :randomvals_file      "E:/HPS_DATA/HPS_INPUTS/HPS_RANDOM_VALUES.DAT")

		 	(HPS_STENO_APPLICATION	 	:OUTPUT_MODE 		   nil))
	        ;; *******************************************************************************************************


	        ;; *******************************************************************************************************
  		((equal APPMODE 2)
			(HPS_TEST_APPLICATION	 	:OUTPUT_MODE 		   t))
	        ;; *******************************************************************************************************


	        ;; *******************************************************************************************************
  		((equal APPMODE 3)
			(HPS_THIN_APPLICATION	 	:OUTPUT_MODE 		   nil))
	        ;; *******************************************************************************************************


	        ;; *******************************************************************************************************
  		((equal APPMODE 4)
			;; ***********************************************************************************************
			;; the DNA sequences ID from the ENTREZ DNA database 
			;; ***********************************************************************************************
			(setf HPS_DNA_ECOLI_PART_1 '( 	1786520TESTCASE )) 
			(setf HPS_DNA_ECOLI_GENOME '(   1786520TESTCASE 1786181 1786192 2367095 1786217 1786230 
							1786415 2367098 2367099 1786454 1786465 2367103 2367108 
							1786501 1786510 1786520 1786532 1786542 1786554 1786568 
							1786580 1786596 1786603 1786614 1786628 1786639 1786649 
							1786660 1786671 1786683 1786692 1786705 1786716 1786728 
							1786739 1786751 1786766 1786782 1786790 1786800 1786808 
							1786819 1786836 1786849 1786862 1786875 1786888 1786896 
							1786910 1786920 1786934 1786947 1786955 1786967 1786978 
							1786988 1786998 1787015 1787025 1787036 1787047 1787058 
							1787070 1787084 1787097 1787106 1787115 1787125 1787134 
							1787148 1787156 1787169 1787180 1787189 1787202 2367111 
							2367113 1787233 1787248 1787256 1787265 1787282 1787293 
							1787308 1787322 1787332 1787345 1787358 1787371 1787382 
							1787405 1787417 1787434 1787444 1787453 1787467 1787476 
							1787486 1787496 1787509 1787523 2367115 1787543 2367117 
							1787566 1787578 1787588 1787600 1787613 1787633 1787643 
							1787652 1787665 1787673 1787682 1787695 1787706 1787720 
							1787730 1787742 1787752 1787764 1787773 1787783 1787790 
							1787801 1787814 2367119 1787841 1787862 1787875 1787888 
							1787898 2367121 1787921 1787935 1787945 1787955 1787966 
							1787978 2367122 1787997 1788011 1788022 1788033 1788045 
							1788058 1788067 1788078 1788089 1788106 1788117 1788129 
							1788139 1788154 1788163 1788179 1788189 1788200 1788214 
							1788229 1788241 1788257 2367124 1788285 2367125 1788298 
							1788310 2367127 1788338 1788354 1788373 1788382 1788395 
							1788413 1788425 2367129 1788447 1788456 1788470 1788479 
							1788489 1788498 1788508 1788520 2367131 1788547 1788555 
							2367132 1788570 1788582 1788594 1788605 1788623 1788634 
							1788647 1788659 1788672 1788684 1788694 1788709 1788718 
							1788731 2367135 2367137 1788763 1788775 1788789 1788805 
							1788813 1788821 1788839 1788850 1788862 1788870 1788883 
							1788899 1788907 2367139 1788927 1788939 2367141 2367142 
							1788975 2367143 2367147 1789011 1789024 1789037 2367149 
							2367151 2367153 2367155 2367156 1789110 2367157 2367160 
							1789143 1789153 2367162 2367163 1789185 1789195 2367165 
							2367168 2367170 1789239 2367171 2367173 1789270 1789282 
							2367176 2367178 1789319 2367179 1789344 2367181 2367182 
							2367184 2367186 1789405 2367187 1789431 1789441 1789451 
							2367189 2367191 2367194 1789499 2367197 1789524 1789536 
							2367199 1789562 2367201 2367203 1789607 1789619 2367205 
							2367207 1789659 2367209 2367211 1789694 1789718 1789734 
							2367213 1789758 2367215 1789783 1789798 2367219 2367220 
							2367222 2367227 1789840 1789854 1789868 1789880 2367230 
							2367232 1789910 1789919 1789931 2367235 2367238 1789957 
							2367241 1789977 1789989 2367244 2367246 2367249 2367251 
							1790036 2367252 1790063 2367253 2367255 2367258 1790105 
							2367261 2367266 1790142 2367269 1790166 2367272 1790188 
							2367276 2367278 2367282 2367291 2367294 2367299 2367306 
							2367315 2367318 1790295 2367320 2367324 2367326 2367328 
							1790356 1790374 1790385 2367332 1790404 2367333 2367336 
							1790440 1790448 1790456 2367338 2367339 2367340 2367344 
							2367346 2367349 2367351 2367352 1790563 1790574 1790582 
							2367354 1790607 2367356 2367357 1790649 2367360 1790670 
							2367361 2367366 1790711 2367368 1790732 2367369 2367372 
							2367374 1790777 1790789 2367375 2367377 2367380 2367382 
							2367383 1790858 ))
			(setf HPS_DNA_ECOLI_GENOME HPS_DNA_ECOLI_PART_1)
			;; ***********************************************************************************************

			;; ***********************************************************************************************
			(HPS_DNA_DBMINER_APPLICATION  	:OUTPUT_MODE 		   nil
					  		:TEST_SEQUENCES		   HPS_DNA_ECOLI_GENOME
				     	  		:REMOTE_DNASEQ_DB 	   nil
						        :LOCAL_DNASEQ_DB  	  "E:/HPS_DATA/HPS_INPUTS/DNA_E_COLI_K12/"
					  		:SIGNATURE_SEQNUM 	   1786520
					  		:SIGNATURE_FROMINDEX 	   5000
					  		:SIGNATURE_TOINDEX   	   6000
					  		:SIGNATURE_MUTATION        nil ))		
			;; ***********************************************************************************************
	        ;; *******************************************************************************************************


	        ;; *******************************************************************************************************
  		(T      (HPS_THIN_APPLICATION 	 	:OUTPUT_MODE 	nil)))
	        ;; *******************************************************************************************************
	;; *******************************************************************************************************

	(when *HPS_FULL_OUTPUT_MODE* 
		(format t "~% HTML report has been created with the results of the analysis in...: [~S]" *HPS_OUTPUT_PATH* ))

	(unless *HPS_FULL_OUTPUT_MODE* 
	  	nil)

  	;; cleanup previous lingered state, if execution was aborted
	(format t "~% Marking lingered memory state for clean-up ....................... : " nil )
  	(HPS_cleanup *HPS_FULL_OUTPUT_MODE* )

	(HPS_print_separator 2)

	(pprint 'HPS_DONE))
;; ****************************************************************************************************************


;; ****************************************************************************************************************
;; this is the invocation of the main entry point to the HPS applications demo
;; ****************************************************************************************************************
(defun MAIN ( &key (WHICH_APP 0)) 


	(setf HPS_APPMODE 	     (nth WHICH_APP
				       '( ( 'HPS_DNA 	
					    "a sublinear time DNA pattern miner and sequence aligner relying in O(N) HPS preprocessing" ) 
					  ( 'HPS_STENO 	
					    "a large-bandwidth STENOGRAPHY (noise signal) encryption/decryption application" )
					  ( 'HPS_TEST 	
					    "an online HPS transform application with full processing" )
					  ( 'HPS_THIN 	
					    "an online HPS transform application w/o overhead processing" )
					  ( 'HPS_DNA_DBMINER
					    "a DNA DB PATTERN miner based on the HPS transform" ))))

	(format t "~% Invoking the HPS ~A APPLICATION OF THE HPS TRANSFORM .............. : [~A]" (first  HPS_APPMODE) 
												 (second  HPS_APPMODE))

	(HPS_APP_main 			:BASE_DIR	"E:/HPS_DATA/"
		  			:APPMODE  	WHICH_APP)

	(format t "~%" nil))
;; ****************************************************************************************************************


;; ****************************************************************************************************************
;; THE INVOCATION OF HPS APPLICATIONS
;; ****************************************************************************************************************
	(setf   HPS_COMPILE_AND_LOAD_IS_ON t)
	(when   HPS_COMPILE_AND_LOAD_IS_ON 
		(MAIN :WHICH_APP 	4))
;; ****************************************************************************************************************


