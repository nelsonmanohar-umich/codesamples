(proclaim '(optimize (speed 3) (safety 1) (space 2) (debug 1)))
;; (proclaim '(optimize (speed HPS_COMPILER_SPEED) (safety HPS_COMPILER_SAFETY) (space HPS_COMPILER_SPACE) (debug HPS_COMPILER_DEBUG)))
;; ******************************************************************************************************
;;                            RANDOM FUNCTIONS OF UNKNOWN PERFORMANCE
;; ******************************************************************************************************


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
(defun HPS_setup_indirection ( ofsize )
  	(setf alist_temp nil)
  	(dotimes (i ofsize 'RANDOM)
		(push i alist_temp))
	(return-from HPS_setup_indirection alist_temp ))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; ******************************************************************************************************
(defun HPS_shuffle_this ( alist )
  	(dotimes (i *HPS_RANDOM_RANGE_LIMIT* alist)
	  	(setf j (random *HPS_RANDOM_RANGE_LIMIT* ))
		(setf temp (aref alist j)) 
		(setf orig (aref alist i)) 
		(setf (aref alist i) temp) 
		(setf (aref alist j) orig))
	(return-from HPS_shuffle_this alist))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; ******************************************************************************************************
(defun HPS_random_sample_from ( alist )
	(return-from HPS_random_sample_from (aref alist (random (length alist)))))
;; ******************************************************************************************************

	  	
;; ******************************************************************************************************
;; this retrieves a value from the random values array
;; ******************************************************************************************************
(defun HPS_random_init( &key (randomvals_file "C:/HPS_DATA/HPS_INPUTS/HPS_RANDOM_VALUES.DAT" ) )
	(setf *STENO_RANDOM_LIMIT* 16383)
	(setf *STENO_randomvalues_series* (subseq (HPS_read_series randomvals_file) 0 *STENO_RANDOM_LIMIT*))
	(setf *STENO_random_index* 0)
	(setf *STENO_randomvalues_array* (coerce *STENO_randomvalues_series* 'VECTOR))
	(return-from HPS_random_init *STENO_RANDOM_LIMIT* ))

(defun STENO_RANDOM_VAL ()
  	(incf *STENO_random_index*)
	(if (>= *STENO_random_index* *STENO_RANDOM_LIMIT*) (setf *STENO_random_index* 0) nil)
	(return-from STENO_RANDOM_VAL (second (aref *STENO_randomvalues_array* *STENO_random_index*))))

(defun HPS_random ( limit ) 
  	(setf *STENO_random_val* (STENO_RANDOM_VAL))
  	(setf *STENO_random_val* (round (* *STENO_random_val* limit)))
  	(return-from HPS_random *STENO_random_val*))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; interpolation function - borrowed from template
;; ******************************************************************************************************
(defun HPS_UNIFORM (a b)
	(setf *STENO_random_uniform_val* (round (+ a (* (- b a) (STENO_RANDOM_VAL)))))
	(return-from HPS_UNIFORM *STENO_random_uniform_val* ))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; this is a conditioning function template from within the HPS noise is added to an input signal 
;; this can also be done with the transform conditioning functions, later on the driver function 
;; span a particular random cloud that increases the discrimination power (SNR) of the HPS transform
;; this fractional number is an uniformly distributed random number between [0, 1)
;; or every other iteration becomes a cancellation factor to the previous iteration's random number
;; scale fractional number into the specified range or produce the previous iteration complement
;; ******************************************************************************************************
(defun HPS_random_noise ( NOISE_SCALING_FACTOR )
  	(setf *nval_temp* (HPS_UNIFORM 0 NOISE_SCALING_FACTOR ))
  	(return-from HPS_random_noise *nval_temp*))
;; ******************************************************************************************************







