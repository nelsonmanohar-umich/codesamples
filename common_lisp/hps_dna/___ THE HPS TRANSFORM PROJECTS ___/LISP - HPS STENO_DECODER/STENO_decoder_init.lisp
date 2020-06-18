;; ****************************************************************************************************
;;                        EXAMPLE INVOCATION OF THE ITERATIVE STENO DECODER
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
;; ********************************************************************************************


;; ****************************************************************************************************
;; function decodes a steno time series looking for the presence of HPS states which are then 
;; mapped into a text string by using a shared knowledge alphabet-code mapping (shown below). 
;; the function reads the HPS time series, which is required to be of 3601 samples, as produced,
;; by the simulator and then generates the corresponding HPS state sequence. These HPS states
;; are of variable length and their mean value represents a representative value coding for a letter.
;; ****************************************************************************************************
(defun STENO_decoder_driver_init ()
	;; the description of the encoding, an alphabet and corresponding numeric mappings for each member of the alphabet
	;; obviously, the message to be encoded/decoded can ONLY have characters that have been defined in this alphabet.
	(setf *STENO_mapalphabet* '(   
	       B   G   Y   A   K   E   V   H   _   R   I   X   W   N   L   P   T   Q   D   S   O   J   M   C   F   Z   U 	))
	(setf *STENO_mapcode*     '(
	      -13 -12 -11 -10 -9  -8  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7   8   9   10  11  12  13	))

	;; if your alphabet has numerical gaps, you must have care on its construction and set specialized fields
	;; that describe within the high (:maphigh), low (:maplow) and middle (:maphalf) points of your coding alphabet
	;; otherwise, these values are computed automatically for you
	(setf *STENO_mapcode*     (STENO_set_cypher *STENO_mapalphabet* *STENO_mapcode*
				    		    :alphabet_description "A SIMPLE LOWERCASE MINIMALISTIC ALPHABET"
				  		    :cypher_description   "AN APPROXIMATE INVERSE FREQUENCY SUBSTITUTION CYPHER" )))





