;; ********************************************************************************************
;;                   SHARED FUNCTIONS BETWEEN ENCODER AND DECODER
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


;; ******************************************************************************************************
;; this is the encryption code (not an essential part of the system). It is used primarely to map the
;; most frequent letters to lower representative values. Representative values represent the means of
;; HPS states, which are to be hidden within a noise signal. Therefore, lower values of such, in particular
;; for the most frequent letters are desirable. 
;; Ed. Note: Paper has to discussed whether this code can be compromised and what happens as well as optimal
;; coding values with respect to the amplitude of the noise, (e.g., what limits of SNR are achievable by the
;; HPS transform with respect to the ration of these values and the amplitude of the underlying noise signal.
;; the dictionary table has the form of ((X -13) (Z -12) ...
;; ******************************************************************************************************
(defun STENO_set_cypher ( alphabet mapcode &key (maplow  (HPS_minimum  mapcode )) 
				                (maphigh (HPS_maximum  mapcode )) 
						(maphalf (HPS_median   mapcode ))
  						(alphabet_description  nil)
  						(cypher_description    nil))
	(setf *STENO_mapalphabet*  	alphabet)
	(setf *STENO_mapcode*      	mapcode)
	(setf *STENO_maplow*       	maplow)
	(setf *STENO_maphigh*      	maphigh)
	(setf *STENO_maphalf*      	maphalf)
	(setf *STENO_mapsize*      	(length *STENO_mapcode*))
	(setf *STENO_alphabetdescr* 	alphabet_description)
	(setf *STENO_cypherdescr*  	cypher_description)

	;; this generates a table array, which represents a dictionary for the letters and codes, 
	(setf *STENO_mapdict* (mapcar #'(lambda (x y) (list x y)) *STENO_mapalphabet* *STENO_mapcode*))

	;; this generates a reverse translation table for the above 
	(setf *STENO_reversedmapdict* (mapcar #'reverse *STENO_mapdict*)))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; this function retrieves the value associated with a letter
;; ******************************************************************************************************
(defun STENO_getmapcode (x) 
  	(second (assoc x *STENO_mapdict*)))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; this function retrieves the letter associated with a code
;; ******************************************************************************************************
(defun STENO_getmapletter (x) 
  	(setf *STENO_tval* (second (assoc (round x) *STENO_reversedmapdict*)))
  	(if (equal *STENO_tval* nil) 
		(second (assoc (round (+ x (expt -1 (random 2)))) *STENO_reversedmapdict*))
		(return-from STENO_getmapletter *STENO_tval*)))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; this is an array containing the ordered (singled) characters that make up the input string 
;; ******************************************************************************************************
(defun STENO_convert_to_list ( inputtext )
	(setf *temp_STENO_inputlist* (loop for char across inputtext collect char)))
;; ******************************************************************************************************


;; ******************************************************************************************************
;; maps ascii codes (those contained within *inputalphabet* into uppercase symbols
;; a call to this function is done as follows: (STENO_mapascii '#\k ) which results in --> K
;; ******************************************************************************************************
(defun STENO_mapascii ( char ) 
  	;; (setf *STENO_retval* (format nil "~A" char)))			;;; CHECK !!!!!!!!!!!!
	(first (cond 	
		 	((equal char '#\Space) (list '_))
			((equal char '#\a) (list 'a))
			((equal char '#\b) (list 'b))
			((equal char '#\c) (list 'c))
			((equal char '#\d) (list 'd))
			((equal char '#\e) (list 'e))
			((equal char '#\f) (list 'f))
			((equal char '#\g) (list 'g))
			((equal char '#\h) (list 'h))
			((equal char '#\i) (list 'i))
			((equal char '#\j) (list 'j))
			((equal char '#\k) (list 'k))
			((equal char '#\l) (list 'l))
			((equal char '#\m) (list 'm))
			((equal char '#\n) (list 'n))
			((equal char '#\o) (list 'o))
			((equal char '#\p) (list 'p))
			((equal char '#\q) (list 'q))
			((equal char '#\r) (list 'r))
			((equal char '#\s) (list 's))
			((equal char '#\t) (list 't))
			((equal char '#\u) (list 'u))
			((equal char '#\v) (list 'v))
			((equal char '#\w) (list 'w))
			((equal char '#\x) (list 'x))
			((equal char '#\y) (list 'y))
			((equal char '#\z) (list 'z))
			((equal char '#\A) (list 'a))
			((equal char '#\B) (list 'b))
			((equal char '#\C) (list 'c))
			((equal char '#\D) (list 'd))
			((equal char '#\E) (list 'e))
			((equal char '#\F) (list 'f))
			((equal char '#\G) (list 'g))
			((equal char '#\H) (list 'h))
			((equal char '#\I) (list 'i))
			((equal char '#\J) (list 'j))
			((equal char '#\K) (list 'k))
			((equal char '#\L) (list 'l))
			((equal char '#\M) (list 'm))
			((equal char '#\N) (list 'n))
			((equal char '#\O) (list 'o))
			((equal char '#\P) (list 'p))
			((equal char '#\Q) (list 'q))
			((equal char '#\R) (list 'r))
			((equal char '#\S) (list 's))
			((equal char '#\T) (list 't))
			((equal char '#\U) (list 'u))
			((equal char '#\V) (list 'v))
			((equal char '#\W) (list 'w))
			((equal char '#\X) (list 'x))
			((equal char '#\Y) (list 'y))
			((equal char '#\Z) (list 'z))
			(T NIL))))
;; ******************************************************************************************************










