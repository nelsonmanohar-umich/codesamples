;; ***********************************************************************************************************************
;; DR. NELSON R. MANOHAR (C) 2007 - FROM "THE HPS_DNA_ TRANSFORM AND ITS APPLICATIONS" - ALL RIGHTS RESERVED - MAY/JUN 2007.
;; ***********************************************************************************************************************


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


;; ***********************************************************************************************************************
;; DR. NELSON R. MANOHAR (C) 2007 - FROM "THE HPS_DNA_ TRANSFORM AND ITS APPLICATIONS" - ALL RIGHTS RESERVED - MAY/JUN 2007.
;;
;;
;; The functions on this file implement an approximate pattern match search for a signature within test sequence. 
;; Both the signature and test sequence are time series of HPS_DNA_ segments. Both these were generated using the HPS_DNA_
;; transform at common baselining values (e.g., HPS_DNA_(60, 30, 0.001)).
;;
;; The approximate pattern match search looks for all the possible instances of the signature within the test sequence.
;; A match is produced when a significant number of ordered HPS_DNA_ segments in a signature are approximately matched to
;; an appropiate set of HPS_DNA_ segments in the test sequence. 
;;
;; The match is a (conditional) probabilistic match based on the combined probability of an ordered set of HPS_DNA_ segments 
;; in the signature to approximately match an ordered set of HPS_DNA_ segments in the test sequence. To do this, each 
;; individual approximate match attempts to find for each segment in the signature, an instance within the test 
;; sequence which has similar duration, relative positioning within the time series, and similar HPS_DNA_ targeting value. 
;;
;; All repeated approximate repeats of the presence of the signature within the test sequence are found and reported. 
;; Note that even overlapping instances are also uncovered. The overall complexity of the approximate pattern match
;; is as follows:
;; 	if n is the number of HPS_DNA_ segments in the signature and m is the number of HPS_DNA_ segments in the test sequence
;; 	the algorithm has O(m(m*n)) time complexity (i.e., time complexity is quadratic on the HPS_DNA_ fractality of the 
;; 	test sequence). This relatively low complexity is achieved through multiple space tradeoffs. 
;; 	The reader should note that this complexity is relatively low once one realizes that m and n are HPS_DNA_ fractalities, 
;; 	and thus could often be several orders of magnitude in reduction from the size of their underlying time series. 
;;
;; 	For example, for a test sequence such as the DNA which prior to HPS_DNA_ encoding was of size equal to 1100 units, 
;; 	its HPS_DNA_ approximation had only 20 or so non-trivial HPS_DNA_ segments. If the test sequence had 60 non-trivial 
;; 	segments, then algorithm complexity is of the order of O( 60 * 60 * 20) which compares favorably to O( 3600^2)
;; 	complexity needed to deterministically find approximate substrings.
;;
;; 	This code is for demonstration purposes and is not intended as an efficient implementation of the approximate pattern
;; 	match but rather as an illustration of such being possible, something which was argued could not be done by those
;; 	who like to critique hoping to sabotage credibility.
;;
;; Each instance is ranked in terms of a cost metric which provides a relative comparison of how good an approximate
;; pattern match is an individual instance. The user is provided the opportunity to reduce the output by choosing a
;; threshold point, for which approximations exhibiting a cost metric higher than such are discarded. 
;;
;; The output consist of a table of the resulting approximate pattern match for repeats of the HPS_DNA_ signature within the
;; test sequence together with cost metrics information.  Of course, the (conditional) probability of the approximate 
;; pattern match could also be computed but that is not done yet.
;; ***********************************************************************************************************************








