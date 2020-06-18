;; ********************************************************************************************
;;                              LOCAL UTILITY FUNCTIONS - TO BE REMOVED
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


;; ***********************************************************************************************************************
;; this function looks at an HPS_DNA_ segment and returns true if the segment start time index is LESS THAN the value of hypothesis_start
;; recall that the HPS_DNA_ segment tuple is of the form ( *start* *end* *dur* *nval* *code* ['SMALLSEG])
;; ***********************************************************************************************************************
(defun HPS_DNA_start_lt ( HPS_DNA_segment &key 
					  (hps_pivot_index *HPS_DNA_pivot_timeindex*))
	(if (< hps_pivot_index (first HPS_DNA_segment))
	  	t 
		nil))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(defun HPS_DNA_start_gt (HPS_DNA_segment &key 
					 (hps_pivot_index *HPS_DNA_pivot_timeindex*))
	(if (>= hps_pivot_index (first HPS_DNA_segment)) 
	  	t 
		nil))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; determines if a number x is within the inclusive interval [low high]
;; ***********************************************************************************************************************
(defmacro IS_BETWEEN (x low high ) 
  	`(if (AND (>= ,x ,low) (<= ,x ,high)) t nil))
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_DNA_is_between (x low high ) 
  	`(if (AND (>= ,x ,low) (<= ,x ,high)) t nil))
;; ***********************************************************************************************************************










