(eval-when (eval compile)
	(proclaim '(optimize (speed 3 ) (safety 2 ) (space 1 ) (debug 1 ))))

(defpackage "HPS"
  	(:use "COMMON-LISP"))

;; "(in-package :hps)"
(in-package :"HPS")

;(EXPLAIN-COMPILER-SETTINGS)

(setf   COMPILER:*CLTL1-COMPILE-FILE-TOPLEVEL-COMPATIBILITY-P*  t )

(defvar HPS_COMPILE_AND_LOAD_IS_ON 			        nil  	"description" )
(defvar HPS_COMPILER_SPEED  					3 	"description" )
(defvar HPS_COMPILER_SAFETY 					2 	"description" )
(defvar HPS_COMPILER_SPACE  					1 	"description" )
(defvar HPS_COMPILER_DEBUG  					1 	"description" )




