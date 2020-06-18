# ######################################################################################################
opts = options(width=206,digits=2, error = function() traceback(2))
LICENSETEXT = "These R code samples (version Sep/2014), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License." 
message( LICENSETEXT )
message("")
# ######################################################################################################


# ######################################################################################################
# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
# ######################################################################################################
library(rpart)				    # Popular decision tree algorithm
library(rpart.plot)				# Enhanced tree plots
library(randomForest)           # randomForest for variable importance and error
library(e1071)
library(class)
library(stringr)
library(parallel)
# ######################################################################################################


# ###############################################################################
source( 'utilities.R' )
OLD_CA = commandArgs()
commandArgs <- function() list(DO_TESTS=FALSE, TEST_ENABLED=FALSE, DO_DISTANCE_VALIDATION=FALSE, RUN_VALIDATE_SGD=FALSE )
VERIFY_OPTARG ( OPTARG_NAME="TEST_ENABLED",  OPTARG_VALUE=FALSE)
# ###############################################################################


# ###############################################################################
source( 'datasets.R' )
source( 'decision_trees.R' )
source( 'utilities.R' )
# ############################################################################################


