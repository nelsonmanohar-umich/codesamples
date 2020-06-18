# ######################################################################################################
# MACHINE LEARNING TOOLBOX FILES IN R
#           Copyright (C) Nelson R. Manohar
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# ######################################################################################################
# @AUTHOR:  Nelson R. Manohar Alers
# @EMAIL:   manohar.nelson@gmail.com
# @DATE:    September, 2014
# @URL:     http://www.bitbucket.org/nelsonmanohar/machinelearning
# ######################################################################################################





# ############################################################################################
#                                      IMPORTANT 
#                                      IMPORTANT 
#                                      IMPORTANT 
# ############################################################################################
load('T3.RData')
# ############################################################################################


# ############################################################################################
# Execution environment
# ############################################################################################
source( 'clt_options.R' )
# ############################################################################################


# ######################################################################################################
opts = options(width=206,digits=2, error = function() traceback(2))
LICENSETEXT = "These R code samples (version Sep/2014), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License." 
message( LICENSETEXT )
message("")
# ######################################################################################################


# ############################################################################################
# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
# ############################################################################################
library(rpart)				    # Popular decision tree algorithm
library(rpart.plot)				# Enhanced tree plots
library(randomForest)           # randomForest for variable importance and error
library(e1071)
library(class)
library(stringr)
# ############################################################################################


# ############################################################################################
source( 'utilities.R' )
OLD_CA = commandArgs()
commandArgs <- function() list(DO_TESTS=FALSE, TEST_ENABLED=FALSE, DO_DISTANCE_VALIDATION=FALSE, RUN_VALIDATE_SGD=FALSE )
VERIFY_OPTARG ( OPTARG_NAME="TEST_ENABLED",  OPTARG_VALUE=FALSE)
# ############################################################################################


# ############################################################################################
source( 'datasets.R' )
source( 'decision_trees.R' )
source( 'utilities.R' )
source( 'fselect.R' )
source( 'clt_basic_functions.R' )
source( 'clt_prob_enhancer.R' )
# ############################################################################################
