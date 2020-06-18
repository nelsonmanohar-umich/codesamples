# ##################################################################################
# TO DO
# ##################################################################################
# knn (should work here if not randomized)
# clustering 
# build learning curves code for cv and train and test
# check dimensionality reduction on the original data or the new data
# ##################################################################################






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
# Execution environment
# ############################################################################################
source( 'clt_options.R' )

if ( CLT_OPTIONS$'do options logging' ) {
    logging_dir <- system('clt_datalog.sh')
}


file.pipe <- pipe('hostname')
    WHERE = read.table(file.pipe, stringsAsFactors=FALSE)[1]
    AWS = CLT_OPTIONS$'running on aws'
    LAPTOP = !AWS
    print( paste( WHERE, AWS, LAPTOP ))

    WHERE = CLT_OPTIONS$'workspace directory'
    if ( WHERE == 'TECRA10' ) setwd( '/home/nrm/LOCAL/ALGORITHMS/CLT_AWS/' )
    if ( WHERE == 'T6859u' )  setwd( '/WORKSPACE/R/' )
    if ( WHERE == 'MM061' )  setwd( '/WORKSPACE/R/' )
    if ( AWS ) setwd( '/home/ubuntu/' )
# ############################################################################################





# ############################################################################################
# load datasets
OUTPUT_FILE = CLT_OPTIONS$'output logfile'
sink( OUTPUT_FILE, split=TRUE)
# ######################################################################################################


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
# ############################################################################################


# ############################################################################################
# VERY IMPORTANT: EMSEMBLE MODEL DEFINITION IN TERMS OF COLUMNS
# ############################################################################################
EMSEMBLE_MODEL_DEFINITIONS = CLT_OPTIONS$'model_column_definitions_file'
source( EMSEMBLE_MODEL_DEFINITIONS )
# ############################################################################################


# ############################################################################################
source( 'clt_basic_functions.R' )
source( 'clt_nb_formula.R' )
# ############################################################################################






# ############################################################################################
BANNER( 'INITIALIZING' )
# ############################################################################################
USE_EXPLORATORY_TRANSFORMS      = FALSE
CENTERS = NA
SCALES  = NA
YPF     = NA
# #################################################################################


# #################################################################################
# PROBABILITY ENHANCERS
# #################################################################################
PROBABILITY_ENHANCE     = TRUE
    PROB_CORRECTION_MODEL_A = NA
    PROB_CORRECTION_MODEL_B = NA
    PROB_CORRECTION_MODEL_1 = NA
    PROB_CORRECTION_MODEL_2 = NA
    PROB_CORRECTION_MODEL_3 = NA
    PROB_CORRECTION_MODEL_4 = NA
    PROB_CORRECTION_MODEL_5 = NA
    PROB_CORRECTION_MODEL_0 = NA
    PROB_CORRECTION_MODEL_6 = NA
    PROB_CORRECTION_MODEL_7 = NA
    PROB_CORRECTION_MODEL_8 = NA
    PROB_CORRECTION_MODEL_9 = NA
    STANDBY_MODEL = NA

PROB_CORRECTION_THRESHOLD= 0.62
KNN_PROB_CORRECTION_DATA= NA
# #################################################################################

# #################################################################################
NUM_MODELS = 1+9+2
WHICH_1GBSTORAGE_INDEXES = seq(1,404,3)
WHICH_1GBTRAINING_CHUNKS = seq(0,9,3)
NCV_ITERS = length(WHICH_1GBSTORAGE_INDEXES) * length(WHICH_1GBTRAINING_CHUNKS)
# #################################################################################


# #################################################################################
USE_PRECOMPUTED_MODELS  = FALSE
if (USE_PRECOMPUTED_MODELS) { load('pcm.rdata') }
# #################################################################################


# #################################################################################
BIGW                     = 1/3
LILW                     = 0.10
NB_W                     = 1.0
# #################################################################################


# #################################################################################
USE_NAIVE_BAYES          = TRUE
LAPLACE                  = 8
NB_THRESHOLD             = 1E-18 ; NB_EPS                          = 1E-18
NB_THRESHOLD             = 1E-12 ; NB_EPS                          = 1E-12
# #################################################################################


# #################################################################################
CVTEST                   = TRUE
# #################################################################################


# #################################################################################
COLNAMES = c( 'ad_id', 'click', 'YYMMDDHH', 'C01', 'banner_pos', 'site_id', 'site_domain', 'site_category', 'app_id', 'app_domain', 'app_category', 'device_id', 'device_ip', 'device_model', 'device_type', 'device_conn_type', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21' )
Ctypes = c( 'C01', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21' )
Htypes = c( 'H01', 'H14', 'H15', 'H16', 'H17', 'H18', 'H19', 'H20', 'H21')
correlated  = c("H18", "H14", "C14", "H15", "H16", Htypes)
minus = function( x, arg ) { setdiff( x, arg ) }
# #################################################################################






# #################################################################################
NEWLINE(20)
BANNER( 'LOADING TRAINING DATASET' )
    # ###############################################################################
    # ###############################################################################
    #                  DATASETS are DEFINED IN CLT_OPTIONS.R
    # ###############################################################################
    # ###############################################################################

    # ###############################################################################
    # POINTS TO THE SLICED DATASET BEING TRAINED WITH
    # ###############################################################################
    PREFIX = CLT_OPTIONS$'dataset scale prefix'
    # ###############################################################################

    DAYOWEEK_TRAINSET = c(sprintf("%s%s",PREFIX,'subsampled_141021.csv'), # 21 - 20 T
                          sprintf("%s%s",PREFIX,'subsampled_141022.csv'), # 22 - 20 W
                          sprintf("%s%s",PREFIX,'subsampled_141023.csv'), # 23 - 20 T
                          sprintf("%s%s",PREFIX,'subsampled_141024.csv'), # 24 - 20 F
                          sprintf("%s%s",PREFIX,'subsampled_141025.csv'), # 25 - 20 S
                          sprintf("%s%s",PREFIX,'subsampled_141026.csv'), # 26 - 20 S
                          sprintf("%s%s",PREFIX,'subsampled_141027.csv'), # 27 - 20 M
                          sprintf("%s%s",PREFIX,'subsampled_141028.csv'), # 28 - 20 T
                          sprintf("%s%s",PREFIX,'subsampled_141029.csv'), # 29 - 20 W
                          sprintf("%s%s",PREFIX,'subsampled_141030.csv')) # 30 - 20 T

    if ( CLT_OPTIONS$'load secondary dataset'  ) {
        PREFIX = CLT_OPTIONS$'dataset scale 2nd prefix'
        DAYOWEEK_TRAINSET = c(DAYOWEEK_TRAINSET, 
                          sprintf("%s%s",PREFIX,'subsampled_141021.csv'), # 21 - 20 T
                          sprintf("%s%s",PREFIX,'subsampled_141022.csv'), # 22 - 20 W
                          sprintf("%s%s",PREFIX,'subsampled_141023.csv'), # 23 - 20 T
                          sprintf("%s%s",PREFIX,'subsampled_141024.csv'), # 24 - 20 F
                          sprintf("%s%s",PREFIX,'subsampled_141025.csv'), # 25 - 20 S
                          sprintf("%s%s",PREFIX,'subsampled_141026.csv'), # 26 - 20 S
                          sprintf("%s%s",PREFIX,'subsampled_141027.csv'), # 27 - 20 M
                          sprintf("%s%s",PREFIX,'subsampled_141028.csv'), # 28 - 20 T
                          sprintf("%s%s",PREFIX,'subsampled_141029.csv'), # 29 - 20 W
                          sprintf("%s%s",PREFIX,'subsampled_141030.csv')) # 30 - 20 T
    }
# ############################################################################################






# ############################################################################################
# SUBSUMED MODELS
# ############################################################################################
# IMPORTANT:
#   THE MODULE BELOW MUST BE EDITED TO MATCH REQUIREMENTS OF THE EMSEMBLE COMPUTE 
#   CAPACITY, RESOURCES, AND MODELING AND REPRESENTS GLOBAL STATE
# ############################################################################################
BANNER( 'LOADING EMSEMBLE MODEL SPECIFICATION' )
    USING_SUBSUMED_MODELS = CLT_OPTIONS$'use_subsumed_models'
    source('clt_model_subsuming.R')
# ############################################################################################





# ############################################################################################
# AGGREGATE PROBABILITIES
# ############################################################################################
BANNER( 'PRELOADING APP AND SITE PROBABILITIES TABLES' )
    ACP = NA
    SCP = NA
    EPS = NA
    F_APP_CAT  = NA
    F_SITE_CAT = NA
    A2C = NA
    S2C = NA
    A_ids = NA
    S_ids = NA
    S2CHP = NA
    A2CHP = NA
    source('clt_probs.R')
# ############################################################################################




# ############################################################################################
BANNER( 'LOADING OF DATA' )
    source('clt_dataloader.R')
    if ( CLT_OPTIONS$'do state checkpointing' )
        save.image(GET_MODEL_NAME('ORIGINAL_DATASET.RData'))
# ############################################################################################






# ############################################################################################
# VISUALIZATION GRAPHICS 1
# ############################################################################################
BANNER( 'VISUALIZATION STAGE 1: ORIGINAL FEATURES' )
if ( CLT_OPTIONS$'do visualization 1' ) {
    source('clt_viz1.R')
}
# ############################################################################################






# ############################################################################################
NEWLINE(20)
BANNER( 'TRANSFORMING TRAINING DATASET' )
    source('clt_traincoding.R')
    print( summary( ORIG_XTRAIN ) )
    if ( CLT_OPTIONS$'do state checkpointing' )
        save.image(GET_MODEL_NAME('TRANSFORMED_DATASET.RData'))
# ############################################################################################





# ############################################################################################
# DATASET READY
# ############################################################################################
BANNER( 'DATASET READY' )
    # ########################################################################################
    CVTEST = TRUE
    source('clt_activated_cols.R')
    # ########################################################################################

    # ########################################################################################
    ORIG_XTRAIN = ORIG_XTRAIN[,ACTIVATED_COLNAMES]
    PREDICT_COL = WHICH_COL_FOR( ORIG_XTRAIN, PREDICT_VAR )
    cat(HEADER)
    print( summary( ORIG_XTRAIN ) )
    cat(HEADER)
    str(ORIG_XTRAIN)
    cat(HEADER)
# ############################################################################################






# ############################################################################################
# OPTIONAL: CREATE TRAINING DATA FILES FOR FACTOR LEVEL MODELING
# NOTE:     REQUIRES SQL DB LOADED AS DESCRIBED IN CLT_CLASSIFIERS.SQL
# ############################################################################################
if ( CLT_OPTIONS$do_rje_processing ) {
    source('clt_aws_factor_level_prebuilder.R')

    RJE_BASED_MODEL_BUILDER(individual_builds=FALSE )

    STEP = 405
    for( i in 1:as.integer(405/STEP) ) {
        RJE_BASED_FACTOR_BASED_PREDICTOR(cvfold=i, until_cvfold=i+STEP, rdata=CLT_OPTIONS$'precomputed_aws_image', delay=30)
    }
}
# ############################################################################################






# ############################################################################################
# VISUALIZATION GRAPHICS 1
# ############################################################################################
if ( CLT_OPTIONS$'do visualization 2' ) {
    NEWLINE(20)
    BANNER( 'VISUALIZATION STEP: TRANSFORMED FEATURES' )
    source('clt_viz2.R')

    if ( CLT_OPTIONS$'do options logging' ) {
        BANNER("LOGGING")
        logcmd=sprintf("./clt_snapshot_metrics.sh %s", CLT_OPTIONS$'output logfile' )
        logging_data <- system(logcmd)
    }
}
# ############################################################################################






# ############################################################################################
# investigation of class weights: 100 to 1 (normalized)
# ############################################################################################
BANNER( 'DEVELOPMENT OF CLASS WEIGHTS' )
ADDITIONAL_INSIGHT = TRUE
    if ( ADDITIONAL_INSIGHT ) {
        initial_wt = GET_WEIGHT_VECTOR_FOR_UNBALANCED_CLASS( Y_TRUE )
        initial_wt[ initial_wt==max(initial_wt) ] = 5 * min(initial_wt)
        initial_wt = GET_WEIGHT_VECTOR_FOR_UNBALANCED_CLASS( Y_TRUE, w=initial_wt )
    }
# ############################################################################################






# ############################################################################################
# PRELIMINARY FEATURE SELECTION ANALYTICS
# ############################################################################################
if ( CLT_OPTIONS$'do feature selection' | CLT_OPTIONS$'do model training' ) {
    NEWLINE(20)
    BANNER( 'BASIC FEATURE EXPLORATION ANALYTICS' )
    MAXDEPTH=8
    MINBCKT=29
    MINSPLT=3*(MINBCKT+1)
    CP=1E-9
    CP_PRUNE=1E-5
    Y_TRUE = ORIG_XTRAIN[,YCOLNAME]
    source('clt_feature_selection_analytics.R')

    if ( CLT_OPTIONS$'do options logging' ) {
        BANNER("LOGGING")
        logcmd=sprintf("./clt_snapshot_metrics.sh %s", CLT_OPTIONS$'output logfile' )
        logging_data <- system(logcmd)
    }
}
# ############################################################################################







# ############################################################################################
# VISUALIZATION GRAPHICS 1
# ############################################################################################
if ( CLT_OPTIONS$'do visualization 3' ) {
    NEWLINE(20)
    BANNER( 'VISUALIZATION STEP: FEATURE FACTOR LEVELS' )
    source('clt_viz3.R')
}
# ############################################################################################







# ############################################################################################
NEWLINE(20)
BANNER( 'PRELIMINARY TRAINING STAGE' )
    source( 'clt_prob_enhancer.R' )

    if ( CLT_OPTIONS$'do options logging' ) {
        BANNER("LOGGING")
        logcmd=sprintf("./clt_snapshot_metrics.sh %s", CLT_OPTIONS$'output logfile' )
        logging_data <- system(logcmd)
    }

    # ########################################################################################
    # Trading precision and recall: decision boundary thresholds
    BANNER( 'SPECIFICATION OF PRECISION/RECALL TRADEOFF VIA THRESHOLD' )
    THRESHOLD = 0.5
    # ########################################################################################

    # ########################################################################################
    # exploratory model specifications
    # ########################################################################################
    BANNER( 'NB TRAINING STAGE' )
        source('clt_nb_formula.R')
        FORMULA6 = GET_NB_FORMULA()
        print ( FORMULA6 )
        if ( FALSE ) {
            BANNER( 'NB TRAINING STAGE: HAND CODED FEATURE SELECTED' )
            cmat = source('clt_naivebayes.R')
            cmat = cmat[1]$value
            print( cmat )
        }
    # ########################################################################################
# ############################################################################################






# ############################################################################################
if ( CLT_OPTIONS$'do model training' ) {
    NEWLINE(20)
    BANNER( 'TRAINING STAGE AND ENHANCED PREDICTOR TRAINING' )
    source('clt_train.R')

    if ( CLT_OPTIONS$'do state checkpointing' )
        save.image(GET_MODEL_NAME('MODELS_BEEN_TRAINED.RData'))

    if ( CLT_OPTIONS$'do options logging' ) {
        BANNER("LOGGING")
        logcmd=sprintf("./clt_snapshot_metrics.sh %s", CLT_OPTIONS$'output logfile' )
        logging_data <- system(logcmd)
    }
}
# ############################################################################################
CREATE_DEFAULT_MODELS( MAXDEPTH=NA, 
                       MINBCKT=NA, 
                       MINSPLT=NA, 
                       CP=NA, 
                       SPLIT_CRITERION=NA, 
                       DO_PRUNE=NA, 
                       FORCED=FALSE) 
# ############################################################################################






# ############################################################################################
if ( CLT_OPTIONS$'do analysis missing cases' ) {
    NEWLINE(20)
    BANNER( 'ANALYSIS OF MISSED CASES' )
    source('clt_missed_cases_analytics.R')
}
# ############################################################################################






# ############################################################################################
# incremental model refinements if any (add or step) 
# http://data.princeton.eu/R/glms.html
# ############################################################################################
if ( CLT_OPTIONS$'do exploration interaction terms' ) {
    NEWLINE(20)
    BANNER( 'EXPLORATION OF ADDITION INTERACTION TERMS' )
    if ( !USE_NAIVE_BAYES ) {
         ADD_MR = add1( BASE_MODEL, ~.^2, test="Chisq" )
         print( ADD_MR )
         cat(HEADER)
    }
}
# ############################################################################################






# ############################################################################################
# decision boundary thresholds
# ############################################################################################
if ( CLT_OPTIONS$'do exploration threshold values' ) {
    NEWLINE(20)
    BANNER( 'EXPLORATION OF THRESHOLD VALUES' )
    if ( FALSE & !USE_NAIVE_BAYES ) {
         ITERATIVE_BINARY_THRESHOLD_EVALUATOR( BASE_MODEL, ORIG_XTRAIN, Y_TRUE, BETA=1.0, THRESHOLDS=c( 0.0001, 0.001, 0.01, 0.1, 0.333, 0.5, 0.667) )
    }
}
graphics.off()
# ############################################################################################






# ############################################################################################
if ( CLT_OPTIONS$'do bootstrap analysis' ) {

    source( 'clt_prob_enhancer.R' )

    if ( !CLT_OPTIONS$'use individualized predictor' ) {
        NEWLINE(20)
        BANNER('EXPLORATION OF BOOTSTRAP AND LEARNING CURVES')
        MAXDEPTH=8
        MINBCKT=29
        MINSPLT=3*(MINBCKT+1)
        CP=1E-9
        CP_PRUNE=1E-5
        source('clt_bootstrap.R')
        if ( CLT_OPTIONS$'do state checkpointing' )
            save.image(GET_MODEL_NAME('BOOSTRAP_DONE.RData'))
    }
}
# ############################################################################################






# ############################################################################################
# release memory but keep factor specification used for training the classifier 
# ############################################################################################
if ( CLT_OPTIONS$'do release training dataset' ) {
    BANNER( 'RELEASING TRAINING DATASET' )
    if ( nrow(ORIG_XTRAIN) > 100000 ) {
        gc(T)
        t=ORIG_XTRAIN[1:1000,]
        gc(T)
        rm(ORIG_XTRAIN)
        gc(T)
        ORIG_XTRAIN=t
        gc(T)
    }
}
# ############################################################################################






# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
if ( CLT_OPTIONS$'do analysis cross validation dataset' ) {
    NEWLINE(20)
    BANNER( 'FULL SCALE CROSS VALIDATION' )
    CVTEST=TRUE

    save.image('T4-precv.RData')
    source('clt_cv.R')
    save.image('T4-postcv.RData')

    if ( CLT_OPTIONS$'do state checkpointing' )
        save.image(GET_MODEL_NAME('CROSS_VALIDATION_DONE.RData'))

    if ( CLT_OPTIONS$'do options logging' ) {
        BANNER("LOGGING")
        logcmd=sprintf("./clt_snapshot_metrics.sh %s", CLT_OPTIONS$'output logfile' )
        logging_data <- pipe(logcmd)
    }
}
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################






# ############################################################################################
# OPTIONAL (BEFORE/AFTER): INCREMENTAL FEATURE SELECTION ANALYTICS: CAN EXPLORE OTHER PARAMETERS
# ############################################################################################
if ( CLT_OPTIONS$'do feature selection' | CLT_OPTIONS$'do model training' ) {
    NEWLINE(20)
    BANNER( 'INCREMENTAL FEATURE EXPLORATION ANALYTICS' )
    MAXDEPTH=8
    MINBCKT=29
    MINSPLT=3*(MINBCKT+1)
    CP=1E-9
    CP_PRUNE=1E-5
    Y_TRUE = ORIG_XTRAIN[,YCOLNAME]
    source('clt_incremental_fselection.R')

    if ( CLT_OPTIONS$'do options logging' ) {
        BANNER("LOGGING")
        logcmd=sprintf("./clt_snapshot_metrics.sh %s", CLT_OPTIONS$'output logfile' )
        logging_data <- system(logcmd)

    if ( CLT_OPTIONS$'do state checkpointing' )
        save.image(GET_MODEL_NAME('CROSS_VALIDATION_COMPLETED.RData'))
    }
}
# ############################################################################################







# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
if ( CLT_OPTIONS$'do grid search' & CLT_OPTIONS$'do feature selection' ) {
    BANNER( 'GRID SEARCH FOR DECISION TREE PARAMETER SUGGESTIONS' )
    source( 'clt_grid.R' )
}






# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
if ( CLT_OPTIONS$'do analysis xval balanced dataset' ) {
    NEWLINE(20)
    BANNER( 'ARTIFICIAL BALANCED MIX CROSS VALIDATION' )
    CVTEST=TRUE
    source('clt_cv_bootstrap.R')
}
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################






# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
if ( CLT_OPTIONS$'do analysis test submission dataset' ) {
    NEWLINE(20)
    BANNER( 'FULL TEST SET EVALUATION' )
    source('clt_test.R')
}
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################





