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




if ( FALSE ) {
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
    library(rpart)                    # Popular decision tree algorithm
    library(rpart.plot)                # Enhanced tree plots
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
}


# ############################################################################################
# ############################################################################################
# ############################################################################################
BANNER( 'DEV TESTSET: MODEL PERFORMANCE WRT COMPETITION SPARSE RANDOM SAMPES OF TRAIN DATA' )
# ############################################################################################
CVTEST = TRUE
source('clt_activated_cols.R')
source('clt_metrics.R' )
# ############################################################################################


# ############################################################################################
WRT="~/WORKSPACE/src/R/KAGGLE/CLICKTHRU/train"
R = 3E4
Q = 1E4
L = list()
PROB_METRICS_VECTOR = VECTOR( NUM_MODELS )
PROB_METRICS_MATRIX = MATRIX( NUM_MODELS, NCV_ITERS )
NTRIALS = 1
METRICS = MATRIX( NCV_ITERS, 12 )
colnames(METRICS) = c( "JCOST", "F_RATIO", "FPR", "FNR", "ACCURACY", "BER", "MSE", "PRECISION", "RECALL", "TPR", "TNR", "LOG_LOSS" )
H = list() # cv metrics tabulated according to each hour
for ( i in seq(0,24) ) { H[[paste(i)]] = c() }
# ############################################################################################


# ############################################################################################
iter = 0

cv_dates = VECTOR( NCV_ITERS )

for ( i in WHICH_1GBSTORAGE_INDEXES )
  for ( k in WHICH_1GBTRAINING_CHUNKS ) {
    if ( FALSE ) {
        I = c(79,65,247,110,224,366,321,283,103,49,387,42,73,35,456,393,409,428,294,222,324,312,296,302,63,199,449,185,300,494,345,382,288,446,334,281,276,208,496,16,45,326,127,188,309,107,147,477,498,200)
        if ( all(I-i != 0) ) { Q = R + Q ; next }
        ORIG_XTEST  = read.csv( WRT, skip=Q, nrows=R, header=FALSE, stringsAsFactors=TRUE, colClasses=c('character',rep('integer',4), rep('factor',9), rep('integer',10)))
        colnames(ORIG_XTEST) = COLNAMES
        Q = R + Q
        BANNER( sprintf('CROSS VALIDATION: FEATURES: RECODING FACTORS WRT [%3s] ITERATION, LINES: %6s - %6s', i, Q-1, Q+R-1 )) 
    } else {
        WRT = sprintf( "TRAIN/training_chunk_%s.csv", 1000+i)
        ORIG_XTEST  = read.csv( WRT, skip=Q*k, nrows=R, header=FALSE, stringsAsFactors=TRUE, colClasses=c('character',rep('integer',4), rep('factor',9), rep('integer',10)))
        colnames(ORIG_XTEST) = COLNAMES
        print( summary( ORIG_XTEST ) )
        start = i*1E5+Q*(k)+1
        end = i*1E5+Q*(k)+1 + R
        BANNER( sprintf('CROSS VALIDATION: FEATURES: RECODING FACTORS WRT [%3s] ITERATION, LINES: %6s - %6s', i, start, end )) 
    }

    M = nrow(ORIG_XTEST)
    N = ncol(ORIG_XTEST)
    if ( M == 0 )
        break

    # #####################################################################################
    BANNER( 'TRANSFORMING FACTORS TO MATCH TRAINING DATASET' )
    ORIG_XTEST[YCOLNAME] = as.factor(ORIG_XTEST[,'click'])
    ORIG_YTEST = SLICE_DATAFRAME(ORIG_XTEST,PREDICT_COL)
    fold_date  = min(ORIG_XTEST[,'YYMMDDHH'])
    # #####################################################################################


        # #################################################################################
        #                               IMPORTANT
        #                               IMPORTANT
        #                               IMPORTANT
        # #################################################################################
        source('clt_testcoding.R')
        # #################################################################################


    # #####################################################################################
    ORIG_XTEST[,'origin'] = 'W'
    ORIG_XTEST  = ORIG_XTEST[,ACTIVATED_COLNAMES]
    # #####################################################################################

    # #####################################################################################
    iter = iter + 1
    # #####################################################################################

    # #####################################################################################
    # performance on the test data
    # #####################################################################################
    Y_TRUE = as.matrix(ORIG_XTEST[,PREDICT_VAR])
    yp_probs = COMPUTE_INITIAL_YP_PROBS(ORIG_XTEST)
    YP_PROBS = CLT_AWS_PROB_ENHANCER( yp_probs )
    YP_CLASS = WHICH_CLASS_TESTING_KERNEL( YP_PROBS )
    CV_CMAT = table( Y_TRUE, YP_CLASS )
    # #################################################################################

    # #################################################################################
    METRICS[iter,2] = GET_F_RATIO( CV_CMAT)
    METRICS[iter,3] = GET_FPR( CV_CMAT)
    METRICS[iter,4] = GET_FNR( CV_CMAT)
    METRICS[iter,5] = GET_ACC( CV_CMAT)
    METRICS[iter,6] = ( GET_TPR( CV_CMAT) + GET_TNR( CV_CMAT ) ) / 2.0
    METRICS[iter,7] = sum( abs(as.numeric(YP_CLASS)-1 - as.numeric(Y_TRUE)-1)^2 )/ length(YP_CLASS)
    METRICS[iter,8] = GET_PRECISION( CV_CMAT )
    METRICS[iter,9] = GET_RECALL( CV_CMAT )
    METRICS[iter,10] = GET_TPR( CV_CMAT )
    METRICS[iter,11] = GET_TNR( CV_CMAT )
    METRICS[iter,12] = COMPUTE_LOG_LOSS( Y_TRUE, YP_PROBS[,2] )
    # #################################################################################
    L[[paste(end)]]  = METRICS[iter,12]
    hour = mean( as.numeric( ORIG_XTEST[,"hour"]))
    H[[paste(hour)]] = append( H[[paste(hour)]], METRICS[iter,12] )
    # #################################################################################

    # #################################################################################
    BANNER( "ATTRIBUTE IMPORTANCE INSIGHT FROM WITHIN THIS PARTICULAR CV FOLD" )
    if ( METRICS[iter,12] > 0.50 ) {
        clf <<- rpart( 'click ~ .', ORIG_XTEST)
        print(clf$variable.importance)
        cat(HEADER)
        if ( TRUE ) clf <<- prune( clf, cp=clf$cptable[which.min(clf$cptable[,"xerror"]),"CP"])
        print(clf$variable.importance)
        cat(HEADER)
        yp = predict(clf, ORIG_XTEST, type="prob")
        ll_alt = COMPUTE_LOG_LOSS( Y_TRUE, yp[,2])
        BANNER( sprintf('*** INSIGHT: %s [%3s] ITERATION, LINES: %6s - %6s [%s --> %s]', Q+R-1, i, Q-1, Q+R-1, METRICS[iter,12], ll_alt )) 
        if ( ll_alt < 0.40 ) {
            print ( "******************* *********************** ***********************" )
            print ( "******************* ** AREA  OF INTEREST ** ***********************" )
            print ( "******************* *********************** ***********************" )
            print ( paste( Q, R, i ) )
            print ( "******************* *********************** ***********************" )
            print( summary( ORIG_XTEST ) )
            print ( "******************* *********************** ***********************" )
            print ( "******************* *********************** ***********************" )
            cat(HEADER)
            print(table(Y_TRUE, round(yp[,2],2) ))
            BANNER( 'SUMMARY MISSED CASES' )
            WHICH_ONES = which( YP_CLASS != Y_TRUE )
            MISSED_SAMPLES = ORIG_XTEST[WHICH_ONES,]
            print( summary( MISSED_SAMPLES ) )
        }
    }
    # #################################################################################

    cv_dates[iter] = fold_date
    # ############################################################################################
    BANNER( 'METRICS' )
    # ############################################################################################
    cat(HEADER)
    print( summary( ORIG_XTEST ) )
    cat(HEADER)
    if ( FALSE ) str(ORIG_XTEST)
    cat(HEADER)
    NEWLINE(1)
    cat(HEADER)
    colnames(METRICS) = c( "JCOST", "F_RATIO", "FPR", "FNR", "ACCURACY", "BER", "MSE", "PRECISION", "RECALL", "TPR", "TNR", "LOG_LOSS" )
    print( summary( METRICS[1:iter,] ) )
    cat(HEADER)
    print( METRICS[iter,] )

    PROB_METRICS_MATRIX[,iter] = PROB_METRICS_VECTOR
    if ( iter %% 10 == 0 | iter > 539 ) {
        # ####################################################################################
        #          #########################   IMPORTANT   ######################## 
        # ####################################################################################
        COMPUTE_CV_METRICS( iter )
        # ####################################################################################

        # ####################################################################################
        if ( CLT_OPTIONS$'do options logging' ) {
            BANNER("LOGGING")
            logcmd=sprintf("./clt_snapshot_metrics.sh %s", CLT_OPTIONS$'output logfile' )
            logging_data <- system(logcmd)
        }
        # ####################################################################################
    }
    print (CV_CMAT)
    # ############################################################################################

    # ############################################################################################
    BANNER( 'LOG LOSS CV PERFORMANCE (CURRENTLY)' )
    L2 = unlist(L)
    cat(HEADER)
    print ( H )

    cat(HEADER)
    cat(HEADER)
    print ( L2 )
    cat(HEADER)
    print ( summary( L2 )) 
    cat(HEADER)
    print ( paste( "MEAN LOG LOSS: ", mean(L2) ) )
    print ( paste( "STDV LOG LOSS: ", sd(L2) ) )
    # ############################################################################################

    # ############################################################################################
    BANNER( "******************************** DONE WITH CV FOLD ********************************" )
    # ############################################################################################
}
# ############################################################################################
# ############################################################################################
# ############################################################################################


# ############################################################################################
# ############################################################################################
# ############################################################################################
BANNER( 'ACCURACY LOGLOSS COMPARISON' )
source('clt_accuracy_logloss_comparison.R')
# ############################################################################################
# ############################################################################################
# ############################################################################################
