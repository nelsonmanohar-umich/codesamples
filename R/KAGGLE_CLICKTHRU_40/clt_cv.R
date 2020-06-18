# ############################################################################################
BANNER( 'DEV TESTSET: MODEL PERFORMANCE WRT COMPETITION SPARSE RANDOM SAMPES OF TRAIN DATA' )
# ############################################################################################
CVTEST = TRUE
if ( FALSE ) {
    load(CLT_OPTIONS$'precomputed_aws_image')
    rm(ORIG_XTEST)
    require(rpart)
    source('utilities.R')
    source('clt_basic_functions.R')
    source('clt_options.R')
    source('clt_prob_enhancer.R')
}
# ############################################################################################
source('clt_activated_cols.R')
source('clt_metrics.R' )
# ############################################################################################


# ############################################################################################
WRT="~/WORKSPACE/src/R/KAGGLE/CLICKTHRU/train"
R = 5E4
Q = 1E4
# ############################################################################################


# ############################################################################################
L = list()
H = list() # cv metrics tabulated according to each hour
for ( i in seq(0,24) ) { H[[paste(i)]] = c() }
# ############################################################################################
PROB_METRICS_VECTOR = VECTOR( NUM_MODELS )
PROB_METRICS_MATRIX = MATRIX( NUM_MODELS, NCV_ITERS )
METRICS = MATRIX( NCV_ITERS, 12 )
colnames(METRICS) = c( "JCOST", "F_RATIO", "FPR", "FNR", "ACCURACY", "BER", "MSE", "PRECISION", "RECALL", "TPR", "TNR", "LOG_LOSS" )
# ############################################################################################


# ############################################################################################
# IF MODELS WERE PRECOMPUTED VIA RJE IN PREVIOUS RUNS AND ARE AVAILABLE IN S3 UNDER VALID ACCNT
# ############################################################################################
if ( CLT_OPTIONS$prefetch_remote_precomputed_models ) {
    WHICH_COLUMNS = minus( ACTIVATED_COLNAMES, c('origin'))
    for ( cname in WHICH_COLUMNS ) { 
        PRECOMPUTED_MODEL_PREFETCHER()
        LIST_MODELS(cname=cname)
    }
    REMOVE_INVALID_MODELS()
    LIST_MODELS()
}
# ############################################################################################


# ############################################################################################
iter = 0
cv_dates = VECTOR( NCV_ITERS )
# ############################################################################################
for ( i in WHICH_1GBSTORAGE_INDEXES ) {
  for ( k in WHICH_1GBTRAINING_CHUNKS ) {

    iter = iter + 1

    # ########################################################################################
    if ( FALSE ) {
        I = c(79,65,247,110,224,366,321,283,103,49,387,42,73,35,456,393,409,428,294,222,324,312,296,302,63,199,449,185,300,494,345,382,288,446,334,281,276,208,496,16,45,326,127,188,309,107,147,477,498,200)
        if ( all(I-i != 0) ) { Q = R + Q ; next }
        ORIG_XTEST  = read.csv( WRT, skip=Q, nrows=R, header=FALSE, stringsAsFactors=TRUE, colClasses=c('character',rep('integer',4), rep('factor',9), rep('integer',10)))
        colnames(ORIG_XTEST) = COLNAMES
        Q = R + Q
        BANNER( sprintf('CROSS VALIDATION: FEATURES: RECODING FACTORS WRT [%3s] ITERATION, LINES: %6s - %6s', iter, Q-1, Q+R-1 )) 
    } else {
        WRT = sprintf( "TRAIN/training_chunk_%s.csv", 1000+i)
        ORIG_XTEST  = read.csv( WRT, skip=Q*k, nrows=R, header=FALSE, stringsAsFactors=TRUE, colClasses=c('character',rep('integer',4), rep('factor',9), rep('integer',10)))
        colnames(ORIG_XTEST) = COLNAMES
        print( summary( ORIG_XTEST ) )
        start = i*1E5+Q*(k)+1
        end = i*1E5+Q*(k)+1 + R
        BANNER( sprintf('CROSS VALIDATION: FEATURES: RECODING FACTORS WRT [%3s] ITERATION, LINES: %6s - %6s', iter, start, end )) 
    }
    # ########################################################################################

    # ########################################################################################
    M = nrow(ORIG_XTEST)
    N = ncol(ORIG_XTEST)
    if ( M == 0 )
        break
    # ########################################################################################

    # #####################################################################################
    BANNER( 'TRANSFORMING FACTORS TO MATCH TRAINING DATASET' )
    # #####################################################################################
    ORIG_XTEST[YCOLNAME] = as.factor(ORIG_XTEST[,'click'])
    ORIG_YTEST           = SLICE_DATAFRAME(ORIG_XTEST,PREDICT_COL)
	Y_TRUE               = as.matrix(ORIG_XTEST[,PREDICT_VAR])
    fold_date            = min(ORIG_XTEST[,'YYMMDDHH'])
    cv_dates[iter]       = fold_date
    # #####################################################################################

    # #####################################################################################
    #                                       IMPORTANT
    # #####################################################################################
    source('clt_testcoding.R')
    # #####################################################################################

    # #####################################################################################
    ORIG_XTEST[,'origin'] = 'W'
    ORIG_XTEST  = ORIG_XTEST[, ACTIVATED_COLNAMES]
    # #####################################################################################
                for ( xvar in ACTIVATED_COLNAMES ) {
                    A = levels(ORIG_XTRAIN$xvar)
                    B = levels(ORIG_XTEST$xvar)
                    C = union(A, B)
                    if ( length(C) != length(B) | length(C) != length(A) ) {
                        cat(HEADER)
                        print( paste( "XVAR LEVELS BEING ADJUSTED", xvar) )
                        cat(HEADER)
                        levels(ORIG_XTEST$xvar) = A
                        w = which(is.na(ORIG_XTEST$xvar))
                        ORIG_XTEST$xvar[w] = "other"
                    }
                }
    # #####################################################################################


    # #####################################################################################
    #                                       IMPORTANT
    # #####################################################################################
    source('clt_cv_prediction_kernel.R')
    # #####################################################################################

    # #####################################################################################
    PROB_METRICS_MATRIX[,iter] = PROB_METRICS_VECTOR
    if ( iter %% 10 == 0 ) {
        try({
            COMPUTE_CV_METRICS( iter )
        })
    }
    print (CV_CMAT)
    # #####################################################################################

    # #####################################################################################
    BANNER( "******************************** DONE WITH CV FOLD ********************************" )
    # #####################################################################################
  }
}
# ############################################################################################

# ############################################################################################
    COMPUTE_CV_METRICS( iter )

    if ( CLT_OPTIONS$'do options logging' ) {
        BANNER("LOGGING")
        try({
            logcmd=sprintf("./clt_snapshot_metrics.sh %s", CLT_OPTIONS$'output logfile' )
            logging_data <- system(logcmd)
        })
    }
    print (CV_CMAT)
# ############################################################################################

# ############################################################################################
BANNER( 'ACCURACY LOGLOSS COMPARISON' )
# ############################################################################################
    # ########################################################################################
    # ########################################################################################
    source('clt_accuracy_logloss_comparison.R')
    # ########################################################################################
    # ########################################################################################
# ############################################################################################


