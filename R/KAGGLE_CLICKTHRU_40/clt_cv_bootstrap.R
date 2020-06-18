# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# load the test data
# ############################################################################################
NEWLINE(20)

BANNER( 'LOADING TEST DATA: CROSS VALIDATION SET' )
CVTEST=TRUE
source('clt_activated_cols.R')

WRT="~/WORKSPACE/src/R/KAGGLE/CLICKTHRU/train"
ORIG_XTEST = LOAD_CLICK_THRU_DATA( FILENAME='~/WORKSPACE/SRC/PYTHON/test_set4.csv', FRACTION=FRACTION_TO_USE, FASTER_TEST= DO_FAST_TEST, nrows=max(250000,as.integer(NROWS/10)))
colnames(ORIG_XTEST) = COLNAMES
# ############################################################################################


# ############################################################################################
BANNER( 'TRANSFORMING FACTORS TO MATCH TRAINING DATASET' )
    ORIG_XTEST[YCOLNAME] = as.factor(ORIG_XTEST[,'click'])
    ORIG_YTEST = SLICE_DATAFRAME(ORIG_XTEST,PREDICT_COL)
    source('clt_testcoding.R')
    ORIG_XTEST  = ORIG_XTEST[,ACTIVATED_COLNAMES]
# ############################################################################################


# ############################################################################################
# performance on the test data
# ############################################################################################
BANNER( 'ANALYSIS OF TRAINED MODEL WRT CROSS VALIDATION TEST DATA' )

LEAVE_OUT = c()
NTRIALS = 1
METRICS = MATRIX( NTRIALS, 12 )
colnames(METRICS) = c( "JCOST", "F_RATIO", "FPR", "FNR", "ACCURACY", "BER", "MSE", "PRECISION", "RECALL", "TPR", "TNR", "LOG_LOSS" )

START = TRUE
METRICS = MATRIX( NTRIALS, 12 )
for ( j in 1:NTRIALS ) {
    # Y_TRUE = as.matrix(as.factor(ORIG_XTEST[BOOT_ORDER,PREDICT_VAR]))
    Y_TRUE = as.matrix(ORIG_XTEST[,PREDICT_VAR])

    if ( USE_NAIVE_BAYES ) {
        YP_PROBS = predict( BASE_MODEL, newdata=ORIG_XTEST, type='raw', threshold=NB_THRESHOLD, eps=NB_EPS)
        YP_CLASS = WHICH_CLASS( YP_PROBS )
        # METRICS[j,1] = COMPUTE_LOG_LOSS( Y_TRUE, YP_CLASS )
        if ( PROBABILITY_ENHANCE ) {
                YP_PROBS = PROBABILITY_ENHANCER ( YP_PROBS, Y_TRUE, TRAIN=FALSE )
	            YP_CLASS = WHICH_CLASS( YP_PROBS )
        }
        METRICS[j,12] = COMPUTE_LOG_LOSS( Y_TRUE, YP_PROBS[,2] )
    } else {
        if (length(LEAVE_OUT)!=0) {
            X_TEST = ORIG_XTEST[,-LEAVE_OUT]
        } else {
            X_TEST = ORIG_XTEST[,]
        }
        ZVALS = predict(BOOTBASE_MODEL, X_TEST)
        JCOST = JCOST_Z( ZVALS, Y_TRUE )
        YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
        METRICS[j,1] = sum(JCOST, na.rm=TRUE)
    }

    CV_CMAT = table( Y_TRUE, YP_CLASS )

    METRICS[j,2] = GET_F_RATIO( CV_CMAT)
    METRICS[j,3] = GET_FPR( CV_CMAT)
    METRICS[j,4] = GET_FNR( CV_CMAT)
    METRICS[j,5] = GET_ACC( CV_CMAT)
    METRICS[j,6] = ( GET_TPR( CV_CMAT) + GET_TNR( CV_CMAT ) ) / 2.0
    METRICS[j,7] = sum( abs(as.numeric(YP_CLASS)-1 - as.numeric(Y_TRUE)-1)^2 )/ length(YP_CLASS)
    METRICS[j,8] = GET_PRECISION( CV_CMAT )
    METRICS[j,9] = GET_RECALL( CV_CMAT )
    METRICS[j,10] = GET_TPR( CV_CMAT )
    METRICS[j,11] = GET_TNR( CV_CMAT )
}
# ############################################################################################


# ############################################################################################
BANNER( 'SUMMARY ANALYTICS OF SELECTED MODEL WRT ARTIFICIALLY BALANCED CV TEST DATA' )
cat(HEADER)
print( summary( ORIG_XTEST ) )
cat(HEADER)
str(ORIG_XTEST)
cat(HEADER)
colnames(METRICS) = c( "JCOST", "F_RATIO", "FPR", "FNR", "ACCURACY", "BER", "MSE", "PRECISION", "RECALL", "TPR", "TNR", "LOG_LOSS" )
print( METRICS )
cat(HEADER)
print (CV_CMAT)


cat(HEADER)
cat(HEADER)
NEWLINE(10)
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# BOUNDARY T2:T3
