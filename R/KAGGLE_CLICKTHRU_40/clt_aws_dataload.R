# ############################################################################################
NEWLINE(20)
BANNER(sprintf( "LOADING FACTOR LEVEL TRAINING FILE %s", CLT_OPTIONS$'factor_level_training_file'))
# ############################################################################################
    ORIG_XTRAIN = read.csv( CLT_OPTIONS$'factor_level_training_file', header=TRUE, stringsAsFactors=TRUE,
                                colClasses=c('character',rep('integer',4), rep('factor',9), rep('integer',10)))
    colnames(ORIG_XTRAIN) = COLNAMES
    # ###########################################################################
    ORIG_XTRAIN[,"origin"] = "W"
    ORIG_XTRAIN[,"origin"] = as.factor( ORIG_XTRAIN[,"origin"] )
    # ###########################################################################
    M = nrow(ORIG_XTRAIN)
    N = ncol(ORIG_XTRAIN)
    rownames(ORIG_XTRAIN) = 1:M

    PREDICT_VAR = "click"
    PREDICT_COL = 2
    YCOLNAME = "click"
    YCOLNUM  = PREDICT_COL
    CLASS0 = 0
    CLASS1 = 1

    ORIG_XTRAIN[YCOLNAME] = as.factor(ORIG_XTRAIN[,'click'])
    Y_TRUE = ORIG_XTRAIN[,YCOLNAME]

    BANNER("FINISHED LOADING DATA")
    print(summary(ORIG_XTRAIN))
    str(ORIG_XTRAIN)
# ############################################################################################


# ############################################################################################
NEWLINE(20)
BANNER( 'TRANSFORMING TRAINING DATASET' )
# ############################################################################################
    source('clt_traincoding.R')
    print( summary( ORIG_XTRAIN ) )
# ############################################################################################


# ############################################################################################
# DATASET READY
# ############################################################################################
NEWLINE(20)
BANNER( 'DATASET READY' )
# ############################################################################################
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
