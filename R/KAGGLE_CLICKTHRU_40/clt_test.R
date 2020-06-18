# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# load datasets
# ############################################################################################
NEWLINE(50)
BANNER( 'TRUE TESTSET: MODEL PERFORMANCE WRT COMPETITION TEST DATA' )
# ############################################################################################

CVTEST = FALSE
# ############################################################################################
source('clt_activated_cols.R')
# ############################################################################################

R = 1E5
R = 0.5E5
Q = 1
Q = 0
for( i in 1:100 ) {
    #if ( i<68 ) { Q = R + Q ; next ; }

    BANNER("TRUE TESTSET: LOADING DATASET SLICE")
    if ( FALSE ) {
        MOD = 10
        if ( ((i) %% MOD) == 1 ) {
            WRT="~/WORKSPACE/src/R/KAGGLE/CLICKTHRU/test"
            FULL_ORIG_XTEST  = read.csv( WRT, skip=Q, nrows=(MOD*R), header=FALSE, stringsAsFactors=TRUE, colClasses=c('character',rep('integer',4-1), rep('factor',9), rep('integer',10)))
            rownames(FULL_ORIG_XTEST)=Q:(MOD*R+Q-1)
            colnames(FULL_ORIG_XTEST) = COLNAMES
        }
        WHICH_ROWS = as.character((Q):(Q+R-1))
        ORIG_XTEST  = FULL_ORIG_XTEST[WHICH_ROWS,]
        colnames(FULL_ORIG_XTEST) = COLNAMES
        rownames(ORIG_XTEST)=1:R
        print( paste( "Q=", Q, "R=", R, "Q+R=", Q+R, "MINR=", min(WHICH_ROWS), "MAXR=", max(WHICH_ROWS), "NIDX=", length(WHICH_ROWS), "NROW=", nrow(ORIG_XTEST), "FNROW=", nrow(FULL_ORIG_XTEST)))
        print( rownames(ORIG_XTEST)[min(WHICH_ROWS)] )
        print( rownames(ORIG_XTEST)[max(WHICH_ROWS)] )
        Q = R + Q
        print( paste( "NEXT TIME: Q=", Q, "R=", R, "NROW=", nrow(ORIG_XTEST)))
        # print( summary( ORIG_XTEST))
    } else {
        WRT  = sprintf("CLT_TESTING/test_chunk_%s.csv", (1000+i))
        ORIG_XTEST  = read.csv( WRT, nrows=R, header=FALSE, stringsAsFactors=TRUE, colClasses=c('character',rep('integer',4-1), rep('factor',9), rep('integer',10)))
        colnames(ORIG_XTEST) = COLNAMES
        Q = R + Q
        print( paste( "NEXT TIME: Q=", Q, "R=", R, "NROW=", nrow(ORIG_XTEST)))
    }

    #par(mfrow=c(3,3))
    #hist(ORIG_XTEST$C01,25)
    #hist(ORIG_XTEST$C14,25)
    #hist(ORIG_XTEST$C15,25)
    #hist(ORIG_XTEST$C16,25)
    #hist(ORIG_XTEST$C17,25)
    #hist(ORIG_XTEST$C18,25)
    #hist(ORIG_XTEST$C19,25)
    #hist(ORIG_XTEST$C20,25)
    #hist(ORIG_XTEST$C21,25)

    # #####################################################################################
    BANNER( sprintf('TRUE TESTSET: FEATURES: RECODING FACTORS WRT [%3s] ITERATION, LINES: %6s - %6s', i, Q-1, Q+R-1 )) 
    BANNER( sprintf('TRUE TESTSET: FEATURES: RECODING FACTORS WRT [%3s] ITERATION, LINES: %6s - %6s', i, Q, Q+R )) 
    # #####################################################################################

    M = nrow(ORIG_XTEST)
    N = ncol(ORIG_XTEST)
    if ( M == 0 )
        break

    # #####################################################################################
    # #####################################################################################
    # #####################################################################################
    BANNER( 'TRANSFORMING FACTORS TO MATCH TRAINING DATASET' )
        #ORIG_XTEST[YCOLNAME] = as.factor(ORIG_XTEST[,'click'])
        #ORIG_YTEST = SLICE_DATAFRAME(ORIG_XTEST,PREDICT_COL)
        source('clt_testcoding.R')
        ORIG_XTEST$origin = 'W'
        ORIG_XTEST  = ORIG_XTEST[,ACTIVATED_COLNAMES]
        print( summary( ORIG_XTEST))
    # #####################################################################################
    # #####################################################################################
    # #####################################################################################

    graphics.off()
    # #####################################################################################
	# performance on the test data
    # #####################################################################################
	ORIG_ORDERING = rownames(ORIG_XTEST)

	if ( CLT_OPTIONS$'bootstrapper' == "DT" ) {
        BANNER( sprintf( 'DECISION TREE MODEL WITH FSELECT VARS' ))
        BASE_MODEL=DT_MODEL
	        YP_PROBS = predict( BASE_MODEL, newdata=ORIG_XTEST, type='prob')
        YP_PROBS = predict(BASE_MODEL, ORIG_XTEST, type='prob')
        YP_CLASS = WHICH_CLASS_KERNEL(YP_PROBS, 0.5)
    }

	if ( CLT_OPTIONS$'bootstrapper' == "NB" ) {
        BANNER( sprintf( 'NAIVE BAYES MODEL WITH FORMULA6 VARS' ))
	        YP_PROBS = predict( BASE_MODEL, newdata=ORIG_XTEST, type='raw' ) #, threshold=NB_THRESHOLD, eps=NB_EPS)
	        YP_CLASS = WHICH_CLASS_TESTING_KERNEL( YP_PROBS )
	        YP_PROBS = predict( BASE_MODEL, newdata=ORIG_XTEST, type='raw', threshold=NB_THRESHOLD, eps=NB_EPS)
	        YP_CLASS = WHICH_CLASS_TESTING_KERNEL( YP_PROBS )
    }

	if ( CLT_OPTIONS$'bootstrapper' == "LR" ) {
	        ZVALS = predict(BOOTBASE_MODEL, ORIG_XTEST)
	        YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
    }

	if ( CLT_OPTIONS$'bootstrapper' == "DUMMY" ) {
        YP_PROBS = rep(0.185,nrow(ORIG_XTEST))
        YP_PROBS = data.frame(1-YP_PROBS, YP_PROBS)
        colnames(YP_PROBS) = c("0", "1")
        YP_CLASS = rep(0,nrow(ORIG_XTEST))
    }

    # ################ IMPORTANT ###################
	    Y_TRUE = YP_CLASS # during test for convenience
    # ################ IMPORTANT ###################
    if ( PROBABILITY_ENHANCE ) {
        if ( PROBABILITY_ENHANCE ) {
            YP_PROBS = PROBABILITY_ENHANCER( YP_PROBS, YP_CLASS, TRAIN=FALSE )
	            YP_CLASS = WHICH_CLASS_TESTING_KERNEL( YP_PROBS )
        }
    }
    # ################ IMPORTANT ###################

    # #####################################################################################
    # DF = data.frame( as.character(ORIG_XTEST[,'ad_id']), apply(YP_PROBS, 1, max))
    DF = data.frame( as.character(ORIG_XTEST[,'ad_id']), YP_PROBS[,2] )
    colnames(DF)=c('id','click')
    filename = sprintf( 'evaluation%dd.clt.csv', i+1000 )
    if ( i > 1 ) {
        write.table( DF, file=filename, row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
    } else {
        write.table( DF, file=filename, row.names=FALSE, quote=FALSE, col.names=TRUE, sep=",")
    }
    # #####################################################################################

    # #####################################################################################
    cat(HEADER)
    print( summary( ORIG_XTEST ) )
    cat(HEADER)
    str(ORIG_XTEST)
    cat(HEADER)
    NEWLINE(3)
    # #####################################################################################
}



sink()

# #####################################################################################
# #####################################################################################
# #####################################################################################
# #####################################################################################
# #####################################################################################
# BOUNDARY T4:END

