    # #####################################################################################
	# cv prediction kernel
    # #####################################################################################
    USE_INCREMENTAL_MODELING = CLT_OPTIONS$'use_incremental_probability_builder'
    if ( USE_INCREMENTAL_MODELING ) {
        YP_PROBS = COMPUTE_INITIAL_YP_PROBS(ORIG_XTEST)
        YP_CLASS = WHICH_CLASS_KERNEL(YP_PROBS, 0.5)
        SUM_JCOST = 0
    }

	if ( !USE_INCREMENTAL_MODELING ) {
        if ( CLT_OPTIONS$'bootstrapper' == "DT" ) {
            BANNER( sprintf( 'DECISION TREE MODEL WITH FSELECT VARS' ))
            BASE_MODEL=DT_MODEL
	        YP_PROBS = predict(BASE_MODEL, ORIG_XTEST, type='prob')
            YP_CLASS = WHICH_CLASS_KERNEL(YP_PROBS, 0.5)
            SUM_JCOST = 0
        }
	    if ( CLT_OPTIONS$'bootstrapper' == "NB" ) {
            BANNER( sprintf( 'NAIVE BAYES MODEL WITH FORMULA6 VARS' ))
	        YP_PROBS = predict( BASE_MODEL, newdata=ORIG_XTEST, type='raw' ) #, threshold=NB_THRESHOLD, eps=NB_EPS)
	        YP_CLASS = WHICH_CLASS_TESTING_KERNEL( YP_PROBS )
            SUM_JCOST = 0
        }
	    if ( CLT_OPTIONS$'bootstrapper' == "LR" ) {
	        ZVALS = predict(BOOTBASE_MODEL, ORIG_XTEST)
	        JCOST = JCOST_Z( ZVALS, Y_TRUE )
	        YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
            SUM_JCOST = sum(JCOST, na.rm=TRUE)
        }
    }

    # ################ IMPORTANT ###################
    # ################ IMPORTANT ###################
    # ################ IMPORTANT ###################
    # ################ IMPORTANT ###################
	if ( !USE_INCREMENTAL_MODELING & PROBABILITY_ENHANCE ) {
        YP_PROBS = PROBABILITY_ENHANCER( YP_PROBS, Y_TRUE, TRAIN=FALSE )
	    YP_CLASS = WHICH_CLASS_TESTING_KERNEL( YP_PROBS )
    }
	if ( USE_INCREMENTAL_MODELING ) {
        YP_PROBS = CLT_AWS_PROB_ENHANCER( YP_PROBS )
        YP_CLASS = WHICH_CLASS_TESTING_KERNEL( YP_PROBS )
    }
    # ################ IMPORTANT ###################
    # ################ IMPORTANT ###################
    # ################ IMPORTANT ###################


    # #################################################################################
	CV_CMAT          = table( Y_TRUE, YP_CLASS )
	METRICS[iter,1]  = SUM_JCOST
	METRICS[iter,2]  = GET_F_RATIO( CV_CMAT)
	METRICS[iter,3]  = GET_FPR( CV_CMAT)
	METRICS[iter,4]  = GET_FNR( CV_CMAT)
	METRICS[iter,5]  = GET_ACC( CV_CMAT)
	METRICS[iter,6]  =(GET_TPR( CV_CMAT) + GET_TNR( CV_CMAT ) ) / 2.0
	METRICS[iter,7]  = sum( abs(as.numeric(YP_CLASS)-1 - as.numeric(Y_TRUE)-1)^2 )/ length(YP_CLASS)
	METRICS[iter,8]  = GET_PRECISION( CV_CMAT )
	METRICS[iter,9]  = GET_RECALL( CV_CMAT )
	METRICS[iter,10] = GET_TPR( CV_CMAT )
	METRICS[iter,11] = GET_TNR( CV_CMAT )
	METRICS[iter,12] = COMPUTE_LOG_LOSS( Y_TRUE, YP_PROBS[,2] )
    # #################################################################################


    # #################################################################################
    if ( METRICS[iter,12] > 0.50 ) {
        BANNER( "ATTRIBUTE IMPORTANCE INSIGHT FROM WITHIN THIS PARTICULAR CV FOLD" )
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
    # ############################################################################################


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
    # ############################################################################################


    # ############################################################################################
    BANNER( 'LOG LOSS CV PERFORMANCE (CURRENTLY)' )
    # ############################################################################################
    L[[paste(end)]]  = METRICS[iter,12]
    hour = mean( as.numeric( ORIG_XTEST[,"hour"]))
    H[[paste(hour)]] = append( H[[paste(hour)]], METRICS[iter,12] )

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

