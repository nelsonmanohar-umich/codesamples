
# ############################################################################################
# Base model selection
# ############################################################################################
BANNER( 'MODEL TRAINING: BASE MODEL SELECTION' )
    BASE_FORMULA = FORMULA6
	if ( CLT_OPTIONS$'bootstrapper' == "DT" ) {
        BANNER( sprintf( 'DECISION TREE MODEL WITH FSELECT VARS' ))
        BASE_MODEL=DT_MODEL
	    Y_TRUE   = ORIG_XTRAIN[,PREDICT_VAR]
        YP_PROBS = predict(BASE_MODEL, ORIG_XTRAIN, type='prob')
        YP_CLASS = WHICH_CLASS_KERNEL(YP_PROBS, 0.5)
    }

	if ( CLT_OPTIONS$'bootstrapper' == "NB" ) {
        BANNER( sprintf( 'NAIVE BAYES MODEL WITH FORMULA6 VARS' ))
        SUBSET = sample(1:nrow(ORIG_XTRAIN),min(100000, nrow(ORIG_XTRAIN)))
        Y_TRUE = ORIG_XTRAIN[,YCOLNAME]
        BASE_MODEL= naiveBayes( x=ORIG_XTRAIN[SUBSET,2:ncol(ORIG_XTRAIN)], y=ORIG_XTRAIN[SUBSET,1] ) # ), laplace=LAPLACE )
        YP_PROBS  = predict( BASE_MODEL, newdata=ORIG_XTRAIN, type='raw' ) #, threshold=NB_THRESHOLD, eps=NB_EPS)
        YP_CLASS  = WHICH_CLASS_KERNEL(YP_PROBS, 0.5)
        print ( BASE_FORMULA )
        print ( summary( BASE_MODEL ) )
        gc(TRUE)
    }

	if ( CLT_OPTIONS$'bootstrapper' == "LR" ) {
        BASE_MODEL = glm( BASE_FORMULA, data=ORIG_XTRAIN, family=binomial(link = "logit"), weights=initial_wt)
        ANOVA_TEST = anova( BASE_MODEL, test="Chisq" )
        AIC_VAL = AIC(BASE_MODEL)
        ZVALS = predict(BASE_MODEL, ORIG_XTRAIN)
        JCOST = JCOST_Z( ZVALS, Y_TRUE )
        YP_PROBS = LOGIT( ZVALS )
        YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
        print( ANOVA_TEST )
    }

    CMAT = table( Y_TRUE, YP_CLASS )
    cat(HEADER)
    print( CMAT )
    cat(HEADER)

    if ( PROBABILITY_ENHANCE ) {
        if ( CLT_OPTIONS$'rebuild_models_already_existing' ) {
            gc()
            # if (FALSE) YP_PROBS = PROBABILITY_ENHANCER ( YP_PROBS, Y_TRUE, TRAIN=TRUE )
            PROBABILITY_ENHANCER_PREDICTOR_TRAINING(); 
            gc()
        } else {
            COMPUTE_INITIAL_MODEL(CLT_OPTIONS$'use which model as default model')
        }
    }

    BANNER("DONE TRAINING")
# ############################################################################################
