# ############################################################################################
# comparative evaluation of models
# ############################################################################################
Y_TRUE = ORIG_XTRAIN[,YCOLNAME]

if ( 0 ) {
    cat(HEADER)
    print( summary(ORIG_XTRAIN) )
    cat(HEADER)
}

NMAX = 250000
NMAX = min(nrow(ORIG_XTRAIN), NMAX)
SUBSET = sample(1:nrow(ORIG_XTRAIN), NMAX)

ADDITIONAL_INSIGHT = TRUE
if ( ADDITIONAL_INSIGHT ) {
    # MODELS = list()
    # for ( f in FORMULAS ) {
        # model = glm( f, data=ORIG_XTRAIN, family=binomial(link = "logit"))
        # MODELS = append( MODELS, model )
    # }

    if ( USE_NAIVE_BAYES ) {
        FORMULAS = list( FORMULA6 )
        model6 = naiveBayes( x=ORIG_XTRAIN[SUBSET, -PREDICT_COL], y=ORIG_XTRAIN[SUBSET, PREDICT_COL] ) #, laplace=LAPLACE )
        MODELS = list( model6 )
    } else {
        FORMULAS = list( FORMULA6 )
        model6 = glm( FORMULA6, data=ORIG_XTRAIN, family=binomial(link = "logit"), weights=initial_wt)
        MODELS = list( model6 )

        for ( i in 1:length(MODELS)) {
            LAST_MODEL = MODELS[[i]]
            print ( paste( "MODEL", i, sep="" ))
            print ( paste( "FORMULA", FORMULAS[[i]] ))
            cat (HEADER )
            if ( length(MODELS) > 1 ) {
                ANOVA = anova( MODELS[[i]], LAST_MODEL )
                cat (HEADER )
                print ( ANOVA )
                cat (HEADER )
            }
        }
    }
    #multiplot( model6 )
    cat (HEADER)
    # ############################################################################################


    # ############################################################################################
    # model performance metrics
    # ############################################################################################
    BANNER( 'MODEL TRAINING: PERFORMANCE METRICS' )
    for ( i in 1:length(MODELS)) {
        MODEL = MODELS[[i]]
        FORMULA = FORMULAS[[i]]

        cat (HEADER)
        print( summary(MODEL) )
        print( FORMULA )

        SUBSET = sample(1:nrow(ORIG_XTRAIN), NMAX)

        if ( USE_NAIVE_BAYES ) {
            YP_PROBS = predict( MODEL, newdata=ORIG_XTRAIN[SUBSET,], type='raw') #, threshold=NB_THRESHOLD, eps=NB_EPS)
            YP_CLASS = WHICH_CLASS_KERNEL(YP_PROBS, 0.5)
        } else {
            ANOVA_TEST = anova( MODEL, test="Chisq" )
            AIC_VAL = AIC(MODEL)
            ZVALS = predict(MODEL, ORIG_XTRAIN)
            JCOST = JCOST_Z( ZVALS, Y_TRUE )
            YP_PROBS = LOGIT(ZVALS)
            YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
            cat (HEADER )
            print( ANOVA_TEST )
            cat (HEADER )
            print( AIC_VAL )
        }

        CMAT = table( Y_TRUE[SUBSET], YP_CLASS )
        cat (HEADER )
        print( CMAT )
        cat(HEADER )
        LL = COMPUTE_LOG_LOSS( Y_TRUE[SUBSET], YP_PROBS[,2] )
        print( LL )
        cat(HEADER )
    }
    # ############################################################################################


    # ############################################################################################
    # Model comparison
    # ############################################################################################
    if ( !USE_NAIVE_BAYES ) {
        BANNER( 'MODEL TRAINING: COMPARING SPECIFIED MODELS' )
        ANOVA = do.call("anova", as.list(MODELS))
        print ( ANOVA )
    }
}

CMAT
