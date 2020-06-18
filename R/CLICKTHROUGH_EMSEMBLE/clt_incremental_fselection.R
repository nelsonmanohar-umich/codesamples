
# ############################################################################################
# LOOKUP INTO INDIVIDUAL FACTORS
# ############################################################################################
ADDITIONAL_INSIGHT = FALSE
if ( ADDITIONAL_INSIGHT ) {
    THRESHOLD = 0.5
    SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),10000))
    WHICH_COLS = setdiff( colnames(ORIG_XTRAIN), c("click", "origin", "ad_id")) 
    LogLoss = list()
    Y = ORIG_XTRAIN[SUBSET,]
    for ( col in WHICH_COLS ) {
        if ( !USE_NAIVE_BAYES) {
            BASE_FORMULA = sprintf('%s ~ %s - 1', PREDICT_VAR, col ) 
            BASE_MODEL = glm( BASE_FORMULA, data=Y, family=binomial(link = "logit"), weights=initial_wt[SUBSET] )
            ZVALS = predict(BASE_MODEL, Y)
            YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
            CMAT = table( Y_TRUE[SUBSET], YP_CLASS )
            ANOVA_TEST = anova( BASE_MODEL, test="Chisq" )
            BANNER( 'MODEL TRAINING: BASE MODEL FEEDBACK' )
            print ( BASE_FORMULA )
            # print ( summary( BASE_MODEL ) )
            print( ANOVA_TEST )
            print( CMAT )
        } else {
            if ( FALSE ) {
                BANNER( sprintf( 'NB TRAINING STAGE: %s FEATURE SELECTED', col  ))
                FORMULA6 = GET_FORMULAE( col )
                print ( FORMULA6 )
                cname = sprintf("NB(clk~%s)", substr(col,1,12))
                cmat = source('clt_naivebayes.R')
                cmat = cmat[1]$value
                LogLoss[[cname]] = GET_F_RATIO( cmat )
                print ( cname )
                print ( cmat )
                print ( LogLoss[[cname]] )
            } else {
                BANNER( sprintf( 'DT TRAINING STAGE: FSELECTOR FEATURE SELECTED: %s TERMS', col ) )
                formula = GET_FORMULAE( col )
                cname = sprintf("DT(clk~%s)", substr(col,1,12))
                print ( formula )
                WHAT = sample(1:nrow(ORIG_XTRAIN), min(5E5,nrow(ORIG_XTRAIN)))
                tree_controls = rpart.control(xval=10, minbucket=MINBCKT, minsplit=MINSPLT, maxdepth=MAXDEPTH, cp=CP, split="information")
                dt_model = FIT_DECISION_TREE( model.frame(formula, ORIG_XTRAIN)[WHAT,], SLICE_DATAFRAME(ORIG_XTRAIN,PREDICT_COL)[WHAT,], FORMULA=formula, DO_PRUNING=TRUE, control=tree_controls )
                # dt_model = prune(dt_model, cp=CP_PRUNE)
                # print ( summary(dt_model) )

                dt_probs = predict(dt_model, ORIG_XTRAIN)
                dt_class = WHICH_CLASS_KERNEL(dt_probs, 0.5)
                print( table( Y_TRUE, dt_class ) )
	            ll = COMPUTE_LOG_LOSS( Y_TRUE, dt_probs[,2] )
	            print( ll )
                LogLoss[[cname]] = ll
                a = dt_model$variable.importance
                #### a = a[a>100] #####
                dtvars = attr(a,"names") 
                dtform = GET_FORMULAE( dtvars )
                print(dtform)
                rm(dt_model, dt_probs, dt_class)
            }
        }
    }

    graphics.off()
    blck <- {
        png( 'clt_log_loss_scoring_of_factors.png', 1200, 800)
        par(mar=c(20,5,3,3))
        x = unlist(COLLECT_VECTOR( LogLoss, function(x) x[1]))
        names(x) = WHICH_COLS
        colors=terrain.colors(length(WHICH_COLS))
        LogLoss = sort(x, decreasing=FALSE)
        barplot( LogLoss, las=2, cex=2, col=colors, cex.lab=1.5, cex.axis=2, cex.main=2, main="LOG LOSS FOR DT (FOR EACH SINGLE FEATURE)", horiz=FALSE )
        dev.off()
    }
}
# ############################################################################################


# ############################################################################################
# incremental effects
# ############################################################################################
ADDITIONAL_INSIGHT = TRUE
if ( ADDITIONAL_INSIGHT ) {
     blck <- {
        if ( CLT_OPTIONS$'do incremental modeling on rank-fselect' ) {

            BANNER( 'TRAINING STAGE: INCREMENTAL FEATURE ANALYSIS' )
            f1_fs = list()
            for ( idx in 1:length(FSELECT_XVARS) ) {
                cnames = CONCAT( FSELECT_XVARS[1:idx])
                if ( FALSE ) {
                    BANNER( sprintf( 'NB TRAINING STAGE: FSELECTOR FEATURE SELECTED: %s TERMS', CONCAT( FSELECT_XVARS[1:idx])) )
                    print ( idx )
                    FORMULA6 = GET_FORMULAE( FSELECT_XVARS[1:idx] )
                    cmat = source('clt_naivebayes.R')
                    cmat = cmat[1]$value
                    print( cmat )
                    f1 = GET_F_RATIO( cmat)
                    print ( f1 )
                    f1_fs[[idx]] = f1
                    title="NAIVE BAYES LOG_LOSS\n(UNDER INCREMENTAL ADDITION OF IMPORTANT ATTRIBUTES)"
                } else {
                    BANNER( sprintf( 'DT TRAINING STAGE: FSELECTOR FEATURE SELECTED: %s TERMS', CONCAT( FSELECT_XVARS[1:idx])) )
                    WHAT = sample(1:nrow(ORIG_XTRAIN), min(5E5, nrow(ORIG_XTRAIN)))
                    formula = GET_FORMULAE( FSELECT_XVARS[1:idx] )
                    tree_controls = rpart.control(xval=10, minbucket=MINBCKT, minsplit=MINSPLT, maxdepth=MAXDEPTH, cp=CP, split="information")
                    dt_model = FIT_DECISION_TREE( model.frame(formula, ORIG_XTRAIN)[WHAT,], SLICE_DATAFRAME(ORIG_XTRAIN,PREDICT_COL)[WHAT,], FORMULA=formula, DO_PRUNING=TRUE, control=tree_controls )
                    # dt_model = prune(dt_model, cp=CP_PRUNE)
                    # print ( summary( dt_model ) )
                    ### WHAT = 1:nrow(ORIG_XTRAIN)
                    ### dt_probs = predict(dt_model, ORIG_XTRAIN[WHAT,])
                    ### dt_class = WHICH_CLASS_KERNEL(dt_probs, 0.5)
                    ### print( table( Y_TRUE[WHAT], dt_class ) )
	                ### ll = COMPUTE_LOG_LOSS( Y_TRUE[WHAT], dt_probs[,2] )

                    dt_probs = predict(dt_model, ORIG_XTRAIN)
                    dt_class = WHICH_CLASS_KERNEL(dt_probs, 0.5)
                    print( table( Y_TRUE, dt_class ) )
	                ll = COMPUTE_LOG_LOSS( Y_TRUE, dt_probs[,2] )
	                print( ll )
                    cname = substr(cnames,1,24)
                    f1_fs[[idx]] = ll
                    a = dt_model$variable.importance
                    #### a = a[a>100] #####
                    dtvars = attr(a,"names") 
                    dtform = GET_FORMULAE( dtvars )
                    print(dtform)
                    title="DECISION TREE LOG_LOSS\n(FOR INCREMENTAL ADDITION OF IMPORTANT TERMS/ATTRIBUTES)"
                    rm(dt_model, dt_probs, dt_class)
                }
            }

            graphics.off()
            png( 'plot_clt_chisquare_fselection_logloss.png', 1200, 800 )
            par(mar=c(5,20,5,5))
                x = unlist(COLLECT_VECTOR( f1_fs, function(x) x[1]))
                names(x) = sapply( 1:length(FSELECT_XVARS), function(x) paste( x,":", FSELECT_XVARS[x], sep=""))
                f1 = x
                colors=terrain.colors(length(FSELECT_XVARS))
                barplot( f1, col=colors, las=1, cex=1.85, cex.lab=2, cex.axis=2, cex.main=2, main=title, horiz=TRUE )
                text( rep(0.2,length(f1)), seq(0.7,(length(x)+2)*1.1,1.2), labels=sprintf("%.4f",f1), cex=2)
            dev.off()
        }
    }
}
# ############################################################################################


# ############################################################################################
# more incremental effects
# ############################################################################################
ADDITIONAL_INSIGHT = TRUE
if ( ADDITIONAL_INSIGHT ) {
    blck <- {
        if ( CLT_OPTIONS$'do incremental modeling on dt-fselect' ) {
            f1_dt = list()
            BANNER( 'NB TRAINING STAGE: BOOTSTRAPPING DECISION TREE FOUND TO BE IMPORTANT' )
            for ( idx in 1:length(DTVARS) ) {
                if ( FALSE ) {
                    BANNER( sprintf( 'NB TRAINING STAGE: DECISION TREE SELECTED: %s TERMS', CONCAT( DTVARS[1:idx])) )
                    FORMULA6 = GET_FORMULAE( DTVARS[1:idx] )
                    print ( FORMULA6 )
                    cmat = source('clt_naivebayes.R')
                    cmat = cmat[1]$value
                    print(cmat)
                    f1 = GET_F_RATIO( cmat)
                    f1_dt[[idx]] = f1
                    print(f1)
                    title="NAIVE BAYES LOG_LOSS\n(UNDER INCREMENTAL ADDITION OF IMPORTANT ATTRIBUTES)"
                } else {
                    BANNER( sprintf( 'DT TRAINING STAGE: FSELECTOR FEATURE SELECTED: %s TERMS', CONCAT( DTVARS[1:idx])) )
                    formula = GET_FORMULAE( DTVARS[1:idx] )
                    tree_controls = rpart.control(xval=10, minbucket=MINBCKT, minsplit=MINSPLT, maxdepth=MAXDEPTH, cp=CP, split="information")
                    WHAT = sample(1:nrow(ORIG_XTRAIN), min(5E5, nrow(ORIG_XTRAIN)))
                    dt_model = FIT_DECISION_TREE( model.frame(formula, ORIG_XTRAIN)[WHAT,], SLICE_DATAFRAME(ORIG_XTRAIN,PREDICT_COL)[WHAT,], FORMULA=formula, DO_PRUNING=TRUE, control=tree_controls )
                    # dt_model = prune(dt_model, cp=CP_PRUNE)
                    # print ( summary(dt_model) )

                    dt_probs = predict(dt_model, ORIG_XTRAIN)
                    dt_class = WHICH_CLASS_KERNEL(dt_probs, 0.5)
                    print( table( Y_TRUE, dt_class ) )
	                ll = COMPUTE_LOG_LOSS( Y_TRUE, dt_probs[,2] )
	                print( ll )
                    cname = substr(cnames,1,24)
                    f1_fs[[idx]] = ll
                    a = dt_model$variable.importance
                    #### a = a[a>100] #####
                    dtvars = attr(a,"names") 
                    dtform = GET_FORMULAE( dtvars )
                    print(dtform)
                    title="DECISION TREE LOG_LOSS\n(FOR INCREMENTAL ADDITION OF IMPORTANT TERMS/ATTRIBUTES)"
                    rm(dt_model, dt_probs, WHAT, dt_class)
                }
            }
    
            graphics.off()
            png( 'plot_clt_bootstrap_dt_fselection_logloss.png', 1200, 800 )
            par(mar=c(5,20,5,5))
                x = unlist(COLLECT_VECTOR( f1_fs, function(x) x[1]))
                names(x) = sapply( 1:length(DTVARS), function(x) paste( x,":", DTVARS[x], sep=""))
                f1 = x
                colors=terrain.colors(length(DTVARS))
                barplot( f1, col=colors, las=1, cex=1.85, cex.lab=2, cex.axis=2, cex.main=2, main=title, horiz=TRUE )
                text( rep(0.2,length(f1)), seq(0.7,(length(x)+2)*1.1,1.2), labels=sprintf("%.4f",f1), cex=2)
            dev.off()
        }
    }
}
# ############################################################################################


