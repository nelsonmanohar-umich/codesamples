# ############################################################################################
IDX_LL_TRAIN = 1
IDX_LL_TRAIN_ENHANCED = 13
IDX_LL_CV    = 14
IDX_LL_CV_ENHANCED = 12
F1=2;FPR=3;FNR=4;ACC=5;BER=6;MSNE=7;P=8;R=9;TPR=10;TNR=11
# ############################################################################################


# ############################################################################################
NVARS = 4
NRUNS = 10
# ############################################################################################


# ############################################################################################
PLOT_METRICS = function( METRICS) {
    par(mfcol=c(3,2))
    plot(   METRICS[,IDX_LL_TRAIN_ENHANCED], t='l', main="LLOSS/JCOST ACROSS FOLDS", col="red",
                                                                  lwd=3, cex=2.0, cex.main=2.0, cex.axis=2.0, cex.lab=2 )
    points( METRICS[,IDX_LL_CV_ENHANCED],    t='l', col="green", cex=2.0, lwd=2 )
    text( 1:nrow(METRICS), METRICS[,IDX_LL_CV_ENHANCED], round(METRICS[,IDX_LL_CV_ENHANCED], 2), col="brown", cex=0.8 )
    legend("bottom", legend=c("DTBOOT-TRAIN", "EMSEMBLE-CV"), pch=c(20,23), col=c("red","green"), ncol=2, cex=1.0, lwd=3)

    plot( METRICS[,F1],  t='l', main="F1 RATIO ACROSS FOLDS",     lwd=3, cex=2.0, cex.main=2.0, cex.axis=2.0, cex.lab=2 )
    plot( METRICS[,ACC], t='l', main="ACCURACY ACROSS FOLDS",     lwd=3, cex=2.0, cex.main=2.0, cex.axis=2.0, cex.lab=2 )
    plot( METRICS[,BER], t='l', main="BAL.ERR.RATE ACROSS FOLDS", lwd=3, cex=2.0, cex.main=2.0, cex.axis=2.0, cex.lab=2 )
    plot( METRICS[,FPR], t='l', main="FPR ACROSS FOLDS",          lwd=3, cex=2.0, cex.main=2.0, cex.axis=2.0, cex.lab=2 )
    # plot( METRICS[,P],  METRICS[,R], t='p', col="brown", pch=24, main="PRECISION vs. RECALL", cex=2.0, cex.main=2.0, cex.axis=2.0, cex.lab=2 )
    plot( METRICS[,FNR], METRICS[,TPR], t='p', col="blue", pch=22, cex=1.2, main="FNR vs TPR ACROSS FOLDS" )
}
# ############################################################################################


# ############################################################################################
# bootstrap of model
# ############################################################################################
BANNER( 'BOOTSTRAP ANALYSIS OF THE SELECTED MODEL' )

TRUE_ORIG_XTRAIN = as.data.frame(ORIG_XTRAIN)

ADDITIONAL_INSIGHT = TRUE
if ( ADDITIONAL_INSIGHT ) {

    PROB_METRICS_VECTOR = VECTOR( NUM_MODELS )
    PROB_METRICS_MATRIX = MATRIX( NUM_MODELS, NRUNS )

	Y_TRUE = as.matrix(ORIG_XTRAIN[,PREDICT_VAR])

	ORIG_ORDERING = rownames(ORIG_XTRAIN)

	LEAVE_OUT = c()

	FULL_BOOTSTRAP = FALSE

	SUBSAMPLING_EXTENT = as.integer(0.650 * nrow(ORIG_XTRAIN))
	SUBSAMPLING_EXTENT_LEVELS = seq(as.integer(0.10 * nrow(ORIG_XTRAIN)), 1*nrow(ORIG_XTRAIN), as.integer(0.10 * nrow(ORIG_XTRAIN)))
	if ( FULL_BOOTSTRAP ) SUBSAMPLING_EXTENT = as.integer(1.00 * nrow(ORIG_XTRAIN))
	
	START = TRUE
	METRICS = MATRIX( NRUNS, 14 )
    LogLoss = list()
	for ( j in 1:NRUNS) {

	    # TODO: stratify this:
	    if ( FULL_BOOTSTRAP ) {
	        BOOT_ORDER = sample(ORIG_ORDERING, SUBSAMPLING_EXTENT, replace=TRUE)
	    } else {
	        BOOT_ORDER = sample(ORIG_ORDERING, SUBSAMPLING_EXTENT_LEVELS[j], replace=TRUE)
	    }
        BOOT_ORDER = BOOT_ORDER[complete.cases( ORIG_XTRAIN[as.integer(BOOT_ORDER),])]
        BANNER( sprintf( "BOOTSTRAP STAGE %s --- with [%s] samples", j, length( BOOT_ORDER ) ) )

	    TRAIN_Y_TRUE = as.matrix(ORIG_XTRAIN[BOOT_ORDER, PREDICT_VAR])
	    #Y_TRUE = TRAIN_Y_TRUE
	

	    if ( CLT_OPTIONS$'bootstrapper' == "NB" ) {
           ORIG_XTRAIN = as.data.frame( TRUE_ORIG_XTRAIN )[BOOT_ORDER,]
	       Y_TRUE = as.matrix(ORIG_XTRAIN[BOOT_ORDER, PREDICT_VAR])
	            BOOTBASE_MODEL = naiveBayes( x=ORIG_XTRAIN[BOOT_ORDER,-PREDICT_COL], y=ORIG_XTRAIN[BOOT_ORDER,PREDICT_COL]) #, laplace=LAPLACE )
	            TRAIN_YP_PROBS = predict( BOOTBASE_MODEL, newdata=ORIG_XTRAIN[BOOT_ORDER,], type='raw' ) #, threshold=NB_THRESHOLD, eps=NB_EPS)
	            TRAIN_YP_CLASS = WHICH_CLASS_KERNEL( TRAIN_YP_PROBS )
	            METRICS[j,IDX_LL_TRAIN] = COMPUTE_LOG_LOSS( TRAIN_Y_TRUE, TRAIN_YP_PROBS[,2])
                if ( PROBABILITY_ENHANCE ) {
                    TRAIN_YP_PROBS = PROBABILITY_ENHANCER ( TRAIN_YP_PROBS, Y_TRUE, TRAIN=TRUE)
	                TRAIN_YP_CLASS = WHICH_CLASS_KERNEL( TRAIN_YP_PROBS )
	                METRICS[j,IDX_LL_TRAIN_ENHANCED] = COMPUTE_LOG_LOSS( Y_TRUE, TRAIN_YP_PROBS[,2])
                }

           ORIG_XTRAIN = as.data.frame( TRUE_ORIG_XTRAIN )
           ORIG_XTEST = ORIG_XTRAIN
	       Y_TRUE = as.matrix(ORIG_XTRAIN[, PREDICT_VAR])
	            CV_YP_PROBS = predict(BOOTBASE_MODEL, newdata=ORIG_XTEST, type='raw')
	            CV_Y_TRUE   = as.matrix(ORIG_XTEST[,PREDICT_VAR])
	            CV_YP_CLASS = WHICH_CLASS_KERNEL( CV_YP_PROBS )
	            METRICS[j,IDX_LL_CV]  = COMPUTE_LOG_LOSS( CV_Y_TRUE, CV_YP_PROBS[,2])
                if ( PROBABILITY_ENHANCE ) {
                    CV_YP_PROBS = PROBABILITY_ENHANCER ( CV_YP_PROBS, Y_TRUE, TRAIN=FALSE)
	                CV_YP_CLASS = WHICH_CLASS_KERNEL( CV_YP_PROBS )
	                METRICS[j,IDX_LL_CV_ENHANCED] = COMPUTE_LOG_LOSS( Y_TRUE, CV_YP_PROBS[,2] )
                }
	        METRICS[j,MSNE] = sum( abs(as.numeric(CV_YP_CLASS)-1 - as.numeric(CV_Y_TRUE)-1)^2 )/ length(CV_YP_CLASS)
	        CV_CMAT = table( CV_Y_TRUE, CV_YP_CLASS )
	        print ( CV_CMAT )
        }

	    if ( CLT_OPTIONS$'bootstrapper' == "DT" ) {
            BANNER( sprintf( 'BOOTSTRAP STAGE VIA DECISION TREE MODEL WITH FSELECT VARS: %s TERMS', NVARS ) )
            cols = FSELECT_XVARS[1:NVARS]
            formula = GET_FORMULAE( cols )
            cname = sprintf("DT(clk~%s)", substr(CONCAT(cols),1,20))
            print ( formula )

            # BOOTSTRAP TRAINING
                ORIG_XTRAIN = as.data.frame( TRUE_ORIG_XTRAIN )[BOOT_ORDER,]
	            Y_TRUE = as.matrix(ORIG_XTRAIN[BOOT_ORDER, PREDICT_VAR])
                    tree_controls = rpart.control(xval=10, minbucket=MINBCKT, minsplit=MINSPLT, maxdepth=MAXDEPTH, cp=CP, split="information")
                    dt_model = FIT_DECISION_TREE( ORIG_XTRAIN[,c(cols,'click')], SLICE_DATAFRAME(ORIG_XTRAIN[,PREDICT_COL]), FORMULA=formula, DO_PRUNING=TRUE, control=tree_controls )
	                Y_TRUE = as.matrix(ORIG_XTRAIN[BOOT_ORDER, PREDICT_VAR])
                    TRAIN_YP_PROBS = predict(dt_model, ORIG_XTRAIN, type='prob')
	                TRAIN_Y_TRUE   = ORIG_XTRAIN[,PREDICT_VAR]
                    TRAIN_YP_CLASS = WHICH_CLASS_KERNEL(TRAIN_YP_PROBS, 0.5)
	                METRICS[j,IDX_LL_TRAIN] = COMPUTE_LOG_LOSS( TRAIN_Y_TRUE, TRAIN_YP_PROBS[,2])
                    if ( PROBABILITY_ENHANCE ) {
                        TRAIN_YP_PROBS = PROBABILITY_ENHANCER ( TRAIN_YP_PROBS, Y_TRUE, TRAIN=TRUE)
	                    TRAIN_YP_CLASS = WHICH_CLASS_KERNEL( TRAIN_YP_PROBS )
	                    METRICS[j,IDX_LL_TRAIN_ENHANCED] = COMPUTE_LOG_LOSS( TRAIN_Y_TRUE, TRAIN_YP_PROBS[,2])
                    }

                # BOOTSTRAP TESTING
                ORIG_XTRAIN = as.data.frame( TRUE_ORIG_XTRAIN )
	            Y_TRUE = as.matrix(ORIG_XTRAIN[, PREDICT_VAR])
                ORIG_XTEST = ORIG_XTRAIN
	                Y_TRUE = as.matrix(ORIG_XTRAIN[, PREDICT_VAR])
                    CV_YP_PROBS = predict(dt_model, ORIG_XTEST, type='prob')
	                CV_Y_TRUE   = ORIG_XTEST[,PREDICT_VAR]
                    CV_YP_CLASS = WHICH_CLASS_KERNEL(CV_YP_PROBS, 0.5)
	                METRICS[j,IDX_LL_CV] = COMPUTE_LOG_LOSS( CV_Y_TRUE, CV_YP_PROBS[,2])
                    LogLoss[[j]] = METRICS[j,IDX_LL_CV_ENHANCED]
                    if ( PROBABILITY_ENHANCE ) {
                        CV_YP_PROBS = PROBABILITY_ENHANCER ( CV_YP_PROBS, CV_Y_TRUE, TRAIN=FALSE, FAST=TRUE)
	                    CV_YP_CLASS = WHICH_CLASS_KERNEL( CV_YP_PROBS )
	                    METRICS[j,IDX_LL_CV_ENHANCED] = COMPUTE_LOG_LOSS( CV_Y_TRUE, CV_YP_PROBS[,2])
                        LogLoss[[j]] = METRICS[j,IDX_LL_CV_ENHANCED]
                    }

	            CV_CMAT = table( CV_Y_TRUE, CV_YP_CLASS )
                print( CV_CMAT )
                a = dt_model$variable.importance
                #### a = a[a>100] #####
                dtvars = attr(a,"names") 
                dtform = GET_FORMULAE( dtvars )
                print(dtform)
	            METRICS[j,MSNE] = sum( abs(as.numeric(CV_YP_CLASS)-1 - as.numeric(CV_Y_TRUE)-1)^2 )/ length(CV_YP_CLASS)
        }

	    if ( CLT_OPTIONS$'bootstrapper' == "LR" ) {
	        BOOTBASE_MODEL = glm( BASE_FORMULA, data=ORIG_XTRAIN[BOOT_ORDER,], family=binomial(link = "logit"), weights=initial_wt )
	        THETA = BOOTBASE_MODEL$coefficients
	        if ( START ) {
	            THETAS = as.data.frame( THETA )
	            START = FALSE
	        } else { 
	            THETAS = cbind(THETAS, as.data.frame(THETA))
	        }
	        TRAIN_ZVALS = predict(BOOTBASE_MODEL, ORIG_XTRAIN)
	        TRAIN_YP_CLASS = sapply( TRAIN_ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
	        ZVALS = predict(BOOTBASE_MODEL, ORIG_XTRAIN)
	        YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
	        JCOST = JCOST_Z( ZVALS, Y_TRUE )
	
	        METRICS[j,1] = sum(JCOST, na.rm=TRUE)
	        CV_CMAT = table( Y_TRUE, YP_CLASS )
	        print ( CV_CMAT )
	        METRICS[j,MSNE] = sum( abs(as.numeric(YP_CLASS)-1 - as.numeric(Y_TRUE)-1)^2 )/ length(YP_CLASS)
	    }
	
        # ######################################
	    METRICS[j,F1]  = GET_F_RATIO( CV_CMAT)
	    METRICS[j,FPR] = GET_FPR( CV_CMAT)
	    METRICS[j,FNR] = GET_FNR( CV_CMAT)
	    METRICS[j,ACC] = GET_ACC( CV_CMAT)
	    METRICS[j,BER] = ( GET_TPR( CV_CMAT) + GET_TNR( CV_CMAT ) ) / 2.0
	    METRICS[j,P]   = GET_PRECISION( CV_CMAT )
	    METRICS[j,R]   = GET_RECALL( CV_CMAT )
	    METRICS[j,TPR] = GET_TPR( CV_CMAT )
	    METRICS[j,TNR] = GET_TNR( CV_CMAT )
        # ######################################

        # ######################################
        METRICS[j,is.nan(METRICS[j,])] = 0
        # ######################################

        PROB_METRICS_MATRIX[,j] = unlist(PROB_METRICS_VECTOR)
    }

    # ############################################################################################
    # a look at the CV performance
    # a look at the theta coefficients
    # ############################################################################################
    BANNER( 'PLOT SUMMARY OF THE BOOTSTRAP ANALYSIS OF THE SELECTED MODEL' )
    METRICS = METRICS[1:j,]
	if ( CLT_OPTIONS$'bootstrapper' == "LR" ) {
	    TMAT = as.matrix( THETAS )
	    colnames(TMAT) = paste( "BOOTSTRAP", 1:NRUNS, sep="")
	    THETAS_COEFF_MEANS = apply( TMAT, 1, mean )
	    THETAS_COEFF_SDEV  = apply( TMAT, 1, sd )
	    hist(JCOST, main="HIST OF JCOST FOR LAST FOLD")
	    boxplot( t(TMAT), las=1, cex=0.7, cex.axis=0.7, main="COEFFICIENTS ACROSS FOLDS" )

        graphics.off()
	    PLOT_METRICS( METRICS )

	} else {
        graphics.off()
        # ####################################################################################
        png('plot_clt_bootstrap_metrics_logloss.png', 1200, 800 )
        par(mfrow=c(2,1))
        ylims = c(min(METRICS[,IDX_LL_TRAIN_ENHANCED],METRICS[,IDX_LL_CV_ENHANCED])-0.03, max(METRICS[,IDX_LL_TRAIN_ENHANCED],METRICS[,IDX_LL_CV_ENHANCED])+0.03)
	    plot( METRICS[,IDX_LL_TRAIN_ENHANCED],    ylim=ylims, t='b', col="red",   lwd=3, pch=21, cex=2, cex.lab=2.0, cex.axis=2.0, cex.main=2.0, ylab="LogLoss", xlab="Fraction ( i/10 ) of The Data Being Trained On", main="LOG LOSS ACROSS FOLDS" )
	    # points( METRICS[,IDX_LL_TRAIN_ENHANCED],         t='b', col="brown", lwd=3, pch=21, cex=2, cex.lab=2.0, cex.axis=2.0, cex.main=2.0)
	    # points( METRICS[,IDX_LL_CV],                     t='b', col="black", lwd=3, pch=22, cex=2, cex.lab=2.0, cex.axis=2.0, cex.main=2.0)
	    points( METRICS[,IDX_LL_CV_ENHANCED],            t='b', col="green", lwd=3, pch=23, cex=2, cex.lab=2.0, cex.axis=2.0, cex.main=2.0)
        legend("topleft", legend=c("EMSEMBLE-TRAIN", "EMSEMBLE-CV"), pch=c(21,23), col=c("brown","green"), ncol=2, cex=2.0, lwd=3)
        dev.off()
 
        graphics.off()
        png('plot_clt_bootstrap_metrics_roc.png', 1200, 800 )
        par(mfrow=c(2,1))
	    plot( METRICS[,FPR], METRICS[,TPR], t='p', col="blue", pch=22, cex=2, cex.lab=2.0, ylim=c(0,1), xlim=c(0,1), cex.axis=2.0, cex.main=2.0, main="ESTIMATED ROC ACROSS FOLDS" )
        dev.off()

        graphics.off()
        png('plot_clt_bootstrap_metrics_cmat_emsemble.png', 2000, 1200)
	    PLOT_METRICS( METRICS )
        dev.off()
        # ####################################################################################
	}
    # ############################################################################################

    # ############################################################################################
        graphics.off()
        # ####################################################################################
        png( 'clt_cv_bootstrap_performance_timeplot.png', 1200, 800 )
        N = 4
        colors = gray.colors(N)
        t = t(PROB_METRICS_MATRIX[,1:NRUNS])
        s = c(21:(21+N))
        plot( t[,12], t='b', pch='o', col=20, lwd=4, xlab="Fold Number", ylim=c(0.30, 0.55), ylab="Log Loss For Fold", cex=1.0, cex.axis=2.0, cex.lab=2.0, cex.main=2.0 )
        for ( l in 1:N) {
            points( t[,l], t='b', col=colors[l], pch=s[l], cex=1.0, lwd=1+l/5 )
        }
        #legend("bottomright", legend = c("EMSEMBLE", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9"), col = c(20,colors), pch=c('o', s), ncol = 5, cex = 1.1, lwd = 3)
        legend("bottomright", legend = c("EMSEMBLE", "M1", "M2", "M3", "M4"), col = c(20,colors), pch=s, ncol = 5, cex = 1.2, lwd = 3)
        dev.off()
        # ####################################################################################

        graphics.off()
        # ####################################################################################
        png( 'clt_cv_bootstrap_performance_histogram.png', 1200, 800)
        par(mfcol=c(2,2))
        t = METRICS
        hist(t[,IDX_LL_TRAIN],          breaks=NRUNS, main="Histogram of (Log Loss for Fold) TRAIN",          xlim=c(0.2,1.0), ylab="Frequency of (Num of Folds)", xlab="Log Loss Value", cex=2.0, cex.lab=2.0, cex=1.0, cex.main=2.0)
        hist(t[,IDX_LL_TRAIN_ENHANCED], breaks=NRUNS, main="Histogram of (Log Loss for Fold) TRAIN ENHANCED", xlim=c(0.2,1.0), ylab="Frequency of (Num of Folds)", xlab="Log Loss Value", cex=2.0, cex.lab=2.0, cex=1.0, cex.main=2.0)
        hist(t[,IDX_LL_CV],             breaks=NRUNS, main="Histogram of (Log Loss for Fold) CV",             xlim=c(0.2,1.0), ylab="Frequency of (Num of Folds)", xlab="Log Loss Value", cex=2.0, cex.lab=2.0, cex=1.0, cex.main=2.0)
        hist(t[,IDX_LL_CV_ENHANCED],    breaks=NRUNS, main="Histogram of (Log Loss for Fold) CV ENHANCED",    xlim=c(0.2,1.0), ylab="Frequency of (Num of Folds)", xlab="Log Loss Value", cex=2.0, cex.lab=2.0, cex=1.0, cex.main=2.0)
        dev.off()
        # ####################################################################################
}

ORIG_XTRAIN = as.data.frame(TRUE_ORIG_XTRAIN)
Y_TRUE = as.matrix(ORIG_XTRAIN[,PREDICT_VAR])
# ############################################################################################
