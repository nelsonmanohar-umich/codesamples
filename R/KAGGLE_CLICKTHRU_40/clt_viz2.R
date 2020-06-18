p = function(fl,y,ft) { ft[fl,y]/(ft[fl,1]+ft[fl,2]) * (ft[fl,1]+ft[fl,2])/sum(ft[1:nrow(ft),1:2]) }

# ############################################################################################
    # ###########################################################################################
    #"ad_id" "C01" #"C14" #"C15" #"C16" #"C17" #"C18" #"C19" #"C20" #"C21"
    # ###########################################################################################
    OCOLS = c("click","YYMMDDHH","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model","device_type","device_conn_type","H01","H14","H15","H16","H17","H18","H19","H20","H21","H44","H54","hour","day","shift","adix","adix_ts","adix_ts2","adix_ts6","sitecatclickprobs0","appcatclickprobs0","sitecatclickprobs1","appcatclickprobs1","sitecatclickprobs2","probs","hourclickprobs","dayclickprobs")

    graphics.off()
    # ###########################################################################################
    # a simple histogram matrix of the original features
    # http://stackoverflow.com/questions/17416453/force-r-to-plot-histogram-as-probability-relative-frequency
    # ###########################################################################################
    NMAX = 0
    TOPN = 50
    histcolors = gray.colors(TOPN)
    if ( TRUE ) {
        OCOLS = c("day","hour","shift","adix_ts6",
                  "hourclickprobs","dayclickprobs","appcatclickprobs0","sitecatclickprobs1",
                  "H14","H15","H16","H17",
                  "H18","H19","H20","H21")
        png( 'plot_clt_histogram_transformed_features.png', 2000, 1260)
        par(mfrow=c(4,4))
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        Y = ORIG_XTRAIN[SUBSET,]
        for ( col in OCOLS) {
            if ( col == "" ) {
                plot.new()
                next
            }
            WHICH_COL = which( colnames(Y) %in% col)
            if (length(WHICH_COL) == 0 ) next
            if ( col %in% c("dayclickprobs", "hourclickprobs")) {
                x = as.numeric(Y[,WHICH_COL])
                h <- hist(x, breaks = 50, plot=FALSE)
                h$counts=h$counts/sum(h$counts)
                plot(h, main=sprintf("Histogram( %s )", col), col=histcolors, cex.lab=2.0, cex.main=3.0, cex=2.0, cex.axis=2.0, xlab=sprintf("uniq_id( %s )",col), ylab="Probabilities")
            } else { 
                x = as.integer(Y[,WHICH_COL])
                h <- hist(x, breaks = 50, plot=FALSE)
                h$counts=h$counts/sum(h$counts)
                plot(h, main=sprintf("Histogram( %s )", col), col=histcolors, cex.lab=2.0, cex.main=3.0, cex=2.0, cex.axis=2.0, xlab=sprintf("uniq_id( %s )",col), ylab="Probabilities")
            }
        }
        dev.off()
    }
    # ###########################################################################################


    graphics.off()
    # ###########################################################################################
    NMAX = 0
    TOPN = 20
    histcolors = seq(20,60,2)
    if ( TRUE ) {
        OCOLS = c("day","hour","shift","adix_ts6",'hourclickprobs','dayclickprobs',"appcatclickprobs0","sitecatclickprobs1","H14","H15","H16","H17","H18","H19","H20","H21")
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        Y = ORIG_XTRAIN[SUBSET,]
        png( 'plot_clt_click_tabulation_plots_per_transformed_features.png', 2000, 1260)
        par(mfrow=c(4,4))
        for (col in OCOLS) {
            if ( col %in% c("YYMMDDHH")) {
                t = table(Y[,col], Y$click)
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot(t(t), beside=TRUE,  main=sprintf("CLICKS wrt [%s]", col), col=c(20,22), cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8)
                #barplot(t(t), beside=FALSE,  main=sprintf("CLICKS wrt [%s]", col), col=c(20,22), cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8)
            } else if ( col %in% c("device_ip","device_id","device_model")) {
                t = GET_TOPN_TABLE_FROM( ORIG_XTRAIN[,col], ORIG_XTRAIN[,PREDICT_COL], TOPN, WRT=c(1,2) )
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot( t(t[1:TOPN,]), las=0, main=sprintf("CLICKS wrt TOP %s [%s]", TOPN, col), col=histcolors, cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8)
            } else if ( col %in% c("hourclickprobs",'dayclickprobs',"shiftclickprobs") ) {
                t = table(Y[,col], Y$click)
                rownames(t) = levels(Y[,col])
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot( t(t), beside=T, las=0, main=sprintf("CLICKS wrt ORDERED [%s]", col), col=c(20,22), cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8)
            } else if ( col %in% c("site_id","app_id","site_domain","app_domain")) {
                t = GET_TOPN_TABLE_FROM( ORIG_XTRAIN[,col], ORIG_XTRAIN[,PREDICT_COL], TOPN, WRT=c(1,2) )
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot( t(t[1:TOPN,]), las=0, main=sprintf("CLICKS wrt TOP %s [%s]", TOPN, col), col=histcolors, cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8)
            } else if ( col %in% c("banner_pos","site_category","app_category","device_conn_type","device_type")) {
                t = table(Y[,col], Y$click)
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot( t(t), las=0, main=sprintf("CLICKS wrt [%s]", col), col=histcolors, cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8)
            } else if ( col %in% c("C14","C17", "C19", "C20")) {
                t = table(Y[,col], Y$click)
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                plot(t[,1], t='l', lwd=2, main=sprintf("CLICKS wrt [%s]", col), col=20, cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8, ylab="clicks", xlab="")
                points( t[,2], t='l', lwd=2, col=22)
                #axis(1, at=idx, labels=tx[idx], las=2)
            } else {
                t = table( as.integer(Y[,col]), Y$click)
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot( t(t), las=0, main=sprintf("CLICKS wrt [%s]", col), col=histcolors, cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8)
            }
        }
        dev.off()
    }
    # ###########################################################################################

    graphics.off()
    NMAX = min(250000, nrow(ORIG_XTRAIN))
    # ###########################################################################################
    # a correlation matrix, cluster plot of the original features 
    # ###########################################################################################
    #
    # ###########################################################################################
    OCOLS = minus(c("click","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model","device_type","device_conn_type",
                    "H01","H14","H15","H16","H17","H18","H19","H20","H21",#"C01","C14","C15","C16","C17","C18","C19","C20","C21",
                    "hour","day","shift", "adix_ts6", "sitecatclickprobs0","appcatclickprobs0","sitecatclickprobs1","appcatclickprobs1","sitecatclickprobs2",
                    "hourclickprobs","dayclickprobs"),
                  c())
    if ( TRUE ) {
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        WHICH_COLS = which( colnames(ORIG_XTRAIN) %in% OCOLS )
        Y = sapply(WHICH_COLS, function(x) as.numeric(ORIG_XTRAIN[SUBSET,x]))
        colnames(Y) = colnames(ORIG_XTRAIN)[WHICH_COLS]
        Y = scale(Y)
        # correlation_between_features = cor(Y) #, method="pearson", use="pairwise.complete.obs")
        correlation_between_features = cor(Y, method="spearman", use="pairwise.complete.obs")
        t = complete.cases(correlation_between_features)
        correlation_between_features = correlation_between_features [t,t]
        png( 'plot_clt_feature_correlation_transformed_features.png', 1200, 800 )
        plot( hclust( dist( abs(correlation_between_features))), cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8, main="CLUSTER DENDOGRAM WRT CORRELATION BETWEEN TRANSFORMED FEATURES")
        dev.off()
        rm(Y)
    }
    # ###########################################################################################

    graphics.off()
    # ############################################################################################
    # insight about potential non-linear and factor/subset interaction effects via decision trees 
    # ############################################################################################
    NMAX = min(250000, nrow(ORIG_XTRAIN))
    ADDITIONAL_INSIGHT = TRUE
    if ( ADDITIONAL_INSIGHT ) {
        OCOLS = minus(c("click","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model","device_type","device_conn_type",
                        "H01","H14","H15","H16","H17","H18","H19","H20","H21",
                        "hour","day","shift", "adix_ts6", "sitecatclickprobs0","appcatclickprobs0","sitecatclickprobs1","appcatclickprobs1","sitecatclickprobs2",
                        "hourclickprobs","dayclickprobs"),
                      c())
        WHICH_COLS = which( colnames(ORIG_XTRAIN) %in% OCOLS )
        BANNER( 'DECISION TREE INSIGHT' )
        FORMULA = 'click ~ .'
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        Y = ORIG_XTRAIN[SUBSET,WHICH_COLS]
        tree_controls = rpart.control(xval=10, minbucket=29, maxdepth=5, cp=1E-5, split="information")
        sink('temp')
        DT_MODEL = FIT_DECISION_TREE( model.frame(FORMULA, Y), SLICE_DATAFRAME(Y,PREDICT_COL), FORMULA=FORMULA, DO_PRUNING=TRUE, control=tree_controls )
        sink()
        DT_MODEL = prune( DT_MODEL, cp=1E-3)
        DT_PROBS = predict(DT_MODEL, ORIG_XTRAIN[SUBSET,])
        DT_CLASS = WHICH_CLASS_KERNEL(DT_PROBS, 0.5)
        A = DT_MODEL$variable.importance
        print( A) 
        cat(HEADER)
        print( table( Y_TRUE[SUBSET], DT_CLASS ) )
        cat(HEADER)
	    print( COMPUTE_LOG_LOSS( Y_TRUE[SUBSET], DT_PROBS[,2] ))
        cat(HEADER)
        #### A = A[A>100] #####
        DTVARS = attr(A,"names") 
        DTFORM = GET_FORMULAE( DTVARS )
        print(DTFORM)
    }
    # ###########################################################################################

    graphics.off()
    NMAX = min(250000, nrow(ORIG_XTRAIN))
    # ###########################################################################################
    # pca a correlation matrix, cluster plot of the original features 
    # ###########################################################################################
    if ( TRUE ) {
        BANNER( 'HCLUST INSIGHT' )
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        WHICH_COLS = which( colnames(ORIG_XTRAIN) %in% c('click',DTVARS))
        Y = sapply(WHICH_COLS, function(x) as.numeric(ORIG_XTRAIN[SUBSET,x]))
        colnames(Y) = colnames(ORIG_XTRAIN)[WHICH_COLS]
        Y = scale(Y)
        correlation_between_features = cor(Y, method="spearman", use="pairwise.complete.obs")
        t = complete.cases(correlation_between_features)
        correlation_between_features = correlation_between_features [t,t]
        png( 'plot_clt_feature_correlation_dtselected_transformed_features.png', 1200, 800 )
        plot( hclust( dist( abs(correlation_between_features))), cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8, main="CLUSTER DENDOGRAM WRT CORRELATION BETWEEN TRANSFORMED FEATURES")
        dev.off()
        rm(Y)
    }
    # ###########################################################################################


# ############################################################################################
if ( FALSE ) {
    BANNER('LDA - TRANSFORMED')
    X = as.data.frame(sapply( 1:ncol(ORIG_XTRAIN), function(x) as.numeric(ORIG_XTRAIN[,x])))
    colnames(X) = colnames(ORIG_XTRAIN)
    WHICH_COLS = setdiff(colnames(ORIG_XTRAIN), c("click", "origin", "ad_id"))
    xform = formula(GET_FORMULAE(WHICH_COLS))

    if ( 0 ) {
        lfit = lda ( xform, data=X , CV=TRUE, na.action="na.omit")
        t = as.numeric(lfit$posterior[,2])
        COMPUTE_LOG_LOSS( Y_TRUE, t)
        print(table( Y_TRUE, lfit$class))
        COMPUTE_LOG_LOSS(Y_TRUE, t)
    }

    png('clt_lda_transformed_features.png', 1200, 800)
    lfit = lda ( xform, data=X)# , CV=TRUE)
    plot(lfit, xlim=c(-10, 10), ylim=c(0,1))
    dev.off()
    rm(X, lfit)
}
# ############################################################################################






