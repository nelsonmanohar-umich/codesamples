
# ############################################################################################
# tables of factor to predict var  
# ############################################################################################
BANNER( 'SUMMARY OF FACTORS TO PREDICTOR' )
ADDITIONAL_INSIGHT = TRUE
if ( ADDITIONAL_INSIGHT ) {
    blck <- {
        TOPN = 10
        for ( i in 1:ncol(ORIG_XTRAIN))  {
            if ( colnames(ORIG_XTRAIN)[i] == "ad_id" ) { next }
            COL = ORIG_XTRAIN[,i]
            print ( colnames(ORIG_XTRAIN)[i] )
            GET_TOPN_TABLE_FROM( ORIG_XTRAIN[,i], ORIG_XTRAIN[,PREDICT_COL], TOPN, WRT=c(1,2) )
        }
    }
}
# ############################################################################################


# ############################################################################################
# individual feature selection
# ############################################################################################
BANNER( 'FEATURE SELECTION' )
ADDITIONAL_INSIGHT = TRUE
if ( ADDITIONAL_INSIGHT ) {
    source('fselect.R')
    NMAX = min(500000, nrow(ORIG_XTRAIN))
    SUBSET = 1:nrow(ORIG_XTRAIN)
    if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))

    cols = minus(colnames(ORIG_XTRAIN), c(Ctypes, "origin", "ad_id", "YYMMDDHH"))
    if ( CLT_OPTIONS$use_ctypes_instead_of_htypes )
        cols = minus(colnames(ORIG_XTRAIN), c(Htypes, "origin"))
    Y = sapply( cols, function(x) as.factor(ORIG_XTRAIN[SUBSET,x]))

    done = FALSE
    try({
        Y = as.data.frame(Y)
        FSELECT_RETVALS = DO_GENERAL_SUBSET_SELECTION( Y, dfname="CLT, C:F",
                                   using_approach=chi.squared,
                                   approach_ppname="chi.squared", 
                                   #rtypes="SUBSAMPLE|COMPLETECASES",
                                   rtypes="COMPLETECASES",
                                   refine=FALSE,
                                   target_var="click",
                                   top=24,
                                   nmax=NMAX,
                                   cmax=0.8 )
        FSELECT_MODEL = FSELECT_RETVALS[[1]]
        FSELECT_XVARS = FSELECT_RETVALS[[3]]
        FSELECT_YVAR  = FSELECT_RETVALS[[2]]
        FSELECT_ATTRIMP = FSELECT_RETVALS[[4]]
        done = TRUE
    })
    if (!done) {
      SUBSET = 1:nrow(ORIG_XTRAIN)
      if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
      try({
        Y = ORIG_XTRAIN[SUBSET, cols]
        FSELECT_RETVALS = DO_GENERAL_SUBSET_SELECTION( Y, dfname="CLT, C:F",
                                   using_approach=chi.squared, approach_ppname="chi.squared", 
                                   #rtypes="SUBSAMPLE|COMPLETECASES",
                                   rtypes="COMPLETECASES",
                                   refine=FALSE,
                                   target_var="click", top=18, nmax=NMAX, cmax=0.8 )
        FSELECT_MODEL = FSELECT_RETVALS[[1]]
        FSELECT_XVARS = FSELECT_RETVALS[[3]]
        FSELECT_YVAR  = FSELECT_RETVALS[[2]]
        FSELECT_ATTRIMP = FSELECT_RETVALS[[4]]
      })
    }

    graphics.off()
    # ############################################################################################
    blck <- {
        png( 'plot_clt_fselection_chisquare.png', 1200, 800 )
        par(mar=c(5,20,3,3))
            ordering = order( FSELECT_ATTRIMP, decreasing=T )
            x = unlist(COLLECT_VECTOR( FSELECT_ATTRIMP, function(x) x[1]))
            names(x) = sapply(rownames(FSELECT_ATTRIMP), function(x) substr(x,1,24))
            colors=terrain.colors(length(cols))
            barplot( x[ordering], col=colors, las=1, cex=1.5, cex.lab=1.5, cex.axis=1.5, cex.main=2.0, 
                    xlab="FSelector Worth Metric (bigger --> more dependent x:y)",
                    main="Chi Square (click~xvar Dependency) Feature Selection", horiz=TRUE )
        dev.off()
    }
    # ############################################################################################

    # ############################################################################################
    # feature exploration
    # ############################################################################################
    DO_PLOT = FALSE
    if ( DO_PLOT ) {
        BANNER( 'FEATURE EXPLORATION PLOTS' )
        pdf( 'plot_clt_feature_exploration.pdf', 12, 9 )
        par(mfrow=c(2,2))
        NF=10
        for ( i in 1:length(FSELECT_XVARS) ) {
            XVAR = FSELECT_XVARS[i]
            print(XVAR)
            cat(HEADER)
            XX = ORIG_XTRAIN[,XVAR]
            YY = ORIG_XTRAIN[,PREDICT_VAR]
            print(summary(XX))
            if (length(grep("factor", class(XX))) > 0) {
                plot( XX, main=paste(XVAR) )
                plot( table( XX, YY), color=20:40 )
            } else {
                hist( XX, main=paste(XVAR) )
                plot( table( RECODE(XX, NF), YY), color=20:40 )
            }
        }
        dev.off()
    }
}
graphics.off()
# ############################################################################################


# ############################################################################################
# exploratory/bootstrapping insight via extremely general decision tree
# ############################################################################################
DTVARS = FSELECT_XVARS
ADDITIONAL_INSIGHT = TRUE
DEPTH = MAXDEPTH; if ( nrow(ORIG_XTRAIN) > 1E6 ) { DEPTH = 1; CP=1E-1 }
if ( ADDITIONAL_INSIGHT ) {
    BANNER( 'BOOTSTRAPPING DECISION TREE INSIGHT' )
    FORMULA = GET_FORMULAE( FSELECT_XVARS[1:4] )
    tree_controls = rpart.control(xval=10, minbucket=MINBCKT, minsplit=MINSPLT, maxdepth=DEPTH, cp=CP, split="information")
    WHAT = sample(1:nrow(ORIG_XTRAIN), min(1E5,nrow(ORIG_XTRAIN)))
    DT_MODEL = FIT_DECISION_TREE( model.frame(FORMULA, ORIG_XTRAIN)[WHAT,], SLICE_DATAFRAME(ORIG_XTRAIN[WHAT,PREDICT_COL]), FORMULA=FORMULA, DO_PRUNING=TRUE, control=tree_controls )
    DT_PROBS = predict(DT_MODEL)
    DT_CLASS = WHICH_CLASS_KERNEL(DT_PROBS, 0.5)
    print( table( Y_TRUE[WHAT], DT_CLASS ) )
	print( COMPUTE_LOG_LOSS( Y_TRUE[WHAT], DT_PROBS[,2] ))
    A = DT_MODEL$variable.importance
    A = A[A>100]
    DTVARS = attr(A,"names") 
    DTFORM = GET_FORMULAE( DTVARS )
    print(DTFORM)
    #rm(DT_MODEL)
}
# ############################################################################################


# ############################################################################################
ENTROPY_FSVARS = DTVARS[1:8]
if ( FALSE )  {
    FORMULA = GET_FORMULAE( attr(A,"names") )
    FORMULA = GET_FORMULAE( setdiff( colnames(ORIG_XTRAIN), c("click", "origin", "ad_id", "device_ip", "adix_ts6", "shift")) ) 
    WHAT = sample(1:nrow(ORIG_XTRAIN), min(5E5, nrow(ORIG_XTRAIN)))
    ENTROPY_FSVARS = cfs( formula(FORMULA), ORIG_XTRAIN[WHAT,])
}
# ############################################################################################


# ############################################################################################
# hclust
# ############################################################################################
ADDITIONAL_INSIGHT = TRUE
if ( nrow(ORIG_XTRAIN) > 1E6 ) ADDITIONAL_INSIGHT = FALSE
if ( ADDITIONAL_INSIGHT ) {
    graphics.off()

    NMAX = min(500000,nrow(ORIG_XTRAIN))
    blck <- {
        BANNER( 'HCLUST INSIGHT' )
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))

        WHICH_COLS = which( colnames(ORIG_XTRAIN) %in% setdiff( colnames(ORIG_XTRAIN), c("origin", "YYMMDDHH", Ctypes)))
        if ( CLT_OPTIONS$use_ctypes_instead_of_htypes )
            WHICH_COLS = which( colnames(ORIG_XTRAIN) %in% setdiff( colnames(ORIG_XTRAIN), c("origin", "YYMMDDHH", Htypes)))

        Y = sapply(WHICH_COLS, function(x) as.numeric(ORIG_XTRAIN[SUBSET,x]))
        colnames(Y) = colnames(ORIG_XTRAIN)[WHICH_COLS]
        Y = scale(Y)
        correlation_between_features = cor(Y, method="spearman", use="pairwise.complete.obs")
        t = complete.cases(correlation_between_features)
        correlation_between_features = correlation_between_features [t,t]
        hc = hclust( dist( abs(correlation_between_features)))

        graphics.off()
        png( 'plot_clt_feature_correlation_all_features_ready0.png', 1200, 800 )
        plot( hc, cex.lab=2.5, cex.main=2.5, cex=2.0, lwd=3, cex.axis=2.5, main="CLUSTER DENDOGRAM WRT CORRELATION BETWEEN FEATURES")
        dev.off()

        # #######################################################################################
        # http://rpubs.com/gaston/dendrograms
        # #######################################################################################
            N = 8
            labelColors = terrain.colors(N)
            labelColors = seq(20,30,2)
            clusMember = cutree(hc, N)
            # function to get color labels
                colLab <- function(n) {
                    if (is.leaf(n)) {
                        a <- attributes(n)
                        labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
                        attr(n, "nodePar") <- c(a$nodePar, lab.font=2, lab.col = labCol)
                    }
                n
            }
            # using dendrapply
        # #######################################################################################
        graphics.off()
        hcd = as.dendrogram(hc)
        clusDendro = dendrapply(hcd, colLab)
        png( 'plot_clt_feature_correlation_all_features_ready1.png', 1200, 800 )
        par(mfrow=c(2,1))
        par(mar=c(10,10,6,5))
        plot( clusDendro, cex.lab=2, cex.axis=2.0, cex.main=2.0, cex=2.0, lwd=4, main="CLUSTER DENDOGRAM WRT CORRELATION BETWEEN FEATURES")
        dev.off()
        rm(Y)
    }
    # ###########################################################################################
}


# ############################################################################################
# PCA
# ############################################################################################
ADDITIONAL_INSIGHT = TRUE
if ( nrow(ORIG_XTRAIN) > 1E6 ) ADDITIONAL_INSIGHT = FALSE
if ( ADDITIONAL_INSIGHT ) {
    BANNER( 'PCA INSIGHT' )
    graphics.off()
    # ###########################################################################################
    NMAX = min(500000,nrow(ORIG_XTRAIN))
    blck <- {

    graphics.off()
    NV = 9

        png( 'plot_clt_pca_features.png', 1600, 1200 )
        par(mfcol=c(2,2))
        par(mar=c(5,6,8,5))
            some_significant_features = setdiff ( FSELECT_XVARS[1:NV], c("H01", "H14", "H15", "H16", "H17", "H18", "H19", "H20" ))
            SUBSET = 1:nrow(ORIG_XTRAIN)
            if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
            WHICH_COLS = which( colnames(ORIG_XTRAIN) %in% some_significant_features )
                Y = as.data.frame(sapply( WHICH_COLS, function(x) as.numeric(ORIG_XTRAIN[SUBSET,x])))
                Y = scale(Y)
                P = DO_PCA(Y, do_scale=FALSE, k=2, nmax=NMAX)
                n = nrow(P$Z)
                xnoise = rnorm(n, 0, 0.000001)
                ynoise = rnorm(n, 0, 0.000001)
                rownames(Y) = SUBSET
                # print ( summary(P))
                clicks = as.numeric(ORIG_XTRAIN$click[SUBSET]) - 1
                s1 = "o" ; s2 = "+" 
                c1 = rgb(0,0,1,0.2) ; c2 = rgb(1,0,0,0.2) 
                dpch = ifelse(clicks==1, s1, s2 ); dcol = ifelse(clicks==1, c1, c2)
                plot( P$Z[,1]+xnoise, P$Z[,2]+ynoise, col=dcol, cex=0.9, pch=dpch, 
                     main=sprintf("2D-PCA [TOP %s CHISQ FEATURES]\n(%.3f VAR RETAINED)", NV, P$cumvar[2]), cex.lab=3.0, cex.main=3.0, cex.axis=3.0)
                legend("topleft", legend=c("noclick", "click"), pch=c(s1,s2), col=c(c1,c2), ncol=2, cex=2.2, lwd=3, text.col=c(c1,c2))
            rm(P,Y,clicks,dpch,dcol)

            if ( FALSE ) {
                some_significant_features = DTVARS
                SUBSET = 1:nrow(ORIG_XTRAIN)
                if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX*10))
                WHICH_COLS = which( colnames(ORIG_XTRAIN) %in% some_significant_features )
                    Y = as.data.frame(sapply( WHICH_COLS, function(x) as.numeric(ORIG_XTRAIN[SUBSET,x])))
                    Y = scale(Y)
                    P = DO_PCA(Y, do_scale=FALSE, k=2, nmax=NMAX)
                    n = nrow(P$Z)
                    rownames(Y) = SUBSET
                    # print ( summary(P))
                    clicks = as.numeric(ORIG_XTRAIN$click[SUBSET]) - 1
                    s1 = "o" ; s2 = "+" ; c1 = rgb(1,0,0,0.2) ; c2 = rgb(0,0,1,0.2) 
                    dpch = ifelse(clicks==1, s1, s2 ); dcol = ifelse(clicks==1, c1, c2)
                    plot( P$Z[,1]+xnoise, P$Z[,2]+ynoise, col=dcol, cex=0.5, pch=dpch, 
                        main=sprintf("2D-PCA [BOOTSTRAP DT IMPORTANT FEATURES]\n(%.3f VAR RETAINED)", P$cumvar[2]), cex.lab=3.0, cex.main=3.0, cex.axis=3.0)
                    legend("topleft", legend=c("noclick", "click"), pch=c(s1,s2), col=c(c1,c2), ncol=2, cex=1.5, lwd=3, text.col=c(c1,c2))
                    rm(P,Y,clicks,dpch,dcol)

                some_significant_features = setdiff ( FSELECT_XVARS[1:12], c("H01", "H14", "H15", "H16", "H17", "H18", "H19", "H20" ))
                SUBSET = 1:nrow(ORIG_XTRAIN)
                if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX*10))
                WHICH_COLS = which( colnames(ORIG_XTRAIN) %in% some_significant_features )
                    Y = as.data.frame(sapply( WHICH_COLS, function(x) as.numeric(ORIG_XTRAIN[SUBSET,x])))
                    Y = scale(Y)
                    P = DO_PCA(Y, do_scale=FALSE, k=2, nmax=NMAX)
                    n = nrow(P$Z)
                    rownames(Y) = SUBSET
                    # print ( summary(P))
                    clicks = as.numeric(ORIG_XTRAIN$click[SUBSET]) - 1
                    dpch = ifelse(clicks==1, "o", "o")
                    dcol = ifelse(clicks==1,  20, 22 )
                    plot( P$Z[,1]+xnoise, P$Z[,2]+ynoise, col=dcol, cex=0.6, pch=dpch, 
                         main=sprintf("2D-PCA [TOP12 CHISQ FEATURES]\n(%.3f VAR RETAINED)", P$cumvar[2]), cex.lab=3.0, cex.main=3.0, cex.axis=3.0)
                    legend("topleft", legend=c("noclick", "click"), pch=c("o", "o"), col=c(20,22), ncol=2, cex=1.5, lwd=3, text.col=dcol)
                    rm(P,Y,clicks,dpch,dcol)

                some_significant_features = ENTROPY_FSVARS
                SUBSET = 1:nrow(ORIG_XTRAIN)
                if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX*10))
                WHICH_COLS = which( colnames(ORIG_XTRAIN) %in% some_significant_features )
                    Y = as.data.frame(sapply( WHICH_COLS, function(x) as.numeric(ORIG_XTRAIN[SUBSET,x])))
                    Y = scale(Y)
                    P = DO_PCA(Y, do_scale=FALSE, k=2, nmax=NMAX)
                    n = nrow(P$Z)
                    rownames(Y) = SUBSET
                    # print ( summary(P))
                    clicks = as.numeric(ORIG_XTRAIN$click[SUBSET]) - 1
                    dpch = ifelse(clicks==1, "o", "o")
                    dcol = ifelse(clicks==1,  20, 22 )
                    plot( P$Z[,1]+xnoise, P$Z[,2]+ynoise, col=dcol, cex=0.6, pch=dpch, 
                         main=sprintf("2D-PCA [ENTROPY/CFS] \n(%.3f VAR RETAINED)", P$cumvar[2]), cex.lab=3.0, cex.main=3.0, cex.axis=3.0)
                    legend("topleft", legend=c("noclick", "click"), pch=c("o", "o"), col=c(20,22), ncol=2, cex=1.5, lwd=3, text.col=dcol)
                    rm(P,Y,clicks,dpch,dcol,ynoise,xnoise)
            } else {
                plot.new()
                plot.new()
            }

        dev.off()
    }
}
    # ###########################################################################################
