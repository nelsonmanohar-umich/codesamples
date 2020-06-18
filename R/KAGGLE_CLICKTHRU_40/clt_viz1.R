p = function(fl,y,ft) { ft[fl,y]/(ft[fl,1]+ft[fl,2]) * (ft[fl,1]+ft[fl,2])/sum(ft[1:nrow(ft),1:2]) }


# ############################################################################################
    source('plot_functions.R')
    library(ggplot2)
    library(GGally)
    library(ggthemes)
    library(MASS)

    # ###########################################################################################
    # http://www.r-bloggers.com/example-8-41-scatterplot-with-marginal-histograms/
    # ###########################################################################################
    scatterhist = function(x, y, xlab="", ylab=""){
        zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
        layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
        xhist = hist(x, breaks=50, plot=FALSE)
        yhist = hist(y, breaks=50, plot=FALSE)
        top = max(c(xhist$counts, yhist$counts))
        par(mar=c(3,3,1,1))
        plot(x,y)
        par(mar=c(0,3,1,1))
        barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
        par(mar=c(3,0,1,1))
        barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
        par(oma=c(3,3,0,0))
        mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
            at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
        mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
            at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
    }
    # ###########################################################################################


    # ###########################################################################################
    # pm <- ggpairs( ORIG_XTRAIN[, c("click", "banner_pos")], upper = list(continuous='blank'), lower = list(discrete = "facethist", combo = "dot"), color = "click", title = "clicks:basic_featureset", axisLabels='none')
    # ###########################################################################################


    # ###########################################################################################
    C0 = c("click","ad_id","YYMMDDHH","banner_pos")
    C1 = c("click", "site_id","site_category","app_id","app_category","site_domain","app_domain")
    C2 = c("click", "device_ip","device_conn_type","device_id","device_type","device_model")
    C3 = c("click", "C01","C14","C15","C16","C17","C18","C19","C20","C21")
    # ###########################################################################################


    graphics.off()
    # ###########################################################################################
    # a simple histogram matrix of the original features
    # http://stackoverflow.com/questions/17416453/force-r-to-plot-histogram-as-probability-relative-frequency
    # ###########################################################################################
    NMAX = 0
    TOPN = 50
    histcolors = gray.colors(TOPN)
    if ( TRUE ) {
        OCOLS = c("click","YYMMDDHH","banner_pos","site_id","site_category","app_id","app_category","site_domain","app_domain",
                  "device_ip","device_conn_type","device_id","device_type","device_model","C01","C14","C15","C16","C17","C18","C19","C20","C21")
        png( 'plot_clt_histogram_original_features.png', 2000, 1260)
        par(mfrow=c(6,4))
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        Y = ORIG_XTRAIN[SUBSET,]
        for ( col in OCOLS) {
            WHICH_COL = which( colnames(Y) %in% col)
            if ( col %in% c("ad_id","C01","C14","C15","C16","C17","C18","C19","C20","C21")) {
                x = as.numeric(Y[,WHICH_COL])
                if ( col %in% c("C20") ) {
                    x[x==-1] = 99999
                }
            } else if ( col %in% c("banner_pos","site_id","site_category","app_id","app_category","site_domain","app_domain","device_ip","device_conn_type","device_id","device_type","device_model")) {
                x = as.integer(Y[,WHICH_COL])
            } else { 
                x = as.integer(Y[,WHICH_COL])
                #hist(x, main=sprintf("Histogram( %s )", col), col=histcolors, breaks=20, cex.lab=2.0, cex.main=3.0, cex=2.0, cex.axis=2.0)
            }
            h <- hist(x, breaks = 50, plot=FALSE)
            h$counts=h$counts/sum(h$counts)
            plot(h, main=sprintf("Histogram( %s )", col), col=histcolors, cex.lab=2.0, cex.main=3.0, cex=2.0, cex.axis=2.0, xlab=sprintf("uniq_id( %s )",col), ylab="Probabilities")
        }
        dev.off()
    }
    # ###########################################################################################


    graphics.off()
    # ###########################################################################################
    # a simple histogram matrix of the selected original features
    # http://stackoverflow.com/questions/17416453/force-r-to-plot-histogram-as-probability-relative-frequency
    # ###########################################################################################
    NMAX = 0
    TOPN = 50
    colors = terrain.colors(TOPN)
    if ( TRUE ) {
        FCOLS = c( "click", "YYMMDDHH", "banner_pos", '', "site_id", "site_category", "app_id", "app_category", "device_id", "device_model", "device_ip", "device_type", "C17", "C18", "C19", "C21")
        png( 'plot_clt_histogram_original_features_selected.png', 2000, 1260)
        par(mfrow=c(4,4))
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        Y = ORIG_XTRAIN[SUBSET,]
        for ( col in FCOLS) {
            if ( col == '' ) {
                plot.new()
                next
            }
            WHICH_COL = which( colnames(Y) %in% col)
            if ( col %in% c("ad_id","C01","C14","C15","C16","C17","C18","C19","C20","C21")) {
                x = as.numeric(Y[,WHICH_COL])
                if ( col %in% c("C20") ) {
                    x[x==-1] = 99999
                }
            } else if ( col %in% c("banner_pos","site_id","site_category","app_id","app_category","site_domain","app_domain","device_ip","device_conn_type","device_id","device_type","device_model")) {
                x = as.integer(Y[,WHICH_COL])
            } else { 
                x = as.integer(Y[,WHICH_COL])
                #hist(x, main=sprintf("Histogram( %s )", col), col=histcolors, breaks=20, cex.lab=2.0, cex.main=3.0, cex=2.0, cex.axis=2.0)
            }
            h <- hist(x, breaks = 50, plot=FALSE)
            h$counts=h$counts/sum(h$counts)
            plot(h, main=sprintf("Histogram( %s )", col), col=histcolors, cex.lab=2.0, cex.main=3.0, cex=2.0, cex.axis=2.0, xlab=sprintf("uniq_id( %s )",col), ylab="Probabilities")
        }
        dev.off()
    }
    # ###########################################################################################


    graphics.off()
    # ###########################################################################################
    # click probabilities over time
    # ###########################################################################################
    png( 'plot_clt_click_probabiities_wrt_time.png', 1600, 400)
    par(mfrow=c(1,4))
    par(mar=c(10,5,4,3))
    clicks = table(Y$YYMMDDHH, Y$click)
    N = nrow(ORIG_XTRAIN)
    tx = rownames(clicks)
    idx = seq(1, length(tx), 12)
    col = "YYMMDDHH"

    clickdensity = clicks[,2]/(clicks[,1]+clicks[,2])
    clickprob = data.frame( "NoClick"=p(1:nrow(clicks),1,clicks), "Click"=p(1:nrow(clicks),2,clicks))
    clicks = data.frame( "NoClick"=1-clickdensity, "Click"=clickdensity)

    plot(clicks[,1], t='l', lwd=3, ylim=c(0,1), main=sprintf("OBSERVED PROB OF CLICKS WITHIN\nEACH HOUR TIME WINDOW IN [%s])", col), col=20, xaxt = "n", cex.lab=2.2, cex.main=2.2, cex=2.2, cex.axis=2.2, ylab="clicks", xlab="")
    legend("topleft", legend = c("noclick", "click"), col = c(20, 22), ncol = 2, cex = 2.0, lwd = 1, text.col = c(20,22))
    points( clicks[,2], t='l', lwd=3, col=22)
    axis(1, at=idx, labels=tx[idx], cex.main=2.0, cex=2.0, cex.axis=2.0, cex.lab=2.0, las=2)

    plot(clickprob[,1], t='l', lwd=3, ylim=c(0, max(clickprob[,1])), main=sprintf("PROB OF CLICK GIVEN\nHOUR IN TIMESPAN [%s]", col), col=20, xaxt = "n", cex.lab=2.2, cex.main=2.2, cex=2.2, cex.axis=2.2, ylab="Click Probability", xlab="")
    points( clickprob[,2], t='l', lwd=3, col=22)
    axis(1, at=idx, labels=tx[idx], cex.main=2.0, cex=2.0, cex.axis=2.0, cex.lab=2.0, las=2)

    DO_HIST(clicks[,2], nbins=20, ptitle="Histogram (PROB(CLICK)\nWITHIN EACH HOUR)", cex.main=2.0, cex.axis=1.8, cex=2.0)
    DO_HIST(clickprob[,2], nbins=20, ptitle="Histogram (PROB(CLICK)\nFOR EACH HOUR AS FACTOR)", cex.main=2.0, cex.axis=1.8, cex=2.0)

    dev.off()
    # ###########################################################################################


  
    graphics.off()
    # ###########################################################################################
    # a http://stackoverflow.com/questions/17416453/force-r-to-plot-histogram-as-probability-relative-frequencysimple histogram matrix of the original features
    # ###########################################################################################
    NMAX = 0
    TOPN = 25
    histcolors = seq(20,60,2)
    if ( TRUE ) {
        OCOLS = c("click","YYMMDDHH","banner_pos","site_id","site_category","app_id","app_category","site_domain","app_domain","device_ip","device_conn_type","device_id","device_type","device_model","C01","C14","C15","C16","C17","C18","C19","C20","C21")
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        Y = ORIG_XTRAIN[SUBSET,]
        png( 'plot_clt_click_tabulation_plots_per_original_feature.png', 2000, 1260)
        par(mfrow=c(6,4))
        for (col in OCOLS) {
            if ( col %in% c("YYMMDDHH")) {
                t = table(Y[,col], Y$click)
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot(t(t), beside=TRUE,  main=sprintf("CLICKS wrt [%s]", col), col=c(20,22), cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8)
                #barplot(t(clicks), beside=FALSE,  main=sprintf("CLICKS wrt [%s]", col), col=c(20,22), cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8)
            } else if ( col %in% c("device_ip","device_id","device_model")) {
                t = GET_TOPN_TABLE_FROM( ORIG_XTRAIN[,col], ORIG_XTRAIN[,PREDICT_COL], TOPN, WRT=c(1,2) )
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot( t(t[1:TOPN,]), las=0, main=sprintf("CLICKS wrt TOP %s [%s]", TOPN, col), col=histcolors, cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8)
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
    # ###########################################################################################
    # a http://stackoverflow.com/questions/17416453/force-r-to-plot-histogram-as-probability-relative-frequencysimple histogram matrix of the original features
    # ###########################################################################################
    NMAX = 0
    TOPN = 50
    histcolors = seq(20,60,2)
    if ( TRUE ) {
        FCOLS = c( "click", "YYMMDDHH", "banner_pos", '', "site_id", "site_category", "app_id", "app_category", "device_id", "device_model", "device_ip", "device_type", "C17", "C18", "C19", "C21")
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        Y = ORIG_XTRAIN[SUBSET,]
        png( 'plot_clt_click_tabulation_plots_per_selected_original_feature.png', 2000, 1260)
        par(mfrow=c(4,4))
        for (col in FCOLS) {
            if ( col == '' ) {
                plot.new()
                next
            }
            if ( col %in% c("YYMMDDHH")) {
                t = table(Y[,col], Y$click)
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot(t(t), beside=TRUE,  main=sprintf("CLICKS wrt [%s]", col), col=c(20,22), cex.lab=2.3, cex.main=2.3, cex=2.3, cex.axis=2.3)
                #barplot(t(clicks), beside=FALSE,  main=sprintf("CLICKS wrt [%s]", col), col=c(20,22), cex.lab=2.3, cex.main=2.3, cex=2.3, cex.axis=2.3)
            } else if ( col %in% c("device_ip","device_id","device_model")) {
                t = GET_TOPN_TABLE_FROM( ORIG_XTRAIN[,col], ORIG_XTRAIN[,PREDICT_COL], TOPN, WRT=c(1,2) )
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot( t(t[1:TOPN,]), las=0, main=sprintf("CLICKS wrt TOP %s [%s]", TOPN, col), col=histcolors, cex.lab=2.3, cex.main=2.3, cex=2.3, cex.axis=2.3)
            } else if ( col %in% c("site_id","app_id","site_domain","app_domain")) {
                t = GET_TOPN_TABLE_FROM( ORIG_XTRAIN[,col], ORIG_XTRAIN[,PREDICT_COL], TOPN, WRT=c(1,2) )
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot( t(t[1:TOPN,]), las=0, main=sprintf("CLICKS wrt TOP %s [%s]", TOPN, col), col=histcolors, cex.lab=2.3, cex.main=2.3, cex=2.3, cex.axis=2.3)
            } else if ( col %in% c("banner_pos","site_category","app_category","device_conn_type","device_type")) {
                t = table(Y[,col], Y$click)
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot( t(t), las=0, main=sprintf("CLICKS wrt [%s]", col), col=histcolors, cex.lab=2.3, cex.main=2.3, cex=2.3, cex.axis=2.3)
            } else if ( col %in% c("C14","C17", "C19", "C20")) {
                t = table(Y[,col], Y$click)
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                plot(t[,1], t='l', lwd=2, main=sprintf("CLICKS wrt [%s]", col), col=20, cex.lab=2.3, cex.main=2.3, cex=2.3, cex.axis=2.3, ylab="clicks", xlab="")
                points( t[,2], t='l', lwd=2, col=22)
                #axis(1, at=idx, labels=tx[idx], las=2)
            } else {
                t = table( as.integer(Y[,col]), Y$click)
                t = data.frame( "NoClick"=p(1:nrow(t),1,t), "Click"=p(1:nrow(t),2,t))
                barplot( t(t), las=0, main=sprintf("CLICKS wrt [%s]", col), col=histcolors, cex.lab=2.3, cex.main=2.3, cex=2.3, cex.axis=2.3)
            }
        }
        dev.off()
    }
    # ###########################################################################################

    graphics.off()
    NMAX = min(250000, nrow(ORIG_XTRAIN))
    # ###########################################################################################
    # pca a correlation matrix, cluster plot of the original features 
    # ###########################################################################################
    if ( TRUE ) {
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        OCOLS = c("click","YYMMDDHH","banner_pos","site_id","site_category","app_id","app_category","site_domain","app_domain",
                  "device_ip","device_conn_type","device_id","device_type","device_model","C01","C14","C15","C16","C17","C18","C19","C20","C21")
        WHICH_COLS = which( colnames(ORIG_XTRAIN) %in% OCOLS )
        Y = sapply(WHICH_COLS, function(x) as.numeric(ORIG_XTRAIN[SUBSET,x]))
        colnames(Y) = colnames(ORIG_XTRAIN)[WHICH_COLS]
        Y = scale(Y)
        # correlation_between_features = cor(Y, method="spearman", use="pairwise.complete.obs")
        correlation_between_features = cor(Y) #, method="spearman", use="pairwise.complete.obs")
        t = complete.cases(correlation_between_features)
        correlation_between_features = correlation_between_features [t,t]
        png( 'plot_clt_feature_correlation_original_features.png', 1200, 800 )
        plot( hclust( dist( abs(correlation_between_features))), cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8, main="CLUSTER DENDOGRAM WRT CORRELATION BETWEEN ORIGINAL FEATURES")
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
        OCOLS = minus( c("click","YYMMDDHH","banner_pos","site_id","site_category","app_id","app_category","site_domain","app_domain",
                         "device_ip","device_conn_type","device_type","device_model","C01","C14","C15","C16","C17","C18","C19","C20","C21"), 
                       c("device_ip"))
        BANNER( 'DECISION TREE INSIGHT' )
        FORMULA = 'click ~ .'
        SUBSET = 1:nrow(ORIG_XTRAIN)
        if ( NMAX > 0 ) SUBSET = sample( 1:nrow(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),NMAX))
        Y = ORIG_XTRAIN[SUBSET,OCOLS]
        tree_controls = rpart.control(xval=10, minbucket=29, maxdepth=5, cp=1E-5, split="information")
        sink('temp')
        DT_MODEL = FIT_DECISION_TREE( model.frame(FORMULA, Y), SLICE_DATAFRAME(Y,PREDICT_COL), FORMULA=FORMULA, DO_PRUNING=TRUE, control=tree_controls )
        sink()
        DT_MODEL = prune( DT_MODEL, cp=1E-3)
        DT_PROBS = predict(DT_MODEL, ORIG_XTRAIN[SUBSET,])
        DT_CLASS = WHICH_CLASS_KERNEL(DT_PROBS, 0.5)
        A = DT_MODEL$variable.importance
        print( A )
        cat(HEADER)
        print( table( Y_TRUE[SUBSET], DT_CLASS ) )
        cat(HEADER)
	    print( COMPUTE_LOG_LOSS( Y_TRUE[SUBSET], DT_PROBS[,2] ))
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
        # correlation_between_features = cor(Y, method="spearman", use="pairwise.complete.obs")
        correlation_between_features = cor(Y, method="spearman", use="pairwise.complete.obs")
        t = complete.cases(correlation_between_features)
        correlation_between_features = correlation_between_features [t,t]
        png( 'plot_clt_feature_correlation_dtselected_features.png', 1200, 800 )
        plot( hclust( dist( abs(correlation_between_features))), cex.lab=1.8, cex.main=1.8, cex=1.8, cex.axis=1.8, main="CLUSTER DENDOGRAM WRT CORRELATION BETWEEN ORIGINAL FEATURES")
        dev.off()
        rm(Y)
    }
    # ###########################################################################################

# ############################################################################################
if ( FALSE ) {
    BANNER('LDA - ORIGINAL')
    NMAX = min(250000, nrow(ORIG_XTRAIN))
    X = as.data.frame(sapply( 1:ncol(ORIG_XTRAIN), function(x) as.numeric(ORIG_XTRAIN[,x])))
    colnames(X) = colnames(ORIG_XTRAIN)
    WHICH_COLS = setdiff(colnames(ORIG_XTRAIN), c("click", "origin", "ad_id"))
    SUBSET = sample( 1:nrow(ORIG_XTRAIN), NMAX )
    xform = formula(GET_FORMULAE(WHICH_COLS))

    lfit = lda ( xform, data=X[SUBSET,] , CV=TRUE, na.action="na.omit")
    t = as.numeric(lfit$posterior[,2])
    COMPUTE_LOG_LOSS( Y_TRUE[SUBSET], t)
    print(table( Y_TRUE[SUBSET], lfit$class))
    COMPUTE_LOG_LOSS(Y_TRUE[SUBSET], t)

    png('clt_lda_original_features.png', 1200, 800)
    lfit = lda ( xform, data=X[SUBSET,])# , CV=TRUE)
    plot(lfit, xlim=c(-10, 10), ylim=c(0,1))
    dev.off()
    rm(X, lfit)
}
# ############################################################################################






