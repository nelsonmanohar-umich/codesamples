# ###########################################################################################
# METRICS
# ###########################################################################################
COMPUTE_CV_METRICS = function( iter ) {
    model_names = CLT_OPTIONS$predictors
    model_names = c( sapply( 1:length(model_names), function(x) ifelse( model_names[x] =='', paste('M',x,sep=''), sprintf("I(%s)", model_names[x]))), "ENSEMBLE")


    # ####################################################################################
    # KERNEL ESTIMATED PROBABILITY DENSITY FUNCTION FOR LOG LOSS 
    # ####################################################################################
    graphics.off()
    t = PROB_METRICS_MATRIX
    png( 'clt_cv_crossval_fold_performance_bias_variance.png', 1280, 800)
    par( mfrow=c(2,1))
        pivot = ifelse( NUM_TRUE_MODELS>5, 4, NUM_TRUE_MODELS )
        models = c(1:pivot,     12)
        plot_density_histograms( t, model_names[models], models, title = 'Distributions of ( Log-Loss( CV Fold[i] ) ) Values (General Models)' )
        models = c((pivot+1):9, 12)
        plot_density_histograms( t, model_names[models], models, title = 'Distributions of ( Log-Loss( CV Fold[i] ) ) Values (Individualized Models)' )
    dev.off()
    # ####################################################################################


    # ############################################################################################
    # TIMEPLOT IN TWO PART
    # ############################################################################################
    graphics.off()
    png( 'clt_cv_crossval_fold_performance_timeplot.png', 2000, 1200 )
        # PMM = 12x540
        t = t(PROB_METRICS_MATRIX[,1:iter])
        par(mfrow=c(2,1))
        par(mar=c(6,6,3,3))
        colors = c(20,        terrain.colors(10))
        names  = c("ENSEMBLE",c(model_names[1:9]))
        v_pch  = c(10,        c(14:23))
        # ########################################################################################
        models = c(12,          1:pivot)
        n      = length(models)
        colors = c(20,        terrain.colors(pivot+1))
        e_idx  = 1;           m_idx  = 2:n
        lg_txt = model_names[ c(10, models[2:n])]
        plot( lowess(t[,12], f=1/10), t='b', pch=v_pch[e_idx], col=colors[e_idx], lwd=4, ylim=c(0.35,0.45), 
                                            main="LOWESS[ LOG-LOSS( CV FOLD[i] ) ]", xlab="FOLD NUMBER", ylab="LOG LOSS FOR FOLD", 
                                            cex=1.2, cex.axis=2.0, cex.lab=2.0, cex.main=2.0 )
        grid (NULL,NULL, lwd=2, lty = 6, col = "cornsilk2")
        for( i in m_idx ) { l=models[i]; points( lowess(t[,l], f=1/10), t='b', pch=v_pch[i], col=colors[i], cex=0.8, lwd=1+l/5 ) }
        legend("bottomright", legend = lg_txt, col    = colors[1:n], pch    = v_pch [1:n], ncol   = as.integer(6), cex = 3.0, lwd = 3)
        # ########################################################################################
        models = c(12,          (pivot+1):9)
        colors = c(20,        terrain.colors(9-pivot+1))
        n = length(models)
        e_idx  = 1;           m_idx  = 2:n
        lg_txt = model_names[ c(10, models[2:n])]
        plot( lowess(t[,12], f=1/10), t='b', pch=v_pch[e_idx], col=colors[e_idx], lwd=4, ylim=c(0.35,0.45), 
                                                main="LOWESS[ LOG-LOSS( CV FOLD[i] ) ]", xlab="FOLD NUMBER", ylab="LOG LOSS FOR FOLD", 
                                                cex=1.2, cex.axis=2.0, cex.lab=2.0, cex.main=2.0 )
        grid (NULL,NULL, lwd=2, lty = 6, col = "cornsilk2")
        for( i in m_idx ) { l=models[i]; points( lowess(t[,l], f=1/10), t='b', pch=v_pch[i], col=colors[i], cex=0.8, lwd=1+l/5 ) }
        legend("bottomright", legend = lg_txt, col    = colors[1:n], pch    = v_pch [1:n], ncol   = as.integer(6), cex = 3.0, lwd = 3)
    dev.off()
    # ############################################################################################


    # ############################################################################################
    # TIMEPLOT
    # ############################################################################################
    graphics.off()
    png( 'clt_cv_crossval_fold_performance_full_timeplot.png', 2000, 1200 )
        t = t(PROB_METRICS_MATRIX[,1:iter])
        par(mar=c(10,7,5,4))
        N = 15
        names  = c("ENSEMBLE",c(model_names[1:9]))
        colors = c(20,        terrain.colors(10))
        models = c(12,        c(1:9))
        v_pch  = c(10,        c(14:23))
        n      = 1          + NUM_TRUE_MODELS                            # n = length(models)
        e_idx  = 1;           m_idx  = 2:n
        lg_txt = model_names[ c(10, models[2:n])]
    
        plot( lowess(t[,12], f=1/10), t='b', pch=v_pch[e_idx], col=colors[e_idx], lwd=4, ylim=c(0.35,0.45), xaxt="n", 
                                                main="LOWESS[ LOG-LOSS( CV FOLD[i] ) ]", xlab="", ylab="LOG LOSS FOR FOLD", 
                                                cex=2.0, cex.axis=3.0, cex.lab=3.0, cex.main=3.0 )
        grid (NULL,NULL, lwd=2, lty = 6, col = "cornsilk2")
        for( i in m_idx ) { l=i-1; points( lowess(t[,l], f=1/10), t='b', pch=v_pch[i], col=colors[i], cex=1.0, lwd=1+l/5 ) }
        legend("bottomright", legend = lg_txt, col    = colors[1:n], pch    = v_pch [1:n], ncol   = as.integer(6), cex = 3.0, lwd = 3)
        idx = seq(1, 540, 20)
        LABELS = unlist(sapply( cv_dates, function(x) sprintf( "\n%s", x ) ))
        axis(1, at=idx, labels=LABELS[idx], cex.main=2.0, cex=2.0, cex.axis=2.0, cex.lab=2.0, las=2)
    dev.off()
    # ############################################################################################


    # ############################################################################################
    # CORRELATION BETWEEN MODELS VISUALIZATION (INDIV 540x10) LOG LOSS AS HEATMAP)
    # ############################################################################################
    graphics.off()
    png( 'clt_cv_crossval_fold_and_model_correlation.png', 1200, 800 )
        t = t(PROB_METRICS_MATRIX[,1:iter])
        models = c(1:NUM_TRUE_MODELS, 12)
        heatmap( t[,models], scale="none", col=terrain.colors(5), labCol=model_names, main="Heatmap: Per-Model Log-Loss Performance Across CV Folds (Time Order Not Preserved)" ) 
    dev.off()
    # ############################################################################################


    # ############################################################################################
    # LOG LOSS CORRELATION
    # ############################################################################################
    graphics.off()
    png( 'clt_cv_crossval_fold_model_correlation_range.png', 640, 480 )
    par(mar=c(5,14,5,2))
        t = t(PROB_METRICS_MATRIX[,1:iter])
        models = c(12, 1:NUM_TRUE_MODELS)
        names  = c("ENSEMBLE",c(model_names[1:NUM_TRUE_MODELS]))
        boxplot( cor(t[,models]), cex=2.0, cex.axis=2.0, cex.main=2.0, cex.lab=2, col=terrain.colors(NUM_TRUE_MODELS+5), 
                horizontal=T, names=names, main="Boxplot of Correlation of Log-Loss Performance\nAcross Folds Per-Model", las=2 ) 
    dev.off()
    # ############################################################################################


    # ############################################################################################
    # LOG LOSS RANGE 
    # ############################################################################################
    graphics.off()
    png( 'clt_cv_crossval_fold_model_range.png', 640, 480 )
    par(mar=c(5,14,5,2))
        t = t(PROB_METRICS_MATRIX[,1:iter])
        models = c(12, 1:NUM_TRUE_MODELS)
        names  = c("ENSEMBLE",c(model_names[1:NUM_TRUE_MODELS]))
        boxplot( t[,models],     cex=2.0, cex.axis=2.0, cex.main=2.0, cex.lab=2, col=terrain.colors(NUM_TRUE_MODELS+5), ylim=c(0.25,0.55), 
                horizontal=T, names=names, main="Boxplot of Log-Loss Performance\nAcross Folds Per-Model", las=2 ) 
    dev.off()
    # ############################################################################################


    # ###########################################################################################
    # CORRECT METRICS FROM DIRECTLY THE IMPLEMENTATION KERNEL 
    # ###########################################################################################
    graphics.off()
    png( 'clt_basic_fold_metrics.png', 640, 480 )
    par(mar=c(10,6,4,4))
        models = c(1:NUM_TRUE_MODELS,10)
        names  = c(c(model_names[1:NUM_TRUE_MODELS], "ENSEMB"))
        t = PROB_METRICS_MATRIX[c(1:NUM_TRUE_MODELS,12), 1:iter]
        t = t( apply( t, 1, function(x) c(mean(x), min(x), max(x), median(x), 3*sd(x) )))
        colnames(t) = c("AVG_LOGLOSS","MIN_LOGLOSS","MAX_LOGLOSS","MED_LOGLOSS","3*SD_LOGLOSS")
        rownames(t) = names
        # barplot( t(t),     cex=2.0, cex.axis=2.0, cex.main=2.0, cex.lab=2, col=c(grey.colors(3),'white','grey'), beside=F, horiz=F, main="Summary Log-Loss Performance\nAveraged Across Folds Per-Model", las=2 ) 
        colors = c('black', terrain.colors(3), 'red') # c(grey.colors(3),'white','grey')
        plot( t[,1],   type='b', lwd=3, pch=20, ylim=c(0.10,0.6), xlab="", ylab="Log Loss", xaxt='n', cex=2.0, cex.main=2.0, cex.lab=2.0, cex.axis=2.0, col=colors[1])
        points( t[,2], type='b', lwd=3, pch=21, col=colors[2])
        points( t[,3], type='b', lwd=3, pch=23, col=colors[3])
        points( t[,4], type='b', lwd=3, pch=24, col=colors[4])
        points( t[,5], type='b', lwd=3, pch=25, col=colors[5])
        idx = 1:length(models)
        axis(1, at=idx, labels=names, cex.main=2.0, cex=2.0, cex.axis=2.0, cex.lab=2.0, las=2)
        legend('topright', colnames(t), fill = colors, bty = 'n', border = NA, cex=1.0)
        BANNER(sprintf( "AT ITER %s, CROSS VALIDATION FOLD METRICS", iter) )
        ops = options(digits=5)
        print( t )
        cat(HEADER)
        print( summary( t ) )
        colnames(t) = c("AVGLL","MINLL","MAXLL","MEDLL","3SDLL")
        rownames(t) = unlist(sapply( names, function(x) sprintf( "%6s", x )))
        t = round(t,6)
        write.table( t, file="clt_cvfolds_metrics.csv", sep="\t" )
        cat(HEADER)
        options(ops)
    dev.off()
    # ###########################################################################################
}
# ###########################################################################################


# ###############################################################################
# http://www.r-bloggers.com/example-8-41-scatterplot-with-marginal-histograms/
# ###############################################################################
scatterhist = function(x, y, xlab="", ylab="", ...){
    zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
    layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
    xhist = hist(x, breaks=50, plot=FALSE)
    yhist = hist(y, breaks=50, plot=FALSE)
    top = max(c(xhist$counts, yhist$counts))
         par(mar=c(5,5,1,1))
    plot(x,y, xlab=xlab, ylab=ylab, ...)
    par(mar=c(0,5,1,1))
    barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0, ...)
    par(mar=c(5,0,1,1))
    barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE, ...)
    par(oma=c(5,5,0,0))
    mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
    mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}
# ###############################################################################
 

# ###############################################################################
# http://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
# ###############################################################################
plot_density_histograms = function( t, mnames, colnums, title ) {
    XD = sapply( colnums, function(x) density(t[x,]) )
    idx = 1:length(colnums)
    xlim <- c(0.30, 0.55) # range(XD[,1]$x, XD[,2]$x)
    ylim <- range(0,XD[,1]$y, XD[,2]$y)
    cols = c( rgb(1,1,0,0.2), rgb(0,0,1,0.2), rgb(0,1,0,0.2),
              rgb(1,0,0,0.2), rgb(0,0,1,0.2), rgb(0,1,0,0.2),
              rgb(1,0,0,0.2), rgb(0,0,1,0.2), rgb(0,1,0,0.2),
              rgb(1,0,0,0.2), rgb(0,0,1,0.2), rgb(0,1,0,0.2))
    n = length(idx)
    EMS = idx[n]
    plot(XD[,EMS]$x, XD[,EMS]$y, xlim = xlim, ylim = ylim, xlab = 'Log-Loss ( CV Fold[i] )', ylab="Est. Prob. Density", col=cols[1], cex=0.4, cex.main=2.0, cex.lab=1.5, cex.axis=1.5, main = title, panel.first = grid())
    polygon( XD[,EMS]$x, XD[,EMS]$y, density = -1, col = cols[1] )
    for ( i in idx[1:(n-1)] ) { polygon( XD[,i]$x, XD[,i]$y, density = -1, col = cols[i+1] ) }
    legend('topleft', c("ENSEMBLE", mnames[1:(n-1)]), fill = cols[1:12], bty = 'n', border = NA, cex=2.0)
}
# ####################################################################################




