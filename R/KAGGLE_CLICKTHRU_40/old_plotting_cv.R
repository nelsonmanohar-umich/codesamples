            model_names = CLT_OPTIONS$predictors
            model_names = c( sapply( 1:length(model_names), function(x) ifelse( model_names[x] =='', paste('M',x,sep=''), sprintf("I(%s)", model_names[x]))), "EMSEMBLE")

            # ####################################################################################
            # http://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
            # ####################################################################################
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
                legend('topleft', c("EMSEMBLE", mnames[1:(n-1)]), fill = cols[1:12], bty = 'n', border = NA, cex=2.0)
            }
            # ####################################################################################
            graphics.off()
            t = PROB_METRICS_MATRIX
            png( 'clt_cv_crossval_fold_performance_bias_variance.png', 1280, 800)
            par( mfrow=c(2,1))
                models = c(1:5, 12)
                plot_density_histograms( t, model_names[models], models, title = 'Distributions of ( Log-Loss( CV Fold[i] ) ) Values (General Models)' )
                models = c(6:9, 12)
                plot_density_histograms( t, model_names[models], models, title = 'Distributions of ( Log-Loss( CV Fold[i] ) ) Values (Individualized Models)' )
            dev.off()
            # ####################################################################################

            # ############################################################################################
            graphics.off()
            png( 'clt_cv_crossval_fold_performance_timeplot.png', 2000, 1200 )
            # PMM = 12x540
            t = t(PROB_METRICS_MATRIX[,1:iter])
            par(mfrow=c(2,1))
            par(mar=c(6,6,3,3))
            colors = seq(22,40,2) #terrain.colors(12)

            models = c(12,1:5)
            plot( lowess(t[,12], f=1/10), t='b', pch='o', col=20, lwd=4, xlab="FOLD NUMBER", ylim=c(0.35,0.45), main="LOWESS[ LOG-LOSS( CV FOLD[i] ) ]", ylab="LOG LOSS FOR FOLD", cex=1.2, cex.axis=2.0, cex.lab=2.0, cex.main=2.0 )
            fval=1/8
            for( i in 2:(length(models)-1) ) { l=models[i]; points( lowess(t[,l], f=fval), t='b', pch=20+i, col=colors[l], cex=0.8, lwd=1+l/5 ) }
            legend("bottomright", legend = model_names[models], col = c(20,colors[2:length(models)]), pch=c(20:25), ncol = 3, cex = 3.0, lwd = 3)
            axis(1, at=idx, labels=tx[idx], cex.main=2.0, cex=2.0, cex.axis=2.0, cex.lab=2.0, las=2)

            models = c(12,6:9)
            plot( lowess(t[,12], f=1/10), t='b', pch='o', col=20, lwd=4, xlab="FOLD NUMBER", ylim=c(0.35,0.45), main="LOWESS[ LOG-LOSS( CV FOLD[i] ) ]", ylab="LOG LOSS FOR FOLD", cex=1.2, cex.axis=2.0, cex.lab=2.0, cex.main=2.0 )
            fval=1/8
            n = length(models)
            for( i in 2:n ) { l=models[i]; points( lowess(t[,l], f=fval), t='b', pch=20+i, col=colors[l], cex=0.8, lwd=1+l/5 ) }
            legend("bottomright", legend = model_names[models], col = c(20,colors[2:length(models)]), pch=c(20:25), ncol = 3, cex = 3.0, lwd = 3)
            dev.off()
            # ############################################################################################

            # ############################################################################################
            graphics.off()
            png( 'clt_cv_crossval_fold_performance_full_timeplot.png', 2000, 1200 )
            t = t(PROB_METRICS_MATRIX[,1:iter])
            par(mar=c(6,7,5,4))
            N = 15
            colors = c(20, 21, 22, 24, 26, 27, 29, 30, 31, 32, 34, 35, 36, 40, 43) #terrain.colors(N)
            models = c(12,1:9)
            plot( lowess(t[,12], f=1/10), t='b', pch=20, col=20, lwd=4, xlab="FOLD NUMBER", ylim=c(0.35,0.45), main="LOWESS[ LOG-LOSS( CV FOLD[i] ) ]", ylab="LOG LOSS FOR FOLD", cex=1.2, cex.axis=2.0, cex.lab=2.0, cex.main=2.0 )
            fval=1/8
            n = length(models)
            for( i in 2:n ) { l=models[i]; points( lowess(t[,l], f=fval), t='b', pch=10+i, col=colors[l+1], cex=0.8, lwd=1+l/5 ) }
            legend("bottomright", legend = c("EMSEMBLE",model_names[1:(length(models)-1)]), col = c(20,colors[models]), pch=c(10:25), ncol = 5, cex = 3.0, lwd = 3)
            axis(1, at=idx, labels=tx[idx], cex.main=2.0, cex=2.0, cex.axis=2.0, cex.lab=2.0, las=2)
            dev.off()
            # ############################################################################################

            # ############################################################################################
            graphics.off()
            png( 'clt_cv_crossval_fold_and_model_correlation.png', 1200, 800 )
            t = t(PROB_METRICS_MATRIX[,1:iter])
            models = c(1:9, 12)
            heatmap( t[,models], scale="none", col=terrain.colors(5), labCol=model_names, main="Heatmap: Per-Model Log-Loss Performance Across CV Folds (Time Order Not Preserved)" ) 
            dev.off()
            # ############################################################################################
        
            # ############################################################################################
            graphics.off()
            png( 'clt_cv_crossval_fold_model_correlation_range.png', 640, 480 )
            par(mar=c(3,10,3,3))
            t = t(PROB_METRICS_MATRIX[,1:iter])
            models = c(1:9, 12)
            boxplot( cor(t[,models]), col=terrain.colors(15), horizontal=T, names=model_names, main="Boxplot of Correlation of Log-Loss Performance Across Folds Per-Model", las=2 ) 
            dev.off()
            # ############################################################################################

            # ############################################################################################
            graphics.off()
            png( 'clt_cv_crossval_fold_model_range.png', 640, 480 )
            par(mar=c(3,10,3,3))
            t = t(PROB_METRICS_MATRIX[,1:iter])
            models = c(1:9, 12)
            boxplot( t[,models], col=terrain.colors(15), horizontal=T, ylim=c(0.25,0.55), names=model_names, main="Boxplot of Log-Loss Performance Across Folds Per-Model", las=2 ) 
            dev.off()
            # ############################################################################################
