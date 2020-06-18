  # ############################################################################################
    # TIMEPLOT
    # ############################################################################################

    #     JCOST      F_RATIO           FPR              FNR          ACCURACY         BER            MSE         PRECISION        RECALL           TPR             TNR          LOG_LOSS   
    #      1            2               3                4             5               6              7              8              9              10              11              12
    # Min.   :0   Min.   :0.030   Min.   :0.0000   Min.   :0.81   Min.   :0.78   Min.   :0.50   Min.   :1.26   Min.   :0.27   Min.   :0.016   Min.   :0.016   Min.   :0.97   Min.   :0.28  
    # 1st Qu.:0   1st Qu.:0.105   1st Qu.:0.0092   1st Qu.:0.90   1st Qu.:0.82   1st Qu.:0.52   1st Qu.:1.41   1st Qu.:0.50   1st Qu.:0.059   1st Qu.:0.059   1st Qu.:0.98   1st Qu.:0.38  
    # Median :0   Median :0.136   Median :0.0122   Median :0.92   Median :0.83   Median :0.53   Median :1.46   Median :0.57   Median :0.077   Median :0.077   Median :0.99   Median :0.41  
    # Mean   :0   Mean   :0.137   Mean   :0.0125   Mean   :0.92   Mean   :0.83   Mean   :0.53   Mean   :1.46   Mean   :0.56   Mean   :0.079   Mean   :0.079   Mean   :0.99   Mean   :0.41  
    # 3rd Qu.:0   3rd Qu.:0.166   3rd Qu.:0.0156   3rd Qu.:0.94   3rd Qu.:0.85   3rd Qu.:0.54   3rd Qu.:1.50   3rd Qu.:0.64   3rd Qu.:0.097   3rd Qu.:0.097   3rd Qu.:0.99   3rd Qu.:0.43  
    # Max.   :0   Max.   :0.308   Max.   :0.0301   Max.   :0.98   Max.   :0.90   Max.   :0.59   Max.   :1.65   Max.   :1.00   Max.   :0.193   Max.   :0.193   Max.   :1.00   Max.   :0.51  

    graphics.off()
    png( 'metrics_t.png', 2000, 1200 )
        t = METRICS
        par(mar=c(10,7,5,4))
        F1=2;FPR=3;FNR=4;ACC=5;BER=6;MSNE=7;P=8;R=9;TPR=10;TNR=11;LL=12
        names  = c("LOGLOSS",c("F1","FPR","FNR","ACC","BER", "P","R","TNR","TPR"))
        models = c(LL,       c( F1 , FPR , FNR , ACC , BER, P , R , TNR , TPR ))

        names  = c("LOGLOSS",c("F1_SCORE", "PRECISION", "ACCURACY", "RECALL"))
        models = c(LL,       c( F1,   P,   ACC, R ))

        v_pch  = models
        n      = length(models)
        colors = models
        e_idx  = 1
        m_idx  = 2:n
    
        fval=1/10
        fval=24/540
        fval=1/100
        ylim = c(-0.1,1)
        plot( lowess(t[,LL], f=fval), t='b', pch=v_pch[e_idx], col=colors[e_idx], lwd=4, 
                                                ylim=ylim, 
                                                xaxt="n", 
                                                main="LOWESS[ LOG-LOSS( CV FOLD[i] ) ] WRT LOWESS(OTHER MEASUREMENTS)", xlab="", ylab="LOG LOSS FOR FOLD", 
                                                cex=2.0, cex.axis=3.0, cex.lab=3.0, cex.main=3.0 )

        grid (NULL,NULL, lwd=2, lty = 6, col = "cornsilk2")

        for ( i in 2:n ) {
            points( lowess(t[,models[i]], f=fval), t='b', pch=v_pch[i], col=colors[i], cex=1.3, lwd=1.5 )
        }

        legend("bottomright", legend = names,
                              col    = colors, 
                              pch    = v_pch , 
                              ncol   = as.integer(5), 
                              cex = 2.5, lwd = 3)
        idx = seq(1, 540, 20)
        LABELS = unlist(sapply( cv_dates, function(x) sprintf( "\n%s", x ) ))
        axis(1, at=idx, labels=LABELS[idx], cex.main=2.0, cex=2.0, cex.axis=2.0, cex.lab=2.0, las=2)
    dev.off()
    # ####


