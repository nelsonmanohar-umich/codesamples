    p = function(fl,y,ft) { ft[fl,y]/(ft[fl,1]+ft[fl,2]) * (ft[fl,1]+ft[fl,2])/sum(ft[1:nrow(ft),1:2]) }

# 
    # ###################################################################################
    # GLOBAL
    # ###################################################################################
    N = nrow(ORIG_XTRAIN)
    # ###################################################################################

    # ###################################################################################
    GET_METRICS_TUPLE = function(clicks) {
        # y represents the yvar split according to levels of the xvars
        C10 = sum(as.integer(clicks)>0)
        C1 = sum(as.integer(clicks)-1) 
        factordensity = C10/N
        clickprob = C1/N
        noclickprob = 1-clickprob
        return (c( clickprob, noclickprob, factordensity ))
    }
    # ###################################################################################

    # ###################################################################################
    GET_BARPLOT_FOR_MODEL = function(t,k,TOPN=10,levelnames=c()) { 
        TOPN = min(TOPN, nrow(t))
        P = as.data.frame(MATRIX(nrow(t), ncol(t)+k))
        P[,1:k] = t[,1:k]
        P[,(k+1):(k+3)] = t[,(k+1)]
        factornames = colnames(t)[1:k]
        colnames(P) = c(factornames, "clickprob", "noclickprob", "factorlevelprior")
        ordering = order(P$clickprob, decreasing=T)[1:TOPN]
        P = P[ordering,]
        if (length(levelnames)==0) { 
            if ( k == 1 ) labels = paste( as.character(P[,1]))
            if ( k == 2 ) labels = paste( as.character(P[,1]), as.character(P[,2]))
            if ( k > 2 )  labels = paste( as.character(P[,1]), as.character(P[,2], "..."))
        } else {
            labels = levelnames[ordering]
        }

        par(mar=c(4,20,4,2)) # increase y-axis margin.
        barplot( t(P[,(k+1):(k+3)]), 
                beside=FALSE,  horiz=TRUE, names=labels, 
                main=sprintf("ClickProb, NoClickProb, FactorLevelPrior\nwrt TOP %s LEVELS FOR %s", TOPN, CONCAT(sprintf(factornames, "+"))),
                xlab="",
                col=c(20,22,21), 
                cex.lab=2.0, cex.main=2.0, cex=2.0, cex.axis=2.0, las=1, xlim=c(0,2))
        text(rep(0.50,TOPN),y=1:TOPN + -1:(TOPN-2)/TOPN, labels=sprintf("%.2f", P[,k+2]), cex=2.0)
        text(rep(1.08,TOPN),y=1:TOPN + -1:(TOPN-2)/TOPN, labels=sprintf("%.2f", P[,k+3]), cex=2.0)
        legend("topright", legend=c("click", "noclick", "factorLevelPrior"), pch=c("o", "o", "o"), col=c(20,22,21), ncol=1, cex=1.5, lwd=1) #, text.col=c(20,22,21))
        cat(HEADER)
        print(P)
        return (P)
    }
    # ###################################################################################


    # ###################################################################################
    graphics.off()
    png("clt_factor_selected_transformed_userside_features.png", 1200, 800 )
    par(mfcol=c(4,2))
    for( col in c("banner_pos", "device_conn_type", "device_type", "device_model")) {
            form = as.formula( sprintf("click ~ %s", col))
            t = aggregate (form, ORIG_XTRAIN, function(x) GET_METRICS_TUPLE(x))
            GET_BARPLOT_FOR_MODEL(t,1,TOPN=5)
        }
    dev.off()
    # ###################################################################################

    # ###################################################################################
    graphics.off()
    png("clt_factor_selected_transformed_temporal_features.png", 1200, 800 )
    par(mfcol=c(3,2))
        t = aggregate (click ~  day, ORIG_XTRAIN, function(x) GET_METRICS_TUPLE(x))
        GET_BARPLOT_FOR_MODEL(t,1,7,
            # levelnames=c('Tue','Wed','Thu','Fri','Sat','Sun','Mon'))
              levelnames=c('Fri','Sat','Sun','Mon','Tue','Wed','Thu'))
        t = aggregate (click ~  hour, ORIG_XTRAIN, function(x) GET_METRICS_TUPLE(x))
        GET_BARPLOT_FOR_MODEL(t,1,7,
            levelnames=c("12AM","01AM","02AM","03AM","04AM","05AM","06AM","07AM","08AM","09AM","10AM","11AM","12PM","01PM","02PM","03PM","04PM","05PM","06PM","07PM","08PM","09PM","10PM","11PM"))
        t = aggregate (click ~  shift, ORIG_XTRAIN, function(x) GET_METRICS_TUPLE(x))
        GET_BARPLOT_FOR_MODEL(t,1,7, levelnames=c(t(sapply( c("12AM_06AM", "06AM_12PM", "12PM_06PM", "06PM_12AM"), function(x) paste( 
                                                                                                            #c('Tue','Wed','Thu','Fri','Sat','Sun','Mon'),
                                                                                                             c('Fri','Sat','Sun','Mon','Tue','Wed','Thu'), "on", x)))))
    dev.off()
    # ###################################################################################

    # ###################################################################################
    graphics.off()
    png("clt_factor_selected_transformed_numerical_features.png", 1200, 800 )
    par(mfcol=c(3,2))
        # for( col in c("H01", "H14", "H15", "H16", "H17", "H18", "H19", "H20", "H21")) {
        for( col in c("H17", "H19", "H21")) {
            form = as.formula( sprintf("click ~ %s", col))
            t = aggregate (form, ORIG_XTRAIN, function(x) GET_METRICS_TUPLE(x))
            GET_BARPLOT_FOR_MODEL(t,1,TOPN=5)
        }
    dev.off()
    # ###################################################################################

    # ###################################################################################
    graphics.off()
    png("clt_factor_selected_transformed_appsite_features.png", 1200, 800 )
    par(mfrow=c(3,2))
    for( col in c("site_id", "app_id", "site_domain", "app_domain", "site_category", "app_category")) {
            form = as.formula( sprintf("click ~ %s", col))
            t = aggregate (form, ORIG_XTRAIN, function(x) GET_METRICS_TUPLE(x))
            GET_BARPLOT_FOR_MODEL(t,1,TOPN=5)
        }
    dev.off()
    # ###################################################################################

    # ###################################################################################
    graphics.off()
    png("clt_factor_selected_interaction_transformed_features.png", 1200, 800 )
    par(mfcol=c(3,2))
        t = aggregate (click ~  site_category + app_category, ORIG_XTRAIN, function(x) GET_METRICS_TUPLE(x))
        GET_BARPLOT_FOR_MODEL(t,2,7)
        t = aggregate (click ~  sitecatclickprobs1 + appcatclickprobs0, ORIG_XTRAIN, function(x) GET_METRICS_TUPLE(x))
        GET_BARPLOT_FOR_MODEL(t,2,7)
        t = aggregate (click ~  banner_pos + device_type, ORIG_XTRAIN, function(x) GET_METRICS_TUPLE(x))
        GET_BARPLOT_FOR_MODEL(t,2,7)
    dev.off()
    # ###################################################################################

