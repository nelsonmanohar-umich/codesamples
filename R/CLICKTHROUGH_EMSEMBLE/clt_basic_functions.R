# ############################################################################################
HEADER    = "------------------------------------------------------------------------\n"
# ############################################################################################


# ############################################################################################
GET_MODEL_NAME = function ( M ) {
   return ( paste( PREFIX, M, sep="" ) )
}
# ############################################################################################


# ############################################################################################
LOCAL_TIMESTAMP = proc.time()
BANNER = function ( cname, nlines=5 ) {
    gc()
    print ( proc.time() - LOCAL_TIMESTAMP )
    LOCAL_TIMESTAMP <<- proc.time()
    cat(HEADER)
    cat(HEADER)
    NEWLINE(nlines)
    cat(HEADER)
    print( cname )
    gc(TRUE)
    cat(HEADER)
}
# ############################################################################################


# ############################################################################################
SAPPLY = function( l, f, stype="V", ncores=2 ) {
    if ( stype == "V" ) {
	    N = length(l)
        ll = VECTOR ( N )
        i = 0
        for( item in l ) {
            i = i + 1
            ll[i] = f(item)
        }
        ll = c(ll)
    }
    if ( stype == "S" ) {
        ll = sapply( l, f )
    }
    if ( stype == "M" ) {
        ll = mclapply( l, f, mc.cores=ncores)
        ll = unlist(ll)
    }
    return ( ll )
}
# ############################################################################################


# ############################################################################################
IFELSE = function( a, b1, b0 ) {
    if ( a )  
        return ( b1 )
    else 
        return ( b0 )
}
# ############################################################################################


# ############################################################################################
LOAD_CLICK_THRU_DATA = function( FILENAME="", FRACTION=0.10, FASTER_TEST=FALSE, ... ) {
    # id: ad identifier
    # click: 0/1 for non-click/click
    # hour: format is YYMMDDHH, so 14091123 means 23:00 on Sept. 11, 2014 UTC.
    # C1 -- anonymized categorical variable
    # banner_pos
    # site_id
    # site_domain
    # site_category
    # app_id
    # app_domain
    # app_category
    # device_id
    # device_ip
    # device_model
    # device_type
    # device_conn_type
    # C14-C21 -- anonymized categorical variables
    
    COLNAMES = c( 'ad_id', 'click', 'YYMMDDHH', 'C01', 'banner_pos', 
                  'site_id', 'site_domain', 'site_category', 
                  'app_id', 'app_domain', 'app_category', 
                  'device_id', 'device_ip', 'device_model', 'device_type', 'device_conn_type', 
                  'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21' )

    XY_TRAIN = read.csv( FILENAME, header=FALSE, stringsAsFactors=TRUE, colClasses=c('character',rep('integer',4), rep('factor',9), rep('integer',10)), ...)
    colnames(XY_TRAIN) = COLNAMES

    if ( FASTER_TEST ) {
        M = nrow(XY_TRAIN)
        P = as.integer( FRACTION * M )
        XY_TRAIN = XY_TRAIN[1:P, ]
    }

    cat(HEADER)
    str(XY_TRAIN)
    cat(HEADER)
    print( summary(XY_TRAIN) )
    cat(HEADER)
    cat(HEADER)
    return ( XY_TRAIN )
}
# ############################################################################################


# ############################################################################################
LOGIT = function( x ) {
    f = 1.0 / ( 1.0 + exp(-x))
    return ( f )
}
# ############################################################################################


# ############################################################################################
GET_Z = function( theta, x ) {
    if ( class(theta) == "numeric" || class(theta) == "integer" ) {
        z = theta * x
    } else {
        z = t(matrix(theta)) %*% x
    }
    return ( z )
}
# ############################################################################################


# ############################################################################################
H0 = function( x, theta=1.0 ) {
    z  = GET_Z( theta, x )
    yp = LOGIT( z )
    return ( yp )
}
# ############################################################################################


# ############################################################################################
LOGIT_PREDICT = function( theta=c(), x=c(), z=NA, THRESHOLD=0.5 ) {
    if ( is.na(z) ) {
        z = GET_Z( theta, x )
    }
    if ( z <= THRESHOLD ) {
        return ( 0 )
    }
    return ( 1 )
}
# ############################################################################################


# ############################################################################################
LOGIT_PREDICT_CLASS = function( X, Y=c(), model, theta, THRESHOLD=0.5 ) {
    YP_LABELS = as.matrix( sapply( X, function(x) { LOGIT_PREDICT( theta=theta, x=x, THRESHOLD=THRESHOLD ) } ) )
    return ( YP_LABELS )
}
# ############################################################################################


# ############################################################################################
LOGIT_PREDICT_PROBS = function( X, Y=c(), model, theta, THRESHOLD=0.5 ) {
    YP_PROBAS = as.matrix(sapply( X, function(x) { H0( x, theta) } ))
    YP_LABELS = LOGIT_PREDICT_CLASS( X, Y=Y, model, theta, THRESHOLD=THRESHOLD )
    YP = cbind( YP_PROBAS, YP_LABELS )
    return ( YP )
}
# ############################################################################################


# ############################################################################################
JCOST_Z = function( z, y_true ) {
    h0x = 1 /(1 + exp(-z))
    jcost = -y_true * log(h0x) + (1-y_true) * log(1-h0x)
    return (jcost)
}
# ############################################################################################


# ############################################################################################
JCOST_X = function( x_sample, y_true, theta ) {
    h0x = H0( x_sample, theta=theta )
    jcost = -y_true * log(h0x) + (1-y_true) * log(1-h0x)
    return (jcost)
}
# ############################################################################################


# ############################################################################################
# INCORRECT:BROKEN
# ############################################################################################
ITERATIVE_BINARY_THRESHOLD_EVALUATOR = function( DT_MODEL, X_TRAIN, Y_TRAIN, BETA=0.5, THRESHOLDS=c( 0.25, 0.333, 0.5, 0.667, 0.75 ) ) {
    FMAX = 0
    FMAX_THRESHOLD = NA
    FMAX_CMAT = NA

    YP = predict(DT_MODEL, X_TRAIN)

    for ( THRESHOLD in THRESHOLDS ) {
        YPP = as.data.frame( ifelse( YP <= THRESHOLD, 0, 1 ) )
        DT_CMAT = table(Y_TRAIN[[1]], YPP[[1]])
        F_RATIO = GET_F_RATIO( DT_CMAT, beta=BETA )
        if ( F_RATIO > FMAX ) {
            FMAX = F_RATIO
            FMAX_THRESHOLD = THRESHOLD
            FMAX_CMAT = DT_CMAT
        }
        print ( sprintf( "THRESHOLD=%.3f, FRATIO=%.3f\nCONFUSION TABLE:\n", THRESHOLD, F_RATIO ) )
        print ( DT_CMAT )
        cat ( HEADER )
        cat ( HEADER )
    }
    cat( HEADER )
    RETVALS = list( F_RATIO=FMAX, CMAT=FMAX_CMAT, THRESHOLD=FMAX_THRESHOLD )
    return ( FMAX_THRESHOLD )
}
# ############################################################################################


# ############################################################################################
WHICH_COL_FOR = function( XY, COLNAME ) {
    return ( which( colnames(XY) == COLNAME) )
}
# ############################################################################################


# ############################################################################################
GET_NUMLEVELS = function(fieldname, NMIN) {
    return( min(length(levels(ORIG_XTRAIN[,fieldname])),NMIN) )
}
# ############################################################################################


# ############################################################################################
APPLY_MAPFUNCTION_WITH_DEFAULT = function(fieldname, MAPPING, default_mapping_value, TRAIN=FALSE, NROW_LIMIT=300000 ) {
    DEFAULT_MAPPING = default_mapping_value
    if ( TRAIN ) {
        if ( nrow(ORIG_XTRAIN) > NROW_LIMIT ) {
            DT0 = VECTOR(nrow(ORIG_XTRAIN))
            for ( row in 1:nrow(ORIG_XTRAIN)) {
                fieldvalue = ORIG_XTRAIN[row,fieldname]
                DT0[row] = ifelse(is.null(MAPPING[[fieldvalue]]), DEFAULT_MAPPING, MAPPING[[fieldvalue]])
            }
            DT0 = c(DT0)
        } else {
            DT0 = sapply( ORIG_XTRAIN[,fieldname], function(x) ifelse(is.null(MAPPING[[x]]), DEFAULT_MAPPING, MAPPING[[x]]))
        }
    } else {
        DT0 = sapply( ORIG_XTEST [,fieldname], function(x) ifelse(is.null(MAPPING[[x]]), DEFAULT_MAPPING, MAPPING[[x]]))
    }
    DT0 = unlist(DT0)
    print( summary( DT0 ) )
    return ( DT0 )
}
# ############################################################################################


# #####################################################################################
CUT = function( XY, SEQ, ... ) {
    xy = cut( XY, SEQ, ... )
    l = levels (xy)
    l = unlist( sapply( l, function(x) gsub("\\]", "\\_", x ) ))
    l = unlist( sapply( l, function(x) gsub("\\(", "\\_", x ) ))
    l = unlist( sapply( l, function(x) gsub("\\+", "\\p", x ) ))
    l = unlist( sapply( l, function(x) gsub("\\-", "\\m", x ) ))
    l = unlist( sapply( l, function(x) gsub("\\,", "\\_", x ) ))
    levels(xy) = l
    return ( xy )
}
# #####################################################################################


# #####################################################################################
RECODE_FACTOR = function( XYMAT, attr_name , N=5, other="other", new_attr_name="", approx=FALSE ) {
    XY = as.factor(XYMAT[,attr_name])
    current_levels = levels(XY)
    M = length(current_levels)
    print( paste( "setup", attr_name, N, M, approx ) )

    new_attr = as.character( XY )
    activated_levels = names(sort( table( XY ), decreasing=TRUE)[1:N])
    non_activated_levels = names(sort( table( XY ), decreasing=TRUE)[(N+1):length(current_levels)])
    #activated_levels = current_levels[1:N]
    #non_activated_levels = current_levels[(N+1):length(current_levels)]

    print( paste( "loop", attr_name, N, M, approx ) )

    #if ( !approx ) {
        #for ( f in non_activated_levels ) {
            #new_attr[ which( new_attr == f )] = other
        #}
    #} else {
        #w = which( XY %in% non_activated_levels )
        #new_attr[ w ] = other
        w = which( XY %in% non_activated_levels )
        levels(XY) = c(levels(XY), other)
        XY[w] = other
    #}

    if ( new_attr_name == "" ) 
        new_attr_name = attr_name

    if ( other %in% activated_levels ) {
        XYMAT[,new_attr_name] = XY #factor( new_attr, rev(activated_levels), ordered=TRUE ) 
    } else {
        activated_levels = append( activated_levels, other )
        XYMAT[,new_attr_name] = XY #factor( new_attr, rev(activated_levels), ordered=TRUE )
    }
    return ( XYMAT )
}
# ############################################################################################


# #####################################################################################
# CHECK INTO ORDERED BUGS AND MISSING FACTORS 
# #####################################################################################
RECODE_FACTOR_USING = function( attr_name, other=NA, activated_levels=c(), new_attr_name="" ) {
    XY = as.factor(ORIG_XTEST[,attr_name])
    current_levels = levels(XY)
    new_attr = as.character( XY )
    this_df_non_activated_levels = setdiff(current_levels, activated_levels)
    # for ( f in this_df_non_activated_levels ) {
        # new_attr[ which( new_attr == f )] = other
    # }
        w = which( XY %in% this_df_non_activated_levels )
        levels(XY) = c(levels(XY), other)
        if ( FALSE ) {
            XY[w] = other
            XY[is.na(XY)] = other
            ORIG_XTEST[,attr_name] <<- XY
        } else {
            new_attr[ w ] = other
            new_attr = as.factor(new_attr)
            new_attr[ is.na(new_attr) ] = other
        }

    new_attr = factor( new_attr, activated_levels, ordered=TRUE )
    if ( new_attr_name == "" ) {
        ORIG_XTEST[,attr_name] <<- as.factor(new_attr)
        if ( FALSE ) {
            cat(HEADER)
            print( summary( ORIG_XTEST[,attr_name]))
        }
    } else {
        ORIG_XTEST[,new_attr_name] <<- as.factor(new_attr)
        if ( FALSE ) {
            cat(HEADER)
            print( summary( ORIG_XTEST[,new_attr_name]))
        }
    }
    if ( length(setdiff(levels(new_attr), activated_levels) ) != 0) {
        print ( sprintf( "DIFFERENCE IN FACTOR LEVELS FOR [%s=>%s] = %s", attr_name, new_attr_name, 
                                            length(setdiff(levels(new_attr), activated_levels) )) )
    }
    print( sprintf('DONE with %s transform [%s] <-- [%s]', attr_name, length(current_levels), length(activated_levels) )) 
    return ()
}
# #####################################################################################


# #####################################################################################
MATCH_APPLIED_TRANSFORMS_FOR = function( for_attribute, wrt_attribute, other="other") {
    RECODE_FACTOR_USING(
                                      for_attribute, other=other, 
                                      activated_levels=levels(ORIG_XTRAIN[,wrt_attribute]), 
                                      new_attr_name=wrt_attribute )
    return ()
}
# #####################################################################################


# ############################################################################################
RECODE_FACTOR_WITH_KNOWN_CUTS = function( activated_intervals=c() ) {
     labs <- activated_intervals
     lo_hi = cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
                   upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
}
# ############################################################################################


# ############################################################################################
RECODE_MISSING_VALUES_WITH_MEAN = function( XY, MISSING_DATA_COLUMN, NA_SYMBOL=-1 ) {
    XYna = XY[,MISSING_DATA_COLUMN]
    which_nas = which( XYna == NA_SYMBOL )
    which_not_nas = which( XYna != NA_SYMBOL )
    XYna[ which_nas ] = mean( XYna [which_not_nas ], na.rm=TRUE )
    XY[,MISSING_DATA_COLUMN] = XYna
    return (XY)
}
# ############################################################################################


# ############################################################################################
COMPUTE_LOG_LOSS <- function(actual, prediction) {
    if ( length(actual) != length(prediction) ) {
        BANNER( "ERROR" )
        print (length(actual))
        print (length(prediction))
        print (summary(actual))
        print (summary(prediction))
        return( 1 )
    }

    actual = as.numeric(as.factor(actual))-1
    if ( class(prediction)[1] == "factor") {
        prediction = as.numeric(as.factor(prediction))-1
    }
    epsilon <- .000000000000001
    yhat <- pmin(pmax(prediction, epsilon), 1.0-epsilon)
    logloss <- -mean(actual*log(yhat) + (1.0-actual)*log(1.0 - yhat))
    P = 3
    if ( FALSE ) {
        print (sprintf( "[%8s] --> (%13s,%13s):%13s         ", 1:P, actual[1:P], prediction[1:P], actual[1:P]-prediction[1:P] ))
        print ( table(actual[1:min(10*P,length(actual))], round(prediction[1:min(10*P,length(prediction))],1)) )
    }
    print ( paste( "LOG_LOSS_VALUE:", round(logloss,4) ) )
    return(logloss)
}
# ############################################################################################


# ############################################################################################
# do a second model that is trained on missed samples, and if the probability is on the questionable region predict with such model instead (fallback)
# ############################################################################################
WHICH_CLASS = function( YP ) {
    THRESHOLDS = c( 0.5 )
    LMETRICS = MATRIX( length(THRESHOLDS), 12 )
    j = 1
    for ( CT in THRESHOLDS ) {
        cat(HEADER) # cat(HEADER) ; print( paste( "THRESHOLD = ", CT ) ) ; cat(HEADER)
        C = WHICH_CLASS_KERNEL( YP, THRESHOLD=CT)
	    LCMAT = table( Y_TRUE, C)
	    print ( LCMAT )
	    LMETRICS[j,1] = CT
	    LMETRICS[j,2] = GET_F_RATIO( LCMAT)
	    LMETRICS[j,3] = GET_FPR( LCMAT)
	    LMETRICS[j,4] = GET_FNR( LCMAT)
	    LMETRICS[j,5] = GET_ACC( LCMAT)
	    LMETRICS[j,6] = ( GET_TPR( LCMAT) + GET_TNR( LCMAT ) ) / 2.0
	    LMETRICS[j,7] = sum( abs(as.numeric(C)-1 - as.numeric(Y_TRUE)-1)^2 )/ length(C)
	    LMETRICS[j,8] = GET_PRECISION( LCMAT )
	    LMETRICS[j,9] = GET_RECALL( LCMAT )
	    LMETRICS[j,10] = GET_TPR( LCMAT )
	    LMETRICS[j,11] = GET_TNR( LCMAT )
	    LMETRICS[j,12] = COMPUTE_LOG_LOSS( Y_TRUE, YP[,2] )
        j = j + 1
    } 
    colnames(LMETRICS) = c( "JCOST", "F_RATIO", "FPR", "FNR", "ACCURACY", "BER", "MSE", "PRECISION", "RECALL", "TPR", "TNR", "LOG_LOSS" )
    print ( LMETRICS )
    cat(HEADER)
    cat(HEADER)

    return ( C )
}
# ############################################################################################


# ############################################################################################
# HI PASS FILTER
# ############################################################################################
WHICH_CLASS_KERNEL = function( YP, THRESHOLD=0.85 ) {
    N = nrow(YP)
    if ( N > 500000 ) {
        C = VECTOR(N)
        for ( i in 1:N) {
            C[i] = colnames(YP)[1]
            if ( YP[i,2] > THRESHOLD ) 
                C[i] = colnames(YP)[2]
        }
        C = as.factor( c(C) )
    } else {
        C = as.factor( ifelse( YP[,2] > THRESHOLD, colnames(YP)[2], colnames(YP)[1] ) )
    }
    return ( C )

    C = VECTOR(N)
    j = 0
    for ( i in 1:N ) {
        wmax = which( YP[i,] == max(YP[i,]))
        C[i] = colnames(YP)[1]

        # reset the default decision under these circumstances
        if ( wmax == 2 ) {
            if ( YP[i,wmax] >= THRESHOLD ) {
                C[i] = colnames(YP)[wmax]
                next
            }
            if ( j>5) next
            if ( C[i] != as.character(Y_TRUE[i] )) {
                print( paste( Y_TRUE[i], C[i], YP[i,], wmax ) )
                j = j+1
            }
        } else {
            if ( j>5) next
            if ( C[i] != as.character(Y_TRUE[i] )) {
                print( paste( Y_TRUE[i], C[i], YP[i,], wmax ) )
                j = j+1
            }
        }
    }
    C = as.factor(C)
    return ( C )
}
# ############################################################################################


# ############################################################################################
WHICH_CLASS_TESTING_KERNEL = function( YP, THRESHOLD=0.5 ) {
    N = nrow(YP)
    C = as.factor( ifelse( YP[,2] > THRESHOLD, colnames(YP)[2], colnames(YP)[1] ) )
    return ( C )
    N = nrow(YP)
    C = VECTOR(N)
    j = 0
    for ( i in 1:N ) {
        wmax = which( YP[i,] == max(YP[i,]))
        C[i] = colnames(YP)[1]
        if ( wmax == 2 ) {
            if ( YP[i,wmax] >= THRESHOLD ) {
                C[i] = colnames(YP)[wmax]
                next
            }
        }
    }
    C = as.factor(C)
    return ( C )
}
# ############################################################################################


# ############################################################################################
GET_TOPN_TABLE_FROM = function( COL, Y, TOPN, WRT=c(1,2) ) {
    t = table(COL, Y)
    x = unlist( sapply( 1:nrow(t), function(x) sum(t[x,WRT]))  )
    ordering = order(x, decreasing=T)
    t = t[ordering,]
    n = min(TOPN,nrow(t))
    tt = as.data.frame( t[1:n,] )
    # print( t(tt) )
    cat(HEADER)
    return ( tt )
}
# ############################################################################################


# ############################################################################################
GET_FORMULAE = function( cols ) {
    f = paste( cols, '+' )
    f = CONCAT(f)
    m = nchar(f)
    f = paste( 'click ~ ', substr(f,1,m-1))
    return ( f )
}
# ############################################################################################




