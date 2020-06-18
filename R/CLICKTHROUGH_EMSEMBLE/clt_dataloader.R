# ##########################################################################################
LOAD_OPTIONAL_DATASET = function( FILENAME, NROWS=1000, SUBSAMPLED=FALSE, RATE=200,
                                  COLCLASSES=c('character',rep('integer',4), rep('factor',9), rep('integer',10))) {
    BANNER(sprintf( "LOADING SUBSAMPLED TRAIN DATA %s", FILENAME))
    if ( !SUBSAMPLED ) {
        ORIG_XTRAIN_PLUS = read.csv( FILENAME, nrows=NROWS,  header=TRUE, stringsAsFactors=TRUE, colClasses=COLCLASSES)
    } else { #stackoverflow/questions/21891841
        file.pipe <- pipe(sprintf("awk 'BEGIN{i=0}{i++;if (i%% %s==1) print $1}' < '%s'", RATE, FILENAME))
        ORIG_XTRAIN_PLUS = read.csv( file.pipe, nrows=NROWS, header=TRUE, stringsAsFactors=TRUE, colClasses=COLCLASSES)
    }
    colnames(ORIG_XTRAIN_PLUS) = COLNAMES
    return ( ORIG_XTRAIN_PLUS )
}
# ##########################################################################################


    # ###########################################################################
    filename = CLT_OPTIONS$'primary balanced dataset'
    if ( FALSE ) {
        ORIG_XTRAIN = LOAD_OPTIONAL_DATASET( filename, NROWS=NROWS )
    } else {
        BANNER(sprintf( "LOADING SUBSAMPLED AUGMENTATION TRAIN DATA %s", CLT_OPTIONS$'primary balanced dataset'))
        ORIG_XTRAIN = LOAD_OPTIONAL_DATASET( filename, NROWS=NROWS, 
                                             COLCLASSES=c('character','numeric','integer','numeric','numeric',rep('factor',9), rep('numeric',10)))
        ORIG_XTRAIN = ORIG_XTRAIN[,COLNAMES]
    }
    # ###########################################################################

    # ###########################################################################
    if ( CLT_OPTIONS$'load small scale balanced set - 2' ) {
        filename = CLT_OPTIONS$'load small scale balanced set - 2'
        ORIG_XTRAIN_PLUS = LOAD_OPTIONAL_DATASET( filename, NROWS=NROWS_2 )
        ORIG_XTRAIN = rbind(ORIG_XTRAIN, ORIG_XTRAIN_PLUS)
        rm(ORIG_XTRAIN_PLUS)
    }
    # ###########################################################################

    # ###########################################################################
    if ( CLT_OPTIONS$'load large scale balanced set - 10' ) {
        filename = CLT_OPTIONS$'load large scale balanced set - 10'
        ORIG_XTRAIN_PLUS = LOAD_OPTIONAL_DATASET( filename, NROWS=NROWS_10 )
        ORIG_XTRAIN = rbind(ORIG_XTRAIN, ORIG_XTRAIN_PLUS)
        rm(ORIG_XTRAIN_PLUS)
    }
    # ###########################################################################

    # ###########################################################################
    if ( CLT_OPTIONS$'load subsampled by day dataset' ) {
        BANNER(sprintf( "LOADING SUBSAMPLED TRAIN DATA %s", "DAILY SUBSAMPLED FILES"))
        for ( FILENAME in DAYOWEEK_TRAINSET ) {
            print( FILENAME )
            ORIG_XTRAIN_PLUS = LOAD_OPTIONAL_DATASET( FILENAME, NROWS=NROWS_W )
            ORIG_XTRAIN = rbind(ORIG_XTRAIN, ORIG_XTRAIN_PLUS)
            print ( paste( nrow(ORIG_XTRAIN_PLUS), nrow(ORIG_XTRAIN)) )
        }
        rm(ORIG_XTRAIN_PLUS)
        print(summary(ORIG_XTRAIN)) 
    }
    # ###########################################################################


    # ###########################################################################
    ORIG_XTRAIN[,"origin"] = "W"
    # ###########################################################################


    # ###########################################################################
    if ( CLT_OPTIONS$'load data chunks w/ known bad perf' ) {
        BANNER(sprintf( "LOADING SUBSAMPLED TRAIN DATA %s", "BAD PERFORMANCE SUBSAMPLED FILES"))
        FILENAME = CLT_OPTIONS$'bad temporal performance dataset'
        ORIG_XTRAIN_PLUS = LOAD_OPTIONAL_DATASET( FILENAME, NROWS=NROWS_B )
        ORIG_XTRAIN_PLUS[,"origin"] = "B"
        print( summary(ORIG_XTRAIN_PLUS )) 
        ORIG_XTRAIN = rbind(ORIG_XTRAIN, ORIG_XTRAIN_PLUS)
        print ( paste( nrow(ORIG_XTRAIN_PLUS), nrow(ORIG_XTRAIN)) )
        rm(ORIG_XTRAIN_PLUS)
    }
    # ###########################################################################

    ORIG_XTRAIN[,"origin"] = as.factor( ORIG_XTRAIN[,"origin"] )
    M = nrow(ORIG_XTRAIN)
    N = ncol(ORIG_XTRAIN)
    rownames(ORIG_XTRAIN) = 1:M

    PREDICT_VAR = "click"
    PREDICT_COL = 2
    YCOLNAME = "click"
    YCOLNUM  = PREDICT_COL
    CLASS0 = 0
    CLASS1 = 1

    ORIG_XTRAIN[YCOLNAME] = as.factor(ORIG_XTRAIN[,'click'])
    Y_TRUE = ORIG_XTRAIN[,YCOLNAME]

    BANNER("FINISHED LOADING DATA")
    print(summary(ORIG_XTRAIN))
    str(ORIG_XTRAIN)
