# ################################################################################
source('utilities.R')
source('clt_basic_functions.R')
source('clt_options.R')
source('clt_columns.R')
source('clt_activated_cols.R')
source('clt_prob_enhancer.R')
# ################################################################################

CVTEST = TRUE
NROWS  = 50000



# ################################################################################
LOAD_WRT = function( cvfold=i, k=1, Q=10, R=NROWS ) {
    Xfile = sprintf("TRAIN/training_chunk_%s.csv", 1000+cvfold)
    x = read.csv( Xfile, skip=Q*k, nrows=R, header=FALSE, stringsAsFactors=TRUE, colClasses=c('character',rep('integer',4), rep('factor',9), rep('integer',10)))
    colnames(x) = COLNAMES
    return ( x )
}
# ################################################################################


# ################################################################################
EVALUATE3 = function( XT, cname="hour", cvfold=1 ) {
    Pfile = sprintf("P%s_cvchunk_%s.probs", cname, 1000+cvfold)
    print( Pfile )
    P = read.csv( Pfile, header=TRUE)
    colnames(P) = c('ad_id', 'click', '0', '1')
    # ----------------------------------------------------------------------------
    validity_test = any(XT[,"ad_id"] != P[,"ad_id"]) 
    print( sprintf( "VALID %s", validity_test ) )
    # ----------------------------------------------------------------------------
    p = COMPUTE_LOG_LOSS( P[,2], P[,4])
    return ( P[,4] )
}
# ################################################################################


# ################################################################################
EVALUATE2 = function( cname="hour", cvfold=1 ) {
    Pfile = sprintf("P%s_cvchunk_%s.probs", cname, 1000+cvfold)
    P = read.csv( Pfile, header=TRUE)
    colnames(P) = c('ad_id', 'click', '0', '1')
    # ----------------------------------------------------------------------------
    p = COMPUTE_LOG_LOSS( P[,2], P[,4])
    return ( p )
}
# ################################################################################


# ################################################################################
EVALUATE1 = function( x=NA, cname="hour", cvfold=1 ) {
    if ( length(x) == 0 ) x = LOAD_WRT( cvfold=i )
    Pfile = sprintf("P%s_cvchunk_%s.probs", cname, 1000+cvfold)
    P = read.csv( Pfile, header=TRUE)
    # ----------------------------------------------------------------------------
    p = COMPUTE_LOG_LOSS( x$click, P[,2])
    return ( p )
}
# ################################################################################


# ################################################################################
GET_RJE_XVARS = function( all=FALSE ) {
    RJE_WHICH_COLUMNS = ACTIVATED_COLNAMES
    if ( !all ) RJE_WHICH_COLUMNS = setdiff(names(sapply(CLT_OPTIONS$predictors, function(x) x)), "")
    return( RJE_WHICH_COLUMNS )
}
# ################################################################################


# ################################################################################
# load('T4.RData')
# ################################################################################


# ################################################################################
# CAN ADD BOOTSTRAP TO THIS ACROSS FOLD AND XVARS, IF IN ONE ITERATION XVAR 
# PERFORMS POORLY WEIGHT CAN BE ADAPTED. NOW LOOP LOGIC BECOMES SIMPLIFY INTO 
# LOADING, TRAINCODING, TESTCODING, RJE-TRAIN, RJE-PREDICT, ENTRY_POINT, INTEGRATE 
# and POSSIBLY: BOOTSTRAP.
# ################################################################################
    x = LOAD_WRT( cvfold=i )
    NFOLDS = 90
    for( i in 1:NFOLDS ) {
        # ----------------------------------------------------------------------------
        XT = LOAD_WRT( cvfold=i )
        # ----------------------------------------------------------------------------
        XVARS = GET_RJE_XVARS()
        MCOLS = length(XVARS)
        NROWS = nrow(XT)
        # ----------------------------------------------------------------------------
        YP_PROBS  = COMPUTE_INITIAL_YP_PROBS(XT)
        # ----------------------------------------------------------------------------
        P = MATRIX( NROWS, MCOLS + 1 )
        P[,1] = YP_PROBS
        for ( i in 1:MCOLS ) { print( sprintf( "%s --> %s", i, XVARS[i] )) }
        for ( i in 1:MCOLS ) { P[,i] = EVALUATE3( XT, cname=cname, cvfold=i ) }
        # ----------------------------------------------------------------------------
        P = apply(P, 1, sum)
        # ----------------------------------------------------------------------------
        p = COMPUTE_LOG_LOSS( XT$click, P )
        # ----------------------------------------------------------------------------
    }
# ################################################################################
