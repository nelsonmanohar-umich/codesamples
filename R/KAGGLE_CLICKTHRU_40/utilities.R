# ######################################################################################################
# MACHINE LEARNING TOOLBOX FILES IN R
#           Copyright (C) Nelson R. Manohar
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# ######################################################################################################
# @AUTHOR:  Nelson R. Manohar Alers
# @EMAIL:   manohar.nelson@gmail.com
# @DATE:    September, 2014
# @URL:     http://www.bitbucket.org/nelsonmanohar/machinelearning
# ######################################################################################################
LICENSETEXT = "These R code samples (version Sep/2014), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License." 
message( LICENSETEXT )
message("")
# ######################################################################################################


# ######################################################################################################
print( commandArgs() )
# ######################################################################################################


# #####################################################################################
library(stringr)
# #####################################################################################


# #####################################################################################
HEADER    = "------------------------------------------------------------------------\n"
SUBHEADER = "        ----------------------------------------------------------------\n"
TIMEREF   = proc.time()
# #####################################################################################


# #####################################################################################
NEWLINE = function(n=1) {
    for( i in 1:n ) cat("\n")
}
# #####################################################################################


# #####################################################################################
PRINT_DATASET_TYPE = function( dname, x, y ) {
    cat( SUBHEADER )
    print ( paste ( dname, x , y ) )
    cat( SUBHEADER )
}
# #####################################################################################


# ######################################################################################################
MDF_STATS = function ( Xx, debug=FALSE ) { 
    cat(SUBHEADER)
    print ( sprintf( "DF STATS: %sx%s [%s w/ %s elems]", nrow(Xx), ncol(Xx), class(Xx), length(Xx) ) ) 
    if ( debug ) print ( paste( 1:ncol(Xx), colnames(Xx) ) )
    NEWLINE(1)
}
# ######################################################################################################


# ######################################################################################################
SUMMARY = function( X, d=3, w=120, w_str=FALSE, debug=FALSE ) {
    p_opts = options( digits=d ) 
    options( width=w ) 
    txt = summary( X )
    cat( HEADER )
    print ( txt )
    if ( w_str ) str(X )
    if ( debug ) cat( HEADER )
    options( p_opts ) 
}
# ######################################################################################################


# #####################################################################################
EXPLAIN_DATASET = function( x, xname ) {
    if ( xname == "BostonHousing" ) PRINT_DATASET_TYPE( xname, "REGRESSION",     "NUMERICAL+FACTOR" )
    if ( xname == "HouseVotes84" )  PRINT_DATASET_TYPE( xname, "CLASSIFICATION", "FACTORS" )
    if ( xname == "iris" )          PRINT_DATASET_TYPE( xname, "CLASSIFICATION", "NUMERICAL" )
    if ( str_count(xname, "C:N"))   PRINT_DATASET_TYPE( xname, "CLASSIFICATION", "NUMERICAL" )
    if ( str_count(xname, "C:F"))   PRINT_DATASET_TYPE( xname, "CLASSIFICATION", "FACTORS" )
    if ( str_count(xname, "C:M"))   PRINT_DATASET_TYPE( xname, "CLASSIFICATION", "NUMERICAL+FACTORS" )
    if ( str_count(xname, "R:N"))   PRINT_DATASET_TYPE( xname, "REGRESSION",     "NUMERICAL" )
    if ( str_count(xname, "R:M"))   PRINT_DATASET_TYPE( xname, "REGRESSION",     "NUMERICAL+FACTORS" )
    if ( str_count(xname, "R:F"))   PRINT_DATASET_TYPE( xname, "REGRESSION",     "FACTORS" )
    # txt = summary( x )
    # print ( txt )
    # str( x ) 
    MDF_STATS(x)
}
# #####################################################################################


# #####################################################################################
PRINT_HEADER = function( x="" ) {
    cat( HEADER )
    NEWLINE(10)
}
# #####################################################################################


# #####################################################################################
GET_TIMESTAMP = function( taskname ) {
    NEWLINE(1)
    print( sprintf( "%20s: ", taskname ) )
    TS = proc.time() - TIMEREF
    print( TS )
    NEWLINE(1)
    TIMEREF   = proc.time()
    results = list(taskname, TS )
    return ( results )
}
# #####################################################################################


# #####################################################################################
# http://stackoverflow.com/questions/10802592/how-to-remove-correlated-or-duplicated-variable-or-individuals-in-r
# #####################################################################################
DROP_HIGHLY_CORRELATED_FEATURES = function( dat, nmax=1024, cmax=0.9, keep="", subsample=FALSE, debug=FALSE ) { 
    if ( debug ) print( paste( nmax, cmax ) )
    if ( nmax < 60 ) {
        print ( "WARNING: subsample is too small wrt dataset, entire dataset used instead" )
        nmax = nrow(dat)
    }
    TS = proc.time()
    # print( summary(dat) )
    if ( subsample ) {
        which_sampled_rows = RANDOM_ROWS( rownames(dat), m=min(nmax,length(rownames(dat))))
        dat = dat[which_sampled_rows,]
    }
    cMat <- abs(cor(dat))
    cMat <- cMat >= cmax
    if ( debug ) print( ifelse( cMat, 111, 000) )
    NEWLINE(1)
    keep = c(1)
    drop = c()
    for ( i in 2:ncol(cMat) ) {
        findings = which( cMat[c(min(i+1,nrow(cMat)):nrow(cMat)),i] )
        if ( length( findings )!= 0 ) {
            if ( debug ) print( findings )
            drop = unique( append( drop, findings ) )
        }
    }
    whichKeep = sort( append( keep, setdiff( 1:ncol(cMat), drop ) ))
    cat( HEADER )
    if ( debug ) { print( paste( "drop", order(drop) )); print( paste( "keep", whichKeep ) ) }
    cat( HEADER )

    if ( debug ) print( all(is.numeric(whichKeep) ) )

    if ( all(is.numeric(whichKeep) ) ) { 
        retval = dat[,whichKeep]
        str(retval)
    } else {
        if (keep != "" && is.character(keep)) {
            if (sum(grep( keep, whichKeep ))==0) {
                whichKeep = append(whichKeep,grep(keep,colnames(dat)))
            }
            retval = dat[whichKeep]
        } 
        print ( paste ( "KEPT: ", colnames(whichKeep) ) )
        print ( paste ( "DROP: ", setdiff(colnames(dat), colnames(whichKeep)) ))
    }

    if ( debug ) { NEWLINE(1) ; print ( proc.time() - TS ) }
    return ( retval )
}
# #####################################################################################


# #####################################################################################
# Apply one or more dimensionality reductions to the dataset
# #####################################################################################
DO_DIMENSIONALITY_REDUCTION = function( mydf, target_var="Y", rtype="SUBSAMPLE|CORRELATION|COMPLETECASES", nmax=256, cmax=0.9 ) {
    print( sprintf( "ORIGINAL no dimensionality reduction: [%s x %s]", nrow(mydf), ncol(mydf) ))
    if ( str_count( "COMPLETECASES", rtype ) > 0 ) {
        COMPLETE_SAMPLES = complete.cases(mydf)
        mydf = mydf[COMPLETE_SAMPLES,]
        print( sprintf( "APPLIED: missingcase() reduction: [%s x %s]", nrow(mydf), ncol(mydf) ))
    }
    if ( str_count( "SUBSAMPLE", rtype ) > 0 ) {
        which_sampled_rows = RANDOM_ROWS(rownames(mydf), m=min(nmax,nrow(mydf)))
        mydf= mydf[which_sampled_rows,]
        print( sprintf( "APPLIED: subsampling() reduction: [%s x %s]", nrow(mydf), ncol(mydf) ))
    }
    if ( str_count( "CORRELATION", rtype ) > 0 ) {
        nmax = min(nmax,nrow(mydf))
        print( which( colnames(mydf) == target_var ) )
        mydf_x = mydf[ , -which( colnames(mydf) == target_var ) ]
        mydf_x = DROP_HIGHLY_CORRELATED_FEATURES(mydf_x, nmax, cmax, keep="", subsample=FALSE)
        print( sprintf( "APPLIED: correlation() reduction: [%s x %s]", nrow(mydf_x), ncol(mydf_x) ))
        mydf = cbind( mydf_x,  mydf[ which( names(mydf) == target_var ) ] )
        # print( which( names(mydf) == target_var ) )
        # str(mydf_x)
        # str(mydf)
    }
    return ( mydf )
}
# #####################################################################################



# ###############################################################################
# Computes a weighted mean and normalizes the weight vector if this is not the case
# ###############################################################################
WEIGHTED_MEAN = function( v, w=c(), nonneg=TRUE ) {
    if ( length(w) == 0 ) {
        print ( 'WARNING: defaulting to normalizing weights' ) 
        w = rep(1/length(v),length(v))
    }
    if ( length(w) != length(v) ) {
        print ( 'WARNING: extending and renormalizing incomplete weights' ) 
        w = append( w, rep(1/length(v),length(v)-length(w)) )
    }
    if (nonneg && any(w < 0) ) {
        print ( 'WARNING: correcting negative weights' ) 
        w = abs(w)
    }
    w = w/sqrt(sum(w)*sum(w))
    weighted.mean(v,w)
}
# ###############################################################################


# ###############################################################################
# for a binary or multiclass label vector, creates weigths to attempt to balance 
# the specified class by proportionally increasing the weights of the unbalanced class 
# ###############################################################################
GET_WEIGHT_VECTOR_FOR_UNBALANCED_CLASS = function( y, ylevel=1, w=c(), nonneg=TRUE ) {
    yt = y[y==ylevel]
    if ( length(w) == 0 ) {
        w_class1 = rep(1/length(yt), length(yt))
        w_class0 = rep(1/length(y),  length(y)-length(w_class1))
        w = append( w_class1, w_class0 )
    }
    if (nonneg && any(w < 0) ) {
        print ( 'WARNING: correcting negative weights' ) 
        w = abs(w)
    }
    if ( length(w) != length(y) ) {
        print ( 'WARNING: extending and renormalizing incomplete weights' ) 
        w_class1 = append( w, rep(1/length(yt), length(yt)-length(w)) )
        w_class0 = rep(1/length(y),  length(y)-length(w_class1))
        w = append( w_class1, w_class0 )
    }
    w = w/sqrt(sum(w)*sum(w))
    return ( w )
}
# ###############################################################################


# ###############################################################################
# selects given a set of specified indexes/rownames/colnames, a subset of size m
# #####################################################################################
RANDOM_ROWS = function( idx, m=100 ) {
    random_rows = sort(sample( idx, m ))
    return ( random_rows )
}
# #####################################################################################


# #####################################################################################
# cbinds a data frame one col at a time while retaining/assigning colnames and rownames
# #####################################################################################
EXTEND_DF = function( X, Y, colname="Y" ) { 
    XX = data.frame( X, Y )
    colnames(XX) = c(colnames(X), colname)
    rownames(XX) = rownames(X)
    return ( XX )
}
# #####################################################################################


# ###############################################################################
# Discretizes a numeric vector into quantile bins
# #####################################################################################
RECODE = function( x, xnbins=10, asfactor=TRUE, debug=FALSE ) {
    q = seq(0,1,1/xnbins)
    p = quantile( x, q )
    pbins = attr(p, "names")
    for ( i in 1:xnbins ) {
        ql = p[pbins[i]]
        qh = p[pbins[i+1]]
        qlset = length(x[ x>=ql & x<qh ])
        if ( qlset ) {
            x[ x>=ql & x<qh ] = ql
        }
        if ( debug ) {
            print ( paste ( ql, qh ) )
            print ( x[ x>=ql & x<qh ] )
        }
    }
    if ( asfactor ) {
        x = factor(x, levels=p, labels=pbins)
    }
    return( x )
}
# #####################################################################################


# ###############################################################################
# stratified sampling function needed
# #####################################################################################
GET_SAMPLES_WITH_YLEVEL = function(  XYdf, y_level, nmax=100 ) {
    idx = XYdf$Y == y_level
    print ( y_level )
    y_level_rows = XYdf[ idx, ]
    yidx = rownames( y_level_rows )
    y_level_idx = RANDOM_ROWS( yidx, m=min(nmax,length(yidx)))
    SUBSAMPLED_Y = XYdf[ y_level_idx, ]
    return ( SUBSAMPLED_Y )
}
# #####################################################################################


# #####################################################################################
# deprecated
# #####################################################################################
STRATIFIED_SUBSAMPLING = function( X, Y, nb=4, nmax=100 ) {
    Y_as_factor = RECODE(Y,nb)
    XYdf = EXTEND_DF( X, Y_as_factor )
    y_levels = levels( Y_as_factor )
    START = TRUE
    for ( y_level in y_levels ) {
        idx = XYdf$Y == y_level
        print ( y_level )
        y_level_rows = XYdf[ idx, ]
        yidx = rownames( y_level_rows )
        y_level_idx = RANDOM_ROWS( yidx, m=min(nmax,length(yidx)))
        if ( START == TRUE ) { 
            SUBSAMPLED_Y_LEVEL_ROWS = XYdf[ y_level_idx, ]
            START = FALSE
        } else {
            subsampled_y_level_rows = XYdf[ y_level_idx, ]
            SUBSAMPLED_Y_LEVEL_ROWS = rbind( SUBSAMPLED_Y_LEVEL_ROWS, subsampled_y_level_rows ) 
        }
        txt = summary( SUBSAMPLED_Y_LEVEL_ROWS )
        print ( txt )
    }
    return ( SUBSAMPLED_Y_LEVEL_ROWS  )
}
# #####################################################################################


# #####################################################################################
# stratified subsampling of a dataframe with respect to a factor/multilabel column class
# #####################################################################################
DO_STRATIFIED_SUBSAMPLING = function( X, Y, nb=4, nmax=100 ) {
    Y_as_factor = RECODE(Y,nb)
    XYdf = EXTEND_DF( X, Y_as_factor )
    y_levels = levels( Y_as_factor )
    START = TRUE
    for ( y_level in y_levels ) {
        if ( START == TRUE ) { 
            SUBSAMPLED_Y_LEVEL_ROWS = GET_SAMPLES_WITH_YLEVEL(  XYdf, y_level, nmax )
            START = FALSE
        } else {
            subsampled_y_level_rows = GET_SAMPLES_WITH_YLEVEL(  XYdf, y_level, nmax )
            SUBSAMPLED_Y_LEVEL_ROWS = rbind( SUBSAMPLED_Y_LEVEL_ROWS, subsampled_y_level_rows ) 
        }
        txt = summary( SUBSAMPLED_Y_LEVEL_ROWS )
        print ( txt )
    }
    return ( SUBSAMPLED_Y_LEVEL_ROWS  )
}
# #####################################################################################


# #####################################################################################
# deprecated reflections functions wrt to a name that identifies a valid scope data frame
# #####################################################################################
COLNAME = function( dfname, i ) {
    df_colnames = colnames( eval(parse(text=dfname)) )
    col_name = paste( dfname, "$", df_colnames[i], sep="")
    return ( col_name )
}
# #####################################################################################
COLDATA = function( dfname, i ) {
    col_name = COLNAME ( dfname, i )
    col_data = eval(parse(text=col_name))
    return ( col_data )
}
# #####################################################################################
COLTYPE = function( col_data ) {
    col_class= class(col_data)
    return ( col_class )
}
# #####################################################################################
GET_XY_COLUMN_CLASS = function( colnamestr, df_name="" ) {
    col_objname_class = class( eval(parse(text=paste(df_name,"$", colnamestr, sep=""))))
    return ( col_objname_class )
}
# #####################################################################################
GET_DF_COLNAME = function( df, i ) {
    this_colname = attr(df,"dimnames")[[2]][i]
    return ( this_colname )
}
# #####################################################################################
# COLS_DATATYPES = mapply( GET_XY_COLUMN_CLASS, colnames(XYnew), df_name="XYnew" )
# #####################################################################################



# #####################################################################################
# SCALE all columns of the given DF, retaining names of rows and columns 
# returns reusable center/scale of the transformation for subsequent use
# #####################################################################################
SCALE_DF = function( XYdf, center=TRUE, scale=TRUE ) {
    XYnew = scale( XYdf, center=TRUE, scale=TRUE )
    colnames(XYnew) = colnames(XYdf)
    rownames(XYnew) = rownames(XYdf)

    XYnew_centers = attr(XYnew, "scaled:center" )
    XYnew_scales  = attr(XYnew, "scaled:scale" )

    utils::str(XYnew)

    retval = list( 'X'=XYnew, 'centers'=XYnew_centers, 'scales'=XYnew_scales )

    return ( retval )
}
# #####################################################################################


# #####################################################################################
DFROW_AS_VECTOR = function( xdf_row ) {
    return ( as.numeric( xdf_row ) )
}
# #####################################################################################


# #####################################################################################
# converts a df into a matrix with type (numeric) compliant elemetn
# #####################################################################################
DF_AS_MATRIX = function( xdf ) {
    xmatrix = matrix(apply(xdf,2,as.numeric), nrow(xdf))
    return( xmatrix )
}
# #####################################################################################


# ###################################################################################################
PROB_X = function( x_vector ) { return( dnorm( x_vector, mean(x_vector), sd(x_vector)) ) }
# ###################################################################################################


# ###################################################################################################
DOT = function( xi, xj ) { sum(xi*xj) }
# ###################################################################################################


# ###################################################################################################
INIT_METRICS = function() {
    ts = proc.time()
    usertime= ts[1]
    cputime = ts[2]
    systime = ts[3]
    m = 0
    ts_event = "START" 
    ts_tuple = list( "event"=1, "usertime"=usertime, "cputime"=cputime, "systime"=systime, 'delta'=0.0 )
    METRICS = data.frame( ts_tuple )
    METRICS[1,1]=as.character(ts_event)
    rownames(METRICS)[m+1] = m+1
    return(METRICS)
}
# ###################################################################################################


# ###################################################################################################
TIMESTAMP = function( ts_event ) {
    ts = proc.time()
    user_time= ts[1]
    cpu_time = ts[2]
    sys_time = ts[3]
        m = nrow(METRICS)
        n = ncol(METRICS)
        t_delta = sys_time - METRICS[m,4]
        ts_tuple = list( "event"=as.character(ts_event), "usertime"=user_time, "cputime"=cpu_time, "systime"=sys_time, 'delta'=t_delta )
        METRICS = rbind( METRICS, ts_tuple )
        METRICS[m+1,1]=as.character(ts_event)
        rownames(METRICS)[m+1] = m+1
    return (METRICS)
}
# ###################################################################################################


# ###################################################################################################
VERIFY = function( what, xX, debug=FALSE ) {
    if ( debug ) { 
        cat(HEADER)
        print( what )
        try( print( paste( "n. rows=", nrow(xX))))
        try( print( paste( "n. cols=", ncol(xX))))
        try( print( paste( "n. elem=", length(xX))))
        cat(HEADER)
        str(xX)
        cat(HEADER)
        NEWLINE(1)
    }
}
# ###################################################################################################


# ###################################################################################################
CHECK_COMMAND_ARGS = function ( args, default_argname, debug=FALSE ) {
    FOUND = FALSE
    VALID = FALSE
    argval = NA
    if (default_argname %in% names(args))  {
        argval=args[[default_argname]]
        FOUND = TRUE
        VALID = TRUE
    }
    retvals = list( 'ARGNAME'=default_argname, 'VALID'=VALID, 'FOUND'=FOUND, 'ARGVAL'=argval )
    if ( debug ) {
        cat(HEADER)
        print( t(as.matrix(retvals) ))
        cat(HEADER)
    }
    return (retvals)
}
# ###################################################################################################


# ###################################################################################################
CONCAT = function( a ) { d = ""; for ( i in a ) d = sprintf( "%s %s", d, i ) ; d}
# ###################################################################################################


# ###################################################################################################
SUM = function( x, ... ) {
    if ( all(is.na(x))) 
        return ( 0.0 )
    return ( sum(x, na.rm=TRUE, ...) )
}
# ###################################################################################################


# ###################################################################################################
MEAN = function( x, ... ) {
    if ( all(is.na(x))) 
        return ( 0.0 )
    return ( mean(x ,na.rm=TRUE, ...) )
}
# ###################################################################################################


# ###################################################################################################
ISNA = function( x, defaultval=0, ... ) {
    if ( is.na(x)   ) return ( defaultval )
    if ( is.null(x) ) return ( defaultval )
    return ( x )
}
# ###################################################################################################


# ###################################################################################################
VECTOR = function( n, flat=FALSE, vertical=!flat, initval=0.0 ) { 
    if ( !flat || vertical )
        matrix(rep(initval,n),n)
    else 
        matrix( rep(initval,n),1) 
}
# ###################################################################################################


# ###################################################################################################
MATRIX = function( m, n=m, initval=0.0 ) { 
    matrix( rep(initval,m*n), m ) 
}
# ###################################################################################################


# ###################################################################################################
ZERO_CLONE = function( M ) {
    MM = MATRIX(nrow(M), ncol(M))
    colnames(MM) = colnames(M)
    rownames(MM) = rownames(M)
    return ( MM )
}
# ###################################################################################################


# ###################################################################################################
CONFORMANT = function( A, B ) { 
    ncol(A) == nrow(B) && all(is.numeric(A)) && all(is.numeric(B))
}
# ###################################################################################################


# ######################################################################################################
# to each element of a list or row of a matrix, it applies the function are returns a vector
# ######################################################################################################
COLLECT_VECTOR = function ( WHAT, APPLY_F="", ... ) {
    A = c(); 
    if ( class(APPLY_F)=="function" ) {
        if ( class(WHAT) == "matrix" ) {
            for ( i in 1:nrow(WHAT) ) A = append( A, APPLY_F( WHAT[i,], ... ))
        } else
        if ( class(WHAT)=="list" ) {
            for ( i in 1:length(WHAT) ) A = append( A, APPLY_F( WHAT[[i]], ... ))
        } else {
            for ( i in 1:length(WHAT) ) A = append( A, APPLY_F( WHAT[i], ... ))
        }
    }
    return ( A )
}
# ######################################################################################################


# ######################################################################################################
GET_PCA_PLOT_VALUES = function( Ureduce, MAT ) {
    k = ncol(Ureduce)
    p = nrow(MAT)
    ZZ = MATRIX( p, k )
    for ( i in 1:p )
        ZZ[i, ] = t(Ureduce) %*% MAT[i,]
    return( ZZ )
}
# ######################################################################################################


# ######################################################################################################
VERIFY_OPTARG = function( OPTARG_NAME="", OPTARG_VALUE=NA ) { 
    V = CHECK_COMMAND_ARGS( commandArgs(), OPTARG_NAME )
    if ( V$'VALID'==TRUE & V$'FOUND'==TRUE ) 
        P = eval( parse( text=sprintf("%s <<- V$'ARGVAL'", OPTARG_NAME )))
    else
        P = eval( parse( text=sprintf("%s <<- %s", OPTARG_NAME, OPTARG_VALUE )))

}
# ######################################################################################################


# ######################################################################################################
# data frame form of named fields, selecting value field
# ######################################################################################################
GET_VECTOR_FROM_NAMED_VALUES = function( NAMED_VALUES, GET_NAMES=FALSE, BOTH=FALSE ) {
    if (BOTH)       return ( as.data.frame(NAMED_VALUES) )
    if (GET_NAMES)  return ( as.data.frame(NAMED_VALUES)[,1] )
    return ( as.data.frame(NAMED_VALUES)[,2] )
}
# ######################################################################################################


# ######################################################################################################
PUSH = function( Q, item, with_name="" ) {
    n = GET_SIZE(Q)
    if ( class(Q) == "list" )               { 
        if( with_name=="" ) Q[[n+1]] = item else Q[[with_name]]= item 
    }
    else if ( class(Q) == "matrix" )        { 
        Qnames = rownames(Q)
        item   = t(as.matrix(item))
        colnames(item) = colnames(Q)
        if (ncol(item)==ncol(Q)) Q = rbind(Q, item ) 
        rownames(Q) = c(Qnames, ifelse( with_name=="", n+1, with_name) )
    }
    else if ( class(Q) == "data.frame" )    { 
        Qnames = rownames(Q)
        item   = t(as.data.frame(item))
        colnames(item) = colnames(Q)
        if (ncol(item) == ncol(Q)) Q = rbind(Q, item ) 
        rownames(Q) = c(Qnames, ifelse( with_name=="", n+1, with_name) )
    }
    else                                    { 
        Qnames = names(Q)
        Q = append( Q, item )
        if( with_name != "" ) 
            names(Q) = c(Qnames, with_name)
    }
    return ( Q )
}
# ######################################################################################################


# ######################################################################################################
GET_SIZE = function( Q ) {
    if ( class(Q) == "list" )               { n = length(Q) }
    else if ( class(Q) == "matrix" )        { n = nrow(Q) }
    else if ( class(Q) == "data.frame" )    { n = nrow(Q) }
    else                                    { n = length(Q) }
    return ( n )
}
# ######################################################################################################


# ######################################################################################################
# LIFO ORDERING
# ######################################################################################################
POP = function( Q, ORDERING="LIFO" ) {
    n = GET_SIZE( Q )
    if ( identical( ORDERING, "LIFO" ) ) { TARGET=n; ORDERING = 1:(n-1) }
    if ( identical( ORDERING, "FIFO" ) ) { TARGET=1; ORDERING = 2:n }

    if ( class(Q) == "list" )               { 
        if( is.null(names(Q))) { 
            item   = Q[[TARGET]] 
            Q      = Q[ORDERING] 
        } else {
            Qnames = names(Q) 
            item   = Q[[Qnames[TARGET]]] 
            Q      = Q[ORDERING] 
        }
    }
    else if ( class(Q) == "matrix" )        { 
        if( is.null(rownames(Q))) {
            item = Q[TARGET,] 
            Q    = Q[ORDERING,] 
        } else {
            item = Q[rownames(Q)[TARGET],] 
            Q    = Q[rownames(Q)[ORDERING],] 
        }
    }
    else if ( class(Q) == "data.frame" )    { 
        if( is.null(rownames(Q))) {
            item = Q[TARGET,] 
            Q    = Q[ORDERING,] 
        } else {
            item = Q[rownames(Q)[TARGET],] 
            Q    = Q[rownames(Q)[ORDERING],] 
        }
    }
    else                                    { 
        if( is.null(names(Q))) { 
            item = Q[[TARGET]] 
            Q    = Q[ORDERING] 
        } else {
            Qnames=names(Q) 
            item = Q[[Qnames[TARGET]]] 
            Q    = Q[Qnames[ORDERING]] 
        }
    }
    return ( list( QUEUE=Q, ITEM=item ) )
}
# ######################################################################################################


# ######################################################################################################
AS_DATAFRAME = function( XX, with_name="NOTSPECIFIED" ) {
    if( class(XX)=="data.frame" )  XXnames= colnames(XX)
    else if( class(XX)=="matrix" ) XXnames= colnames(XX)
    else XXnames= c( with_name )
    N = GET_SIZE( XX )
    XX     = as.data.frame(cbind(XX,1:N))
    colnames(XX) = c(XXnames, "DUMMY") 
    return ( XX )
}
# ######################################################################################################


# ######################################################################################################
SLICE_DATAFRAME = function( XX, which_columns=1:ncol(XX) ) {
    return( as.data.frame(XX[,which_columns]) )
    if ( length(which_columns) == 1 )
        DF = XX[which_columns]
    else
        DF = XX[,which_columns]
    return ( DF )
}
# ######################################################################################################


# ######################################################################################################
CMAT_CONVERTER = function( cmat ) {
    cmat2 = MATRIX( 2,2 )
    for ( j in 1:ncol(cmat)) {
        for ( i in 1:nrow(cmat)) {
            cmat2[i,j] = cmat[i,j]
        }
    }
    cmat2 = data.frame('0'=cmat2[,1],'1'=cmat2[,2])
    rownames(cmat2)=c('0','1')
    colnames(cmat2)=c('0','1')
    return ( cmat2 )
}
# ######################################################################################################

# ######################################################################################################
GET_F_RATIO = function( cmat, beta=1.0 ) { 
    # need to correct for switched ordering using ordering above: here is given for: true then predict
    cmat = CMAT_CONVERTER(cmat)
    tp = cmat['1','1']
    fp = cmat['0','1']
    fn = cmat['1','0']
    tn = cmat['0','0']
    P = tp / ( tp + fp )
    R = tp / ( tp + fn )
    G = (1+beta^2) * P * R / ( (beta^2 * P) + R )
    retvals = G
    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
GET_FPR = function( cmat ) {
    cmat = CMAT_CONVERTER(cmat)
    tp = cmat['1','1']
    fp = cmat['0','1']
    fn = cmat['1','0']
    tn = cmat['0','0']
    return ( fp/(fp+tn) )
}
# ######################################################################################################


# ######################################################################################################
GET_PRECISION = function( cmat ) {
    cmat = CMAT_CONVERTER(cmat)
    tp = cmat['1','1']
    fp = cmat['0','1']
    fn = cmat['1','0']
    tn = cmat['0','0']
    return ( tp/(tp+fp) )
}
# ######################################################################################################


# ######################################################################################################
GET_RECALL = function( cmat ) {
    cmat = CMAT_CONVERTER(cmat)
    tp = cmat['1','1']
    fp = cmat['0','1']
    fn = cmat['1','0']
    tn = cmat['0','0']
    return ( tp/(tp+fn) )
}
# ######################################################################################################


# ######################################################################################################
GET_FNR = function( cmat ) {
    cmat = CMAT_CONVERTER(cmat)
    tp = cmat['1','1']
    fp = cmat['0','1']
    fn = cmat['1','0']
    tn = cmat['0','0']
    return ( fn/(fn+tp) )
}
# ######################################################################################################

# ######################################################################################################
GET_ACC = function( cmat, w=c(1.0,1.0) ) {
    cmat = CMAT_CONVERTER(cmat)
    tp = cmat['1','1']
    fp = cmat['0','1']
    fn = cmat['1','0']
    tn = cmat['0','0']
    return ( (tp*w[1] +tn*w[2])/ sum(cmat,na.rm=TRUE) )
}
# ######################################################################################################

# ######################################################################################################
GET_TPR = function( cmat ) {
    cmat = CMAT_CONVERTER(cmat)
    tp = cmat['1','1']
    fp = cmat['0','1']
    fn = cmat['1','0']
    tn = cmat['0','0']
    return ( tp / ( tp + fn ) )
}
# ######################################################################################################

# ######################################################################################################
GET_TNR = function( cmat ) {
    cmat = CMAT_CONVERTER(cmat)
    tp = cmat['1','1']
    fp = cmat['0','1']
    fn = cmat['1','0']
    tn = cmat['0','0']
    return ( tn / ( tn + fp ) )
}
# ######################################################################################################

