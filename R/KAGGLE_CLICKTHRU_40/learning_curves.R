
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


source( 'utilities.R' )
source( 'plot_functions.R' )


# #####################################################################################
GET_NUMBER_LABELS = function( yv ) {
    p = length(unique( yv ) )
    return( p-1 )
}
# #####################################################################################


# #####################################################################################
MY_DUMMY_CLASSIFIER = function( my_x, my_y, ... ) {
    params = list( ... )
    m = nrow( my_x )
    n = ncol( my_x )

    p = GET_NUMBER_LABELS( my_y )

    y_probas = runif( m, 0, p )
    yp = round(runif( m, 0, p )) 

    retvals = list( 'probas'=y_probas, 'true'=my_y, 'predict'=yp )
    return ( retvals )
}
# #####################################################################################


# #####################################################################################
COMPUTE_METRICS = function( x, y, yp ) {
    # f=table(yp,y)
    #   y
    #yp   0  1
    #  0 31 20
    #  1 27 22
    cmat = table(yp,y)
    tn = cmat[1,1]
    fn = cmat[1,2]
    fp = cmat[2,1]
    tp = cmat[2,2]
    P = tp / ( tp + fp )
    R = tp / ( tp + fn )
    retval = list( 'tp'=tp, 'tn'=tn, 'fp'=fp, 'fn'=fp )
    return ( retval )
}
# #####################################################################################


# #####################################################################################
GET_F_RATIO = function( cmat ) { 
    tp = cmat[1]
    tn = cmat[2]
    fp = cmat[3]
    fn = cmat[4]
    P = tp / ( tp + fp )
    R = tp / ( tp + fn )
    G = 1/2 * P * R / ( P + R )
    retvals = G
    return ( retvals )
}
# #####################################################################################


# #####################################################################################
# A PARTICULAR J COST FUNCTION
# #####################################################################################
COMPUTE_JCOST = function( x, ytrue, ypred ) {
    M = length(ypred)
    J = cumsum((ytrue-ypred)^2)[M]
    J = J/M
    return (J)
}
# #####################################################################################


# #####################################################################################
LEARNING_CURVE_ITERATOR = function( myclf, x=data.frame(), y=c(), NSTEPS=100, RANDOMIZE=FALSE, USE="JCOST", ... ) {
	START = TRUE
    M = nrow( x )
    y = data.frame(y)
    rownames(y) = rownames(x)

	for ( i in 1:NSTEPS ) {
        NSIZE = M * i/NSTEPS 
        if ( RANDOMIZE )
	        idx = sample( rownames(x), NSIZE )
        else
	        idx = c( 1: NSIZE )

	    xx = x[idx,]
	    yy = y[idx,]

	    retvals   = myclf( xx, yy, ... )
	    yy_pred   = retvals$'predict'
	    yy_probas = retvals$'probas'
        if ( USE == "JCOST" )
            acc_vals  = COMPUTE_JCOST( xx, yy, yy_probas )
        else 
	        acc_vals  = COMPUTE_METRICS( xx, yy, yy_pred ) 

	    if ( START ) {
	        CMAT_ROWS = data.frame( acc_vals ) 
	        START = FALSE
	    } else {
	        CMAT_ROWS = rbind( CMAT_ROWS, acc_vals )
	    }
	}
    return ( CMAT_ROWS )
}
# #####################################################################################









# #####################################################################################
# INPUTS
# #####################################################################################
    M    = 10000
    NSTEPS = ifelse( M/10<50, M/50, min(20, M/20))  
    USE_METRIC = "F_RATIO"
    X = data.frame( 'v1'=round(runif( M, 0, 1 )),  
                    'v2'=round(runif( M, 0, 1 )))
    Y = round(runif( M, 0, 1 )) 
# #####################################################################################


# #####################################################################################
# COMPUTE THE LEARNING CURVE FOR THIS PARTICULAR CLASSIFIER FUNCTION )
# #####################################################################################
INCREMENTAL_CMATS = LEARNING_CURVE_ITERATOR( MY_DUMMY_CLASSIFIER, x=X, y=Y, NSTEPS=NSTEPS, RANDOMIZE=FALSE, USE=USE_METRIC)


    # #####################################################################################
    SAMPLE_SIZES       = seq(M/NSTEPS, M, M/NSTEPS )
    # #####################################################################################


    # #####################################################################################
    if ( USE_METRIC == "JCOST" ) {
        JCOST_VALS = INCREMENTAL_CMATS[,1] 
        DO_PERFORMANCE_CURVE_PLOT( SAMPLE_SIZES, JCOST_VALS,         
                                   xlab="NUM. SAMPLES CONSIDERED", 
                                   ylab="J_COST()", 
                                   cex=0.8  )
        # scatter.smooth( SAMPLE_SIZES, JCOST_VALS )
    } else {
        F_RATIO_TIMESERIES = apply( INCREMENTAL_CMATS, 1, GET_F_RATIO )
        DO_PERFORMANCE_CURVE_PLOT( SAMPLE_SIZES, F_RATIO_TIMESERIES, 
                                   xlab="NUM. SAMPLES CONSIDERED", 
                                   ylab="F_RATIO=P*R/(P+R)", 
                                   cex=0.8  )
        # scatter.smooth( SAMPLE_SIZES, F_RATIO_TIMESERIES )
    }
    # #####################################################################################

    # #####################################################################################
    # if used as script, returns the incremental confusion tables
    # #####################################################################################
    str(INCREMENTAL_CMATS)
    print(INCREMENTAL_CMATS)
    INCREMENTAL_CMATS
