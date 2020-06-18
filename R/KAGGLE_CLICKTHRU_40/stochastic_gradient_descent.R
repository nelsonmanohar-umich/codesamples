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
OLD_CA = commandArgs()
commandArgs <- function() list(TEST_ENABLED=FALSE, DO_DISTANCE_VALIDATION=FALSE, RUN_VALIDATE_SGD=FALSE )
VERIFY_OPTARG ( OPTARG_NAME="RUN_VALIDATE_SGD", OPTARG_VALUE=TRUE )
# ######################################################################################################


# ######################################################################################################
source( 'utilities.R' )
source( 't_tests.R' )
source( 'fselect.R' )
source( 'datasets.R' )
source( 'regression.R' )
# ######################################################################################################


# ######################################################################################################
# TODO:
# hooks to simplify:
#   imbalance of classes (from data.R)? (subsample
# minibatch fixing? 
# trycatch on matrix vs. loop (if matrix fails)
# plotting periodically to file? for stochastic?
# gradient composer for linear regression
# gradient composer for logistic regression
# fminunc equivalent alternative
# conjugate descent and/or bjcs alternative
# formalized iteractor
# grid search
# coef plot and confidence intervals
# classifications and probabilities
# regression/classification performance plot
# feature selection on scaled train-data subset
# feature names dictionary
# confidence intervals and plots for regression and significant wrt anova
# feature generators for test data given selected features from training data (also for cross validation)
# pass percentage to dataset builder so that it picks the first rows for training
# select and store feature generators (interaction terms dictionary so that the indices can again be applied to  te cv and test
# ######################################################################################################


# ##################################################################################################
#  http://en.wikibooks.org/wiki/R_Programming/Optimization
# ##################################################################################################
myfunc <- function(x) { return ( (x-3)^3+(x-2)^2 ) }
FIND_MINIMUM_FOR_UNIVARIATE_FUNCTION = function( my_univariate_func=function(x) x, search_range=c(-1E24,1E24), PLOT=TRUE ) {
    obj_minimum = optimize( f=my_univariate_func, interval=search_range, maximum=FALSE )
    obj_maximum = optimize( f=my_univariate_func, interval=search_range, maximum=TRUE  )
    xmin = obj_minimum$minimum
    xmax = obj_maximum$maximum
    ymin = obj_minimum$objective
    ymax = obj_minimum$objective
        smin = search_range[1]
        smax = search_range[2]
        i = 0
        while( sign(ymin) == sign(ymax) ) {
            smin = smin-2*abs(smin)
            smax = smax+2*abs(smax)
            search_range=c(smin,smax)
            obj_minimum = optimize( f=my_univariate_func, interval=search_range, maximum=FALSE )
            obj_maximum = optimize( f=my_univariate_func, interval=search_range, maximum=TRUE  )
            xmin = obj_minimum$minimum
            xmax = obj_maximum$maximum
            ymin = obj_minimum$objective
            ymax = obj_maximum$objective
            print ( paste( i, smin, smax, ymin, ymax ) )
        }
        search_range = c( xmin, xmax )
        vunit = uniroot( f=my_univariate_func, interval=( search_range ))
        str(vunit)

    print( sprintf( "zero for the function= (%10.6g) reached at x=%10.6g", vunit$f.root, vunit$root ) )
    print( sprintf( "minimization objective (%10.6g) reached at x=%10.6g", ymin, xmin ) )
    print( sprintf( "maximization objective (%10.6g) reached at x=%10.6g", ymax, xmax ) )

    smin = min(smin,smax,xmin,xmax)
    smax = max(smin,smax,xmin,xmax)

    ylab = sprintf( "f(x)=%s",as.character( body( my_univariate_func ) ) )
    xlab = sprintf( "grid search [%.2g, %.2g]", smin, smax ) 
    ptitle = paste( "min, max, zero of f(x)\nwrt", xlab )

    delta = (smax-smin)/50
    grid <- seq(smin,smax, by=delta)
    my_univariate_func( grid )
    plot(grid, my_univariate_func(grid), t='b', pch="-", cex=1.0, xlab=xlab, ylab=ylab, main=ptitle)
    grid()

    tmin = sprintf( "%s:\nf(%.2g)", "MIN", xmin )
    tmax = sprintf( "%s:\nf(%.2g)", "MAX", xmax)
    tzer = sprintf( "%s:\nf(%.2g)", "ZERO", vunit$root )

    text( xmin, ymin, tmin, c="red",  cex=0.8 )
    text( xmax, ymax, tmax, c="red", cex=0.8 )
    text( vunit$root, vunit$f.root, tzer, c="red", cex=0.8 )

    retval = list( 'min'=obj_minimum, 'max'=obj_maximum, 'root'=vunit )
    return ( retval )
}
# ######################################################################################################


# ######################################################################################################
# FIND_MINIMUM_FOR_UNIVARIATE_FUNCTION( myfunc )
# ######################################################################################################


# ######################################################################################################
QUADRATIC_ERROR = function( h0x, y ) { (h0x - y)^2 }
# ######################################################################################################


# ######################################################################################################
APPLY_SCALING_TRANSFORM_TO = function( OBJ_TEST, SCALED_WRT=matrix(), do_center=TRUE, do_scale=TRUE ) {
    OBJ_TRAIN_SCALED = scale( SCALED_WRT, center=do_center, scale=do_scale )
    OBJ_TRAIN_SD_VECTOR = attr(OBJ_TRAIN_SCALED,"scaled:scale")
    OBJ_TRAIN_MU_VECTOR = attr(OBJ_TRAIN_SCALED,"scaled:center")

    if (identical(OBJ_TEST, SCALED_WRT ))
        OBJ_TRAIN_SCALED 
    else
        OBJ_TEST_SCALED = scale( OBJ_TEST, center=OBJ_TRAIN_MU_VECTOR, scale=OBJ_TRAIN_SD_VECTOR)
}
# ######################################################################################################


# ######################################################################################################
DO_BENCHMARK_COMPARISON = function( BENCHMARK_NAME, YB, BENCHMARK=c(), epsilon=5E-4, debug=FALSE ) {
    if ( debug ) {
        cat( HEADER )
        print( BENCHMARK_NAME ) 
        cat( HEADER )
    }

    YB = as.matrix(YB)
    B  = as.matrix(BENCHMARK)
    indices = 1:nrow(YB)
    if ( nrow(YB) > 5E3 ) indices = sample( 1:nrow(YB), min( 2E3, nrow(YB) ) )
    m = length(indices)

    DIFF   = YB[indices] - B[indices]
    ci_lo  = mean(DIFF) - sd(DIFF)
    ci_hi  = mean(DIFF) + sd(DIFF)
    errors = which( ci_lo < DIFF || DIFF > ci_hi )
    cnt    = length(errors)
    dev    = sum(DIFF[errors])^2

    if ( debug ) { 
        cat( HEADER )
        print( sprintf( "%8s %18s %18s %18s", "i", "Y[i]", "B[i]", "Y[i]-B[i]" ))
        cat( HEADER )
        for ( i in errors )
            print( sprintf( "%8d %18.4g %18.4g %18.4g", i, YB[i], B[i], DIFF[i] ))
        cat( HEADER )
        if ( cnt >= as.integer(m)*.01)  str( retvals )
        cat( HEADER )
    }

    ci = sprintf( "[%.4g,%.4g]", ci_lo, ci_hi )
    retvals = list( 'n_incompatible'=cnt, 'benchmark_err'=sqrt(dev), 'benchmark_mse'=dev/nrow(YB), 'ci'=ci )

    print ( sprintf( "BENCHMARK [%32s] FOUND: WRT 2-SIGMA C.I. %20s, A %6.2f%% AGREEMENT IS ACHIEVED (W/ %s:%s RANDOM SAMPLES)", 
                      BENCHMARK_NAME, ci, (1-cnt/m)*100, m, nrow(YB) ) )

    return (retvals)
}
# ######################################################################################################


# ######################################################################################################
GET_JCOST = function( Xx, Yx, theta, regularization_lambda=0.1, debug=FALSE ) {
    m = nrow(Xx)
    n = ncol(Xx)

    # H0X = as.matrix(t(theta %*% t(X)))

    # FIT_COST = 0
    # for( i in 1:m ) {
         # xterm = theta %*% Xx[i,] - Yx[i]
         # FIT_COST = FIT_COST + ( xterm^2 )
    # }
    FIT_COST = sum((t( theta %*% t(Xx) ) - Yx )^2)/(2*m)
    REG_COST = regularization_lambda/(2*m) * as.numeric(t(theta[2:n]) %*% theta[2:n])/(n-1)
    JCOST    = FIT_COST + REG_COST

    # retvals = c( 'JCOST'=JCOST, 'REG_COST'=REG_COST, 'FIT_COST'=FIT_COST )

    return ( JCOST )
}
# ######################################################################################################


# ######################################################################################################
GET_SGD_NEXT_THETA = function( idx, GIVEN_THETA_OLD, X, Y, mini_batch=c(), alpha=1E-3, regparam=1E-3, debug=FALSE) {
    m = nrow(X)
    X_idx = as.matrix(X[,idx])

    indices = 1:m
    if ( length(mini_batch) != 0  ) indices = mini_batch

    # h0x = 0
    # hdiff = 0
    # for( i in indices ) {
         # xterm = GIVEN_THETA_OLD %*% X[i,] - Y[i]
         # h0x = h0x + ( xterm ) * X[i,idx]
         # hdiff = hdiff + xterm
    # }
    COST_VECTOR = t(GIVEN_THETA_OLD %*% t(X)) - Y
    hdiff = sum ( COST_VECTOR )                       # hdiff = sum (t(GIVEN_THETA_OLD %*% t(X)) - Y)
    h0x   = sum ( COST_VECTOR * X[,idx]  )            # h0x   = sum((t(GIVEN_THETA_OLD %*% t(X)) - Y) * X[,idx])

    UPDATE = ( 1 - alpha/m * regparam )
    if ( idx == 1 ) {
        SGD_NG_THETA_NEW = as.numeric(UPDATE * as.numeric(GIVEN_THETA_OLD[idx]) - alpha/m * hdiff)
    } else {
        SGD_NG_THETA_NEW = as.numeric(UPDATE * as.numeric(GIVEN_THETA_OLD[idx]) - alpha/m * h0x)
    }

    if ( debug ) print( sprintf( "%4d %16.10f %16.10f", idx, GIVEN_THETA_OLD[idx], SGD_NG_THETA_NEW  ))

    return ( SGD_NG_THETA_NEW )
}
# ######################################################################################################


# ######################################################################################################
GET_INITIAL_THETA = function( n ) {
    theta = as.matrix(rnorm( n+1, 1E-8, 1E-4 ))
}
# ######################################################################################################


# ######################################################################################################
DO_LEARNING_RATE_RESET = function( learning_rate, sx=10, debug=TRUE ) {
    new_learning_rate = learning_rate/sx
    if ( debug ) print( paste( "STATUS: LEARNING RATE AND THETA,", sx, "RESET", learning_rate, "-->", new_learning_rate ) )
    return ( new_learning_rate )
}
# ######################################################################################################


# ######################################################################################################
APPLY_TERMINATION_CRITERIA = function( JCOST_OLD, DELTA_JCOST, MEAN_JCOST, epsilon, stabilization_level, learning_rate, iter, imax, imin ) {
    sx_change    = 1
    WHY          = ""
    END          = FALSE

        if ( JCOST_OLD > 1E2 && DELTA_JCOST > 1E1 ) {
            END=FALSE
            WHY = sprintf( 'STATUS: JCOST IS DIVERGING, DECREASING ALPHA: (%.2g <= %.2g) AT ITER %d', JCOST_OLD, learning_rate, iter )
            if( JCOST_OLD > 1E3 || DELTA_JCOST>1E3 ) {
                sx_change = 10
            } else {
                if( JCOST_OLD > 1E2 || DELTA_JCOST>1E1 ) {
                    sx_change = 2
                } else
                    if( JCOST_OLD > 1E-1 ) {
                        WHY = sprintf( 'STATUS: JCOST STILL DIVERGING, INCREASING ALPHA: (%.2g <= %.2g) AT ITER %d', JCOST_OLD, learning_rate, iter )
                        sx_change = 0.25
                    }
            }
            print( WHY )
        } else
            if ( is.na( JCOST_OLD ) | JCOST_OLD > 1E6) {
                END=TRUE
                WHY = sprintf( 'STATUS: JCOST IS TOO LARGE, DECREASING ALPHA: (%.2g <= %.2g) AT ITER %d', JCOST_OLD, learning_rate, iter )
                sx_change = 100
                print( WHY )
            }

        if ( iter>=imax ) {
            END=TRUE
            WHY = sprintf( 'STATUS: MAXIMUM NUMBER ITERATIONS EXCEEDED: (%.2g <= %.2g)', iter, imax)
            print( WHY )
        }

        if ( iter>=imin && JCOST_OLD <= epsilon && DELTA_JCOST <= 1E-6 ) {
            END=TRUE
            WHY = sprintf( 'STATUS: EPSILON GOAL ACHIEVED (DELTA:%.2g <= %.2g) AND (MEAN:%.2g <= %.2g)', JCOST_OLD, epsilon, DELTA_JCOST, epsilon )
            print( WHY )
        } 

        if ( iter>=imin && MEAN_JCOST<= stabilization_level ) {
            END=TRUE
            WHY = sprintf( 'STATUS: JCOST STABILIZATION LEVEL ACHIEVED (%.2g <= %.2g)', MEAN_JCOST, stabilization_level )
            print( WHY )
        }

    why_change   = ifelse( WHY!="",       TRUE, FALSE )
    theta_change = ifelse( sx_change!=1,  TRUE, FALSE )
    retvals = list( 'Theta'=theta_change, 'sx'=sx_change, 'why'=why_change, 'WHY'=WHY, 'END'=END )
    return( retvals )
}
# ######################################################################################################


# ######################################################################################################
DO_STOCHASTIC_GRADIENT_DESCENT = function ( Xdf, Y, theta=c(), epsilon=1E-3, learning_rate=0.1, regularization_parameter=1E-3, stabilization_level=1E-4, imax=1000, enable_mini_batch=FALSE, mini_batch_size=256, do_scale=FALSE, imin=10, do_plot=TRUE, debug=FALSE) {
    if ( debug )  {
        PARAMS = list(theta=theta, 
                    epsilon=epsilon, 
                    learning_rate=learning_rate, 
                    regularization_parameter=regularization_parameter, 
                    stabilization_level=stabilization_level, 
                    imax=imax, 
                    enable_mini_batch=enable_mini_batch, 
                    mini_batch_size=mini_batch_size, 
                    do_scale=do_scale,
                    imin=imin,
                    do_plot=do_plot,
                    debug=debug )
        PRINT_PARAMS( PARAMS )
    }

    m = nrow(Xdf)
    n = ncol(Xdf)
    X = GET_PLUS1_MATRIX_FROM( Xdf, do_scale=do_scale )

    if ( do_scale ) Y = as.matrix( scale(Y, center=TRUE, scale=TRUE ) )

    if ( length(theta) == 0 )  {
            theta = as.matrix(rep(0,n+1))
            theta = GET_INITIAL_THETA( n )
    }

    iter = 1

    RESETS = c()
    THETA_OLD = t(as.matrix(theta))
    JCOST_OLD = GET_JCOST( X, Y, THETA_OLD, regularization_parameter, debug=FALSE )
    JCOST_VALUES = data.frame( "iter"=iter, "JCOST"=JCOST_OLD, "eps"=epsilon, "alpha"=learning_rate, 'theta'=THETA_OLD )
    END=FALSE
    WHY="STATUS: EXCEPTION CONDITION"
    mini_batch = c()

    while( !END ) {
        iter = iter + 1
        if ( enable_mini_batch ) mini_batch = sample( 1:m, min(mini_batch_size,m))

        THETA_NEW_TEMP =c() 
        for( j in 1:(n+1) ) { 
            idx = j
            THETA_NEW_TEMP = append( THETA_NEW_TEMP, GET_SGD_NEXT_THETA( idx, 
                                                                        THETA_OLD, 
                                                                        X, Y,
                                                                        mini_batch=mini_batch,
                                                                        alpha=learning_rate, regparam=regularization_parameter ) )
        }

        THETA_OLD = t(as.matrix(THETA_NEW_TEMP))
        JCOST_OLD = GET_JCOST( X, Y, THETA_OLD, regularization_parameter, debug=FALSE )
        JCOST_VALUES = rbind( JCOST_VALUES, data.frame("iter"=iter, "JCOST"=JCOST_OLD, "eps"=epsilon, "alpha"=learning_rate, 'theta'=THETA_OLD))

        DELTA_JCOST = JCOST_VALUES[iter,2] - JCOST_VALUES[iter-1,2]
        JCOST_DIFFS = JCOST_VALUES[c(2:iter),2]-JCOST_VALUES[c(1:iter-1),2]
        MEAN_JCOST = abs(median(JCOST_DIFFS))

        if ( debug ) {
            cat(SUBHEADER)
            print( JCOST_VALUES[iter,] )
            print( JCOST_DIFFS )
            print( MEAN_JCOST )
            cat(SUBHEADER)
        }

        term_rvals = APPLY_TERMINATION_CRITERIA( JCOST_OLD, DELTA_JCOST, MEAN_JCOST, epsilon, stabilization_level, learning_rate, iter, imax, imin )

        # process termination criteria findings
        if ( term_rvals$Theta )   
            THETA_OLD = GET_INITIAL_THETA( n )           # THETA_OLD = t(as.matrix(rnorm( n+1, 1E-6, 1E-3 )))
        if ( term_rvals$sx != 1 ) {
            learning_rate = DO_LEARNING_RATE_RESET( learning_rate, sx=term_rvals$sx, debug=TRUE )
            RESETS = append(RESETS,iter)
        }
        if ( term_rvals$why )     WHY = term_rvals$WHY
        END = term_rvals$END
    }

    retvals = list( 'jcost_matrix'=JCOST_VALUES, 'exit_condition'=WHY, 'final_theta'=THETA_OLD, 'mse'=JCOST_OLD*2, 'theta'=THETA_OLD )

    graphics.off()
    if ( do_plot ) {
        do_pdf = TRUE
        #mar	numerical vector indicating margin size c(bottom, left, top, right) in lines. default = c(5, 4, 4, 2) + 0.1
        #mai	numerical vector indicating margin size c(bottom, left, top, right) in inches
        #pin	plot dimensions (width, height) in inches
        #op <- par(mfrow=c(p,p), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0), pin=c(16,16))
        if( do_pdf ) { pdf("learning_curve_and_coefficient_plots.pdf") ; print( "PLOT BEING GENERATED TO FILE" ); cat(HEADER); cat(HEADER) } 
	    p = as.integer(sqrt(n+2))+1
        ip = 0
        op <- par(mfcol=c(3,3))
	        plot( JCOST_VALUES[,1], JCOST_VALUES[,2], type='b', pch=24, bg="red", cex=0.6, main="JCOST(THETA) AT EACH ITERATION" )
	        for( ix in RESETS ) text( ix+0.25, JCOST_VALUES[ix,2], sprintf("RESET=\n(%g->%g)",JCOST_VALUES[ix,4],JCOST_VALUES[(ix+1),4]), cex=0.8 )
	        grid()
	    
	        ixs = c(1:iter)
	        if (length(RESETS)!=0) ixs = c(max(RESETS):iter)
	        for ( k in c(5:(n+5) )) {
                DO_THETA_COEFF_PLOT( JCOST_VALUES, ixs, k, iter, THETA_OLD )
	            # plot( JCOST_VALUES[ixs,1], JCOST_VALUES[ixs,k], type='p', pch=24, bg="brown", cex=0.6, main=sprintf("THETA %s AT EACH ITERATION", k-5) )
	            # text( JCOST_VALUES[iter-1,1], JCOST_VALUES[iter-1,k], sprintf("THETA[%s]=\n%.4g", k-5, THETA_OLD[k-4]), cex=1.0)
	            # grid()
	        }

	        plot( sqrt(JCOST_VALUES[ixs,2]), JCOST_VALUES[ixs,2], type='b', pch=24, bg="red", cex=0.6, main="JCOST TRAJECTORY" )
	        grid()
	
	    par(op)
        if( do_pdf ) { dev.off(); graphics.off() }

        # plot a brief summary of the SGD to the screen
        op <- par(mfrow=c(2,2))
	        plot( JCOST_VALUES[,1], JCOST_VALUES[,2], type='b', pch=24, bg="red", cex=0.6, main="JCOST(THETA) AT EACH ITERATION" ); grid()
	        for( ix in RESETS ) text( ix+0.25, JCOST_VALUES[ix,2], sprintf("RESET=\n(%g->%g)",JCOST_VALUES[ix,4],JCOST_VALUES[(ix+1),4]), cex=0.8 )
	        plot( sqrt(JCOST_VALUES[ixs,2]), JCOST_VALUES[ixs,2], type='b', pch=24, bg="red", cex=0.6, main="JCOST TRAJECTORY" );   grid()
            DO_THETA_COEFF_PLOT( JCOST_VALUES, ixs, 5,   iter, THETA_OLD )
            DO_THETA_COEFF_PLOT( JCOST_VALUES, ixs, n+5, iter, THETA_OLD )
	    par(op)
    }

    if ( debug ) {
        cat(HEADER)
        print( str(JCOST_VALUES[iter,]) )
        cat(HEADER)
        print( WHY )
        cat(HEADER)
    }

    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
DO_THETA_COEFF_PLOT = function( JCOST_VALUES, ixs, k, iter, THETA_OLD ) {
	            plot( JCOST_VALUES[ixs,1], JCOST_VALUES[ixs,k], type='p', pch=24, bg="brown", cex=0.6, main=sprintf("THETA %s AT EACH ITERATION", k-5) )
	            text( JCOST_VALUES[iter-1,1], JCOST_VALUES[iter-1,k], sprintf("THETA[%s]=\n%.4g", k-5, THETA_OLD[k-4]), cex=1.0)
	            grid()
}
# ######################################################################################################


# ######################################################################################################
PRINT_PARAMS = function( ... ) {
    params = list( ... )
    NEWLINE(5)
    cat(HEADER)
    for ( i in 1:length(params) ) print( sprintf( "%02d %32s = %32s", i, names(params[[i]]), params[[i]] ) )
    cat(HEADER)
    NEWLINE(1)
}
# ######################################################################################################


# ######################################################################################################
# ######################################################################################################
SGD_ITERATOR = function( X, Y, learning_rate=5E-2, regularization_parameter=1E-3, debug=TRUE, ... ) {
    m = nrow(X)
    RETVALS = DO_STOCHASTIC_GRADIENT_DESCENT(X, Y, 
                                             epsilon=min(1/(2*m),5E-5),                 # what is a small individual jcost
                                             learning_rate=1E-1,                        # alpha: strength of the adaptation effort
                                             regularization_parameter=1E-3,             # regularization cost
                                             stabilization_level=min(1/(2*m),5E-7),     # what is a small jcost tendency (median of differences) 
                                             imax=1E3,                                  # how many iterations in degenerate case
                                             enable_mini_batch=TRUE,                    # whether subsampling used
                                             mini_batch_size=1024,                      # size of the subsampling
                                             do_plot=TRUE,                              # plot coefficient plots and jcost
                                             imin=100, ... )                            # num. iterations to wait before end criteria applied 

    retvals = RETRIEVE_SGD_RETVALS( RETVALS, debug=TRUE )
    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
# RETVALS = list( 'jcost_matrix'=JCOST_VALUES, 'exit_condition'=WHY, 'final_theta'=THETA_OLD, 'mse'=JCOST_OLD*2, 'theta'=THETA_OLD )
# ######################################################################################################
RETRIEVE_SGD_RETVALS = function( RETVALS, debug=TRUE ) { 
    JCOST_VALUES = RETVALS$jcost_matrix
    SGD_THETA    = t(RETVALS$theta)                # SGD_THETA = t(as.matrix(JCOST_VALUES[nrow(JCOST_VALUES),5:ncol(JCOST_VALUES)]))
    SGD_MSE      = as.numeric(RETVALS$mse)         # SGD_MSE   = JCOST_VALUES[nrow(JCOST_VALUES),2]*2.0

    if ( debug ) 
        str( RETVALS )

    colnames(SGD_THETA) = "THETA"

    retvals = list( 'JCOST_VALUES'=JCOST_VALUES, 'OPT_THETA'=SGD_THETA, 'MSE'=SGD_MSE )

    return( retvals )
}
# ######################################################################################################


# ######################################################################################################
    # FIXME: needs to scale testing data splt wrt scaes using for train
    #X   = GET_PLUS1_MATRIX_FROM( X, do_scale=TRUE )
    #Y   = as.matrix( scale(Y, center=TRUE, scale=TRUE ) )
    #SGD_THETA = t(as.matrix(SGD[nrow(SGD),5:ncol(SGD)]))
    #YP  = PREDICT_LINEAR_REGRESSION_MODEL_WITH( SGD_THETA, X, Y )
    #MSE = GET_MSE_FOR(Y, YP)
# ######################################################################################################


# ######################################################################################################
TRAIN_LINEAR_REGRESSION_MODEL_WITH   = function( Xtrain_, Ytrain_, debug=FALSE ) {
}
# ######################################################################################################


# ######################################################################################################
PREDICT_LINEAR_REGRESSION_MODEL_WITH = function( theta, X_test, Y_test=c(), debug=FALSE ) {
    ONES   = as.matrix(rep(as.numeric(1),nrow(X_test)))
    X_test = cbind( ONES, X_test)

    theta  = t(as.matrix(theta))
    #X_test = as.matrix(X_test)
    Y_test = as.matrix(Y_test)

    Y_predict = t(theta %*% t(X_test))

    Y_predict_ = c()
    for ( ix in 1:nrow(X_test) ) {
        yp = as.numeric(theta %*% as.matrix(X_test[ix,]))
        Y_predict_ = append( Y_predict_, yp )
    }
    Y_predict_ = as.matrix(Y_predict_)

    if ( any( Y_predict-Y_predict_ > 1E-10 ) ) print( paste( Y_predict-Y_predict_, Y_predict, Y_predict_ ) )

    if ( debug ) {
        cat( SUBHEADER )
        str(theta)
        str(Y_test_)
        str(Y_predict_)
        MSE = GET_MSE_FOR( Y_test, Y_predict )
        print( sprintf( "MSE = %16.8f", MSE ) ) 
        cat( SUBHEADER )
    }

    return ( Y_predict )
}
# ######################################################################################################


# ######################################################################################################
GET_MSE_FOR = function( Y_, YP_ ) { sum((as.matrix(YP_) - as.matrix(Y_))^2)/nrow(as.matrix(Y_)) }
# ######################################################################################################


# ######################################################################################################
GET_PLUS1_MATRIX_FROM = function( Xdf, do_scale=FALSE ) {
    if ( class(Xdf) == "data.frame" )
        Xdm = as.matrix(DF_AS_MATRIX(Xdf))
    else 
        Xdm = Xdf

    if ( do_scale ) Xdm = scale(Xdm, center=TRUE, scale=TRUE )

    ONES = as.matrix(rep(as.numeric(1.0),nrow(Xdm)))
    Xdm    = as.matrix( cbind( ONES, Xdm ) )
    VERIFY( "INPUT X", Xdm, debug=FALSE)

    return ( Xdm )
}
# ######################################################################################################


# ######################################################################################################
DO_NORMAL_EQUATIONS = function (Xdf, Y, w_intercept=TRUE, do_scale=FALSE, regularization_parameter=0, debug=TRUE ) {
    if ( w_intercept )
        Xdm = GET_PLUS1_MATRIX_FROM( Xdf, do_scale=do_scale )
    else
        Xdm = as.matrix(DF_AS_MATRIX(Xdf))

    if ( do_scale ) Y = as.matrix( scale(Y, center=TRUE, scale=TRUE ) )

    m = nrow(Xdm)
    n = ncol(Xdm)

    REGULARIZATION_DIAG = regularization_parameter * diag( 1.0, n, n )
    OPT_THETA = solve(t(Xdm) %*% Xdm + REGULARIZATION_DIAG) %*% t(Xdm) %*% Y

    colnames(OPT_THETA) = "THETA"

    retvals = list( 'theta'=OPT_THETA )

    if ( debug ) {
        NEWLINE(5)
        cat( HEADER )
        print( "NORMAL_EQUATIONS YIELD" )
        cat( HEADER )
        print( retvals )
        cat( HEADER )
        NEWLINE(1)
    }

    return( retvals )
}
# ######################################################################################################


# ######################################################################################################
VALIDATE_SGD_WITH_NEQS = function( SGD_THETA, NEQ_THETA=c(), X_SCALED, Y_SCALED, MODE="TRAIN", agreement_eps=5E-4 ) {
    SGD_Y_PRED = PREDICT_LINEAR_REGRESSION_MODEL_WITH( SGD_THETA, X_SCALED, Y_SCALED )
    NEQ_Y_PRED = PREDICT_LINEAR_REGRESSION_MODEL_WITH( NEQ_THETA, X_SCALED, Y_SCALED )

    SGD_MSE   = GET_MSE_FOR(Y_SCALED, SGD_Y_PRED)  # SGD_MSE  = SGD$MSE
    NEQ_MSE   = GET_MSE_FOR(Y_SCALED, NEQ_Y_PRED)

    NEQ_L2 = as.numeric(t(NEQ_THETA)%*%NEQ_THETA)
    SGD_L2 = as.numeric(t(SGD_THETA)%*%SGD_THETA)

    NEWLINE(3); cat(HEADER)
    BENCHMARK_TRAIN = DO_BENCHMARK_COMPARISON( paste("SGD VS NEQ  LABELS WRT",MODE), SGD_Y_PRED, BENCHMARK=NEQ_Y_PRED )
    BENCHMARK_SGD   = DO_BENCHMARK_COMPARISON( paste("SGD VS TRUE LABELS WRT",MODE), SGD_Y_PRED, BENCHMARK=Y_SCALED   )
    BENCHMARK_NEQ   = DO_BENCHMARK_COMPARISON( paste("NEQ VS TRUE LABELS WRT",MODE), NEQ_Y_PRED, BENCHMARK=Y_SCALED   )

    retvals = list( 'SGD_MSE'=SGD_MSE,             'NEQ_MSE'=NEQ_MSE, 
                    'SGD_L2'=SGD_L2,               'NEQ_L2'=NEQ_L2, 
                    'SGD_NEQ_BENCHMARK'=BENCHMARK_TRAIN, 
                    'SGD_BENCHMARK'    =BENCHMARK_SGD, 
                    'NEQ_BENCHMARK'    =BENCHMARK_NEQ )

    if ( abs(SGD_L2-NEQ_L2)>1E-1 ) {
        cat(HEADER)
        str( retvals )
    }

    return( retvals )
}    
# ######################################################################################################


# ######################################################################################################
# UNCONSTRAINED MINIMIZATION
# ######################################################################################################
DO_FMINUNC = function( X, Y ) {
    theta = GET_INITIAL_THETA(ncol(X)) 

    NEWLINE(2)
    cat( HEADER )
    print( "FMINUNC VIA OPTIM W/ BFGS" )
    cat( HEADER )

    Xx    = GET_PLUS1_MATRIX_FROM( X, do_scale=FALSE )
    Yx    = Y


    OPTIM = function( theta ) {
        GET_JCOST( Xx, Yx, t(theta) ) 
    }

    GRADIENT = function( theta ) {
        t = c()
        for ( i in 1:length(theta) ) {
            ti = GET_SGD_NEXT_THETA( i, theta, Xx, Yx )
            t = append( t, ti )
        }
        theta <<- t
        return ( t )
    }

    WO_GR  = optim( theta, OPTIM, gr=NULL, method="BFGS" )
    retvals = list( 'WO_GR'=WO_GR )
    print( retvals )
    cat( HEADER )

    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
DO_CONJUGATE_DESCENT = function(X, Y ) { return ( DO_FMINUNC( X, Y ) ) }
# ######################################################################################################


# ######################################################################################################
GET_SGD_THETA = function( SGD ) {
    SGD_THETA = SGD$OPT_THETA ; colnames(SGD_THETA)   = "THETA"
    return (SGD_THETA) 
}
# ######################################################################################################


# ######################################################################################################
GET_NEQ_THETA = function( NEQ ) {
    NEQ_THETA = NEQ$theta
}
# ######################################################################################################


# ######################################################################################################
GET_FMU_THETA = function( FMU ) {
    FMU_THETA = FMU$WO_GR$par; colnames(FMU_THETA) = "THETA"
    return( FMU_THETA )
}
# ######################################################################################################



# ######################################################################################################
VERIFY_AND_VALIDATE_SGD = function( SYNTHETIC_TEST=FALSE, REGULARIZATION_STRENGTH=1E-3 ) {
    # ######################################################################################################
    METRICS = INIT_METRICS()
    # ######################################################################################################

    # ######################################################################################################
    SYNTHETIC_TEST          = FALSE
    REGULARIZATION_STRENGTH = 1E-6
    # ######################################################################################################

    # ######################################################################################################
    # BOSTON DATASET (TOO SMALL FOR ANYTHING?)
    # ######################################################################################################
    if ( !SYNTHETIC_TEST ) {
        BMLEVEL   = 2E-1
        DATA_RETVALS = BUILD_BOSTONHOUSING_DATASET(pix=14, do_log=TRUE, DO_FSELECTION=TRUE, DO_FEATURE_EXPLORATION=TRUE ) 
        X = DATA_RETVALS$X
        Y = DATA_RETVALS$Y
            FINAL_MAPPING     = DATA_RETVALS$FINAL_MAPPING
            SELECTED_FEATURES = DATA_RETVALS$SELECTED_FEATURES
    }
    # ######################################################################################################

    # ######################################################################################################
    # SYNTHETIC TEST
    # ######################################################################################################
    if ( SYNTHETIC_TEST ) {
        BMLEVEL   = 5E-2
        DATA_RETVALS = BUILD_SYNTHETIC_DATASET( DO_FSELECTION=TRUE, DO_FEATURE_EXPLORATION=TRUE )
        X = DATA_RETVALS$X
        Y = DATA_RETVALS$Y
            FINAL_MAPPING     = DATA_RETVALS$FINAL_MAPPING
            SELECTED_FEATURES = DATA_RETVALS$SELECTED_FEATURES
    }
    # ######################################################################################################

    # ######################################################################################################
    # FEEDBACK ABOUT THE DATA WE ARE OPERATING ON
    # ######################################################################################################
    # MDF_STATS ( X ); 
    # str( DATA_RETVALS )
    # ######################################################################################################

    # ######################################################################################################
    METRICS = TIMESTAMP( "FSELECTION" )
    # ######################################################################################################
    
    # ######################################################################################################
    M       = nrow(X)
    N       = ncol(X)
    NC      = unique(Y)
    NR      = nrow(X)
    # ######################################################################################################

    # ######################################################################################################
    N_TRAIN = as.integer(0.60 * NR)
    N_CROSS = as.integer(0.15 * NR)
    N_TEST  = as.integer(0.25 * NR)
    # ######################################################################################################

    # ######################################################################################################
    shuffled_samples = sample( 1:nrow(X) )
    # ######################################################################################################

    # ######################################################################################################
    TRAIN_SAMPLES = shuffled_samples[1:N_TRAIN]
    CROSS_SAMPLES = shuffled_samples[(N_TRAIN+1):(N_TRAIN+N_CROSS)]
    TEST_SAMPLES  = shuffled_samples[(N_TRAIN+N_CROSS+1):M]
    # ######################################################################################################

    # ######################################################################################################
    X_TRAIN = X[TRAIN_SAMPLES,]  ; Y_TRAIN = Y[TRAIN_SAMPLES]
    X_CROSS = X[CROSS_SAMPLES,]  ; Y_CROSS = Y[CROSS_SAMPLES]
    X_TEST  = X[TEST_SAMPLES,]   ; Y_TEST  = Y[TEST_SAMPLES]
    # ######################################################################################################

    # ######################################################################################################
    X_TRAIN_SCALED = APPLY_SCALING_TRANSFORM_TO( X_TRAIN, SCALED_WRT=X_TRAIN )
    X_CROSS_SCALED = APPLY_SCALING_TRANSFORM_TO( X_CROSS, SCALED_WRT=X_TRAIN )
    X_TEST_SCALED  = APPLY_SCALING_TRANSFORM_TO( X_TEST,  SCALED_WRT=X_TRAIN )
    # ######################################################################################################

    # ######################################################################################################
    Y_TRAIN_SCALED = APPLY_SCALING_TRANSFORM_TO( Y_TRAIN, SCALED_WRT=Y_TRAIN )
    Y_CROSS_SCALED = APPLY_SCALING_TRANSFORM_TO( Y_CROSS, SCALED_WRT=Y_TRAIN )
    Y_TEST_SCALED  = APPLY_SCALING_TRANSFORM_TO( Y_TEST,  SCALED_WRT=Y_TRAIN )
    # ######################################################################################################

    # ######################################################################################################
    # NEQ TRAINING 
    # ######################################################################################################
    NEQ     = DO_NORMAL_EQUATIONS( X_TRAIN_SCALED, Y_TRAIN_SCALED, regularization_parameter=REGULARIZATION_STRENGTH, do_scale=FALSE )
    METRICS = TIMESTAMP( "NEQ" )
    # ######################################################################################################


    # ######################################################################################################
    # FMINUNC OPTIMIZATION SEARCH
    # ######################################################################################################
    FMINUNC = DO_FMINUNC ( X_TRAIN_SCALED, Y_TRAIN_SCALED )
    METRICS = TIMESTAMP( "FMINUNC" )
    # ######################################################################################################


    # ######################################################################################################
    # SGD TRAINING 
    # SGD = SGD_ITERATOR( X_TRAIN_SCALED, Y_TRAIN_SCALED, regularization_parameter=1E-3, do_scale=FALSE )
    # ######################################################################################################
    RETVALS = DO_STOCHASTIC_GRADIENT_DESCENT(X_TRAIN_SCALED, Y_TRAIN_SCALED, 
                                             epsilon=min(1/(2*M),5E-5),                 # what is a small individual jcost
                                             learning_rate=1E-1,                        # alpha: strength of the adaptation effort
                                             regularization_parameter=REGULARIZATION_STRENGTH,   # regularization cost
                                             stabilization_level=min(1/(2*M),5E-7),     # what is a small jcost tendency (median of differences) 
                                             imax=5E3,                                  # how many iterations in degenerate case
                                             enable_mini_batch=FALSE,                   # whether subsampling used
                                             mini_batch_size=1024,                      # size of the subsampling
                                             do_plot=TRUE,                              # plot coefficient plots and jcost
                                             imin=500 )                                 # num. iterations to wait before end criteria applied 
    SGD     = RETRIEVE_SGD_RETVALS( RETVALS, debug=TRUE )
    METRICS = TIMESTAMP( "SGD" )
    # ######################################################################################################

    # ######################################################################################################
    NEQ_THETA = GET_NEQ_THETA( NEQ )                    # NEQ$theta
    SGD_THETA = GET_SGD_THETA( SGD )                    # SGD$OPT_THETA ; colnames(SGD_THETA)   = "THETA"
    FMU_THETA = GET_FMU_THETA( FMINUNC )                # FMINUNC$WO_GR$par; colnames(FMU_THETA) = "THETA"
    # ######################################################################################################

    # ######################################################################################################
    # HOW TO OBTAIN THE Y_PREDICTED VALUES YP=t(THETA)*X 
    # ######################################################################################################
    # NEQ_Y_TEST = PREDICT_LINEAR_REGRESSION_MODEL_WITH( NEQ_THETA, X_TEST_SCALED, Y_TEST_SCALED, debug=FALSE )
    # ######################################################################################################

    # ######################################################################################################
    # VALIDATE THE FINDINGS OF SGD AGAINST NORMAL EQUATIONS OPTIMAL THETA FOR TRAINING DATA
    # ######################################################################################################
    T_RETVALS = VALIDATE_SGD_WITH_NEQS( SGD_THETA, NEQ_THETA=NEQ_THETA, X_TRAIN_SCALED,Y_TRAIN_SCALED,MODE="TRAINING",agreement_eps=BMLEVEL )
    C_RETVALS = VALIDATE_SGD_WITH_NEQS( SGD_THETA, NEQ_THETA=NEQ_THETA, X_CROSS_SCALED,Y_CROSS_SCALED,MODE="CROSSVAL",agreement_eps=BMLEVEL )
    P_RETVALS = VALIDATE_SGD_WITH_NEQS( SGD_THETA, NEQ_THETA=NEQ_THETA, X_TEST_SCALED, Y_TEST_SCALED, MODE="TESTING ",agreement_eps=BMLEVEL )
    # ######################################################################################################

    # ######################################################################################################
    # PRELIMINARY DIAGNOSTICS
    # ######################################################################################################
    NEWLINE(3); cat(HEADER)
    SGD_Y_TEST = PREDICT_LINEAR_REGRESSION_MODEL_WITH( SGD_THETA, X_TEST_SCALED, Y_TEST_SCALED, debug=FALSE )
    FIT        = DO_REGRESSION_DIAGNOSTICS( X_TEST_SCALED, Y_TEST_SCALED, SGD_Y_TEST )
    METRICS    = TIMESTAMP ('DIAGNOSTICS')
    # ######################################################################################################

    # ######################################################################################################
    # SUMMARY STAT COMPARISON OF NEQ VS SGD
    # ######################################################################################################
    NEWLINE(3); cat(HEADER)
    MODE    = c("TRAINING", "CROSSVAL", "TESTING " )
    SGD_MSE = c(T_RETVALS$SGD_MSE, C_RETVALS$SGD_MSE, P_RETVALS$SGD_MSE)
    NEQ_MSE = c(T_RETVALS$NEQ_MSE, C_RETVALS$NEQ_MSE, P_RETVALS$NEQ_MSE)
    SGD_L2  = c(T_RETVALS$SGD_L2,  C_RETVALS$SGD_L2,  P_RETVALS$SGD_L2)
    NEQ_L2  = c(T_RETVALS$NEQ_L2,  C_RETVALS$NEQ_L2,  P_RETVALS$NEQ_L2)
    # ######################################################################################################

    # ######################################################################################################
    cat( HEADER )
    print( "THETA COEFFICIENTS, CROSS COMPARISON" )
    cat( HEADER )
    print ( sprintf( "THETA[%3s]= %12s %12s %12s %12s", "i", "SGD", "NEQ", "FMINUNC", "" ))
    print ( sprintf( "THETA[%3d]= %12.8f %12.8f %12.8f %12s", c(0:(length(SGD_THETA)-1)), SGD_THETA, NEQ_THETA, FMINUNC$WO_GR$par, "" ) )
    cat( HEADER )
    print ( sprintf( "%12s %12.8f %12.8f %12s",  paste("MSE",MODE),   SGD_MSE,   NEQ_MSE, ""  ))
    cat( HEADER )
    print ( sprintf( "%12s %12.8f %12.8f %12s",  paste("L2N(THETA)"), SGD_L2[1], NEQ_L2[1], ""))
    cat( HEADER )
    METRICS   = TIMESTAMP( "VERIFICATION" )
    # ######################################################################################################

    # ######################################################################################################
    # PRINT TIMESTAMP METRICS
    # ######################################################################################################
    NEWLINE(3); cat(HEADER)
    cat( HEADER )
    print( METRICS )
    cat(HEADER)
    # ######################################################################################################

    return ( SGD )
}







# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
NEWLINE(10)
cat(HEADER)
cat(HEADER)
cat(HEADER)


    if ( RUN_VALIDATE_SGD ) {
        sink( 'output.stochastic_gradient_descent.out', split=TRUE )
        SGD = VERIFY_AND_VALIDATE_SGD ( SYNTHETIC_TEST=FALSE, REGULARIZATION_STRENGTH=1E-3 )
        sink()
    }


cat(HEADER)
cat(HEADER)
cat(HEADER)
NEWLINE(10)
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################





