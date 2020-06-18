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
OLD_CA = commandArgs()
commandArgs <- function() list(TEST_ENABLED=FALSE, DO_DISTANCE_VALIDATION=FALSE, RUN_VALIDATE_SGD=FALSE )
# ######################################################################################################


# ######################################################################################################
VARGOAL=0.95
# ######################################################################################################

# ######################################################################################################
require(graphics)
source('utilities.R')
source('distances.R')
source('datasets.R')
# ######################################################################################################


# ######################################################################################################
# http://www.statmethods.net/advstats/cluster.html
# Prior to clustering data, you may want to remove or estimate missing data and rescale variables for comparability.
# ######################################################################################################
DATA_CLEANING = function( mydata, nmax=0 ) {
    mydata <- na.omit(mydata)           # listwise deletion of missing
    mydata <- scale(mydata)             # standardize variables
    if ( nmax==0 ) {
        nmax = nrow(mydata )
        which_rows = order( sample( rownames(mydata), nmax ) )
        mydata = mydata[which_rows, ]
    }
    return ( mydata )
}
# ######################################################################################################


# ######################################################################################################
# http://www.statmethods.net/advstats/cluster.html
# Partitioning: # estimation of the bend-point estimation on the error fit
# Determine number of clusters
# ######################################################################################################
FIND_WSS_BEND = function( mydata ) {
    wss <- (nrow(mydata)-1) * sum( apply(mydata, 2, var) )
    for (i in 2:15) 
        wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
    plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
    k = 5
    return ( k )
}
# ######################################################################################################


# ######################################################################################################
# sum of squares
# ######################################################################################################
ss <- function(x) sum(scale(x, scale = FALSE)^2)
# ######################################################################################################


# ######################################################################################################
# K-Means Cluster Analysis
# http://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html
# ######################################################################################################
DO_KMEANS = function( x=matrix(), K=3, nstarts=30, do_scaling=FALSE, do_plot=TRUE, ... ) {
    if ( nrow(x) == 0 ) {
        # a 2-dimensional example
        x <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2),
                   matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
        colnames(x) <- c("x", "y")
        rownames(x) <- 1:nrow(x)
    }

    if ( do_scaling )
        x = scale( x )

    cat(HEADER)
    cat(HEADER)
    cat(HEADER)

    (KMEANS_FIT <- kmeans( x, K, nstart=nstarts, ... ))
    print( KMEANS_FIT )

    if ( do_plot ) {
        plot(x, col = KMEANS_FIT$cluster, pch=KMEANS_FIT$cluster, cex=0.8 )
        points(KMEANS_FIT$centers, col = rep("blue",K), pch = 'o', cex = 3.0)
    }

    ## cluster centers "fitted" to each obs.:
    fitted.x <- fitted(KMEANS_FIT)
    head(fitted.x)
    resid.x  <- x - fitted(KMEANS_FIT)

    ## Equalities : ----------------------------------
    cbind(KMEANS_FIT[c("betweenss", "tot.withinss", "totss")], # the same two columns
                     c(ss(fitted.x), ss(resid.x),    ss(x)))

    stopifnot(all.equal(KMEANS_FIT$ totss,        ss(x)), 
              all.equal(KMEANS_FIT$ tot.withinss, ss(resid.x)),
	            ## these three are the same:
	            all.equal(KMEANS_FIT$ betweenss,    ss(fitted.x)),
	            all.equal(KMEANS_FIT$ betweenss, KMEANS_FIT$totss - KMEANS_FIT$tot.withinss),
	            all.equal(ss(x), ss(fitted.x) + ss(resid.x))
	         )

    # trivial one-cluster, (its W.SS == ss(x))
    print( kmeans(x,1)$withinss ) 

    cat(HEADER)
    cat(HEADER)
    cat(HEADER)
    NEWLINE(3)

    retvals = list( 'FIT'=KMEANS_FIT )

    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
# retvals = list( 'MAPPINGS'=MAPPING, 'CENTROIDS'=CENTROIDS 'METRICS'=list( END=END, WHY=WHY, ERROR=ERROR ))
# ######################################################################################################
KMEANS_ITERATOR = function( X, K, CENTROIDS=matrix(), do_scaling=TRUE, do_plot=FALSE, transforms="in_pca_domain,in_probas_domain", nstarts=100, vargoal=VARGOAL, EPSILON=1E-3, LIMIT=3, MAX_ITER=30, debug=FALSE ) {
    ITERATION = list()
    for ( i in 1:nstarts ) {
        RETVALS = DO_DETAILED_INSIGHTS_KMEANS( Xdf, K, CENTROIDS,
                                            do_scaling,
                                            EPSILON,
                                            do_plot,
                                            transforms,
                                            vargoal,
                                            LIMIT,
                                            MAX_ITER,
                                            debug )
        ITERATION[[i]] = RETVALS
    }

    for ( i in 1:nstarts ) {
        print( ITERATION[[i]]$CENTROIDS )
    }
}
# ######################################################################################################
    

# ######################################################################################################
# BEST USE FOR: Once K is determined for X, the detailed diagnostics provide insights about the clusters 
# ######################################################################################################
DO_DETAILED_INSIGHTS_KMEANS = function( Xdf, K, 
                                        CENTROIDS =matrix(),
                                        do_scaling=TRUE, 
                                        EPSILON=1E-3, 
                                        do_plot=TRUE, 
                                        transforms="in_pca_domain, in_probas_domain", 
                                        vargoal=VARGOAL, 
                                        LIMIT=3,
                                        MAX_ITER=50,
                                        debug=FALSE  ) {

    opts = options( digits=2, width=132 )

    if( is.null(rownames(Xdf)) ) rownames(Xdf) = 1:nrow(Xdf)
    if( is.null(colnames(Xdf)) ) colnames(Xdf) = 1:ncol(Xdf)

    MU = c()
    SD = c()
    if ( do_scaling ) {
        X = as.matrix(scale(Xdf))
        colnames(X) = colnames(Xdf)
        rownames(X) = rownames(Xdf)

        MU = attr(X,"scaled:center")
        SD = attr(X,"scaled:scale")
    } 
    str(X)

    ITEMS = rownames(X)
    M     = length(ITEMS)

    if ( nrow(CENTROIDS)!= K ) {
        if ( nrow(CENTROIDS)!=K ) print( "USING RANDOM SAMPLES AS CENTROIDS" )
        INITIAL_RANDOM_CENTERS = sample(ITEMS, K) 
        CENTROIDS = X[INITIAL_RANDOM_CENTERS,]
    }
    rownames(CENTROIDS)    = CLUSTER_NAMES( 1:K )
    CIDX                   = CLUSTER_NAMES( 1:K )
    CENTROID_COLORS        = 1:K
    names(CENTROID_COLORS) = CLUSTER_NAMES( 1:K )

    OLD_CENTROIDS = CENTROIDS

    MAPPING = MATRIX( M, MAX_ITER )
        rownames(MAPPING) = rownames(X)

    METRICS = list()

    JCOSTS = c()

    if ( do_plot ) {
        print( 'PDF FILE: plot_clustering_methods_cluster_assignments.pdf shows cluster assignments at each iteration' )
        pdf( 'plot_clustering_methods_cluster_assignments.pdf', 11, 8 )
        op = par( mfcol=c(2,2) )
    }

    for (iter in 1:MAX_ITER ) {
        ITER_MAPPING = DO_KMEANS_CLUSTER_ASSIGNMENT( X, ITEMS, CIDX, OLD_CENTROIDS )
        MAPPING[,iter] = ITER_MAPPING

        if ( do_plot ) {
            Z = DO_PCA( rbind(X, OLD_CENTROIDS), vargoal=vargoal, silent=TRUE, debug=FALSE )$Z
            COLORS = append(CENTROID_COLORS[MAPPING[,iter]] + 1,  rep("black", K ))
            SYMBOLS= append(CENTROID_COLORS[MAPPING[,iter]] + 10, rep(1,K))
            SIZES  = append(rep(0.6, nrow(X)),                    rep(3.0, K ) )
            plot( Z[,1], Z[,2], main=paste("CLUSTER_ASSIGNMENTS WITHIN PCA(X,2) SPACE at ITERATION", iter), 
                                cex=SIZES, pch=SYMBOLS, col=COLORS, xlim=c(-LIMIT,LIMIT), ylim=c(-LIMIT,LIMIT) )
        }

        JCOST = COMPUTE_JCOST( OLD_CENTROIDS, X, MAPPING, AT_ITER=iter )
        JCOSTS = append( JCOSTS, JCOST )

        NEW_CENTROIDS = DO_RECOMPUTE_CLUSTER_MEANS( X, CIDX, MAPPING, iter )

        RETVALS = ANALYZE_ENDING_CONDITION( iter, NEW_CENTROIDS, OLD_CENTROIDS, JCOSTS, EPSILON=EPSILON )
        METRICS[[iter]] = RETVALS
        if ( RETVALS$END ) {
            PRINT_SUMMARY_LINES( RETVALS$END, RETVALS$WHY, NEW_CENTROIDS, MU, SD, do_scaling )
            break
        }

        OLD_CENTROIDS = NEW_CENTROIDS
    }

    retvals = list( 'MAPPINGS'=MAPPING, 'CENTROIDS'=NEW_CENTROIDS, 'METRICS'=METRICS, 'JCOSTS'=JCOSTS, 'MU'=MU, 'SD'=SD )

    if ( do_plot ) dev.off()

    options( opts )

    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
# Determine number of clusters  # http://www.statmethods.net/advstats/cluster.html 
# ######################################################################################################
FIND_AN_APPROXIMATE_KMEANS_BEND = function( myX, KMAX=20, BUILDUP_DELAY=5, P=2 ) {
    KMAX=min(max(5,KMAX), 0.9 * nrow(myX))

    wss <- (nrow(myX)-1) * sum(apply(myX,2,var))
    for (i in 2:KMAX) 
        wss[i] <- sum(kmeans(myX, centers=i, nstart=2, iter.max=10)$withinss)
    names(wss) = 1:KMAX
    
    WSS   = cumsum(wss)[length(wss)]
    D     = 5
    SIGNIFICANT_CONTRIBUTION = P * WSS/KMAX
    WHICH_ONE = as.numeric(which( wss[BUILDUP_DELAY:length(wss)] <= SIGNIFICANT_CONTRIBUTION )) + BUILDUP_DELAY
    print( WHICH_ONE )

    op = par(mfrow=c(2,1))
        plot(1:KMAX, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
        points( WHICH_ONE, wss[WHICH_ONE], pch=8, cex=2, col="green" )
    par(op)

    retvals = list( 'wss'=wss, 'pivot'=pivot, 'bend' = WHICH_ONE )
    print( retvals )
    return ( wss )
}
# ######################################################################################################


# ######################################################################################################
DO_KMEANS_CLUSTER_ASSIGNMENT = function( X, ITEMS, CIDX, OLD_CENTROIDS, do_plot=FALSE, vargoal=VARGOAL, debug=FALSE ) {
    ITER_MAPPING = c()

    XXX = rbind(X, OLD_CENTROIDS )
        rownames(XXX) = c( rownames(X), CIDX ) 
        colnames(XXX) = colnames( X )

    if ( do_plot ) {
        PCA = DO_PCA( XXX, vargoal=vargoal, silent=TRUE, debug=FALSE )
        Z = PCA$Z
        Ureduce = PCA$Ureduce
        DO_PCA_FULL_PLOT( Z, cex=0.8 ) #, xlim=c(-15,15), ylim=c(-15,15) )
    }

    for ( item in ITEMS ) {
        XX = rbind( OLD_CENTROIDS, X[item,] )
        rownames(XX) = c( CIDX, item) 

        D = dist( XX )

        Zz = matrix()

        RETVALS = MIN_DISTANCE_FROM ( XX, item, D=D, PCA=Zz, do_plot=FALSE )
        CLOSEST = RETVALS$minidx

        CLUSTER_MAPPING = CIDX[CLOSEST]
        ITER_MAPPING = append( ITER_MAPPING, CLUSTER_MAPPING )

        if ( debug ) 
            print( paste( item, CLOSEST ) )
    }
    return ( ITER_MAPPING )
}
# ######################################################################################################


# ######################################################################################################
DO_RECOMPUTE_CLUSTER_MEANS = function( X, CIDX, MAPPING, iter, method="centroids", useTrim=0.01, debug=FALSE ) {
    START = TRUE

    for ( cid in CIDX ) {
        VITER_MAPPING = c(MAPPING[,iter])

        WHICH_ROWS = names(which(VITER_MAPPING==cid))

        if ( method == "centroids" )
            MU = t(as.matrix( colMeans(X[WHICH_ROWS,], na.rm=TRUE ) ))
        else
            if ( method == "medoids" )
                MU = t(as.matrix( apply(X[WHICH_ROWS,], 2, median)))
            else 
                MU = t(as.matrix( apply(X[WHICH_ROWS,], 2, mean, trim=useTrim)))

        if ( debug ) { cat(HEADER); print ( cid ) ; print( WHICH_ROWS ) ; print( MU ) ; cat( HEADER ) }

        if ( START ) {
            START = FALSE
            NEW_CENTROIDS = as.data.frame( MU )
        } else  {
            NEW_CENTROIDS = rbind( NEW_CENTROIDS, as.data.frame( MU ) )
        }
    }
    rownames(NEW_CENTROIDS) = CIDX

    return ( NEW_CENTROIDS )
}
# ######################################################################################################


# ######################################################################################################
GET_ERROR_MEASUREMENT = function( NEW_CENTROIDS, OLD_CENTROIDS) {

    FIN  = rowSums(as.matrix(OLD_CENTROIDS) %*% t(as.matrix(OLD_CENTROIDS))) - 
           rowSums(as.matrix(NEW_CENTROIDS) %*% t(as.matrix(NEW_CENTROIDS)))

}
# ######################################################################################################


# ######################################################################################################
COMPUTE_JCOST = function( OLD_CENTROIDS, X, MAPPING, AT_ITER=1, debug=FALSE ) {
    JCOST = 0

    for ( cid in  CLUSTER_NAMES(1:K) ) {

        WHICH_ONES = which( MAPPING[,AT_ITER] == cid )
        if ( length(WHICH_ONES) == 0 ) next

        MSE_TERMS = (X[WHICH_ONES,] - OLD_CENTROIDS[cid,])^2

        if ( debug ) {
            print ( paste( cid, WHICH_ONES, sep=":" ) )
            print( sprintf( "%.5f", MSE_TERMS))
        }

        INCREMENT = sum(rowSums(MSE_TERMS))

        JCOST = INCREMENT + JCOST

        print ( sprintf( "JCOST [CLUSTER: %5s, AT ITER=%5s; A CLUSTER WITH %5s SAMPLES ASSIGNED] = %8.4f --> JCOST (TOTAL) = %8.4f WITHIN = %8.4f", 
                          cid, AT_ITER, length(WHICH_ONES), INCREMENT, JCOST, INCREMENT/length(WHICH_ONES) ) )

    }

    JCOST  = JCOST / nrow(X)

    print ( sprintf( "JCOST [OVERALL AT ITER=%5s ] = %8.4f", AT_ITER, JCOST ) )
    cat( HEADER )

    return ( JCOST )
}
# ######################################################################################################


# ######################################################################################################
ANALYZE_ENDING_CONDITION = function( iter, NEW_CENTROIDS, OLD_CENTROIDS, JCOSTS, EPSILON=1E-3, ITERMAX=300 ) {

    ERROR = GET_ERROR_MEASUREMENT( NEW_CENTROIDS, OLD_CENTROIDS )

    NEWLINE(3)
    MSG = CONCAT( sprintf( "%2s DELTA=%6.3f; ", 1:nrow(NEW_CENTROIDS), ERROR ) )
    cat(HEADER)
    print( paste( "ITER: ", iter, MSG )) 

    cat(SUBHEADER)
    print( NEW_CENTROIDS )
    cat(SUBHEADER)

    END=FALSE
    WHY=NA

    Jn = length(JCOSTS)
    FINAL_JCOST = JCOSTS[Jn]

    DELTA_JCOST = Inf
    if( Jn > 3 )
        DELTA_JCOST  = mean(JCOSTS[(Jn-3):Jn])


    if( FINAL_JCOST <= EPSILON ) {
        END=TRUE
        WHY=sprintf( "CONVERGED (JCOST OPTIMIZED):   ITER= %5d DELTA=%8.4f %8.4f [EPSILON=%s]", iter, ERROR, FINAL_JCOST, EPSILON )
    }

    if( DELTA_JCOST <= EPSILON ) {
        END=TRUE
        WHY=sprintf( "CONVERGED (JCOST CONVERGED):   ITER= %5d DELTA=%8.4f %8.4f [EPSILON=%s]", iter, ERROR, FINAL_JCOST, EPSILON )
    }

    if( all(abs(ERROR) <= EPSILON) ) {
        END=TRUE
        WHY=sprintf( "CONVERGED (CENTROID MOVEMENT): ITER= %5d DELTA=%8.4f %8.4f [EPSILON=%s]", iter, ERROR, FINAL_JCOST, EPSILON )
    }

    if ( iter > ITERMAX ) { 
        END=TRUE
        WHY=sprintf( "MAXITER, (DID NOT CONVERGE):   ITER= %5d DELTA=%8.4f %8.4f [EPSILON=%s]", iter, ERROR, FINAL_JCOST, EPSILON )
    }

    retvals = list( END=END, WHY=WHY, ERROR=ERROR )

    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
PRINT_SUMMARY_LINES = function( END, WHY, NEW_CENTROIDS, MU, SD, do_scaling ) {
    if ( END ) {
        cat(HEADER)
        NEWLINE(3)
        cat(HEADER)
        print( WHY )
        if ( do_scaling ) {
            NEW_CENTROIDS = (NEW_CENTROIDS * SD) + MU 
            print( NEW_CENTROIDS )
        }
        cat(HEADER)
        cat(HEADER)
        cat(HEADER)
    }
}
# ######################################################################################################


# ######################################################################################################
CLUSTER_NAMES = function( i ) { paste( "c", i, sep="" ) }
# ######################################################################################################


# ######################################################################################################
DOMINANT_CLUSTER = function( m_row, debug=FALSE, get_type="cluster" ) {
    m_categorized = table( m_row )
    cluster_maxval = as.numeric(max( m_categorized ))
    dominant_cluster = which( m_categorized== cluster_maxval )[1]
    cluster_name = names(m_categorized)[dominant_cluster]
    dominance = round( cluster_maxval / length(m_row), 4 ) * 100.0
    if ( debug ) {
        print( m_row )
        print( m_categorized )
        print( paste( 'max', cluster_maxval ) )
        print( paste( 'dominant', cluster_name ) )
        print( paste( cluster_name, dominance ) )
        cat(HEADER)
    }
    if ( get_type == "cluster" )   retvals = as.character(cluster_name)
    if ( get_type == "dominance" ) retvals = as.numeric(dominance)
    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
EXTRACT_CONVERGENCE_TERMS = function( M0, K, N_ITERS, field="ERROR", debug=FALSE ) {
    a = c()
    for ( i in 1:N_ITERS) {
        fld = M0$METRICS[[i]]
        vals =eval(parse( text=sprintf("fld$%s",field)))
        if ( debug ) print( vals )
        a = append(a, vals )
    }

    A = MATRIX( N_ITERS, K )
    colnames(A) = CLUSTER_NAMES( 1:K )
    rownames(A) = 1:N_ITERS

    if ( field == "ERROR" ) {
        for ( cid in  CLUSTER_NAMES( 1:K ) ) {
            A[,cid] = a[which( names(a)==cid )]
            if ( debug ) {
                print ( cid )
                print( which( names(a)==cid ) )
                str(A)
            }
        }
    }
    return ( A )
}
# ######################################################################################################


# ######################################################################################################
GET_NUMBER_ITERATIONS_TO_CONVERGENCE = function( M0 ) { IMAX = length(M0$METRICS) }
# ######################################################################################################


# ######################################################################################################
GET_CLUSTER_MAPPINGS = function( M0 ) { MAPPINGS = M0$MAPPINGS }
# ######################################################################################################


# ######################################################################################################
DO_DETAILED_DIAGNOSTICS = function( M0, psfile="clustering_methods.pdf" ) {
    if ( psfile!="" )  {
        pdf( psfile, 11, 8 ) 
        print( paste( 'PDF FILE:', psfile, 'provides with detailed diagnostics plots' ) )
    }

    IMAX  = GET_NUMBER_ITERATIONS_TO_CONVERGENCE( M0 )
    WHICH_ITERS = 1:IMAX

    ERROR = EXTRACT_CONVERGENCE_TERMS( M0, K, IMAX, field="ERROR" )

    M = GET_CLUSTER_MAPPINGS( M0 )
    M = M[,WHICH_ITERS]

    CLUSTER_MEMBERSHIP = COLLECT_VECTOR( M, APPLY_F=DOMINANT_CLUSTER, get_type="cluster" )
    CLUSTER_DOMINANCE  = COLLECT_VECTOR( M, APPLY_F=DOMINANT_CLUSTER, get_type="dominance" )
    CLUSTER_MEMBERSHIP_INCONSISTENCY = data.frame( 'CLUSTER'  =CLUSTER_MEMBERSHIP, 
                                                   'INCONSISTENCY IN SAMPLE-TO-CLUSTER ASSIGNMENTS'=100-CLUSTER_DOMINANCE )

    CLUSTER_SIZE_AT_EACH_ITER = apply( M[,WHICH_ITERS], 2, table )
    colnames(CLUSTER_SIZE_AT_EACH_ITER) = paste(WHICH_ITERS)

    ASSIGMENT_HOMEGENOUSNESS  = apply( M, 1, table )

    par( mfrow=c(2,2))
        plot( WHICH_ITERS, sqrt(M0$JCOSTS), pch=23, bg="gray", t="b", cex=0.8, 
             main=sprintf("ERROR BEING MINIMIZED BY \nSAMPLE-TO-CLUSTER ASSIGNMENTS AT EACH ITERATION"),
             ylab=sprintf("ERROR [JCOST(KMEANS(K=%s)]",K),
             xlab="ITERATION NUMBER")
         grid()
 
        # http://stackoverflow.com/questions/18688847/position-legend-of-a-stacked-bar-plot 
        barplot( CLUSTER_SIZE_AT_EACH_ITER, 
         beside=T, 
         cex=0.8, 
         cex.axis=0.8, 
         las=2,
         main="CLUSTER SIZE AT EACH ITERATION", 
         xlab="ITERATION NUMBER", 
         ylab="CLUSTER SIZE",
         legend=rownames(CLUSTER_SIZE_AT_EACH_ITER),
         args.legend = list(x = "topright", bty = "n", inset=c(-.03, 0)))

        plot( CLUSTER_MEMBERSHIP_INCONSISTENCY, main="OBSERVED INCONSISTENCY\nIN CLUSTER ASSIGNMENTS", cex=0.8, ylim=c(0,100) )
         grid()

        PER_SAMPLE=2
        hist( CLUSTER_MEMBERSHIP_INCONSISTENCY[,PER_SAMPLE], breaks=32, main="FREQUENCY OF OBSERVED INCONSISTENCY\nIN CLUSTER ASSIGNMENTS", cex=0.8)
         grid()

    ops = par( mfrow=c(3, 2) )
    for ( i in 1:K ) {
        plot( sqrt(abs(ERROR[,i])), t='l', main=sprintf("MOVEMENT OF CENTROID: c%s\nAT EACH ITERATION", i) )
        grid()
    }

    par( ops )

    if ( psfile!="" ) dev.off()

}
# ######################################################################################################








# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
CLUSTER_TESTS = TRUE

if ( CLUSTER_TESTS ) {
    sink( "output_clustering_methods_diagnostics.out", split=TRUE )
    graphics.off()


    # ######################################################################################################
    M = 1000
    N = 3
    X  = as.data.frame(GET_RANDOM_XY(M,N))
    x <- rbind(matrix(rnorm(M, mean = 0, sd = 0.3), ncol = 2),
               matrix(rnorm(M, mean = 1, sd = 0.3), ncol = 2))
         colnames(x) <- c("x", "y")

    # X = x
      K = N-0
    # ######################################################################################################

        cat( HEADER )
        cat( HEADER )
        cat( HEADER )
    
        M0 = DO_DETAILED_INSIGHTS_KMEANS( X, K )
        DO_DETAILED_DIAGNOSTICS( M0, psfile="plot_clustering_methods_diagnostics.pdf" )
    
        cat( HEADER )
        cat( HEADER )
        cat( HEADER )
    
        DO_KMEANS( X, K )
    
        cat( HEADER )
        cat( HEADER )
        cat( HEADER )

    sink()
}

# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################





