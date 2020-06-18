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


# ######################################################################################################
LICENSETEXT = "These R code samples (version Sep/2014), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License." 
message( LICENSETEXT )
message("")
# ######################################################################################################


# ######################################################################################################
OLD_CA = commandArgs()
commandArgs <- function() list(TEST_ENABLED=FALSE, DO_DBTEST=TRUE, DO_DISTANCE_VALIDATION=FALSE, RUN_VALIDATE_SGD=FALSE )
# ######################################################################################################
source( 'utilities.R' )
source( 'datasets.R' )
source( 'fselect.R' )
source( 'contour.R' )
# ######################################################################################################
VERIFY_OPTARG ( OPTARG_NAME="DO_DBTEST", OPTARG_VALUE=TRUE )
# ######################################################################################################


# #################################################################################################
TRANSITIVE_CLOSURE_FROM = function( ITEM, EPSILON, CID, debug=0 ) {
    if ( debug > 1) 
        print( paste( "INITIATING RECURSION ON ITEM", ITEM, CID ))

    if (all(VISITED != 0)) {
        if ( debug > 1 ) 
            print( paste( "FINISHED  RECURSIONS" ) )
        return
    }

    if (VISITED[ITEM] != 0) {
        if ( debug > 2) 
            print( sprintf( 'ITEM %s ALREADY VISITED', ITEM, CID ) )
    } else {
        VISITED[ ITEM ] <<- CID

        WHICH_ONES = GET_WHICH( ITEM )
        WHICH_ONES = which( VISITED[WHICH_ONES] == 0 )

            if ( debug > 3 ) 
                print( paste( "PROCESSING CLOSURE FOR ITEM", ITEM, CID, "LENGTH", length(WHICH_ONES) ))

            if ( length( WHICH_ONES > 0 ) ) {
                if ( debug > 4 ) 
                    print( paste( "2] CLOSURE:", WHICH_ONES ) )

                for ( subitem in WHICH_ONES ) {
                    TRANSITIVE_CLOSURE_FROM( subitem, EPSILON, CID )
                }

            } else {
                if ( debug > 3 ) 
                    print( paste( "2] CLOSURE:", WHICH_ONES, "DONE" ) )
            }
    }
    if ( debug > 1 )print( paste( "FINISHED   RECURSION ON ITEM", ITEM, CID ))

    print( paste( "ITEM=", ITEM, "CID=", CID, "VISITED=", length(which(VISITED==CID))))
    # print( which( VISITED == CID ) )

    return
}
# #################################################################################################


# ############################################################################################
VALIDATE_UPDATE = function( UPDATE, cid ) {
    if ( length(UPDATE) != 0 ) {
        ALREADY_VISITED = which( VISITED !=0 )
        if ( length( ALREADY_VISITED ) == 0 ) ALREADY_VISITED = c()
        UPDATE = setdiff( UPDATE, ALREADY_VISITED )
    } else {
        UPDATE = c()
    }
    return ( UPDATE )
}
# ############################################################################################


# ############################################################################################
UPDATE_WHICH = function( WHICH, UPDATE ) {
    WHICH = append( WHICH, UPDATE )
    WHICH = unique( WHICH )
    return ( WHICH )
}
# ############################################################################################


# ############################################################################################
GET_WHICH = function( topitem ) {
    WHICH  = as.numeric(which( DISTANCE_MATRIX_X[,topitem] < EPSILON ))
    return ( WHICH )
}
# ############################################################################################


# ############################################################################################
GET_UPDATE = function( WHICH ) {
    UPDATE = c()
    for( i in WHICH ) {
        UPDATE = unique(append( UPDATE, GET_WHICH( i ) ))
    }
    return ( UPDATE )
}
# ############################################################################################


# ############################################################################################
UPDATE_VISITED = function( WHICH, cid ) {
    VISITED[ WHICH ] <<- cid
}
# ############################################################################################


# ############################################################################################
#### 0 1
TRANSITIVE_CLOSURE = function ( topitem, cid, MIN_LEADSIZE=5, debug=FALSE ) {
    WHICH  = GET_WHICH ( topitem )
    UPDATE = GET_UPDATE ( WHICH )
    UPDATE = VALIDATE_UPDATE( UPDATE, cid )

    while ( length(UPDATE) > MIN_LEADSIZE ) {
        WHICH  = UPDATE_WHICH( WHICH, UPDATE )
        UPDATE_VISITED( WHICH, cid )

        UPDATE = GET_UPDATE ( WHICH )
        UPDATE = VALIDATE_UPDATE( UPDATE, cid )
    }
    if ( debug  ) print( paste( "LEN(WHICH)", length(WHICH), topitem, cid  ) )
    retvals = list( WHICH, length(WHICH), topitem, cid )
    return ( retvals )
}
# ############################################################################################


# ######################################################################################################
DO_DBSCAN_PLOT = function( CLUSTERS ) {
    NOISE   = which( VISITED == 0 )
    VALID   = 1:M
    C  = table(VISITED)
    LC = length(which(names(C)!="0"))

    if ( ncol(X) == 2 ) {
        plot( X[,1], X[,2], 
            pch=".", col="gray", cex=0.8, 
            main=sprintf("DBSCAN W/ EPS= %.3f M=%s |C|=%s", EPSILON, MIN_SIZE, LC ))
    } else {
        Z = DO_PCA(X, vargoal=0.95, ntries=2  )$Z
        plot( Z[,1], Z[,2], 
            pch=".", col="gray", cex=0.8, 
            main=sprintf("DBSCAN W/ EPS= %.3f M=%s |C|=%s", EPSILON, MIN_SIZE, LC ))
    }
    grid()

    if ( length(NOISE) > 0 ) 
        VALID = setdiff( VALID, NOISE )

    points( X[VALID,1], X[VALID,2], 
                pch=(10 + VISITED[VALID] %% 5), 
                col=(20 + VISITED[VALID] %% 256 ),
                bg =(40 + VISITED[VALID] %% 256 ),
                cex=1.2 )

    if ( length(NOISE) > 0 ) 
        points( X[NOISE,1], X[NOISE,2], 
                pch=".", 
                col="gray", 
                cex=0.5 )

}
# ######################################################################################################


# #################################################################################################
# for ( e in seq(0.05,0.6,0.05)) hist( rowSums(DISTANCE_MATRIX_X < e ))
# #################################################################################################
GET_INITIAL_EPSILON_ESTIMATE_FOR = function( X, D, epsilon=NA, NMIN=500, q=1.0, STEP=1E-3, debug=FALSE ) {
    M      = nrow(X)
    NMIN   = min(NMIN,M)
    MINCON = min(0.05*M, sqrt(M))

    SUBSAMPLED = sample(1:M, NMIN )
    DD = D[SUBSAMPLED, ] 

    EPSILON = NA
    for ( EPSILON in  seq(STEP,1,STEP) ) {
        OUT_DEGREES = rowSums(ifelse( DD<EPSILON, 1, 0 ))
        OUTDEGREE_STAT = quantile(OUT_DEGREES, c(q))[1]
        if( debug) print( paste ( EPSILON, OUTDEGREE_STAT, MINCON ) )
        if ( OUTDEGREE_STAT > MINCON ) break
    }

    EPSILON = ifelse( is.na(EPSILON), 0.3, EPSILON )

    if ( debug ) {
        cat ( HEADER )
        print( t(rowSums(ifelse( D<EPSILON, 1, 0 ))) ) 
        cat ( HEADER )
    }

    if ( !is.na(epsilon) )
        if ( EPSILON < epsilon ) 
            EPSILON = epsilon

    return ( EPSILON )
}
# ######################################################################################################


# ######################################################################################################
UPDATE_EPSILON = function( CHANGE, debug=FALSE ) { 
    EPSILON <<- EPSILON + CHANGE 

    if ( debug ) 
        print( paste( "EPSILON INCREASED", EPSILON ) )

    return ( EPSILON )
}
# ######################################################################################################


# ######################################################################################################
DO_DBSCAN = function( X, D, MIN_SIZE, ORDERING_IN_USE, debug=FALSE ) {
    CLUSTERS = c()
    CID = 1
    for ( TOPITEM in ORDERING_IN_USE ) {
      if ( any(VISITED == 0))
         if ( any(VISITED[ which(DISTANCE_MATRIX_X[TOPITEM,] < (10*EPSILON)) ] == 0) ) {

            TRANSITIVE_CLOSURE( TOPITEM, CID, MIN_LEADSIZE = MIN_SIZE )

            WHICH = which( VISITED == CID )
            if ( debug ) print( paste( "length(which(table(VISITED)<MIN_SIZE))", length(which(table(VISITED)<MIN_SIZE))))

            NOISE_CLUSTERS = names( which( table(VISITED)<=MIN_SIZE) )
            VALID_CLUSTERS = names( which( table(VISITED)>MIN_SIZE) )

            for ( i in NOISE_CLUSTERS )
                VISITED[ VISITED==as.integer(i) ] <<- 0

            if ( debug ) print( table(VISITED) )

            if ( length(WHICH) < MIN_SIZE ) { 
                for ( item in WHICH ) {
                    nearby = as.integer(names(which( DISTANCE_MATRIX_X[,item] < EPSILON )))
                    nearby_itemized = table(VISITED[nearby])
                    if ( max(nearby_itemized) > MIN_SIZE ) {
                        BEST_CID = as.integer(names(which( nearby_itemized == max(nearby_itemized) )))
                        UPDATE_VISITED( item, BEST_CID )
                    } else
                        UPDATE_VISITED( item, 0 )
                }
                # EPSILON = UPDATE_EPSILON( STEP/16)
            } 
            else {
                CLUSTERS = append( CLUSTERS, CID )
                CID = CID + 1
                if ( debug ) {
                    cat( HEADER )
                    print ( table ( VISITED ) )
                    cat( HEADER )
                    NEWLINE(3)
                }
            }

            NOISE = length( which(VISITED==0) )
            if ( NOISE > 2*sqrt(M) )                # it adapts heavily at beginning and slows down as it progresses 
                EPSILON = UPDATE_EPSILON( STEP/10 )

        } else
            break
    }

    RETVALS = list( 'EPSILON'=EPSILON, 'MAPPINGS'=VISITED, 'CLUSTERS'=CLUSTERS, 'NOISE'=NOISE, 'SUMMARY'=SUMMARY )

    return ( RETVALS )

}
# ######################################################################################################


# ######################################################################################################
PRINT_SUMMARY =function( CLUSTERS, debug=TRUE ) {
    cat( HEADER )
    C  = table ( VISITED )
    LC = length(which(names(C)!="0"))

    if ( debug ) {
        NOISE = length( which(VISITED==0) )
        CENTROIDS = GET_CENTROIDS_FOR( X, VISITED )
        SUM_WSS   = TOTAL_WSS_SUM = sum(GET_MSE_FROM_CENTROIDS_FOR(X, VISITED, CENTROIDS=CENTROIDS, GET_WSS=TRUE ))
        AVG_WSS   = TOTAL_MSE_SUM = sum(GET_MSE_FROM_CENTROIDS_FOR(X, VISITED, CENTROIDS=CENTROIDS, GET_WSS=FALSE ))
        AVG_ICD   = AVG_BETWEEN_CLUSTER_DIST = mean(GET_DISTANCE_BETWEEN_CENTROIDS( CENTROIDS ))
        NCLUSTERS = LC

        CLUSTERING_COST = round(SUM_WSS + (AVG_WSS * NCLUSTERS) - AVG_ICD + sqrt(NOISE) + NCLUSTERS,2)

        print( paste( "FINAL EPSILON=",   round(EPSILON,3),
                  "NOISE THRESHOLD=", MIN_SIZE, 
                  "NOISE=", NOISE, 
                  "|C|=", LC, 
                  'SUM(WSS)=', round(TOTAL_WSS_SUM,2), 
                  'AVG(WSS)=', round(TOTAL_MSE_SUM,2),
                  'AVG(ICD)=', round(AVG_BETWEEN_CLUSTER_DIST,2),
                  'METRIC=', CLUSTERING_COST ))
    }
    print( C )
    cat( HEADER )
    NEWLINE(1)

    retvals = list ( "FINAL_EPSILON"=round(EPSILON,3),
                     "NOISE_THRESHOLD"=MIN_SIZE, 
                     "NOISE"=NOISE, 
                     "NC"=LC,
                     'SUM_WSS'=round(TOTAL_WSS_SUM,2), 
                     'AVG_WSS'=round(TOTAL_MSE_SUM,2),
                     'AVG_ICD'=round(AVG_BETWEEN_CLUSTER_DIST,2),
                     'METRIC'=CLUSTERING_COST )
    return(  retvals )
}
# ######################################################################################################


# ######################################################################################################
GET_CENTROIDS_FOR = function( X, MAPPING ) {
    C    = table( MAPPING )
    C[which(names(C)!="0")]
    CIDS = as.numeric(names(C))

    NC = length(CIDS)
    NF = ncol(X)
    CENTROIDS = MATRIX( NC, NF ) 

    for ( i in 1:NC) {
        WHICH_ONES    = which(MAPPING==CIDS[i])
        CENTROIDS[i,] = apply( X[WHICH_ONES,], 2, mean) #  mean(X[WHICH_ONES,])
    }
    return ( CENTROIDS )
}
# ######################################################################################################


# ######################################################################################################
GET_MSE_FROM_CENTROIDS_FOR = function( X, MAPPING, CENTROIDS=matrix(), GET_WSS=TRUE ) {
    if ( nrow(CENTROIDS) == 0 )
        CENTROIDS = GET_CENTROIDS_FOR( X, MAPPING )
    NC   = nrow( CENTROIDS )
    WSS  = VECTOR(NC)
    C    = table( MAPPING )
    CIDS = as.numeric(names(C))
    for ( i in 1:NC) {
        if ( CIDS[i] == 0 ) next
        WHICH_ONES    = which(MAPPING==CIDS[i])
        if ( GET_WSS )
            WSS[i] = sum(rowSums( X[WHICH_ONES,] - CENTROIDS[i,])^2)
        else
            WSS[i] = sum(rowSums( X[WHICH_ONES,] - CENTROIDS[i,])^2) /length(WHICH_ONES)
    }
    return ( WSS )
}
# ######################################################################################################


# ######################################################################################################
GET_DISTANCE_BETWEEN_CENTROIDS = function( CENTROIDS ) {
    CD = as.matrix(dist( CENTROIDS, upper=TRUE, diag=TRUE ))
    return ( CD )
}
# ######################################################################################################












# ############################################################################################
# ############################################################################################


# ######################################################################################################
if ( DO_DBTEST ) {
    # ############################################################################################
    options( width=132 )
    sink( 'output.dbscan.out', split=T )
    graphics.off()
    pdf( 'plot_dbscan_cluster_assignments.pdf', 12, 8 )
    # ############################################################################################
    
    # ############################################################################################
    NOISE_THRESHOLDS        = c(4,8,12,16,20)
    NUM_SAMPLES_PER_CLUSTER = 500
    NUM_CLUSTERS            = 5
    DIMENSIONALITY          = 2
    X      = GET_CLUSTERED_X( M=NUM_SAMPLES_PER_CLUSTER, 
                              N=DIMENSIONALITY, 
                              NC=NUM_CLUSTERS, 
                              MU_X=seq(0.0, NUM_CLUSTERS-1, 1),
                              SD_X=rep(0.3, NUM_CLUSTERS) )
    M      = nrow(X)
    N      = ncol(X)

    STEP   = 1E-3

    # ############################################################################################

    # ############################################################################################
    DISTANCE_MATRIX_X = as.matrix( dist( X, upper=TRUE, diag=TRUE ) )

    ORIGINAL_EPSILON  = GET_INITIAL_EPSILON_ESTIMATE_FOR( X, DISTANCE_MATRIX_X, epsilon=0.10, q=0.99, STEP=STEP )

    ORDERING_IN_USE   = as.numeric(names( sort( rowSums(DISTANCE_MATRIX_X<ORIGINAL_EPSILON), decreasing=TRUE ) ))          
    # ############################################################################################

    # ############################################################################################
    par( mfrow=c(3,2) )
        i = 1
        SUMMARY = list()
        for ( MIN_SIZE in NOISE_THRESHOLDS ) {
            EPSILON = ORIGINAL_EPSILON
            VISITED <<- VECTOR(M); rownames(VISITED) = rownames(X)
            DBSCAN_RETVALS = DO_DBSCAN( X, D, MIN_SIZE, ORDERING_IN_USE )
            DO_DBSCAN_PLOT( CLUSTERS )
            if ( TRUE ) DRAW_BOUNDARIES( X[,1], X[,2], NUM_CLUSTERS, new_plot=FALSE )
            SUMMARY[[i]] = PRINT_SUMMARY( CLUSTERS )
            i = i + 1
        }
        # ############################################################################################
        MINSIZE  = COLLECT_VECTOR( SUMMARY, function ( x ) x$NOISE_THRESHOLD)
        EPSILON  = COLLECT_VECTOR( SUMMARY, function ( x ) x$FINAL_EPSILON)
        NOISE    = COLLECT_VECTOR( SUMMARY, function ( x ) x$NOISE)
        NCLUSTERS= COLLECT_VECTOR( SUMMARY, function ( x ) x$NC)
        SUM_WSS  = COLLECT_VECTOR( SUMMARY, function ( x ) x$SUM_WSS)
        AVG_WSS  = COLLECT_VECTOR( SUMMARY, function ( x ) x$AVG_WSS)
        AVG_ICD  = COLLECT_VECTOR( SUMMARY, function ( x ) x$AVG_ICD)
        METRICS  = COLLECT_VECTOR( SUMMARY, function ( x ) x$METRIC)
        # ############################################################################################

        # ############################################################################################
        CLUSTERING_COST = SUM_WSS + (AVG_WSS * NCLUSTERS) - AVG_ICD + sqrt(NOISE) + NCLUSTERS^2
                             
        plot( NCLUSTERS,     CLUSTERING_COST,
                             t='p', main="CLUSTERING COST (Y) vs. NUM.CLUSTERS (X)", 
                             cex=1.0, cex.axis=0.8, 
                             xlab="SUM_WSS + (AVG_WSS * NCLUSTERS) - AVG_ICD + SQRT(|NOISE|) + NCLUSTERS^2",
                             xlim=c(min(NCLUSTERS)-1, max(NCLUSTERS)+1) )

        points( NCLUSTERS,   CLUSTERING_COST, 
                             pch='o', cex=3.0, col="black", cex.axis=1.0 )

        TEXT_POSITION = ifelse( NCLUSTERS>max(NCLUSTERS), NCLUSTERS-1, NCLUSTERS+0.2 )
        text( TEXT_POSITION, CLUSTERING_COST, sprintf("M=%s E=%.2f N=%.2f", MINSIZE, EPSILON, NOISE/nrow(X)),
                             cex=0.7, col="red", cex.axis=0.7 )
        # ############################################################################################

    dev.off()
    sink()
}
# ######################################################################################################

