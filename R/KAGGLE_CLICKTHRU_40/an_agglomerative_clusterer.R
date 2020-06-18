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
sink('output.hierarchical.agg.clustering.out', split=TRUE)
# ######################################################################################################

LICENSETEXT = "****
These R code samples (version Sep/2014), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License."

WARNING = "*****
*****                          UNFINISHED
*****                  FOR ILLUSTRATIVE PURPOSES 
***** This is NOT the PROPER hierarchical agglomerative clustering                   ****
***** This is an experimental and O(inefficient) variant of agglomerative clustering ****
***** YET better suited for illustrative/explorative purposes.                       ****
*****"


message( LICENSETEXT )
message( WARNING )
message("")
# ######################################################################################################


# ######################################################################################################
OLD_CA = commandArgs()
commandArgs <- function() list(TEST_ENABLED=FALSE, DO_DBTEST=FALSE, DO_DISTANCE_VALIDATION=FALSE, RUN_VALIDATE_SGD=FALSE )
opts = options( digits=1, width=140 )
# ######################################################################################################


# ######################################################################################################
source( 'utilities.R' )
source( 'datasets.R' )
source( 'fselect.R' )
# ######################################################################################################


# ######################################################################################################
require( 'igraph' )
# ######################################################################################################


# ######################################################################################################
FIND_ALL_NEARBY = function( D, i, epsilon ) {
    NEARBY_ROWS = c()
    if ( i %in% rownames(D) )
        NEARBY_ROWS = names(which( D[,as.character(i)] < epsilon ))
    return ( NEARBY_ROWS )
}
# ######################################################################################################


# ######################################################################################################
GET_D = function( X ) {
    D   = as.matrix(dist( X, diag=TRUE, upper=TRUE ))
    return ( D )
}
# ######################################################################################################


# ######################################################################################################
TRACK_ISET = function( ROWNAMES, CIDNAME ) {
    for ( rown in ROWNAMES ) {
        MAPPING[ rown ] <<- CIDNAME
    }
    return ( MAPPING )
}
# ######################################################################################################


# ######################################################################################################
PRINT_MAPPING = function( ) {
    cat( HEADER )
    print( table( MAPPING ) )
    cat(HEADER)
    cat(HEADER)
    cat(HEADER)
    NEWLINE(1)
}
# ######################################################################################################


# ######################################################################################################
GET_CLUSTER_NUMBER = function() {
    NUMBER_CLUSTERS <<- NUMBER_CLUSTERS + 1
    return ( NUMBER_CLUSTERS )
}
# ######################################################################################################


# ######################################################################################################
GET_CLUSTER_NAME = function( cid ) {
    paste( CLUSTERNAME_BASE, cid, sep="")
}
# ######################################################################################################


# ######################################################################################################
GET_DISTANCES_FROM = function( X, CID )  {
    cid = GET_CLUSTER_NAME(CID)
    Xi = X[cid,]
    d = as.matrix((rowSums( X - Xi )^2))
    return ( d )
}
# ######################################################################################################


# ######################################################################################################
GET_CLUSTER_LEVEL = function( CIDNAME ) {
    which( LETTERS == substr(CIDNAME,1,1) )
}
# ######################################################################################################


# ######################################################################################################
IS_CLUSTER = function( ROWNAMES ) {
    rval = c()
    for( ROWNAME in ROWNAMES ) {
        val = FALSE
        if ( substr(ROWNAME,1,1) %in% LETTERS ) val = TRUE
        rval = append( rval, val )
    }
    return ( rval )
}
# ######################################################################################################


# ######################################################################################################
UPDATE_CENTROID_TRACKING = function( DX, CIDNAME ) {
    UPDATE = MATRIX(1,(ncol(ORIG_X)+1))
    UPDATE[1,1] = CIDNAME
    for ( i in 2:ncol(UPDATE) )
        UPDATE[1,i] = DX[i-1]
    TOTAL_NC     <<- TOTAL_NC + 1
    CLUSTMAT[TOTAL_NC,] <<- UPDATE
}
# ######################################################################################################


# ######################################################################################################
COLLAPSE_X = function( X, CID, REMAINDER_ROWNAMES, NEARBY_ROWNAMES, CIDNAME ) {
    DX   = apply( X[NEARBY_ROWNAMES, ], 2, mean )

    SUBX = X[REMAINDER_ROWNAMES, ]

    UPDATE_CENTROID_TRACKING( DX, CIDNAME )

    NX = rbind( SUBX, DX )

    rownames(NX) = c( REMAINDER_ROWNAMES, CIDNAME )

    return ( NX )
}
# ######################################################################################################


# ######################################################################################################
COLLAPSE_D = function( X, CID, REMAINDER_ROWNAMES, NEARBY_ROWNAMES ) {
    D <<- GET_D( X )
    return ( D)
}
# ######################################################################################################


# ######################################################################################################
UPDATE_D = function ( X, D, CID, iset ) {
    A = GET_DISTANCES_FROM( X, CID )
    M = nrow(D)
    D[,M] = A
    D[M,] = t(A)
    TRACK_ISET( iset, CID )
    return ( D )
}
# ######################################################################################################


# ######################################################################################################
FIND_ALL_CLUSTERED_ITEMS = function( X, D, EPSILON, debug=FALSE ) {
    RETVALS = list( 'X'=X, 'D'=D )

    ORDER_TO_USE = names( sort( rowSums(D<EPSILON,na.rm=TRUE), decreasing=TRUE ) )

    for ( i in ORDER_TO_USE ) {

        CURRENT_ROWNAMES = rownames(X)

        if ( i %in% CURRENT_ROWNAMES  ) {

            NEARBY_ROWNAMES = FIND_ALL_NEARBY( D, i, EPSILON )

            NEARBY_ROWNAMES = setdiff( NEARBY_ROWNAMES, names(MAPPING) )

            if ( length(NEARBY_ROWNAMES) > NOISE_THRESHOLD ) {

                REMAINDER_ROWNAMES = setdiff( CURRENT_ROWNAMES, NEARBY_ROWNAMES )

                CID = GET_CLUSTER_NUMBER()

                CIDNAME = GET_CLUSTER_NAME(CID)

                X = COLLAPSE_X( X, CID, REMAINDER_ROWNAMES, NEARBY_ROWNAMES, CIDNAME )
                D = GET_D( X ) # D = COLLAPSE_D( X, CID, REMAINDER_ROWNAMES, NEARBY_ROWNAMES )
                TRACK_ISET( NEARBY_ROWNAMES, CIDNAME )

                print( sprintf( "CID=%s CIDNAME=%s   REMAINDER_ROWS=%s    TARGET=%5s --> NEARBY= %s",
                               CID, CIDNAME, length(REMAINDER_ROWNAMES), i, CONCAT(NEARBY_ROWNAMES)))

                print( sprintf("PROCESSING: CID: %s  w/ D=%sx%s and X=%sx%s", CIDNAME, nrow(D), ncol(D), nrow(X), ncol(X) ) )

                if ( debug ) points ( X[NEARBY_ROWNAMES,1], X[NEARBY_ROWNAMES,2], pch=CID%%25, col=CID%%300, cex=0.3 )

                NEW_ELEMENTS = c()
                OVERALL_CLUSTER_ELEMENTS = c() 
                for( rown in NEARBY_ROWNAMES ) {
                    NEW_ELEMENTS = INDEXING[[rown]]
                    OVERALL_CLUSTER_ELEMENTS = append( OVERALL_CLUSTER_ELEMENTS, NEW_ELEMENTS )
                    if ( IS_CLUSTER(rown) ) print( sprintf("APPENDING SUBSUMED CLUSTER %6s <-- %6s [%4s items]", CIDNAME, rown, length(NEW_ELEMENTS)))
                }

                NEW_ELEMENTS = append( NEARBY_ROWNAMES, OVERALL_CLUSTER_ELEMENTS )

                INDEXING[[ CIDNAME ]] <<- unique( NEW_ELEMENTS )

                print( paste( "CLUSTERNAME", CIDNAME, "TOTAL ELEMENTS:", length(INDEXING[[CIDNAME]]) ) )
                if ( debug ) print( INDEXING[[CIDNAME]] )

                cidx = NEW_ELEMENTS[which(IS_CLUSTER(NEW_ELEMENTS))]
                cidx = as.numeric(sapply( cidx, function( x ) { which( CLUSTMAT == x ) } ))
                points( CLUSTMAT[cidx,2],CLUSTMAT[cidx,3], pch=CID%%25, col=CID%%300, cex=0.8 )

                pidx = NEW_ELEMENTS[which(!IS_CLUSTER(NEW_ELEMENTS))]
                pidx = as.character(sapply( pidx, function( x ) { which( rownames(ORIG_X) == x ) } ))
                points( ORIG_X[pidx,1],  ORIG_X[pidx,2],   pch=CID%%25, col=CID%%300, cex=0.3 )

                cidx = as.numeric(which( CLUSTMAT == CIDNAME ))
                points( CLUSTMAT[cidx,2], CLUSTMAT[cidx,3],   pch=CID%%25, col="brown", cex=1) #, CID%%300, cex=1.5 )
                text(as.numeric(CLUSTMAT[cidx,2]), as.numeric(CLUSTMAT[cidx,3]), labels=CIDNAME, col="black", cex=log(GET_CLUSTER_LEVEL(CIDNAME))+1/2)

                if ( debug )
                    for( rown in INDEXING[[CIDNAME]] ) {
                        if ( IS_CLUSTER(rown) ) {
                            print( sprintf("PLOTTING CENTER OF SUBSUMED CLUSTER %6s <-- %6s [%4s items]", CIDNAME, rown, length(INDEXING[[rown]])))
                            cidx = which( CLUSTMAT == rown )
                            points( CLUSTMAT[cidx,2],      CLUSTMAT[cidx,3],      pch=CID%%25, col=CID%%300, cex=0.8 )
                        } else {
                            pidx = which( rownames(ORIG_X) == rown )
                            points( ORIG_X[pidx,1], ORIG_X[pidx,2], pch=CID%%25, col=CID%%300, cex=0.3 )
                        }
                    }

                if ( debug ) print( paste( CID, nrow(D), ncol(D), nrow(X), ncol(X) ) )

                cat(HEADER)
            }
        }
    }
    RETVALS = list( 'X'=X, 'D'=D )
    return ( RETVALS )
}
# ######################################################################################################


# ######################################################################################################
DO_SUMMARY = function( DD, XX, ITER, debug=FALSE ) {
    LENGTHS = sapply( INDEXING, function(x) length(x))
    CLUSTER_LENGTHS = LENGTHS[which( IS_CLUSTER(names(LENGTHS)))]

    cat(HEADER)
    cat(HEADER)
        print( "AGGLOMERATED CLUSTER MEMBERSHIPS")
        print( INDEXING[names(CLUSTER_LENGTHS)])
    cat(HEADER)
        print( "AGGLOMERATED SIZE")
        print( paste( "NUMBER_CLUSTERS", length(CLUSTER_LENGTHS) ) )
    cat(HEADER)
        print( "MAPPING")
        print( table(MAPPING ))
    cat(HEADER)
        print( "NOISE")
        NC = length(table(MAPPING))
        CL = sum(table(MAPPING)) - NC
        NOISE = c()
        for( i in 1:nrow(ORIG_X)) 
            if ( !(i %in% names(MAPPING))) 
                NOISE = append( NOISE, i )
        print( NOISE )
    cat(HEADER)
        print( paste( "ORIGINAL EPSILON  ", ORIGINAL_EPS))
        print( paste( "FINAL EPSILON     ", EPS))
        print( paste( "STEP_DISTANCE     ", STEP))
        print( paste( "NUMBER OF CLUSTERS", NC ) )
        print( paste( "NUMBER OF SAMPLES ", nrow(ORIG_X) ) )
        print( paste( "NUMBER OF MAPPINGS", CL ) )
        print( paste( "NUMBER OF MISSINGS", length(NOISE) ))
    cat(HEADER)
        print ( "TOP LEVEL CENTROIDS" )
        w = which(IS_CLUSTER(rownames(XX)))
        print ( XX[rownames(XX)[w],] )
    cat(HEADER)
        WRITE_TREE()
        if ( debug ) DRAW_TREE( ITER )
    cat(HEADER)

    RETVALS = list( INDEXING=INDEXING, 
                    MAPPINGS=MAPPINGS, 
                    EPS=EPS, 
                    STEP=STEP, 
                    ORIGINAL_EPS=ORIGINAL_EPS, 
                    NUM_CLUSTERS=NC, 
                    CLUSTER_LENGTHS=CL, 
                    NOISE_SAMPLES=NOISE,
                    CENTROID_DISTANCES=DD,
                    CENTROID_VALUES=XX )


    return ( RETVALS )
}
# ######################################################################################################


# ######################################################################################################
TERMINATE = function (debug=FALSE) {
    FIN = FALSE
    W = which( names(table(MAPPING)) > CLUSTERNAME_BASE )
    if ( !is.null(W) & length(W)!= 0 ) {
        WN = names(table(MAPPING))[W]
        IWN = INDEXING[ WN ]
        LIWN = sapply( IWN, length )
        SLIWN = sum( LIWN, na.rm=TRUE )
        if ( debug ) {
            print( WN )
            print( IWN )
            print( LIWN )
        }
        print( WN )
        print( SLIWN )
        cat( HEADER )
        if ( length(W) <= 2 )
            if ( abs( SLIWN - nrow(ORIG_X) ) < (0.10 * nrow(ORIG_X))) {
                print ( "TERMINATING" )
                FIN = TRUE
            }
    }
    return ( FIN )
}
# ######################################################################################################


# ######################################################################################################
AGGLOMERATIVE_ITERATOR = function( ORIG_X, D, EPSILON, STEP, REWRITE_PDF=TRUE, DEBUG=FALSE ) {
    XX = ORIG_X
    DD = D

    graphics.off()
    dev.new( X11, 16, 12 )
    dPDF = dev.set()
    op = par( mfrow=c( P, P ) )

    # X11() # http://stackoverflow.com/questions/8058606/saving-plot-as-pdf-and-simultaneously-display-it-in-the-window-x11
    dev.new( X11, 11, 8 )
    dX11 = dev.cur()
    dX11 = dev.set( dX11 )

    sunflowerplot( ORIG_X[,1], ORIG_X[,2], main=sprintf( "USING BASE CLUSTERING DISTANCE (EPS)=%.2f", EPSILON ), pch="+", col="gray", cex=1.0 )

    for ( idx in 1:min(2*P^2,(10*MAXHEIGTH)) ) {
        gc()

        cat(HEADER)
        cat(HEADER)

        if( TERMINATE() ) break

        EPSILON = EPSILON + STEP
        EPS  <<- EPSILON
        CLUSTERNAME_BASE <<- LETTERS[idx]
        NUMBER_CLUSTERS  <<- 0

        print( paste( idx, CLUSTERNAME_BASE, EPSILON, length(table(MAPPING)) ) )
    
        RETVALS = FIND_ALL_CLUSTERED_ITEMS( XX, DD, EPSILON )
        if ( TRUE ) {
            XXx = as.matrix(RETVALS$X)
            ORIGROWNAMES  = rownames(XX)
            CENTROID_ROWS = setdiff(rownames(XXx),rownames(XX))
            XXx = XXx[CENTROID_ROWS,]
            XX = rbind(XX,XXx)
            rownames(XX) = c(ORIGROWNAMES,CENTROID_ROWS)
            DD = GET_D( XX )
        } else {
            DD = RETVALS$D
            XX = RETVALS$X
        }

        MAPPINGS[[idx]] <<- MAPPING         # BOTH ARE GLOBALS **** MAPPING IS UPDATED ACROSS ITERATIONS ****

        PRINT_MAPPING( )

        if (!REWRITE_PDF)
            dev.copy2pdf(file = sprintf("plot_agglclust_cluster_generation_%s.pdf", idx ))

        dev.set( dPDF )
        DRAW_TREE( idx )
        dev.set( dev.prev() )
        NEWLINE(3)
    }

    if (REWRITE_PDF) dev.copy2pdf(file = sprintf("plot_agglclust_cluster_current_generation.pdf"))

    print( "PDF plots were generated under filenames plot_aggclust_cluster_generation_x.pdf to provide a visual snapshop of the cluster assigment" )
    print( "PDF plots were generated under filenames plot_aggclust_cluster_generation_x.pdf to provide a visual snapshop of the cluster assigment" )

    RETVALS = DO_SUMMARY( DD, XX )

    return ( RETVALS )
}
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################



# ######################################################################################################
CLUSTER_CHILDREN_OF = function( CIDNAME ) {
    CLUSTER_MEMBERSHIP = INDEXING[CIDNAME]
    CHILDREN = names(which(sapply( CLUSTER_MEMBERSHIP[[1]], IS_CLUSTER )))
}
# ######################################################################################################


# ######################################################################################################
CLUSTER_PREDECESSORS_OF = function( CIDNAME ) {
    CLUSTER_NAMES = names(table(MAPPING))
    PREDECESSORS = c() 
    for ( cidname in CLUSTER_NAMES ) { 
        cluster_children = CLUSTER_CHILDREN_OF( cidname )
        if ( CIDNAME %in% cluster_children ) {
            PREDECESSORS = append( PREDECESSORS, cidname )
        }
    }
    return ( PREDECESSORS )
}
# ######################################################################################################


# ######################################################################################################
EXTRACT_TREE = function() {
    CLUSTER_NAMES = names(table(MAPPING))
    CTREE = list()
    PTREE = list()
    for ( CIDNAME in CLUSTER_NAMES ) {
        CTREE[[CIDNAME]] = CLUSTER_CHILDREN_OF( CIDNAME )
        PTREE[[CIDNAME]] = CLUSTER_PREDECESSORS_OF( CIDNAME )
    }
    RETVALS = list( CTREE=CTREE, PTREE=PTREE )
    return ( RETVALS )
}
# ######################################################################################################


# ######################################################################################################
WRITE_TREE = function( debug=FALSE ) {
    cat( HEADER)
    print( "RELATIONSHIPS BETWEEN AGGLOMERATED CLUSTERS" )
    cat( HEADER)
    C = table(MAPPING)
    CLUSTER_NAMES = names(C)
    TREES = list( )
    for ( CIDNAME in sort(CLUSTER_NAMES,decreasing=TRUE) ) {
        CTREE = CLUSTER_CHILDREN_OF( CIDNAME )
        PTREE = CLUSTER_PREDECESSORS_OF( CIDNAME )
        TREES[[ CIDNAME ]] = list( PREDECESSOR=PTREE, CHILDREN=CTREE )
        print( sprintf( "%4s --> |N|=%4d SAMPLES, P=%24s |C|=%4d CLUSTERS: %s", 
                       CIDNAME, C[CIDNAME], CONCAT(PTREE), length(CTREE), CONCAT( CTREE ) ) )
    }
    if ( debug ) print( TREES )
    cat( HEADER )
    return ( TREES )
}
# ######################################################################################################


# ######################################################################################################
GET_CLUSTER_NAMES = function() {
    CLUSTER_SUMMARY= table(MAPPING)
    CLUSTER_NAMES  = names(CLUSTER_SUMMARY)
    return ( CLUSTER_NAMES )
}
# ######################################################################################################


# ######################################################################################################
GET_CENTROIDS = function() {
    N              = ncol(ORIG_X)
    CLUSTER_NAMES  = GET_CLUSTER_NAMES()
    N_CLUSTERS     = length(CLUSTER_NAMES)
    CENTROIDS      = CLUSTMAT[1:N_CLUSTERS,1:(N+1)]
    CENTROIDS      = t(apply( CENTROIDS[,2:(N+1)], 1, function(x) { as.numeric(as.numeric(x)) }))
    rownames(CENTROIDS) = CLUSTER_NAMES
    return ( CENTROIDS )
}
# ######################################################################################################


# ######################################################################################################
# http://www.statmethods.net/advstats/cluster.html
# ######################################################################################################
DRAW_DENDROGRAM = function( K=0, DO_PDF=TRUE ) {
    CENTROIDS      = GET_CENTROIDS()
    DENDROGRAM     = hclust( dist( CENTROIDS ) )
    plot( DENDROGRAM, hang=TRUE, main=paste("DENDROGRAM(AGGLOMERATIVE CLUSTERING RELATIONSHIPS)"), cex=0.6, cex.axis=0.8 )
    if ( K != 0 ) {
        groups <- cutree(DENDROGRAM, k=K) # cut tree into K clusters
        rect.hclust(DENDROGRAM, k=K, border="red") 
    }
    if ( DO_PDF ) {
        print( "PDF plots generated under filename plot_aggclust_dendrogram_hierarchy.pdf to provide a visual snapshop of the cluster hierarchy" )
        dev.copy( pdf, 'plot_aggclust_dendrogram_hierarchy.pdf', 11, 8 )
        dev.off()
    }
    return (DENDROGRAM)
}
# ######################################################################################################


# ######################################################################################################
DRAW_TREE = function( iter="" ) {
    N              = ncol(ORIG_X)
    CLUSTER_SUMMARY= table(MAPPING)
    CLUSTER_NAMES  = names(CLUSTER_SUMMARY)
    N_CLUSTERS     = length(CLUSTER_NAMES)

    CENTROIDS      = CLUSTMAT[1:N_CLUSTERS,1:(N+1)]
    CENTROIDS      = t(apply( CENTROIDS[,2:(N+1)], 1, function(x) { as.numeric(as.numeric(x)) }))
    CENTROID_DIST  =     dist(CENTROIDS, upper=TRUE, diag=TRUE )

    CLSTR_NUMBERS  = sapply( CLUSTER_NAMES,  function(x) which(CLUSTER_NAMES==x ))
    CLSTR_LETTERS  = sapply( CLUSTER_NAMES,  function(x) which(LETTERS==substr(x,1,1)))
    CLSTR_LEVELS   = unique(CLSTR_LETTERS) 

    CLSTR_SIZES    = CLUSTER_SUMMARY
    CLSTR_CUMSUM   = cumsum(CLSTR_SIZES)
    CLSTR_CHILDREN = sapply( CLUSTER_NAMES,  CLUSTER_CHILDREN_OF )
    CLSTR_NUMCHILD = sapply( CLSTR_CHILDREN, length )

    SSE = sapply(rownames(ORIG_X), function(x) { sum(ORIG_X[x,] - CENTROIDS[which(CLUSTMAT==MAPPING[x]),])^2 })
    MSE = sum(SSE)/nrow(ORIG_X)

    # g <- graph.empty() + vertices(c(names(MAPPING),CLUSTER_NAMES ))
    # V(g)$size  = 0
    # V(g)$color = "gray"
    g <- graph.empty() + vertices(CLUSTER_NAMES )
    for ( CIDNAME in sort(CLUSTER_NAMES,decreasing=TRUE) ) {
        V(g)[CIDNAME]$size  = 2 * (2 + (CLSTR_LETTERS[CIDNAME]/1.75)^2)
        V(g)[CIDNAME]$color = CLSTR_LETTERS[CIDNAME] + 2
            # rulesofreason.wordpress.com/2012/11/05/...
            V(g)[CIDNAME]$label.font=2
            V(g)[CIDNAME]$label.cex=0.6
        CTREE = CLSTR_CHILDREN[ CIDNAME ][[1]]
        if ( length(CTREE)==0 ) next
        print( paste( CIDNAME, CTREE ) )
        for( child in CTREE ) {
            g = add.edges( g, c( CIDNAME, child ))
            # v = names(which( MAPPING == child ))
            # for ( vv in v ) g = add.edges( g, c( child, vv ))
        }
        # V(g)[CIDNAME]$label.font=2
        # V(g)[CIDNAME]$label.color="black"
        # V(g)[CIDNAME]$label.cex=0.80
        # E(g)[ child %--% vv ]$weight = ...
        g = simplify(g)
    }
    plot.igraph( g, edge.arrow.size=0.15, main=sprintf("AGGLOMERATIVE CLUSTERING\nAT GENERATION %s", iter), xlab=sprintf( "MSE=%.3f  EPS=%.3f", MSE, EPS ))
    dev.copy2pdf(file = sprintf("plot_agglclust_cluster_hierachy-%s.pdf", as.integer((iter-1)/P^2)))
    return()
}
# ######################################################################################################





# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
ORIG_X          = GET_CLUSTERED_X( M=500, NCLUSTERS=5, NFEATURES=4, SD_X=rep(0.50,5), PATTERN="SQUARE", SD=0.0 )
D               = GET_D ( ORIG_X )
CLUSTMAT        = MATRIX( nrow(ORIG_X), ncol(ORIG_X)+1, initval="" )
NOISE_THRESHOLD = round(sqrt(nrow(ORIG_X))/2)
EPS             = (max(D)+3*sd(D))/sqrt(nrow(D))
ORIGINAL_EPS    = EPS
STEP            = 3 * mean(D)/sqrt(nrow(D))
MAXHEIGTH       = 1 + round(3 * EPS/STEP) 
TOTAL_NC        = 0
MAPPINGS        = list()
MAPPING         = c()
DEBUG           = FALSE
CLUSTERNAME_BASE= LETTERS
NUMBER_CLUSTERS = 0
P               = 2

INDEXING        = list()
for ( rown in rownames(ORIG_X) )
    INDEXING[[rown]] = rown

RETVALS = AGGLOMERATIVE_ITERATOR( ORIG_X, D, EPS, STEP, DEBUG=FALSE )

# K could be extracted from the NUMBER OF SCC in the hierarchy graph 
DRAW_DENDROGRAM( K=5, DO_PDF=TRUE )

message( WARNING )

options( opts )
sink()

