# ##########################################################################################
col1 = "site_category"
col2 = "banner_pos"
# ##########################################################################################

# ######################################################################################################
library(igraph)
require(igraph)
# ######################################################################################################


# ######################################################################################################
PLOT_MATRIX_2 = function( MAT, ROWS, COLS, VNAMES=c(), WITH_NAMES=TRUE, layout=layout.fruchterman.reingold, WHO="USERS", DELETE_EMPTY=TRUE, debug=TRUE ) {
    if (nrow(MAT)!=ncol(MAT)) return 

    g = graph.adjacency( MAT )

    colors = terrain.colors(length(V(g)))

    V(g)$size  = 0
    V(g)$name  = VNAMES
    V(g)$visible = FALSE

    for ( i in 1:nrow(MAT) ) {

        vname = V(g)$name[i]

        if ( i <= length(ROWS) ) {
            V(g)[i]$visible = TRUE
            V(g)[i]$label.dist=0.0
            V(g)[i]$label.font=2
            V(g)[i]$label.color="black"
            V(g)[i]$label.cex=0.30

            which_ones  = which( MAT[,i]>0 )
            which_size  = ifelse( length(which)>0, length(which_ones), 0 )
            wsum = length(which_ones)

            V(g)[i]$size  = wsum
            V(g)[i]$color = colors[i]
            E(g)[ i %--% which_ones ]$color = V(g)[i]$color
            E(g)[ i %--% which_ones ]$edge.width = 1 + wsum

            if ( debug ) {
                print( paste ( i ) )
                print( paste(  V(g)[which_ones], V(g)[which_ones]$color, V(g)[which_ones]$size ) )
                print( paste(  E(g)[ i %--% which_ones ], E(g)[ i %--% which_ones ]$color, E(g)[ i %--% which_ones ]$edge.width ) )
                cat( HEADER )
            }
        } else {
            # rulesofreason.wordpress.com/2012/11/05/...
            V(g)[i]$visible = TRUE
            V(g)[i]$color = colors[i]
            V(g)[i]$size  = 5
            V(g)[i]$label.font=3
            V(g)[i]$label.dist=0.0
            V(g)[i]$label.color="brown"
            V(g)[i]$label.cex=0.40
            which_ones = c()
        }

    }

    # if ( DELETE_EMPTY ) g = delete.vertices(  g, names(which(degree(g) == 0 )))

    plot_title = paste("COLLABORATIVELY FILTERED\nRECOMMENDATION SOCIAL NETWORK BETWEEN", WHO )
    if ( WITH_NAMES )
        plot.igraph( g, layout=layout, edge.arrow.size=0.05, main=plot_title )
    else
        plot.igraph( g, layout=layout, vertex.label=NA, edge.arrow.size=0.05, main=plot_title )

    return ( g )
}
# ######################################################################################################


gc(TRUE)
graphics.off()
# ##########################################################################################
filename = sprintf( "clt_sna_%s_%s.png", col1, col2)
png( filename, 2000, 1200)
    t = table( X[,col1], X[,col2])
    OMAT = as.matrix( t )
    w = order(apply( OMAT, 1, sum), decreasing=T)
    OMAT = MAT[w,]
    ROWS = rownames(OMAT)
    COLS = colnames(OMAT)
    VNAMES = c(ROWS, COLS)

    n = nrow(OMAT)
    m = ncol(OMAT)
    XYZ = rbind( cbind( MATRIX(n,n),       OMAT),        
                 cbind( t(OMAT),   MATRIX(m,m)))
    rownames(XYZ) = VNAMES
    colnames(XYZ) = VNAMES
    MAT = as.matrix(XYZ)
    PLOT_MATRIX_2(MAT, ROWS, COLS, VNAMES=VNAMES )# , layout=layout.spring)
dev.off()
# ##########################################################################################




#     g = graph.data.frame( MAT )
#     cat(HEADER)
#     print ( g )
#     cat(HEADER)
#     print ( V(g) )
#     cat(HEADER)
#     print ( E(g) )
#     cat(HEADER)
#     # ######################################################################################
# 
#     # ######################################################################################
#     WHO=sprintf("%s-->%s", col1, col2)
#     WITH_NAMES=TRUE
#     DELETE_EMPTY=TRUE
#     debug=FALSE
#     plot_title = sprintf("NETWORK BETWEEN %s TO %s", col1, col2 )
#     # ######################################################################################
# 
#     V(g)$name = VNAMES
# 
#     N = length(VNAMES) 
#     M = length(ROWS)
#     P = length(COLS)
#     col_colors = c(terrain.colors(M), gray.colors(P))
# 
#     # ######################################################################################
#     V(g)$size  = 0
#     for ( i in V(g) ) {
#         cat(HEADER)
# 
#         vname = V(g)[i]$name
# 
#         V(g)[i]$label=vname
#         V(g)[i]$label.dist=0.0
#         V(g)[i]$visible = FALSE
#         V(g)[i]$color = "gray"
# 
# 
#         if ( vname %in% ROWS ) {
#             which_color = col_colors[i]
#             which_ones  = names(which( OMAT[vname,] > 0 ))
#             which_size  = ISNA(length(which_ones))
#             m = length(which_ones)
# 
#             V(g)[i]$visible = TRUE
#             V(g)[i]$size = 3 + as.integer(1/10* length(which_ones))
#             V(g)[i]$label.font=2                # rulesofreason.wordpress.com/2012/11/05/...
#             V(g)[i]$label.color="white"
#             V(g)[i]$label.cex=2.4
#             vidxs = which( V(g)$name %in% which_ones)
#         } else {
#             which_color = col_colors[i]
#             which_ones  = 20
#             which_size  = 5
# 
#             m = 0
#             V(g)[i]$visible = TRUE
#             V(g)[i]$size = 5
#             V(g)[i]$label.font=3
#             V(g)[i]$label.color="white"
#             V(g)[i]$label.cex=5
#             vidxs = c()
#         }
# 
#         print( paste( i, vname, vname %in% ROWS, length(which_ones) ))
#         print( paste( which_ones, vidxs ))
# 
#         if ( m > 0 ) {
#             # V(g)[vidxs]$size = V(g)[which( V(g)$name %in% which_ones )]$size + 1
#             print( vidxs )
#             E(g)[ i %--% vidxs ]$color = V(g)[i]$color
#             E(g)[ i %--% vidxs ]$edge.width = 2 + round(m/5)
#             print ( E(g)[i] )
#             print ( E(g)[i] )
#         }
#         print( paste( "DONE WITH", i ))
#     }
#     # ######################################################################################
# 
#     # ######################################################################################
#     if ( FALSE ) { 
#         w = which(degree(g) == 0 )
#         if ( length(w) > 0 ) {
#             g = delete.vertices(  g, w )
#         }
#     }
# 
#     layout = layout.spring
#     if ( WITH_NAMES ) plot.igraph( g, layout=layout, edge.arrow.size=0.05, main=plot_title )
#     if (!WITH_NAMES ) plot.igraph( g, layout=layout, vertex.label=NA, edge.arrow.size=0.05, main=plot_title )
#     # ######################################################################################
