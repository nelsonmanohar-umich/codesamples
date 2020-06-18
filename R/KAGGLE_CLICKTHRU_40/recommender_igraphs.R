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
library(igraph)
require(igraph)
# ######################################################################################################


# ######################################################################################################
PLOT_MATRIX = function( MAT, VNAMES=c(), WITH_NAMES=TRUE, layout=layout.fruchterman.reingold, WHO="USERS", DELETE_EMPTY=TRUE, debug=FALSE ) {
    if (nrow(MAT)==ncol(MAT))
        g = graph.adjacency( MAT, mode="directed", weighted=TRUE )
    else
        g=graph.data.frame(  MAT, directed=TRUE )

    V(g)$size  = 0

    for( i in 1:ncol(MAT) )
        V(g)[i]$name  = VNAMES[i]

    for ( i in 1:nrow(MAT) ) {
        which_ones  = names(which( MAT[,i]>=1, arr.ind=T))
        which_size  = ISNA(length(which_ones))

        V(g)[i]$color = 'green'
        V(g)[i]$size  = which_size
        V(g)[i]$name  = VNAMES[i]

        if ( which_size == 0 )
            V(g)[i]$visible = FALSE

        if ( which_size != 0 ) {
            which_color = sample(32:255,1)

            V(g)[which_ones]$color = which_color
            V(g)[which_ones]$size  = round(log(which_size)+2)

            E(g)[ i %--% which_ones ]$color = which_color
            E(g)[ i %--% which_ones ]$edge.width = round(log(which_size+2))

            if ( debug ) {
                print( paste ( i ) )
                print( paste(  V(g)[which_ones], V(g)[which_ones]$color, V(g)[which_ones]$size ) )
                print( paste(  E(g)[ i %--% which_ones ], E(g)[ i %--% which_ones ]$color, E(g)[ i %--% which_ones ]$edge.width ) )
                cat( HEADER )
            }
        }
    }

    for ( i in 1:nrow(MAT) ) {
        which_ones  = names(which( MAT[,i]>=1, arr.ind=T))
        which_size  = ISNA(length(which_ones))

        if( which_size ) {
            # rulesofreason.wordpress.com/2012/11/05/...
            V(g)[i]$name =VNAMES[i]
            V(g)[i]$label=VNAMES[i]
            V(g)[i]$label.dist=0.0
            if ( substr(VNAMES[i],1,1) == "M")  {
                V(g)[i]$label.font=2
                V(g)[i]$label.color="black"
                V(g)[i]$label.cex=0.40
            }
            if ( substr(VNAMES[i],1,1) == "U")  {
                V(g)[i]$label.font=3
                V(g)[i]$label.color="brown"
                V(g)[i]$label.cex=0.30
            }
        }
    }

    if ( DELETE_EMPTY )
        g = delete.vertices(  g, names(which(degree(g) == 0 )))

    plot_title = paste("COLLABORATIVELY FILTERED\nRECOMMENDATION SOCIAL NETWORK BETWEEN", WHO )
    if ( WITH_NAMES )
        plot.igraph( g, layout=layout, edge.arrow.size=0.05, main=plot_title )
    else
        plot.igraph( g, layout=layout, vertex.label=NA, edge.arrow.size=0.05, main=plot_title )

    return ( g )
}
# ######################################################################################################

