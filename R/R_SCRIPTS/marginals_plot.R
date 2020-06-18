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
LABEL_RULES = function( XY, dx=0.25, cex=0.5 ) {
    rules = sapply( PARSED_RULES, function(x)  {    LHS = sprintf("%s%s,%s%%:%s", x$LHS, x$YVAL, x$ACCURACY, x$FREQUENCY)
                                                    LHS = gsub(' ', '', LHS)
                                                    LHS = gsub("X\\$", '', LHS)
                                                    LHS = gsub("'", '', LHS)
                                                    LHS = gsub("==", '=', LHS)
                                                })
    for ( i in 1:GET_SIZE(PARSED_RULES)) {
        if ( GET_COVERAGE_OF_RULE( i ) != 0 )
            if ( GET_ACCURACY_OF_RULE( i ) != 0 )
                text( XY[i,1], XY[i,2], rules[i], pos=4, cex=0.7, col=XY[i,3])
    }
    return ()
}
# ######################################################################################################


# ######################################################################################################
DRAW_SQUARE_SEGMENTS = function( SEGMENTS ) {
    for ( i in 1:GET_SIZE(SEGMENTS)) {
        SEGMENT = SEGMENTS[i,] 
        x1 = as.integer(SEGMENT[1])
        y1 = as.integer(SEGMENT[2])
        x2 = as.integer(SEGMENT[3])
        y2 = as.integer(SEGMENT[4])
        order = as.integer(SEGMENT[5])+1
        dx = ( i*1 %% 30 ) / 180
        if ( i > 1 ) {
            curr_label = PARSED_RULES[[i]]$EXPRESSION
            prev_label = PARSED_RULES[[i-1]]$EXPRESSION
            if ( identical(substr(curr_label,1,12),substr(prev_label,1,12) ))
                segments( x1,y1, x1+dx, y1=y2, lwd=order )
        } else if ( i == 1 ) 
            segments( x1,y1, x1, y1=y2, lwd=order )
        segments( x1+dx,y2, x2, y1=y2, lwd=order )
    }
}
# ######################################################################################################


# ######################################################################################################
GET_ORDER_OF_RULE = function( idx ) {
    order = as.integer(PARSED_RULES[[idx]]$ORDER)
}
# ######################################################################################################


# ######################################################################################################
GET_COVERAGE_OF_RULE = function( idx ) {
    coverage = as.integer(PARSED_RULES[[idx]]$FREQUENCY)
}
# ######################################################################################################


# ######################################################################################################
GET_ACCURACY_OF_RULE = function( idx ) {
    label = PARSED_RULES[[idx]]$ACCURACY
}
# ######################################################################################################


# ######################################################################################################
GET_LABEL_OF_RULE = function( idx ) {
    label = PARSED_RULES[[idx]]$TARGET_FACTOR
}
# ######################################################################################################


# ######################################################################################################
IS_RESET_NEEDED = function( idx, order, N, L, n=12 ) {
    if ( idx > 1 ) {
        curr_label = PARSED_RULES[[idx]]$EXPRESSION
        prev_label = PARSED_RULES[[idx-1]]$EXPRESSION
        if ( !identical(substr(curr_label,1,n),substr(prev_label,1,n) ))
            L = c(order-1, N-idx*2)
    }
    return ( L )
}
# ######################################################################################################


# ######################################################################################################
BUILD_DRAW_DECISION_RULES = function (R, N, M, debug=FALSE) {
    XY       = MATRIX(R, 5 )
    SEGMENTS = MATRIX(R, 6 )
    XY       = as.data.frame(XY)
    SEGMENTS = as.data.frame(SEGMENTS)
    SEGMENTS = SEGMENTS[,]

    L1 = c( 1, M)
    for ( idx in 1:R ) {
        order    = GET_ORDER_OF_RULE ( idx )
        coverage = GET_COVERAGE_OF_RULE( idx )
        label    = GET_LABEL_OF_RULE ( idx )
        coords   = c( order, (N - idx * 2) )
    

        if ( order == 1 ) {
            L1 = coords
        } else if ( order == 2 ) {
            L1 = IS_RESET_NEEDED( idx, order, N, L1 )
            L2 = coords
            SEGMENTS[idx,] = c(L1, L2, 3-order, label )
        } else if ( order == 3 ) {
            L2 = IS_RESET_NEEDED( idx, order, N, L2 )
            L3 = coords
            SEGMENTS[idx,] = c(L2, L3, 3-order, label)
        }
        if ( debug ) print( sprintf( "%10s %10s %10s %10s %10s", idx, coords, order, coverage, label ) )
        XY[idx,] = c( as.integer(order), as.integer(N - idx * 2), as.integer(order), as.integer(coverage), 0) #as.character(label) )
    }

    RETVALS = list( XY=XY, SEGMENTS=SEGMENTS )
}
# ######################################################################################################


# ######################################################################################################
PLOT_DECISION_RULES = function( R, N, M, XY, SEGMENTS ) {
    xlim = c(1,M)
    ylim = c(1,N)
    plot ( XY[,1], XY[,2], t='p', xlim=xlim, ylim=ylim, col="black", cex=1, pch=23, bg="black", 
          xlab="RULE ORDER (SCOPING COMPLEXITY)",
          ylab="RULE DFS DISCOVERY ORDER",
          main=sprintf("PRECONDITIONED SEQUENTIAL EVALUATION\n3RD-ORDER DECISION RULES FOR %s.\n(EVALUATION ORDER: TOP-DOWN)", GET_YCLASSNAME()) )
    DRAW_SQUARE_SEGMENTS( SEGMENTS )
    LABEL_RULES( XY, dx=0.25, cex=0.5 )
}
# ######################################################################################################


# ######################################################################################################
DO_RULE_PLOT = function() {
    R = GET_SIZE(PARSED_RULES)
    N = R * 2 + 1
    M = 8

    DRAWING  = BUILD_DRAW_DECISION_RULES(R, N, M)
    SEGMENTS = DRAWING$SEGMENTS
    XY       = DRAWING$XY

    PLOT_DECISION_RULES( R, N, M, XY, SEGMENTS )
    return ( XY )
}
# ######################################################################################################



