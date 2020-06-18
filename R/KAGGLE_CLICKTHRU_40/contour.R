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
VERIFY_OPTARG ( OPTARG_NAME="TEST_ENABLED", OPTARG_VALUE=TRUE )
# ######################################################################################################


# ######################################################################################################
source( 'utilities.R' )
source( 'datasets.R' )
# ######################################################################################################


# #################################################################################################
INTERIOR_POINTS = function( X, Y, a, b, c, d, i=1 ) {
    W1 = which( a <= X  & X <= b )
    W2 = which( c <= Y  & Y <= d )
    W = intersect( W1, W2 )
    points( X1[W], X2[W], col=i, pch=i, cex=0.3 )
    return ( W1 )
}
# #################################################################################################


# #################################################################################################
CONTAINER = function( X, Y, xmin, xmax, fmin=min, fmax=max, tight=TRUE) {
    eps = 0
    if ( tight ) eps = (xmax-xmin) / 100

    #purposedly trim edges - does not work in boundary cases
    W1 = which( (xmin+eps) < X  & X < (xmax-eps) )
    a = fmin(X[W1]) 
    b = fmax(X[W1])
    c = fmin(Y[W1]) 
    d = fmax(Y[W1])

    points( a, y=c, pch="+", col="green", cex=0.3, bg="green" )
    points( b, y=c, pch="+", col="green", cex=0.3, bg="green" )
    points( a, y=d, pch="+", col="green", cex=0.3, bg="green" )
    points( b, y=d, pch="+", col="green", cex=0.3, bg="green" )

    segments( a, c, b, c, lwd=1, col="green" ) 
    segments( b, c, b, d, lwd=1, col="green" ) 
    segments( b, d, a, d, lwd=1, col="green" ) 
    segments( a, d, a, c, lwd=1, col="green" ) 

    retvals = c(a,b,c,d)
    return ( retvals )
}
# #################################################################################################


# #################################################################################################
RUNNING_SD = function( Y, w=20 ) {
    N = length(Y)
    Ysd = VECTOR(N)
    for( i in 1:w)  Ysd[i] = sd(Y[1:w])
    for( i in w:N ) Ysd[i] = sd(Y[(i-w):i])
    return ( Ysd )
}
# #################################################################################################


# #################################################################################################
RUNNING_MU = function( Y, w=30 ) {
    N = length(Y)
    Ymu = VECTOR(N)
    for( i in 1:w)  Ymu[i] = mean(Y[1:w])
    for( i in w:N ) Ymu[i] = mean(Y[(i-w):i])
    return ( Ymu )
}
# #################################################################################################


# #################################################################################################
EXTERIOR_BOUND = function( X, Y, w=2, lw1=3, lw2=15 ) {
    lw2 = 10*lw1
    N = length(Y)
    Y_SD = RUNNING_SD(Y)

    X = lowess(X, f=1/lw1)$y
    Y = lowess(Y, f=1/lw2)$y
    Y_MU = RUNNING_MU( Y )

    Y_MU = RUNNING_MU( Y )

    Ymax = Y_MU + 2.0 * lowess(Y_SD, f=1/lw1)$y
    Ymin = Y_MU - 2.0 * lowess(Y_SD, f=1/lw1)$y

    segments( X[1:N-1], Ymin[1:N-1], X[2:N],   Ymin[2:N], lwd=2, col="brown" )
    segments( X[1:N-1], Ymax[1:N-1], X[2:N],   Ymax[2:N], lwd=2, col="brown" )

    retvals = list(X=X, Ymin=Ymin, Ymax=Ymax )
    return ( retvals )
}
# #################################################################################################


# #################################################################################################
APPROXIMATE_EXTERIOR_POINTS = function( X, Y, W=20, lw1=5, lw2=15 ) {
    lw2 = 10*lw1
    N = length(Y)

    Ymin = VECTOR(N)
    Ymax = VECTOR(N)
    delta = W * (max(X) - min(X))/N
    for ( i in seq(1,N,W) ) {
        xmin = X[i] - delta
        xmax = X[i] + delta
        W = which( X > xmin & X < xmax )
        Ymin[i] = round(min(Y[W]), 3)
        Ymax[i] = round(max(Y[W]), 3)
        points( X[i], Ymin[i], col="black", pch=8, cex=0.6 )
        points( X[i], Ymax[i], col="black", pch=8, cex=0.6 )
    }
    retvals = list(X=X, Ymin=Ymin, Ymax=Ymax )
    return (retvals )
}
# #################################################################################################


# #################################################################################################
GET_SEQUENTIAL_ITERATORS = function( X, N ) {
    lx = min(X)
    hx = max(X)
    delta_x = (hx-lx)/N
    ix = seq( lx, hx, delta_x )
    return ( ix )
}
# #################################################################################################


# #################################################################################################
# http://www.math.com/tables/geometry/circles.htm 
# #################################################################################################
CENTER = function( a, b, c, d ) {
    x = a + (b - a)/2
    y = c + (d - c)/2
    r = (b-a)/2
    retvals = c(x, y, r)
    return( retvals )
}
# #################################################################################################


# #################################################################################################
CIRCLE = function( x, y, r, M=10 ) {
    theta=(0:(M-1))/M
    lines(x+r*cos(2*pi*theta),y+r*sin(2*pi*theta),asp=1)
}
# #################################################################################################


# ######################################################################################################
DRAW_BOUNDARIES = function( X1, X2, NUMBER_PARTITIONS, new_plot=TRUE ) {
    ix = GET_SEQUENTIAL_ITERATORS( X1, NUMBER_PARTITIONS )
    iy = GET_SEQUENTIAL_ITERATORS( X2, NUMBER_PARTITIONS )

    if ( new_plot ) 
        sunflowerplot(  X1,  X2, cex=0.4, col="red" )

    for ( i in 1:NUMBER_PARTITIONS ) {
        coords = CONTAINER( X1, X2, ix[i], ix[i+1] )
        a=coords[1]; b=coords[2]; c=coords[3]; d=coords[4]
    
        circ = CENTER ( a, b, c, d )
        x = circ[1]; y = circ[2]; r = circ[3]
             
        if ( new_plot )
            W = ( INTERIOR_POINTS( X1, X2, a, b, c, d, i ) )

        # APPROXIMATE_EXTERIOR_POINTS( X1[W], X2[W], W=20 )
    }

    EXTERIOR_BOUND ( X1, X2, lw1=1, lw2=60)
}
# #################################################################################################


# ######################################################################################################
if ( TEST_ENABLED ) {
    SD = 0.3
    NUMBER_PARTITIONS = 3

    SD_X <<- c(SD+.1, SD+0.1, SD+0.1, SD+0.1, SD+0.1 )
    MU_X <<- c(    0,      1,      2,      3,      4 )
    X = GET_CLUSTERED_X( MM=500, NN=5, N=2, SD_X, MU_X )

    X1 = X[,1]
    X2 = X[,2]

    DRAW_BOUNDARIES( X1, X2, NUMBER_PARTITIONS )
}
# ######################################################################################################

    
