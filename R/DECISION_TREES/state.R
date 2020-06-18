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
source( 'utilities.R')
# ######################################################################################################


# ######################################################################################################
VERIFY_OPTARG ( OPTARG_NAME="TEST_ENABLED",  OPTARG_VALUE=TRUE)
# ######################################################################################################


# ######################################################################################################
# GLOBALS
# ######################################################################################################
MAPPING_INIT_VAL_= 0
GLOBAL_INDEX_    = ""
GLOBAL_X_        = ""
GLOBAL_Y_        = ""
# ######################################################################################################


# ######################################################################################################
GET_INDEX_GENERATION = function( ITER="" ) {
    GENERATION = ITER
    if ( ITER== "" ) GENERATION = max(GLOBAL_INDEX_, na.rm=TRUE) + 1
    GENERATION = ifelse( is.na( GENERATION), 1 , GENERATION )
    return ( GENERATION )
}
# ######################################################################################################


# ######################################################################################################
UPDATE_XY_INDEXING = function( ACTIVATED_ROWS, ITER="", silent=TRUE, debug=FALSE ) {
    ACTIVATED_ROWS = ACTIVATED_ROWS[which(!is.na(ACTIVATED_ROWS))]
    GENERATION = ITER
    GENERATION = GET_INDEX_GENERATION ( ITER=ITER )

    NEW_INDEX  = GLOBAL_INDEX_
    NEW_INDEX[ACTIVATED_ROWS] = GENERATION
    GLOBAL_INDEX_ <<- NEW_INDEX

    if ( debug )  print( sprintf( "GENERATION [%3s] ACTIVATED ROWS: %s", GENERATION, CONCAT(ACTIVATED_ROWS)))
    if (!silent ) PRINT_INDEXING()

    return ( NEW_INDEX )
}
# ######################################################################################################


# ######################################################################################################
PRINT_INDEXING = function(debug=FALSE) {
    ACTIVATED_ROWS = which( GLOBAL_INDEX_[,1] != MAPPING_INIT_VAL_ ) 
    print( sprintf( "MAPPED SET [NAMES]: %s", CONCAT( rownames(GLOBAL_INDEX_)[ACTIVATED_ROWS] ) ) )
    if ( debug ) print( sprintf( "MAPPED SET [INDXS]: %s", CONCAT( ACTIVATED_ROWS ) ) )
    cat(HEADER)
    return ( ACTIVATED_ROWS )
}
# ######################################################################################################


# ######################################################################################################
IS_MAPPING_COMPLETE = function() {
    NON_ACTIVATED_ROWS = which( GLOBAL_INDEX_[,1] == MAPPING_INIT_VAL_ ) 
    return( NON_ACTIVATED_ROWS )
}
# ######################################################################################################


# ######################################################################################################
GET_CURRENT_INDEX = function( NAMES=TRUE, silent=TRUE ) {
    ACTIVATED_ROWNUMBERS = which( GLOBAL_INDEX_[,1] != MAPPING_INIT_VAL_ )
    if ( !silent ) {
        cat(HEADER)
        PRINT_INDEXING()
    }
    if ( NAMES ) {
        ACTIVATED_ROWNAMES  = rownames(GLOBAL_INDEX_)[ACTIVATED_ROWNUMBERS]
        return ( ACTIVATED_ROWNAMES  )
    }
    return ( ACTIVATED_ROWNUMBERS )
}
# ######################################################################################################


# ######################################################################################################
GET_CURRENT_INDEX_AT = function( ITER="", NAMES=TRUE, debug=TRUE ) {
    if ( ITER=="" ) return ( GET_CURRENT_INDEX( NAMES=NAMES ) )
    print( paste( "LOOKUP INTO A PREVIOUS GENERATION ACTIVATED:", ITER ) )
    ACTIVATED_ROWNUMBERS = which( GLOBAL_INDEX_[,1] != MAPPING_INIT_VAL_ & GLOBAL_INDEX_[,1]<=ITER )
    if ( NAMES ) {
        ACTIVATED_ROWNAMESS  = rownames(GLOBAL_INDEX_)[ACTIVATED_ROWNUMBERS]
        return ( ACTIVATED_ROWNAMESS )
    }
    return ( ACTIVATED_ROWNUMBERS )
}
# ######################################################################################################


# ######################################################################################################
GET_CURRENT_X = function(ITER="", NOT=TRUE ) {
    IDX = GET_CURRENT_INDEX_AT(ITER=ITER)
    if ( NOT ) IDX = setdiff( rownames(GLOBAL_INDEX_), IDX )
    XVALS = GLOBAL_X_[IDX,]
    return ( XVALS )
}
# ######################################################################################################


# ######################################################################################################
GET_CURRENT_Y = function(ITER="", NOT=TRUE ) {
    IDX = GET_CURRENT_INDEX_AT(ITER=ITER)
    if ( NOT ) IDX = setdiff( rownames(GLOBAL_INDEX_), IDX )
    if ( class(GLOBAL_Y_) != "matrix" & class(GLOBAL_Y_) != "data.frame" )
        YVALS = GLOBAL_Y_[IDX]
    else
        YVALS = GLOBAL_Y_[IDX,]
    return ( YVALS )
}
# ######################################################################################################


# ######################################################################################################
ALIGN_INDEXING = function( X, Y, ORDERING=1:nrow(X) ) {
    NEW_X = X
    rownames(NEW_X) = ORDERING

    NEW_INDEX = GLOBAL_INDEX_
    rownames(NEW_INDEX) = rownames(NEW_X)

    NEW_Y = Y
    rownames(NEW_Y) = rownames(NEW_X)

    GLOBAL_INDEX_ <<- NEW_INDEX
    GLOBAL_Y_     <<- NEW_Y
    GLOBAL_X_     <<- NEW_X
    return ( NEW_INDEX )
}
# ######################################################################################################


# ######################################################################################################
INITIALIZE_XY_INDEXING = function(X, Y, ORDERING=1:nrow(X), INIT_VAL=0) {
    MAPPING_INIT_VAL_ = INIT_VAL
    NEW_INDEX = MATRIX( GET_SIZE(X), 2, INIT_VAL ) 
    GLOBAL_INDEX_ <<- NEW_INDEX
    GLOBAL_INDEX_ <<- ALIGN_INDEXING( X, Y, ORDERING=ORDERING )
    return (NEW_INDEX)
}
# ######################################################################################################


# ######################################################################################################
RETRIEVE_GLOBAL_STATE = function() {
    RETVALS = list( X=GLOBAL_X_, Y=GLOBAL_Y_, INDEX=GLOBAL_INDEX_, INIT_VAL=MAPPING_INIT_VAL_ )
    return ( RETVALS )
}
# ######################################################################################################


# ######################################################################################################
if ( TEST_ENABLED ) {
    X = MATRIX(10,10)
    Y = MATRIX(10,2)
    
    for (i in 1:10)
        for (j in 1:10) 
            X[i,j] = (i-1)*10 + j
    
    INITIALIZE_XY_INDEXING( X, Y, ORDERING=20:29, INIT_VAL=MAPPING_INIT_VAL_ ) 

    UPDATE_XY_INDEXING( c(1,2) )
    UPDATE_XY_INDEXING( c(7,3))
    UPDATE_XY_INDEXING( c(8), )
    UPDATE_XY_INDEXING( c(9))

    LOCAL_X = GET_CURRENT_X()
    print( LOCAL_X )
    LOCAL_Y = GET_CURRENT_Y()
    print( LOCAL_Y )

    LOCAL_X = GET_CURRENT_X(ITER=2)
    print( LOCAL_X )
    LOCAL_Y = GET_CURRENT_Y(ITER=2)
    print( LOCAL_Y )
}
# ######################################################################################################


