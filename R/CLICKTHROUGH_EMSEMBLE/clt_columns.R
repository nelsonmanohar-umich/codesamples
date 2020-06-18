# ############################################################################################
COLS = function( i ) {

    i = SUBSUME_COLUMNS(i)
    MIN_FSELECTION_IMPORTANCE = 0.01

    minus = function( x, arg ) { setdiff( x, arg ) }

    Ctypes = c( 'C01', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21' )
    Htypes = c( 'H01', 'H14', 'H15', 'H16', 'H17', 'H18', 'H19', 'H20', 'H21')

    correlated  = c(Ctypes)
    if ( CLT_OPTIONS$use_ctypes_instead_of_htypes )
        correlated  = c(Htypes)

    csfselect03 = c( minus(FSELECT_XVARS[1:3],  correlated))
    csfselect06 = c( minus(FSELECT_XVARS[1:6],  correlated))
    csfselect09 = c( minus(FSELECT_XVARS[1:9],  correlated))
    csfselect12 = c( minus(FSELECT_XVARS[1:12], correlated))
    csfselect15 = c( minus(FSELECT_XVARS[1:15], correlated))
    csfselect18 = c( minus(FSELECT_XVARS[1:18], correlated))
    csfselect21 = c( minus(FSELECT_XVARS[1:21], correlated))
    csfselect24 = c( minus(FSELECT_XVARS[1:24], correlated))

    dtvselect04 = c( minus(DTVARS[1:4], correlated))
    dtvselect08 = c( minus(DTVARS[1:8], correlated))
    dtvselect12 = c( minus(DTVARS[1:12], correlated))

    # ----------------------------------------------------------------------------------------
    csfselect00 = c( minus( rownames(FSELECT_ATTRIMP)[FSELECT_ATTRIMP>MIN_FSELECTION_IMPORTANCE], c(correlated) ))
    # ----------------------------------------------------------------------------------------
    if ( i == -1 )       { cols = csfselect24
    } else if ( i == 1 ) { cols = csfselect09
    } else if ( i == 2 ) { cols = csfselect12
    } else if ( i == 3 ) { cols = csfselect15
    } else if ( i == 4 ) { cols = csfselect18
    } else if ( i == 5 ) { cols = csfselect21
    } else if ( i == 6 ) { cols = csfselect24
    } else if ( i == 7 ) { cols = csfselect06
    } else if ( i == 8 ) { cols = csfselect09
    } else if ( i == 9 ) { cols = csfselect12
    } else if ( i == 0 ) { cols = c( csfselect00, 'device_ip' )
    } 

    cols = unique(cols)
    cols = cols[!is.na(cols)]

    return ( cols )
}
# ############################################################################################


