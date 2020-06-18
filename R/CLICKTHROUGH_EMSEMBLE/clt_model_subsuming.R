# ############################################################################################
# SUBSUMED MODELS
# ############################################################################################
USING_SUBSUMED_MODELS = CLT_OPTIONS[['use subsumed models']]
S0 = FALSE
S1 = FALSE
S2 = FALSE
S3 = FALSE
S4 = FALSE
S5 = FALSE
NUM_TRUE_MODELS = 9
# ############################################################################################


# ############################################################################################
SUBSUME_MODELS = function() {
    if ( USING_SUBSUMED_MODELS ) {
        if ( S0 ) {
            PROB_CORRECTION_MODEL_1 <<- PROB_CORRECTION_MODEL_2   # MATCHES SUBSUBE_COLS
            NUM_TRUE_MODELS = 8
        }
        if ( S1 ) {
            PROB_CORRECTION_MODEL_4 <<- PROB_CORRECTION_MODEL_9   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_5 <<- PROB_CORRECTION_MODEL_3   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_6 <<- PROB_CORRECTION_MODEL_3   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_7 <<- PROB_CORRECTION_MODEL_9   # MATCHES SUBSUBE_COLS
            NUM_TRUE_MODELS = 5
        }
        if ( S2 ) {
            PROB_CORRECTION_MODEL_4 <<- PROB_CORRECTION_MODEL_2   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_5 <<- PROB_CORRECTION_MODEL_2   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_6 <<- PROB_CORRECTION_MODEL_2   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_7 <<- PROB_CORRECTION_MODEL_3   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_8 <<- PROB_CORRECTION_MODEL_3   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_9 <<- PROB_CORRECTION_MODEL_3   # MATCHES SUBSUBE_COLS
            NUM_TRUE_MODELS = 3
        }
        if ( S3 ) {
            PROB_CORRECTION_MODEL_6 <<- PROB_CORRECTION_MODEL_1   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_7 <<- PROB_CORRECTION_MODEL_2   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_8 <<- PROB_CORRECTION_MODEL_3   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_9 <<- PROB_CORRECTION_MODEL_4   # MATCHES SUBSUBE_COLS
            NUM_TRUE_MODELS = 5
        }
        if ( S4 ) {
            PROB_CORRECTION_MODEL_6 <<- PROB_CORRECTION_MODEL_1   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_7 <<- PROB_CORRECTION_MODEL_1   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_8 <<- PROB_CORRECTION_MODEL_2   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_9 <<- PROB_CORRECTION_MODEL_2   # MATCHES SUBSUBE_COLS
            NUM_TRUE_MODELS = 5
        }
        if ( S5 ) {
            PROB_CORRECTION_MODEL_3 <<- PROB_CORRECTION_MODEL_8   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_4 <<- PROB_CORRECTION_MODEL_8   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_5 <<- PROB_CORRECTION_MODEL_9   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_6 <<- PROB_CORRECTION_MODEL_9   # MATCHES SUBSUBE_COLS
            PROB_CORRECTION_MODEL_7 <<- PROB_CORRECTION_MODEL_9   # MATCHES SUBSUBE_COLS
            NUM_TRUE_MODELS = 4
        }
        return()
    }
}
# ############################################################################################


# ############################################################################################
NOT_A_SUBSUMED_MODEL = function(i) {
    if ( S0 ) if ( i %in% c(1) )            return (FALSE)              # MATCHES ABOVE/BELOW
    if ( S1 ) if ( i %in% c(4,5,6,7) )      return (FALSE)              # MATCHES ABOVE/BELOW
    if ( S2 ) if ( i %in% c(4,5,6,7,8,9) )  return (FALSE)              # MATCHES ABOVE/BELOW
    if ( S3 ) if ( i %in% c(6,7,8,9) )      return (FALSE)              # MATCHES ABOVE/BELOW
    if ( S4 ) if ( i %in% c(6,7,8,9) )      return (FALSE)              # MATCHES ABOVE/BELOW
    if ( S5 ) if ( i %in% c(3,4,5,6,7) )    return (FALSE)              # MATCHES ABOVE/BELOW
    return (TRUE)
}
# ############################################################################################


# ############################################################################################
SUBSUME_COLUMNS = function(i) {
    if ( USING_SUBSUMED_MODELS ) {
        if ( S0 ) {
            if ( i == 1 ) return ( 2 ) # must exact match SUBSUME_MODELS SPECIFICATION
        }
        if ( S1 ) {
            if ( i == 4 ) return ( 3 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 5 ) return ( 3 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 6 ) return ( 9 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 7 ) return ( 9 ) # must exact match SUBSUME_MODELS SPECIFICATION
        }
        if ( S2 ) {
            if ( i == 4 ) return ( 2 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 5 ) return ( 2 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 6 ) return ( 2 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 7 ) return ( 3 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 8 ) return ( 3 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 9 ) return ( 3 ) # must exact match SUBSUME_MODELS SPECIFICATION
        }
        if ( S3 ) {
            if ( i == 6 ) return ( 1 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 7 ) return ( 2 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 8 ) return ( 3 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 9 ) return ( 4 ) # must exact match SUBSUME_MODELS SPECIFICATION
        }
        if ( S4 ) {
            if ( i == 6 ) return ( 1 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 7 ) return ( 1 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 8 ) return ( 2 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 9 ) return ( 2 ) # must exact match SUBSUME_MODELS SPECIFICATION
        }
        if ( S5 ) {
            if ( i == 3 ) return ( 8 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 4 ) return ( 8 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 5 ) return ( 9 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 6 ) return ( 9 ) # must exact match SUBSUME_MODELS SPECIFICATION
            if ( i == 7 ) return ( 9 ) # must exact match SUBSUME_MODELS SPECIFICATION
        }
        return(i)
        return(i)
    }
}
# ############################################################################################
