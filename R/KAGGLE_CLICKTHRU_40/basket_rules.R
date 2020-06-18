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
require(stringr)
library(arules)
# ######################################################################################################


# ##############################################################################################################
source( 'utilities.R' )
# ##############################################################################################################


# #####################################################################################
VERIFY_OPTARG ( OPTARG_NAME="TEST_ENABLED", OPTARG_VALUE=TRUE )
# #####################################################################################


# ##############################################################################################################
PRINT_RULESET = function( rules0, debug=FALSE ) {
    cat(HEADER)
    summary(rules0)
    inspect( rules0 )
    return
}
# ##############################################################################################################


# ##############################################################################################################
WHICH_ARE_SPECIFIC_RULES = function( rules, ITEM, QUALITY_LIFT_TRIGGER=4.0, debug=FALSE ) {  
    lhs_set = c()

    itemsets_lhs = as(attr(rules,"lhs"), "list" )
    if ( length(itemsets_lhs)==0 ) 
        return ( lhs_set )

    for ( i in 1:length(itemsets_lhs)) {
        lhs_item = itemsets_lhs[[i]][1]
        if ( !is.na(lhs_item))
            if ( str_detect(lhs_item, ITEM ) ) {
                quality = as(attr(rules,"quality"), "list" )
                lift = quality[[3]][i]
                if ( as.numeric(lift) > QUALITY_LIFT_TRIGGER )
                    lhs_set = append( lhs_set, lhs_item )
            }
    }

    if ( debug ) {
        cat(HEADER)
        print( length(lhs_set) )
    }

    return ( lhs_set )
}
# ######################################################################################################


# ######################################################################################################
ARE_SPECIFIC_RULES = function( new_rules, ITEM, debug=FALSE ) { 
    retval = ifelse(length( WHICH_ARE_SPECIFIC_RULES( new_rules, ITEM )) == 0, FALSE, TRUE )
    if ( debug ) {
        cat(HEADER)
        print( retval ) 
    }
    return ( retval )
}
# ######################################################################################################


# ######################################################################################################
# http://www.salemmarafi.com/code/collaborative-filtering-r/
# ##############################################################################################################
FIND_RULES_FOR_THIS_ITEM = function( RR, ITEM, SUPPORT, CONF, STEP=1/3, MIN_SUPPORT=0.05, debug=FALSE, silent=FALSE ) {
    CONTINUE = TRUE
    FOUND_ONE= FALSE

    rules = matrix()
    while ( CONTINUE ) {
        new_rules = apriori(data=RR, parameter=list(supp=SUPPORT,conf = CONF), appearance = list(default="rhs", lhs=ITEM), control=list(verbose=FALSE)) 

        # inspect( new_rules )

        ARE_SPECIFIC = ARE_SPECIFIC_RULES(new_rules, ITEM)

        if ( !FOUND_ONE & ARE_SPECIFIC ) 
            FOUND_ONE=TRUE
        if ( FOUND_ONE &  !ARE_SPECIFIC )
            break

        if ( !FOUND_ONE ) 
            NEW_SUPPORT = (1-STEP) * SUPPORT
        if ( FOUND_ONE ) 
            NEW_SUPPORT = (1+STEP) * SUPPORT

        if ( NEW_SUPPORT < MIN_SUPPORT )
            break
        if ( NEW_SUPPORT > 1 )
            break

        if ( debug ) print( paste( "FOUND=", FOUND_ONE, 
                                   "SUPPORT=", SUPPORT, 
                                   'ARE_SPECIFIC=', ARE_SPECIFIC, 
                                   "CONTINUE=", CONTINUE, 
                                   length(new_rules) ))

        rules   = new_rules
        SUPPORT = NEW_SUPPORT
    }

    # see if confidence can be increased while retaining some knowledge about item
    while ( ARE_SPECIFIC_RULES(rules, ITEM) ) {
        if ( debug ) print( paste( "FOUND=", FOUND_ONE, "SUPPORT=", SUPPORT, "CONTINUE=", CONTINUE, "CONF=", CONF, length(rules) ))

        CONF = (1+STEP) * CONF
        if ( CONF >= 1 )
            break

        new_rules = apriori(data=RR, parameter=list(supp=SUPPORT,conf = CONF), appearance = list(default="rhs", lhs=ITEM), control=list(verbose=FALSE) )
        if ( ARE_SPECIFIC_RULES(new_rules, ITEM) )
            break

        rules    = new_rules
    }

    if(debug) print(paste("FOUND=",FOUND_ONE,"SUPPORT=",SUPPORT,"CONTINUE=",CONTINUE,"CONF=",CONF,
                          'TOTAL=',length(rules),'SPECIFIC=',WHICH_ARE_SPECIFIC_RULES(rules,ITEM)))
    
    if ( !silent ) PRINT_RULESET( rules )

    return ( rules )
}
# ##############################################################################################################


# ##############################################################################################################
PRINT_FINDINGS = function( MOVIE, mba_findings, nmax=5 ) {
    if( !ARE_SPECIFIC_RULES(mba_findings, MOVIE) ) return

    mba_findings <-sort(mba_findings , decreasing=TRUE, by="lift")
    itemsets_rhs = as(attr(mba_findings,"rhs"), "list" )
    nmax = min(nmax,length(itemsets_rhs))

    if ( nmax != 0 ) {
        print( sprintf( "[MBA]: OTHER CUSTOMERS WHO LIKED THIS OTHER MOVIE %s: [%s]", MOVIE, M[M2ID(MOVIE),"movie_title"] ))
        print( sprintf( "   ALSO CHECKED OUT THESE OTHER %s TITLES:", nmax) )
        quality      = as(attr(mba_findings,"quality"), "list" )
        for ( i in 1:nmax) {
            M2_MOVIE = itemsets_rhs[[i]][1]
            M2 = M2ID( M2_MOVIE )

            support    = quality[[1]][i]
            confidence = quality[[2]][i]
            lift       = quality[[3]][i]

            RATING  = mean(ORIG_RATINGS[,M2_MOVIE], na.rm=TRUE)

            if ( TRUE ) {
                x0 = Z2[MOVIE,1]
                y0 = Z2[MOVIE,2]
                x1 = Z2[M2_MOVIE,1]
                y1 = Z2[M2_MOVIE,2]
                segments( x0, y0, x1=x1, y1=y1, col="red", lwd=2 )
            }

            print( sprintf( "   %5s-->%5s [%.1f stars]: [%5.2f, %5.2f, %5.2f] %s", MOVIE, M2_MOVIE, RATING, support, confidence, lift, as.character(M[M2,'movie_title'] )))
        }
    }

    return ( nmax )
}
# ##############################################################################################################


# ##############################################################################################################
APPLY_MARKET_BASKET_ASSOCIATON_ANALYSIS_WRT = function( RR, ITEM, SUPPORT=0.1, CONF=0.51, 
                                                        STEP=0.10, PIVOT=5, MIN_SUPPORT=0.05, 
                                                        debug=FALSE ) { 

    t0 = proc.time()
    RR = ifelse( is.na(RR), 0, RR )
    RR = ifelse( RR<PIVOT, 0, 1 )

    M1 = M2ID( ITEM )
    if ( debug ) print( paste( ITEM, as.character(M[M1,'movie_title'] )))

    mba_findings = FIND_RULES_FOR_THIS_ITEM( RR, ITEM, SUPPORT, CONF, STEP, MIN_SUPPORT, debug=FALSE, silent=TRUE )

    PRINT_FINDINGS( ITEM, mba_findings )

    if (debug) print( proc.time() - t0 )

    return ( mba_findings )
}
# ##############################################################################################################


# ##############################################################################################################
NUM_MOVIES=200
if ( TEST_ENABLED )
    for ( MOVIE in sample(MOVIE_LABELS, NUM_MOVIES) ) {
        cat(HEADER)
        R4R = RECOMMENDATIONS_FOR_U2M_RATINGS
        findings = APPLY_MARKET_BASKET_ASSOCIATON_ANALYSIS_WRT( R4R, 
                                                                MOVIE, 
                                                                SUPPORT=0.33,
                                                                CONF=0.80, 
                                                                STEP=0.33, 
                                                                PIVOT=4,
                                                                MIN_SUPPORT=5/nrow(R4R) )
        NEWLINE(3)
    }
# ##############################################################################################################


