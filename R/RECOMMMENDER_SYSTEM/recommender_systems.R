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
opts = options(width=120, digits=2, error = function() traceback(2))
sink( "output.recommender_system.out", split=TRUE )

LICENSETEXT = "These R code samples (version Sep/2014), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License." 
message( LICENSETEXT )
message("")
# ######################################################################################################


# ######################################################################################################
require(stringr)
# ######################################################################################################


# ######################################################################################################
OLD_CA = commandArgs()
commandArgs <- function() list(TEST_ENABLED=FALSE, DO_DISTANCE_VALIDATION=FALSE, RUN_VALIDATE_SGD=FALSE )
source( 'utilities.R' )
source( 'fselect.R' )
source( 'datasets.R' )
source( 'distances.R' )
source( 'stochastic_gradient_descent.R' )
source( 'basket_rules.R' )
source( 'recommender_diagnostics.R' )
source( 'recommender_igraphs.R')
# ######################################################################################################



# ###################################################################################################
U2ID = function( USER,  p=10 ) { as.integer(substr( USER,  2, p )) }
M2ID = function( MOVIE, p=10 ) { as.integer(substr( MOVIE, 2, p )) }
ID2M = function( ID ) { paste( "M", ID, sep="")}
ID2U = function( ID ) { paste( "U", ID, sep="")}
# ###################################################################################################



# ###################################################################################################
COMBINED_RECOMMENDER_SYSTEM = function() {
}
# ###################################################################################################


# ###################################################################################################
GENERATE_FEATURE_RATING_RECOMMENDATIONS_FOR_USER = function () {
}
# ###################################################################################################


# ###################################################################################################
IDENTIFY_TOP_MOVIE_RECOMMENDATIONS_FOR_USER = function () {
}
# ###################################################################################################


# ######################################################################################################
EXTRACT_RATINGS_SUBMATRIX = function( OR, m=100, n=200, WO_ZERO_ENTRIES=TRUE, debug=FALSE ) {
    m = min(nrow(OR), m)
    n = min(ncol(OR), n)
    if ( m == 0 ) m = nrow(OR)
    if ( n == 0 ) n = ncol(OR)

    sampled_users  = sort(sample(1:nrow(OR),m))
    sampled_movies = sort(sample(1:ncol(OR),n))

    mdat = OR[sampled_users, sampled_movies]

    retvals = list( 'RATMAT'=mdat, 'WHICH_USERS'=sampled_users, 'WHICH_MOVIES'=sampled_movies )
    if (debug ) { 
        cat(HEADER)
        str(retvals)
        cat(HEADER)
    }
    return ( retvals )
}
# ######################################################################################################


# ###################################################################################################
GET_XY_FOR_GENRE_AS_X_AND_USER_RATING_AS_Y = function( ORIG_RATINGS, NUSERS=100, NMOVIES=200, debug=TRUE ) {
    RATINGS_RETVALS = EXTRACT_RATINGS_SUBMATRIX( ORIG_RATINGS, m=NUSERS, n=NMOVIES)
        M2U = t( RATINGS_RETVALS$RATMAT )
        Y = M2U
        WHICH_USERS  = RATINGS_RETVALS$WHICH_USERS
        WHICH_MOVIES = RATINGS_RETVALS$WHICH_MOVIES

        # ###########################################################################
                               ##### ADDED FEATURES #####
        # ###########################################################################
            X  = MOVIES[WHICH_MOVIES, c(1,6:ncol(M))] 

            MU = matrix(apply(M2U,1,mean,  na.rm=TRUE))

            SD = matrix(apply(M2U,1,sd,    na.rm=TRUE))  
            SD = ifelse( is.na(SD), 0, SD )
    
            MIN= matrix(apply(M2U,1,min,   na.rm=TRUE))

            MAX= matrix(apply(M2U,1,max,   na.rm=TRUE))

            MED= matrix(apply(M2U,1,median,na.rm=TRUE))  * MAX / MIN

            RNG= MAX-MIN

            RSD= MU/(SD+1)

            if ( FALSE ) {
                MU = MU + mean(MU,na.rm=TRUE)
                SD = SD + mean(SD,na.rm=TRUE)
            }
        # ###########################################################################
                               ##### ADDED FEATURES #####
        # ###########################################################################
        NX = data.frame(            "rating_mean"=MU, 'rating_muvar'=RSD )
        SUMMARY_STATS_COLNAMES = c( "rating_mean",    'rating_muvar')


        # ###########################################################################
        # BOOTSTRAP the FIRST generation of the PREDICTIVE FEATURES (i.e., GENREs) as 
        # the AVERAGE Rating BY THOSE WHO RATED THE MOVIE plus STD.DEV on such ratings
        # ###########################################################################
        for( i in 2:ncol(X)) {
            Xi = matrix(X[,i])
            if ( TRUE ) Xi = Xi * MU/(SD+1)
            NX = cbind( NX, Xi )
        }

        colnames(NX) = c( SUMMARY_STATS_COLNAMES, colnames(MOVIES[c(6:ncol(M))]))
        rownames(NX) = rownames(M2U)

    retvals = list ( 'X'=NX, 'Y'=Y, 'WHICH_USERS'=WHICH_USERS, 'WHICH_MOVIES'=WHICH_MOVIES )

    if ( debug ) str(retvals)

    return ( retvals )
}
# ###################################################################################################


# ###################################################################################################
SUBSELECT_THOSE_REVIEWED_BY_USER = function( X, Y, debug=FALSE ) {
    XY = cbind(X,Y)
    idx = which( complete.cases(XY) )
    if ( debug ) {
        print ( idx )
        SUMMARY( XY[idx,] )
    }
    return ( idx )
}
# ###################################################################################################


# ###################################################################################################
GET_XY_FOR_USER_PREFERENCES_FOR_GENRE_MATRIX = function( X_GEN, Y_M2U, regparam=2E-1, w_intercept=FALSE, debug=FALSE ) {
    DROP=c()

    GENRE_COLS = c(1:ncol(X_GEN))
    N_GENRE = length(GENRE_COLS)
    N_USERS = ncol(Y_M2U)
    DEFAULT_THETA = as.matrix(rep(0.0, length(GENRE_COLS)))         # colnames(DEFAULT_THETA)=c("theta")

    if ( w_intercept )
        PREFERENCES = matrix(rep(NA, N_USERS*(N_GENRE+1)), N_USERS)
    else
        PREFERENCES = matrix(rep(NA, N_USERS*(N_GENRE)), N_USERS)

    X_GEN = X_GEN[, GENRE_COLS ]

    if ( debug ) cat(HEADER)
    for ( USER in 1:ncol(Y_M2U)) {
        idx = SUBSELECT_THOSE_REVIEWED_BY_USER( X_GEN, Y_M2U[,USER] )
        if ( length(idx) <= DROP_TRIGGER ) { 
            DROP = append(DROP, USER)
            PREFERENCES[ USER, ] = DEFAULT_THETA
            next 
        }

        XX = X_GEN[idx, ]
        YY = Y_M2U[idx, USER]

        NEQ =  DO_NORMAL_EQUATIONS( XX, YY, regularization_parameter=regparam, do_scale=FALSE, w_intercept=w_intercept, debug=debug )
        THETA_GENRE = GET_NEQ_THETA( NEQ )

        PREFERENCES[ USER, ] = THETA_GENRE
    }

    # if ( length(DROP)!= 0 ) print( DROP )

    PREFERENCES = as.data.frame( PREFERENCES )
    if ( w_intercept )
        colnames( PREFERENCES ) = c( "UINTERCEPT", colnames(X_GEN)[GENRE_COLS] )
    else
        colnames( PREFERENCES ) = colnames(X_GEN)[GENRE_COLS]

    rownames( PREFERENCES ) = rownames(t(Y_M2U))
    PREFERENCES = as.matrix( PREFERENCES )
    return ( PREFERENCES )
}
# ###################################################################################################


# ###################################################################################################
GET_XY_FOR_PREFERENCES_AS_X_AND_USER_RATING_AS_Y = function( X_GEN, Y_U2M, regparam=2E-1, w_intercept=FALSE, debug=FALSE ) {
    DROP=c()

    GENRE_COLS = c(1:ncol(X_GEN))
    N_GENRE = length(GENRE_COLS)

    N_USERS  = nrow(Y_U2M)
    N_MOVIES = ncol(Y_U2M)

    if ( w_intercept )
        MOVIEGENRE_FEATURES = matrix( rep(NA, N_MOVIES*(N_GENRE+1)), N_MOVIES )
    else
        MOVIEGENRE_FEATURES = matrix( rep(NA, N_MOVIES*(N_GENRE)), N_MOVIES )

    X_GEN = X_GEN[, GENRE_COLS ]

    if ( debug ) cat(HEADER)
    for ( MOVIE in 1:ncol(Y_U2M)) {
        idx = SUBSELECT_THOSE_REVIEWED_BY_USER( X_GEN, Y_U2M[,MOVIE] )
        if ( length(idx) <= DROP_TRIGGER ) { 
            DROP = append(DROP, MOVIE)
            next 
        }

        XX = X_GEN[idx, ]
        YY = Y_U2M[idx, MOVIE]

        NEQ =  DO_NORMAL_EQUATIONS( XX, YY, regularization_parameter=regparam, do_scale=FALSE, w_intercept=w_intercept, debug=debug )
        THETA_MOVIE = GET_NEQ_THETA( NEQ )

        MOVIEGENRE_FEATURES[ MOVIE, ] = THETA_MOVIE
    }

    # if ( length(DROP)!= 0 ) print( DROP )

    MOVIEGENRE_FEATURES = as.data.frame( MOVIEGENRE_FEATURES )
    if ( w_intercept )
        colnames( MOVIEGENRE_FEATURES ) = c( "MINTERCEPT", colnames(X_GEN) )
    else
        colnames( MOVIEGENRE_FEATURES ) = colnames(X_GEN)
    rownames( MOVIEGENRE_FEATURES ) = rownames(t(Y_U2M))
    MOVIEGENRE_FEATURES = as.matrix( MOVIEGENRE_FEATURES )
    return ( MOVIEGENRE_FEATURES )
}
# ###################################################################################################


# ###################################################################################################
DO_PLOTTING_ITERATION = function( L_PREFERENCES, L_MOVIEGEN_FEATURES, xval=PCA_PLOT_LIMITS ) {
    XYZ0 = L_PREFERENCES [complete.cases(L_PREFERENCES ),]
        Z = DO_PCA( XYZ0, silent=TRUE, debug=FALSE )$Z
        plot( Z[,1], Z[,2], pch=".", cex=0.4, xlim=c(-(xval+1),xval+1), ylim=c(-(xval+1),xval+1) )
        text( Z[,1], Z[,2], rownames(XYZ0), col='blue', cex=0.4 )

    XYZ1 = L_MOVIEGEN_FEATURES[complete.cases(L_MOVIEGEN_FEATURES),]
        Z = DO_PCA( XYZ1, silent=TRUE, debug=FALSE )$Z
        plot( Z[,1], Z[,2], pch=".", cex=0.4, xlim=c(-xval,xval), ylim=c(-xval,xval) )
        text( Z[,1], Z[,2], rownames(XYZ1), col='brown', cex=0.4 )

    retvals = list ( XYZ0, XYZ1 )

    return ( retvals )
}
# ###################################################################################################


# ###################################################################################################
DO_ANALYSIS_DIFFERENCES_BETWEEN_COEFFICIENTS = function( PREFERENCES_2, PREFERENCES_1, ptitle="PREFERENCES DIFF" )  {
    PREFERENCES_DIFF   = PREFERENCES_2  -  PREFERENCES_1
    PREFERENCES_DIFF   = PREFERENCES_DIFF[complete.cases(PREFERENCES_DIFF),]
    P_TS = apply( PREFERENCES_DIFF, 1, function(x) { t = t(x)%*%x ; ifelse(is.na(t),0,t)} )
    DO_HIST(P_TS, nbins=64, ptitle=ptitle)
    T_MSE = as.numeric( t(P_TS) %*% P_TS ) / length(P_TS) 
    return ( T_MSE )
}
# ###################################################################################################


# ###################################################################################################
# RATINGS     : BOOTSTRAP OF USER TO MOVIE RATINGS
# PREFERENCES : USER DATA INDICATING PREFERENCES TOWARDS GENRES (M users, P genres)
# X_GEN, Y_M2U: GENRES AS PREDICTORS OF MOVIE RATINGS FOR EACH USER (resulting in PREFERENCES) AS THETA VECTORS
# GEN_RATINGS : GIVEN USER PREFERENCES AS PREDICTORS OF X_GEN VALUES AS THETA VECTORS
# ###################################################################################################
DO_ITERATIVE_CONVERGENCE_FOR_RECOMMENDATION_PARAMETERS = function( ORIG_RATINGS, NUSERS=1000, NMOVIES=2000, debug=FALSE ) {

    NEWLINE(3); cat( HEADER )
    GENRE_RETVALS = GET_XY_FOR_GENRE_AS_X_AND_USER_RATING_AS_Y( ORIG_RATINGS, NUSERS=NUSERS, NMOVIES=NMOVIES, debug=debug )
    X_GEN = GENRE_RETVALS$X
    Y_M2U = GENRE_RETVALS$Y
    WHICH_USERS  <<- GENRE_RETVALS$WHICH_USERS
    WHICH_MOVIES <<- GENRE_RETVALS$WHICH_MOVIES

    X_GEN = scale( X_GEN, center=FALSE,  scale=FALSE)
    Y_M2U = scale( Y_M2U, center=TRUE,   scale=FALSE)

    if ( debug ) {
        NEWLINE(3)
        cat( HEADER )
        SUMMARY( Y_M2U, w_str=TRUE )
        cat( HEADER )
        NEWLINE(3)
    }
    # ###################################################################################################

    # ###################################################################################################
    NEWLINE(3); cat( HEADER )
    PREFERENCES2 = GET_XY_FOR_USER_PREFERENCES_FOR_GENRE_MATRIX( X_GEN, Y_M2U, regparam=REGPARAM, debug=debug)
    # ###################################################################################################

    # ###################################################################################################
    NEWLINE(3); cat( HEADER )
    Y_U2M = t(Y_M2U)
    MOVIEGEN_FEATURES2 = GET_XY_FOR_PREFERENCES_AS_X_AND_USER_RATING_AS_Y( PREFERENCES2, Y_U2M )
    # ###################################################################################################

    # ###################################################################################################
    # THIS IMPLEMENTS ITERATIVE CONVERGENCE WHICH IS A LOT SLOWER THAN TO DO IT ALL AT ONCE BUT ALLOWS
    # ACCESS TO EACH ITERATION OF THE CONVERGENCE TO PLOT THINGS SUCH AS COEFFICIENT TRAVERSAL AND STABILITY
    # ###################################################################################################
    if ( DO_PDF ) pdf( 'plot_coefficient_convergence_collabfilt.pdf' )
    print( "PDF FILE contains plots tracing the convergence of coefficients: plot_coefficient_convergence_collabfilt.pdf" )
    op = par( mfrow=c(3,2) )

    ITERATIVE_MSE = data.frame( "GEN"=0, "P_MSE"=NA, "M_MSE"=NA )

    WHY = "MAXIMUM NUMBER OF ITERATIONS EXCEEDED"
    for( iter in 2:N_ITER ) {
        PREFERENCES1       = GET_XY_FOR_USER_PREFERENCES_FOR_GENRE_MATRIX( MOVIEGEN_FEATURES2, Y_M2U, regparam=REGPARAM, debug=FALSE )
        MOVIEGEN_FEATURES1 = GET_XY_FOR_PREFERENCES_AS_X_AND_USER_RATING_AS_Y( PREFERENCES2,   Y_U2M )

        PREFERENCES2       = GET_XY_FOR_USER_PREFERENCES_FOR_GENRE_MATRIX( MOVIEGEN_FEATURES1, Y_M2U, regparam=REGPARAM, debug=FALSE )
        MOVIEGEN_FEATURES2 = GET_XY_FOR_PREFERENCES_AS_X_AND_USER_RATING_AS_Y( PREFERENCES1,   Y_U2M )

        PLOT_RETVALS       = DO_PLOTTING_ITERATION( PREFERENCES1, MOVIEGEN_FEATURES1 )
        PLOT_RETVALS       = DO_PLOTTING_ITERATION( PREFERENCES2, MOVIEGEN_FEATURES2 )

        P_MSE = DO_ANALYSIS_DIFFERENCES_BETWEEN_COEFFICIENTS( PREFERENCES2,       PREFERENCES1,       ptitle="PREFERENCES DIFF" )
        M_MSE = DO_ANALYSIS_DIFFERENCES_BETWEEN_COEFFICIENTS( MOVIEGEN_FEATURES2, MOVIEGEN_FEATURES1, ptitle="MOVIEGEN_DIFF" )

        ITERATIVE_MSE = rbind( ITERATIVE_MSE, data.frame( "GEN"=iter, "P_MSE"=P_MSE, "M_MSE"=M_MSE ))
        print( sprintf( "ITERATION=%4s MSE(COEFF_DIFF BETWEEN GENERATIONS) P=%16.8g   M=%16.8g", iter, P_MSE, M_MSE ) )

        idx = c(1:nrow(ITERATIVE_MSE))
        if ( nrow(ITERATIVE_MSE) > 5 ) 
            idx = c( (nrow(ITERATIVE_MSE)-3) : nrow(ITERATIVE_MSE))

        print ( ITERATIVE_MSE )

        MEAN_P_MSE = mean(ITERATIVE_MSE$P_MSE[idx], na.rm=TRUE)
        MEAN_M_MSE = mean(ITERATIVE_MSE$M_MSE[idx], na.rm=TRUE)

        METRICS <<- TIMESTAMP( paste( "CONVERGENCE_ITERATION", iter ) )

        if ( P_MSE < EPSILON  & 
             M_MSE < EPSILON ) {                 
            cat(HEADER)
            WHY = "STABILITY TERMINATION CONDITION REACHED"
            if ( MEAN_P_MSE<EPSILON  & MEAN_M_MSE<EPSILON ) WHY = "STRONG STABILITY TERMINATION CONDITION REACHED"
            print( WHY )
            print( paste( P_MSE,      M_MSE ) )
            print( paste( MEAN_P_MSE, MEAN_M_MSE ) )
            cat(HEADER)
            break 
        }
    }
    if ( DO_PDF ) { dev.off() ; graphics.off() }

    ITER_RETVALS = list( 'PREFERENCES'=PREFERENCES2, 
                         'MOVIEGEN_FEATURES'=MOVIEGEN_FEATURES2,
                         'MSE_STATS'=ITERATIVE_MSE,
                         'Y_M2U'=Y_M2U,
                         'WHY'=WHY )

    return( ITER_RETVALS )
}
# ###################################################################################################


# ###################################################################################################
GENERATE_LOW_RANK = function( USER_PREFERENCES, MOVIE_FEATURES, NA_RM=TRUE ) {
    if ( NA_RM ) {
        NA_MOVIES = c()
        for ( MOVIE in rownames(MOVIE_FEATURES) ) {
            mfv = MOVIE_FEATURES[MOVIE,]
            if ( all(is.na(mfv)) ) NA_MOVIES = append( NA_MOVIES, MOVIE ) 
        }
        MOVIE_FEATURES[NA_MOVIES,] = 0
        SUMMARY( NA_MOVIES )
    
        NA_MOVIES = c()
        for ( MOVIE in rownames(MOVIE_FEATURES) ) {
            mfv = MOVIE_FEATURES[MOVIE,]
            if ( any(is.na(mfv)) ) NA_MOVIES = append( NA_MOVIES, MOVIE ) 
        }
        # SUMMARY( NA_MOVIES )
    }
    NORMALIZED_PREDICTIONS = USER_PREFERENCES %*% t(MOVIE_FEATURES)
}
# ###################################################################################################


# ###################################################################################################
# COLLABORATIVELY-FILTERED RATING GENERATION FOR USERS RATINGS AND MOVIE RATING-PREDICTIVE FEATURES
# ###################################################################################################
GENERATE_COLLABORATIVE_FILTERED_RATINGS = function( WHICH_RATINGS, 
                                                    USER_PREFERENCES, MOVIE_FEATURES, USER_MEAN_NORMALIZATIONS, 
                                                    sample_n=200, nl=100, debug=TRUE ) {

    NORMALIZED_PREDICTIONS = GENERATE_LOW_RANK( USER_PREFERENCES, MOVIE_FEATURES )

    RECOMMENDATIONS_FOR_U2M_RATINGS  = apply( NORMALIZED_PREDICTIONS, 2, DENORMALIZE_MEAN_NORMALIZED_PREDICTIONS, 
                                                                         USER_MEANS=USER_MEAN_NORMALIZATIONS, 
                                                                         PRODUCTION_SYSTEM_CORRECTION=TRUE )

    colnames(RECOMMENDATIONS_FOR_U2M_RATINGS) = colnames(WHICH_RATINGS)
    rownames(RECOMMENDATIONS_FOR_U2M_RATINGS) = rownames(WHICH_RATINGS)

    if ( sample_n == 0 ) sample_n = nrow(WHICH_RATINGS)

    r_idxs = sample( rownames(WHICH_RATINGS), min(sample_n, nrow(WHICH_RATINGS)) )

    MSE = c()
    for ( MOVIE in colnames(USER_TO_MOVIE_RATINGS)) {
        u_idxs = names(which( !is.na(WHICH_RATINGS[,MOVIE]) ))

        print( sprintf( "%s : [%4d w/ selected ratings] : %s ", MOVIE, length(u_idxs), as.character(M[M2ID(MOVIE),"movie_title"]) )) 

        SAMPLED_RECM = sprintf("%s[%.0f:%s]", r_idxs, 
                                     RECOMMENDATIONS_FOR_U2M_RATINGS[r_idxs, MOVIE], WHICH_RATINGS[r_idxs, MOVIE])

        SAMPLED_FITS = sprintf("%s[%.1f:%s]", u_idxs, 
                                     RECOMMENDATIONS_FOR_U2M_RATINGS[u_idxs, MOVIE], WHICH_RATINGS[u_idxs, MOVIE])

        FIT_MSE = NA
        if ( length(u_idxs) ) 
            FIT_MSE = sum(RECOMMENDATIONS_FOR_U2M_RATINGS[u_idxs,MOVIE] - WHICH_RATINGS[u_idxs,MOVIE])^2 / length(u_idxs)
        MSE = append( MSE, FIT_MSE )

        if ( !is.na( FIT_MSE ) )
            if ( FIT_MSE > 1 ) {
                SUMMARY(RECOMMENDATIONS_FOR_U2M_RATINGS[,MOVIE])
                SUMMARY(WHICH_RATINGS[,MOVIE])
            }

        cat(SUBHEADER)
        print( paste( "REGRESSION FITS WRT USER: ", substr(CONCAT(SAMPLED_FITS),1,nl) ))
        print( paste( "SAMPLE RECOMMENDATIONS  : ", substr(CONCAT(SAMPLED_RECM),1,nl) ))
        print( paste( "MSE (WRT GIVEN MOVIE)   :  ", round(FIT_MSE,6) ))
        cat(HEADER)

        NEWLINE(1)
    }
    names(MSE) = colnames(USER_TO_MOVIE_RATINGS)
    CUMSUM_MSE = cumsum(ifelse(is.na(MSE),0,MSE))
    ITERATION_BASED_BUILDUP_OF_MEAN_MSE =  CUMSUM_MSE/1:length(MSE)

    cat(HEADER)
    try( print(sprintf(" AVG(PER-MOVIE MSE) = %.6f", mean(MSE, na.rm=TRUE))))
    try( print(sprintf(" SD (PER-MOVIE MSE) = %.6f", sd(MSE,   na.rm=TRUE))))
    cat(HEADER)

    pdf('plot_accumulative_mse_progression_collabfilt.pdf', 12, 8)
    print( "PDF FILE contains plots tracing accumulation of MSE error for known labels wrt recommendations generated: plot_accumulative_mse_progression_collabfilt.pdf" )
    par(mfrow=c(2,2))
        barplot( ifelse(is.na(MSE),0,MSE), cex=0.4, cex.axis=0.3, main="MSE INCREASE PER MOVIE",  las=2 )
        hist( MSE, breaks=32, main="FREQ. OF MAGNITUDE OF MSE INCREASE" )
        plot( CUMSUM_MSE, cex=0.6, cex.axis=0.4, pch="+", main="ACCUM. MSE PROGRESSION",  las=2 )
        plot( ITERATION_BASED_BUILDUP_OF_MEAN_MSE, pch="+", cex=0.6, cex.axis=0.4, main="AVG MSE BUILDUP AT K-TH MOVIE",  las=2 )
    dev.off()

    return (RECOMMENDATIONS_FOR_U2M_RATINGS)
}
# ###################################################################################################


# ###################################################################################################
# NUMBER OF STARS MAY ACTUALLY BE SLIGHTLY LARGER THAN FIVE
# ###################################################################################################
DENORMALIZE_MEAN_NORMALIZED_PREDICTIONS = function( USERS_WRT_MOVIE_PRED, USER_MEANS=c(), 
                                                   PRODUCTION_SYSTEM_CORRECTION=TRUE, TOP_RATING=5, LOW_RATING=0, debug=FALSE ) {

    NUMBER_STARS_PREDICTED_RATING = USERS_WRT_MOVIE_PRED + USER_MEANS

    # a 0:5 correction may be used here as values will be in range -0:5+some-extra
    if ( PRODUCTION_SYSTEM_CORRECTION ) {
        NUMBER_STARS_PREDICTED_RATING = apply( NUMBER_STARS_PREDICTED_RATING, 2, function(x) x = ifelse(x>TOP_RATING, TOP_RATING, x))
        NUMBER_STARS_PREDICTED_RATING = apply( NUMBER_STARS_PREDICTED_RATING, 2, function(x) x = ifelse(x<LOW_RATING, LOW_RATING, x))
        NUMBER_STARS_PREDICTED_RATING = round( NUMBER_STARS_PREDICTED_RATING )
    }

    if ( debug )
        SUMMARY( NUMBER_STARS_PREDICTED_RATING ) #, w_str=TRUE ) 

    return( NUMBER_STARS_PREDICTED_RATING ) 
}
# ###################################################################################################


# ###################################################################################################
EXTRACT_DISTANCE_MATRIX_AND_PCA_FROM = function( DR2, REF=matrix(), debug=FALSE ) {
    Z2   = as.matrix(DR2$pca_space)
    D2   = as.matrix(DR2$distances)

    if ( debug ) {
        cat(HEADER)
        str(DR2)
        str(D2)
        str(Z2)
        str(REF)
        cat(HEADER)
    }

    # D is square matrix wrt ref
    rownames(D2) = rownames(REF)
    colnames(D2) = rownames(REF)

    # PCA has as many rows as the ref
    rownames(Z2) = rownames(REF)

    retvals = list( 'Z'=Z2, 'D'=D2 )

    return ( retvals )
}    
# ###################################################################################################


# ###################################################################################################
IDENTIFY_SIMILAR_USERS = function( USER, U2M_RECMAT, D=matrix(), Z=matrix(), debug=FALSE ) {
    M1 = MIN_DISTANCE_FROM( U2M_RECMAT, USER,  D=D, PCA=Z, do_plot=TRUE, new_plot=TRUE, debug_=FALSE )

    FDS = FIND_NEIGHBORING_POINTS( U2M_RECMAT, USER, D=D, PCA=Z, color="brown", new_plot=FALSE)

    print( sprintf( "BASED ON FIT/PREDICTED MOVIE RATING SIMILARITIES, SIMILAR USERS ARE: %s", FDS$closest ))

    NEARBY = FDS$nearby   

    if ( debug  ) {
        u1_idx = U2ID(USER)
        for ( NUSER in NEARBY ) {
            u2_idx = U2ID(NUSER)
            print( sprintf( "%30s ---> %30s", CONCAT(U[u1_idx,]), CONCAT(U[u2_idx,]) ) )
        }
    }

    retvals = list( 'NEIGHBORS'=FDS, 'MIN_DETAILS'=M1 )

    return ( retvals )
}
# ###################################################################################################


# ###################################################################################################
ANALYZE_SIMILAR_USER_FOR_MOVIE_RECOMMENDATIONS = function( USER1, NEARBY, nl=120 ) {
    u1_idx = U2ID(USER1)
    U1_MOVIES = which( !is.na( ORIG_RATINGS[USER1,])  )

    mtxt = "NONE"
    stxt = "NONE"

    for ( USER2 in NEARBY ) {
        u2_idx = U2ID(USER2)
        U2_MOVIES = which( !is.na( ORIG_RATINGS[USER2,])  )

        SAME_MOVIES = intersect( U2_MOVIES, U1_MOVIES )
        MORE_MOVIES = setdiff(   U2_MOVIES, U1_MOVIES )

        if ( length(MORE_MOVIES) == 0 ) next

        if ( length(MORE_MOVIES!= 0 ))
        mtxt = CONCAT(sprintf( "[%s stars]: %s,  ", as.numeric(ORIG_RATINGS[USER2,ID2M(MORE_MOVIES)]), as.character(M[MORE_MOVIES,"movie_title"])))

        if ( length(SAME_MOVIES!= 0 ))
        stxt = CONCAT(sprintf( "[%s:%s stars]: %s,  ", as.numeric(ORIG_RATINGS[USER1,ID2M(SAME_MOVIES)]),
                                                       as.numeric(ORIG_RATINGS[USER2,ID2M(SAME_MOVIES)]), as.character(M[SAME_MOVIES,"movie_title"])))

        print( sprintf( "%30s ---> %30s", CONCAT(U[u1_idx,]), CONCAT(U[u2_idx,]) ) )
        print( paste( sprintf("[SAME(%4s:%4s)] -> ", USER1, USER2), substr( stxt, 1, nl ), "..." ) )
        print( paste( sprintf("[MORE(%4s:%4s)] -> ", USER1, USER2), substr( mtxt, 1, nl ), "..." ) )
        cat(HEADER)
    }

    # for some movie wrt to user1 find movies matching distance on user2
    # dist( ORIG_RATINGS[USER1,U1_MOVIES], ORIG_RATINGS[USER2,U2_MOVIES], method=p
    TOPMOST = c()

    retvals = list( 'SAME'=SAME_MOVIES, 'MORE'=MORE_MOVIES, 'TOP_RANK'=TOPMOST, 'SUMMARY_S'=stxt, 'SUMMARY_M'=mtxt ) 

    return ( retvals ) 
}
# ###################################################################################################


# ###################################################################################################
# TODO: add weighted distance too, wrt ratings * distance, plot all points
# ###################################################################################################
WHICH_ONES_TO_RECOMMEND = function( USER1, USER2, SAME, MORE, D=matrix(), Z=matrix(), debug=FALSE ) {

    D_RETVALS = data.frame( 'BECAUSE_YOU_WATCHED'=NA, 'RECOMMENDATION'=NA, 'RATINGS'=NA, 'IDX'=matrix() )

    D3i = intersect(colnames(D),ID2M(SAME))
    D3j = intersect(colnames(D),ID2M(MORE))
    D3j = setdiff( D3j, D3i )

    if ( length(D3i) == 0 ) return ( D_RETVALS )
    if ( length(D3j) == 0 ) return ( D_RETVALS )

    D3 = D[D3i,D3j]
    if (identical(D3, matrix())) return ( D_RETVALS ) 

    MOVIE_DISTANCE_GOAL = 1.01 * min(D3) 

    D_RETVALS = WHICH_MOVIES_FROM_THIS_USER_ARE_CLOSE( USER1, USER2, D3, Z, MOVIE_DISTANCE_GOAL )

    if ( debug ) print ( D_RETVALS )

    return ( D_RETVALS )

}
# ###################################################################################################


# ###################################################################################################
WHICH_MOVIES_FROM_THIS_USER_ARE_CLOSE = function( USER1, USER2, D3, Z2, distance_goal, nmax=3, PLOT_EXTRA_REC=TRUE, debug=FALSE ) { 

    RECOMMENDATIONS = data.frame( 'BECAUSE_YOU_WATCHED'=NA, 'RECOMMENDATION'=NA, 'RATINGS'=NA, 'IDX'=matrix() )

    min_inds = which(D3 <= distance_goal, arr.ind=TRUE)
    if ( class(min_inds)  != "matrix" ) 
        return ( RECOMMENDATIONS )

    START = TRUE
    for ( ix in 1:min(nrow( min_inds ),nmax) ) {
        U1_MOVIE = min_inds[ix,1]
        U2_MOVIE = min_inds[ix,2]
        U1_MOVIE = rownames(D3)[U1_MOVIE]
        U2_MOVIE = colnames(D3)[U2_MOVIE]
        RATING = ORIG_RATINGS[USER2, U2_MOVIE]

        if ( START ) {
            START = FALSE
            RECOMMENDATIONS = data.frame( 'BECAUSE_YOU_WATCHED'=U1_MOVIE, 'RECOMMENDATION'=U2_MOVIE, 'RATINGS'=RATING, 'IDX'=min_inds[ix,] )
        } else
            RECOMMENDATIONS = rbind( RECOMMENDATIONS, 
                              data.frame( 'BECAUSE_YOU_WATCHED'=U1_MOVIE, 'RECOMMENDATION'=U2_MOVIE, 'RATINGS'=RATING, 'IDX'=min_inds[ix,] ) )

        UPDATE_MAPPING( mtype="UU1", USER1,    USER2 )        
        UPDATE_MAPPING( mtype="MM1", U1_MOVIE, U2_MOVIE )     
        UPDATE_MAPPING( mtype="UM1", USER1,    U2_MOVIE )      

        if ( ix == 2 ) {
            cat(HEADER)
            print( sprintf( "IN ADDITION, THE FOLLOWING SECONDARY RECOMMENDATIONS WRT TO OTHER SIMILAR YET MORE DISTANT USERS ARE SHOWN IN GRAY" ) )
        }

        print( sprintf( "BECAUSE YOU LIKED %s: [%s]", U1_MOVIE, M[M2ID(U1_MOVIE),"movie_title"] ))
        print( sprintf( "   A MYSTEROUS CLOUD NOW RECOMMENDS YOU %s [%s stars]: [%s]", U2_MOVIE, RATING, M[M2ID(U2_MOVIE),"movie_title"] ))

        if ( PLOT_EXTRA_REC )
            DO_PCA_NEIGHBORING_PLOT( U1_MOVIE, U2_MOVIE, Z2, new_plot=FALSE, color="gray" )
    }

    return( RECOMMENDATIONS )
}
# ###################################################################################################


# ###################################################################################################
HOW_MANY_RECOMMENDATION_WERE_FOUND = function( RECOMMENDATIONS ) {
    HOW_MANY = 0
        if ( nrow(RECOMMENDATIONS) > 0 )
            if ( !is.na(RECOMMENDATIONS[1,'RECOMMENDATION'] ))
                HOW_MANY = nrow(RECOMMENDATIONS)
    return( HOW_MANY )
}
# ###################################################################################################


# ###################################################################################################
# We could also try other neighbors in NEARBY to find a recommendation
# ###################################################################################################
FIND_ALTERNATIVE_RECOMMENDATION_WRT = function( USER1, CLOSEST ) { # , D1, Z1, D2, Z2, RU2M ) {

    SDS = FIND_NEIGHBORING_POINTS( RU2M, CLOSEST, D=D1, PCA=Z1, color="blue", new_plot=FALSE)

    SDS_CLOSEST = SDS$closest
    # if (SDS_CLOSEST == USER1 ) SDS_CLOSEST = SDS$nearby

    print( paste( "FOUND NO IMMEDIATE RECOMMENDATION, REACHING OUT TO CLOSEST 2ND DEGREE", SDS_CLOSEST ) )

    SDS_RETVALS = ANALYZE_SIMILAR_USER_FOR_MOVIE_RECOMMENDATIONS( USER1, SDS_CLOSEST )
        SDS_SAME = SDS_RETVALS$SAME
        SDS_MORE = SDS_RETVALS$MORE

    SDS_RECOMMENDED_FOR_USER = WHICH_ONES_TO_RECOMMEND( USER1, SDS_CLOSEST, SDS_SAME, SDS_MORE, D=D2, Z=Z2 )

    if ( HOW_MANY_RECOMMENDATION_WERE_FOUND(SDS_RECOMMENDED_FOR_USER) == 0 )
        print( paste( "FOUND NO IMMEDIATE RECOMMENDATION EVEN FOR CLOSEST 2ND DEGREE", SDS_CLOSEST ) )

    return ( SDS_RECOMMENDED_FOR_USER  )
}
# ###################################################################################################


# ###################################################################################################
FIND_VALID_ORIG_RATED_MOVIES_FOR_THIS_USER = function( USERX, PIVOT=5 ) {
    MOVIES_IDX = NA
    HIGHLY_RATED = intersect(MOVIE_LABELS, names(which( ORIG_RATINGS[ USERX, ] >= PIVOT )))
    if ( length(HIGHLY_RATED)==0 ) HIGHLY_RATED = intersect(MOVIE_LABELS, names(which( ORIG_RATINGS[ USERX, ] >= (PIVOT-1))))
    if ( length(HIGHLY_RATED)!= 0 ) MOVIES_IDX = sample( HIGHLY_RATED, 1 )
    return ( MOVIES_IDX )
}
# ###################################################################################################


# ###################################################################################################
DO_USER_RECOMMENDATION = function( COMPARISON_USER ) { # RU2M, D1, D2, Z1, Z2 )  {
    SIMILAR_USERS_RETVALS = IDENTIFY_SIMILAR_USERS( COMPARISON_USER, RECOMMENDATIONS_FOR_U2M_RATINGS, D=D1, Z=Z1 )
        NEARBY  = SIMILAR_USERS_RETVALS$NEIGHBORS$nearby   
        CLOSEST = SIMILAR_USERS_RETVALS$NEIGHBORS$closest

    ANALYSIS_CLOSEST_RETVALS = ANALYZE_SIMILAR_USER_FOR_MOVIE_RECOMMENDATIONS( COMPARISON_USER, CLOSEST )
        SAME = ANALYSIS_CLOSEST_RETVALS$SAME
        MORE = ANALYSIS_CLOSEST_RETVALS$MORE

    RECOMMENDED_FOR_USER = WHICH_ONES_TO_RECOMMEND( COMPARISON_USER, CLOSEST, SAME, MORE, D=D2, Z=Z2 )

    if ( HOW_MANY_RECOMMENDATION_WERE_FOUND(RECOMMENDED_FOR_USER) == 0 )
        RECOMMENDED_FOR_USER = FIND_ALTERNATIVE_RECOMMENDATION_WRT( COMPARISON_USER, CLOSEST ) # , D1, Z1, D2, Z2, RECOMMENDATIONS_FOR_U2M_RATING)
    else {
        REC_TOPMOST = RECOMMENDED_FOR_USER[1,'RECOMMENDATION'] 
        REC_RATING  = RECOMMENDED_FOR_USER[1,'RATING']
    }

    METRICS = TIMESTAMP( paste('COLLAB.FILTERED RECOMMENDATIONS FOR', COMPARISON_USER) )

    UREC_RETVALS = list( 'USER'=COMPARISON_USER,
                         'CLOSEST'=CLOSEST,
                         'SIMILAR_USERS'=SIMILAR_USERS_RETVALS, 
                         'ANALYSIS_OF_CLOSEST'=ANALYSIS_CLOSEST_RETVALS,
                         'USER_RECOMMENDATIONS'=RECOMMENDED_FOR_USER )

    return ( UREC_RETVALS )
}
# ###################################################################################################


# ###################################################################################################
DO_BASKET_ANALYSYS_BASED_RECOMMENDATION_FOR = function( MOVIE ) {  
    BASKET_ANALYSIS_FINDINGS = APPLY_MARKET_BASKET_ASSOCIATON_ANALYSIS_WRT( RECOMMENDATIONS_FOR_U2M_RATINGS,
                                                MOVIE,
                                                SUPPORT=0.50,
                                                CONF=0.65, 
                                                STEP=0.20,
                                                PIVOT=5,
                                                MIN_SUPPORT=5/nrow(RECOMMENDATIONS_FOR_U2M_RATINGS))
    return ( BASKET_ANALYSIS_FINDINGS ) 
}
# ###################################################################################################


# ###################################################################################################
# TODO, DO NEIGHBOR ANALYSIS FOR CLOSEST MOVIE TO AN SPECIFIED MOVIE
# ###################################################################################################
DO_MOVIE_RECOMMENDATION = function( COMPARISON_USER, ATTEMPT_BASKET_ANALYSIS_STYLE_RECOMMENDATION=FALSE ) {
    BASKET_ANALYSIS_FINDINGS = matrix() 

    if ( ATTEMPT_BASKET_ANALYSIS_STYLE_RECOMMENDATION== FALSE ) 
        return ( BASKET_ANALYSIS_FINDINGS )
 
    COMPARISON_USER_MOVIES = FIND_VALID_ORIG_RATED_MOVIES_FOR_THIS_USER( COMPARISON_USER )

    if (!is.na(COMPARISON_USER_MOVIES)) {
        MOVIE = COMPARISON_USER_MOVIES[1]
        BASKET_ANALYSIS_FINDINGS = DO_BASKET_ANALYSYS_BASED_RECOMMENDATION_FOR( MOVIE )
        METRICS = TIMESTAMP( paste('MARKET BASKET RECOMMENDATIONS FOR', COMPARISON_USER, MOVIE ) )
    }

    return (BASKET_ANALYSIS_FINDINGS ) 
}
# ###################################################################################################


# ###################################################################################################
WHICH_GENRES_FOR_MOVIE = function( mids, concat=TRUE, n=2 ) {
    fm = c()
    for ( mid in mids ) {
        m = substr(colnames(M)[5+as.numeric(which(M[mid,6:24]==1,arr.ind=TRUE)[,2])],1,n)
        if ( concat ) m = substr( CONCAT( m ), 1, 12 )
        fm = append( fm, m ) 
    }
    return ( fm )
}
# ###################################################################################################


# ###################################################################################################
HEADING = function( heading="", nl=1 ) {
    NEWLINE(nl)
    cat( HEADER )
    if ( !heading=="" ) {
        print ( heading )
        cat( HEADER )
    }
}
# ###################################################################################################


# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################






























# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################






# ###################################################################################################
# ###################################################################################################
BUILD_RECOMMENDATIONS = TRUE
CHECK_RECOMMENDATIONS = TRUE
# ###################################################################################################
# ###################################################################################################






# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
if ( BUILD_RECOMMENDATIONS ) {
    METRICS <<- INIT_METRICS()
    METRICS <<- TIMESTAMP( "START" )

    # ###################################################################################################
    # ###################################################################################################
    # GLOBALS PARAMETERS
    # ###################################################################################################
    N_USERS_TO_USE  = as.integer(1000 * 1.0)
    N_MOVIES_TO_USE = as.integer(3000 * 1.0)
        EPSILON      = 5E-4
        REGPARAM     = 1E-1
        N_ITER       = 50
        DO_PDF       = TRUE
        DROP_TRIGGER = 1
        PCA_PLOT_LIMITS = 5
    WHICH_USERS  <<- c()
    WHICH_MOVIES <<- c()
    # ###################################################################################################
    # ###################################################################################################
    # ###################################################################################################

    # ###################################################################################################
    NEWLINE(3); cat( HEADER )
    ML = BUILD_MOVIELENS_DATASET()
    # ###################################################################################################
        ORIG_RATINGS = ML$user_movies
        USERS    = U = ML$users
        MOVIES   = M = ML$movies
        GENRES   = G = ML$genre
        OCCUP    = O = ML$occupation
        COUNTS   = C = ML$counts
        TRIPLETS = R = ML$ratings
    # ###################################################################################################
    
    # ###################################################################################################
    NEWLINE(3); cat( HEADER )
    i = 0
    for ( db in ML ) {
        i = i + 1
        cat ( HEADER )
        print( names(ML)[i] )
        cat ( HEADER )
        str(db)
        cat ( HEADER )
        NEWLINE(1)
    }
    # ###################################################################################################

    # ###################################################################################################
    ITER_RETVALS = DO_ITERATIVE_CONVERGENCE_FOR_RECOMMENDATION_PARAMETERS( ORIG_RATINGS, NUSERS=N_USERS_TO_USE, NMOVIES=N_MOVIES_TO_USE, debug=FALSE )
    METRICS <<- TIMESTAMP( paste( "ITERATIVE CONVERGENCE COMPLETED", i ) )
    # ###################################################################################################
        USER_PREFERENCES       =   ITER_RETVALS$'PREFERENCES'
        MOVIE_FEATURES         =   ITER_RETVALS$'MOVIEGEN_FEATURES'
        MSE_ON_COEFF_CHANGES   =   ITER_RETVALS$'MSE_STATS'
        MOVIE_TO_USER_RATINGS  =   ITER_RETVALS$'Y_M2U'
        USER_TO_MOVIE_RATINGS  = t(ITER_RETVALS$'Y_M2U')
        CONVERGENCE            =   ITER_RETVALS$'WHY'
        if (!is.null(attr(USER_TO_MOVIE_RATINGS,"scaled:center"))) USER_MEAN_NORMALIZATIONS  =   as.matrix(attr(USER_TO_MOVIE_RATINGS, "scaled:center"))
        if  (is.null(attr(USER_TO_MOVIE_RATINGS,"scaled:center"))) USER_MEAN_NORMALIZATIONS  =      rep(0, nrow(USER_TO_MOVIE_RATINGS))

        if (!is.null(attr(MOVIE_TO_USER_RATINGS,"scaled:center"))) MOVIE_MEAN_NORMALIZATIONS =   as.matrix(attr(MOVIE_TO_USER_RATINGS, "scaled:center"))
        if  (is.null(attr(MOVIE_TO_USER_RATINGS,"scaled:center"))) MOVIE_MEAN_NORMALIZATIONS =      rep(0, nrow(MOVIE_TO_USER_RATINGS))
    # ###################################################################################################

    # ###################################################################################################
    NEWLINE(10)
    WHICH_RATINGS = ORIG_RATINGS[WHICH_USERS,WHICH_MOVIES]
    RECOMMENDATIONS_FOR_U2M_RATINGS = GENERATE_COLLABORATIVE_FILTERED_RATINGS( WHICH_RATINGS, 
                                                                     USER_PREFERENCES, 
                                                                     MOVIE_FEATURES, 
                                                                     USER_MEAN_NORMALIZATIONS, 
                                                                     sample_n=7, debug=TRUE )
    # ###################################################################################################
    
    # ###################################################################################################
    NEWLINE(10)
    DO_BASIC_PLOTTING()
    # ###################################################################################################

    # ###################################################################################################
    PREVIOUS_OPTIONS = options( digits=4 )
                       options( width=120 )
        NEWLINE(3); cat( HEADER )
            print( "USER PREFERENCES" )
            MDF_STATS(USER_PREFERENCES)
            SUMMARY( USER_PREFERENCES )
            cat( HEADER ); cat( HEADER )
        NEWLINE(3); cat( HEADER )
            print( "MOVIE FEATURES" )
            MDF_STATS(MOVIE_FEATURES)
            SUMMARY( MOVIE_FEATURES )
            cat( HEADER ); cat( HEADER )
        NEWLINE(3); cat( HEADER )
            print( "MSE WRT TO COEFFICIENT CHANGE ACROSS ITERATIONS" )
            print( MSE_ON_COEFF_CHANGES )
            cat( HEADER ); cat( HEADER )
        NEWLINE(3); cat( HEADER )
            print( "TIMING STATS" )
            print( METRICS )
            cat( HEADER ); cat( HEADER )
    options( PREVIOUS_OPTIONS )
    # ###################################################################################################
}
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################











# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
if ( CHECK_RECOMMENDATIONS ) {
    # ###################################################################################################
    NEWLINE(10)
    # ###################################################################################################
    USER_LABELS  = rownames(WHICH_RATINGS)
    MOVIE_LABELS = colnames(WHICH_RATINGS)
    # ###################################################################################################
    RU2M = RECOMMENDATIONS_FOR_U2M_RATINGS 
    RM2U = t(RECOMMENDATIONS_FOR_U2M_RATINGS)
    # ###################################################################################################

    # ###################################################################################################
    SAMPLED_MOVIES = sample( colnames(RECOMMENDATIONS_FOR_U2M_RATINGS), 
                                 nrow(RECOMMENDATIONS_FOR_U2M_RATINGS))
    MOST_POPULAR   = names(sort( c(colSums(ORIG_RATINGS,na.rm=TRUE)), decreasing=TRUE))[1:nrow(RECOMMENDATIONS_FOR_U2M_RATINGS)]
    SAMPLED_MOVIES = MOST_POPULAR
    # ###################################################################################################

    # ###################################################################################################
    RET1 = GET_DISTANCE_MATRIX(RECOMMENDATIONS_FOR_U2M_RATINGS[,SAMPLED_MOVIES],    do_scaling=TRUE, do_plot=TRUE, transform="") 
    DR1  = EXTRACT_DISTANCE_MATRIX_AND_PCA_FROM( RET1, REF=RECOMMENDATIONS_FOR_U2M_RATINGS, debug=TRUE )
    Z1   = DR1$Z
    D1   = DR1$D
    # ###################################################################################################

    # ###################################################################################################
    RET2 = GET_DISTANCE_MATRIX(RM2U, do_scaling=TRUE, do_plot=TRUE, transform="in_pca_domain", vargoal=0.975) 
    DR2  = EXTRACT_DISTANCE_MATRIX_AND_PCA_FROM( RET2, REF=RM2U, debug=TRUE )
    Z2   = DR2$Z
    D2   = DR2$D
    # ###################################################################################################

    # ###################################################################################################
    # USING THE ABOVE PRECOMPUTED COMMON MATRICES FOR ALL USERS: RU2M, D1, D2, Z1, Z2 )
    # ###################################################################################################
    UUMAP1 <<- ifelse(D1,0,0)
    MMMAP1 <<- ifelse(D2,0,0)
    UMMAP1 <<- ifelse(RU2M,0,0)
    MUMAP1 <<- ifelse(RM2U,0,0)

    DO_PCA_FULL_PLOT( Z2, nmax=2000, cex=0.4, pch="o", xlim=c(-PCA_PLOT_LIMITS,PCA_PLOT_LIMITS), 
                                                       ylim=c(-PCA_PLOT_LIMITS,PCA_PLOT_LIMITS) )

    NEWLINE(10)
    for( COMPARISON_USER in USER_LABELS ) {

        USER_RECOMMENDATIONS = DO_USER_RECOMMENDATION( COMPARISON_USER ) 

        cat(HEADER)

        OTHERS_ALSO_WATCHED =  DO_MOVIE_RECOMMENDATION( COMPARISON_USER, 
                                                        ATTEMPT_BASKET_ANALYSIS_STYLE_RECOMMENDATION=FALSE )

        cat(HEADER)
        cat(HEADER)
        cat(HEADER)
        NEWLINE(1)
    }
    # ###################################################################################################

    # ###################################################################################################
    if ( DO_PDF ) { 
        cat( HEADER )
        # dev.copy(pdf, 'plot_recommendation_neighborhood_collabfilt.pdf' )
        # dev.off()
        # print( "PDF FILE contains plot of the resulting recommendation neighborhood: plot_recommendation_neighborhood_collabfilt.pdf" )
    }
    # ###################################################################################################

    # ###################################################################################################
    UU1 = BUILD_EDGELIST(UUMAP1,    PIVOT=1)
    MM1 = BUILD_EDGELIST(MMMAP1,    PIVOT=1)
    UM1 = BUILD_EDGELIST(UMMAP1,    PIVOT=1)
    # ###################################################################################################
    
    # ###################################################################################################
    sink( 'output_mappings_collabfilt.out', split=TRUE )
    HEADING( "TIER 1" )
    EXTRACT_MAPPINGS( MM1, TITLE="TIER_1 MOVIE RECOMMENDATIONS",         MAPPER=MOVIE_COMPARATOR,  TIER=1 )
    EXTRACT_MAPPINGS( UU1, TITLE="TIER_1 USER RECOMMENDATIONS",          MAPPER=USER_COMPARATOR,   TIER=1 )
    EXTRACT_MAPPINGS( UM1, TITLE="TIER_1 USER TO MOVIE RECOMMENDATIONS", MAPPER=USER2MOVIE_PRINTER,TIER=1 )
    sink( )
    # ###################################################################################################

    # ###################################################################################################
    if ( DO_PDF ) { 
        graphics.off()

        pdf( 'plot_recommendation_network_collabfilt.pdf', 11, 11 )

            g1 = PLOT_MATRIX( UUMAP1, VNAMES=rownames(UUMAP1), layout=layout.fruchterman.reingold, WHO="USERS" )
            summary(g1)

            g2 = PLOT_MATRIX( MMMAP1, VNAMES=rownames(MMMAP1), layout=layout.fruchterman.reingold, WHO="MOVIES" )
            summary(g2)

            XYZ = rbind( cbind( ZERO_CLONE( UUMAP1 ),              UMMAP1 ),        
                         cbind(           t(UMMAP1),   ZERO_CLONE( MMMAP1 )) )

            g3 = PLOT_MATRIX( XYZ,    VNAMES=c(rownames(UMMAP1),colnames(UMMAP1)), 
                                                          layout=layout.fruchterman.reingold, WHO="USER-to-MOVIES" )
            summary(g3)

        dev.off()
    }

    # ###################################################################################################

sink()

}
# ###################################################################################################


 







