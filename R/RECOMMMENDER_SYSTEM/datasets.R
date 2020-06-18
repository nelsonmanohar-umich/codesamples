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
LICENSETEXT = "Copyright (C) Nelson R. Manohar, comes with ABSOLUTELY NO WARRANTY."  
message( LICENSETEXT )
message("")
# ######################################################################################################


# ######################################################################################################
source( 'utilities.R' )
# ######################################################################################################


# ######################################################################################################
DO_FEATURE_SET_SUBSELECTION = function( XY, dfname="R:N", cmax=0.9, topn=10, nmax=nrow(X), from_quantile=0.5, refine=TRUE, debug=FALSE, key="T" ) {
    new_colnames = c(1:(ncol(XY)-1)) + 1000
    old_colnames = paste( key, new_colnames, sep="" )
    old_colnames = append( old_colnames, "Y" )
    new_colnames = append( new_colnames, "Y" )
    if ( debug ) { cat( HEADER ) ; print( paste( "MAPS", old_colnames, new_colnames ) ) ; cat( HEADER ) }
    colnames(XY) = old_colnames
	F10 = DO_GENERAL_SUBSET_SELECTION( XY, dfname=dfname,
	                                           using_approach=linear.correlation, 
                                               approach_ppname="linear.correlation", 
	                                           rtypes="SUBSAMPLE|COMPLETECASES|CORRELATION",
	                                           target_var="Y", 
                                               top=topn, 
                                               nmax=nmax, 
                                               cmax=cmax, 
                                               percentile_threshold=from_quantile, 
                                               refine=refine )
    #colnames(XY) = old_colnames
	WHICH_VARS = F10[[3]]           
	if ( debug ) {
        cat( HEADER )
        print( F10 )
        str( F10 )
        cat( HEADER )
        print(paste("any(WHICH_VARS==Y)", any(WHICH_VARS=="Y")))
        print( setdiff( old_colnames, WHICH_VARS) )
        print( WHICH_VARS )
        cat( HEADER )
    } 

    # SELECTED = c(); for ( i in 1:length(WHICH_VARS) ) { SELECTED= append( SELECTED, which( colnames(XY)==WHICH_VARS[i] ))}
    Xx = as.data.frame(as.matrix(XY[, WHICH_VARS]))
    colnames(Xx) = WHICH_VARS 
	# Xx = Xx[,-ncol(Xx)]                  
	# Zx = DO_PCA( scale( Xx ) )$Z    
    if ( debug ) 
        MDF_STATS( Xx )

    return ( Xx)
}
# ######################################################################################################


# ######################################################################################################
GENERATE_INTERACTION_TERMS = function( X, interaction="mult", op=as.numeric ) {
    START = TRUE
    for ( i in 1:(ncol(X)-1) ) {
        for ( j in (i+1):ncol(X) ) {
            if ( interaction == "mult" ) ITERM = op(X[,i]) * op(X[,j])
            if ( interaction == "div" )  ITERM = op(X[,i]) / op(X[,j])
            if ( interaction == "add" )  ITERM = op(X[,i]) + op(X[,j])
            if ( interaction == "sub" )  ITERM = op(X[,i]) - op(X[,j])
            if ( START ) {
                INTERACTION_TERMS = data.frame( 'ITERM'=ITERM )
                START = FALSE
            } else {
                INTERACTION_TERMS = cbind( INTERACTION_TERMS, ITERM ) 
            }
        }
    }
    return ( INTERACTION_TERMS )
}
# ######################################################################################################


# ######################################################################################################
PRINT_DESIGN_MATRIX_DETAILS = function( XX, FINAL_MAPPING, debug=FALSE ) {
    cat(HEADER)
    cat(HEADER)
        SELECTED = c()
        for ( i in 1:length(colnames(XX)) ) { SELECTED= append( SELECTED, which( str_detect( FINAL_MAPPING, colnames(XX)[i]) == TRUE ))}
        if ( debug ) { print ( SELECTED ) ; print( FINAL_MAPPING[SELECTED] ) }

        X = XX
        if ( debug ) str(X)
        cat( HEADER )
        print( "FINAL_MAPPING[FEATURE SELECTED VARIABLES]" )
        print( FINAL_MAPPING[SELECTED] )
        cat( HEADER )
        print( "CORRELATION BETWEEN SELECTED FEATURES" )
        print( ifelse(cor(X) > 0.8, 111, 0 ) )
        cat( HEADER )

        HASH_SELECTED = list()
        for ( i in 1:length(colnames(XX)) ) { 
            HASH_SELECTED[[i]] = c( colnames(XX)[i], FINAL_MAPPING[SELECTED[i]]) 
            if ( debug ) print( HASH_SELECTED[[i]] )
        }
    cat(HEADER)
    cat(HEADER)
    return ( HASH_SELECTED )
}
# ######################################################################################################


# ######################################################################################################
    # ######################################################################################################
	# > summary(X)
	#        zn             indus            nox               rm             age              dis        
	#  Min.   :  0.00   Min.   : 0.46   Min.   :0.3850   Min.   :3.561   Min.   :  2.90   Min.   : 1.130  
	#  1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100  
	#  Median :  0.00   Median : 9.69   Median :0.5380   Median :6.208   Median : 77.50   Median : 3.207  
	#  Mean   : 11.36   Mean   :11.14   Mean   :0.5547   Mean   :6.285   Mean   : 68.57   Mean   : 3.795  
	#  3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188  
	#  Max.   :100.00   Max.   :27.74   Max.   :0.8710   Max.   :8.780   Max.   :100.00   Max.   :12.127  
	#       rad              tax           ptratio            b         
	#  Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   :  0.32  
	#  1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38  
	#  Median : 5.000   Median :330.0   Median :19.05   Median :391.44  
	#  Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :356.67  
	#  3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:396.23  
	#  Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :396.90  
	# > 
    # ######################################################################################################
    # Boston {MASS}	R Documentation     Housing Values in Suburbs of Boston     Description
	# The Boston data frame has 506 rows and 14 columns.  [Package MASS version 7.2-29 Index]
    # ######################################################################################################
	# crim    per capita crime rate by town 
	# zn      proportion of residential land zoned for lots over 25,000 sq.ft. 
	# indus   proportion of non-retail business acres per town 
	# chas    Charles River dummy variable (= 1 if tract bounds river; 0 otherwise) 
	# nox     nitrogen oxides concentration (parts per 10 million) 
	# rm      average number of rooms per dwelling 
	# age     proportion of owner-occupied units built prior to 1940 
	# dis     weighted mean of distances to five Boston employment centres 
	# rad     index of accessibility to radial highways 
	# tax     full-value property-tax rate per $10,000 
	# ptratio pupil-teacher ratio by town 
	# black   1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town 
	# lstat   lower status of the population (percent) 
	# medv    median value of owner-occupied homes in $1000 
    # ######################################################################################################
	# Source
	# Harrison, D. and Rubinfeld, D.L. (1978) Hedonic prices and the demand for clean air. J. Environ. Economics and Management 5, 81–102.
	# Belsley D.A., Kuh, E. and Welsch, R.E. (1980) Regression Diagnostics. Identifying Influential Data and Sources of Collinearity. New York: Wiley.
    # ######################################################################################################
# ######################################################################################################
BUILD_BOSTONHOUSING_DATASET = function( DO_FSELECTION=TRUE, DO_FEATURE_EXPLORATION=TRUE, pix=14, do_log=TRUE ) {
    require('mlbench')
    data(BostonHousing)

    Y = log(BostonHousing[pix])
    X = BostonHousing[-c(4,pix)]
    if ( do_log ) Y = log(BostonHousing[pix] + 1)
    if ( do_log ) X = log(BostonHousing[-c(4,pix)]+ 1)

    FEATURE_SELECTION = DO_FSELECTION

    if ( DO_FEATURE_EXPLORATION ) {
        INTERACTION_TERMS   = GENERATE_INTERACTION_TERMS( X, interaction="mult", op=sqrt )
        HIGHER_DEGREE_TERMS = cbind(  X^(3/2), X^(1/3), sqrt(X+1) )
        INTERESTING_TERMS   = with(X, cbind( rm^2,dis^2,
                                                (log(age)*rm)/(lstat+1),
                                                1/(rad*dis),
                                                1/sqrt(lstat),
                                                dis^2*nox,
                                                1/sqrt(crim),
                                                log(tax^2/rm),
                                                age/(ptratio+1)))
        COMPLETE_TERMS_DF   = cbind(  X, INTERACTION_TERMS, HIGHER_DEGREE_TERMS, INTERESTING_TERMS)
        colnames(COMPLETE_TERMS_DF) = paste("X",1:ncol(COMPLETE_TERMS_DF), sep="")
    } else {
        COMPLETE_TERMS_DF = X
    }

    FULL_DF    = EXTEND_DF( COMPLETE_TERMS_DF, Y, colname="Y" )
    WHICH_ROWS = complete.cases(FULL_DF)
    YCOL       = ncol(FULL_DF)
    Y          = FULL_DF[WHICH_ROWS,         YCOL]
    FULL_DF    = FULL_DF[WHICH_ROWS,        -YCOL]
    ORIG_X     = FULL_DF[WHICH_ROWS,c(1:ncol(FULL_DF))]
    XX         = ORIG_X

    FINAL_MAPPING = sprintf("FINAL_MAPPING: %16s-->%16s", colnames(XX), paste("S",1000+1:ncol(XX), sep=""))

        NR         = nrow(FULL_DF)
        if ( FEATURE_SELECTION ) {
            if ( DO_FEATURE_EXPLORATION ) {
                XY = EXTEND_DF( INTERACTION_TERMS,   Y, colname="Y" )
                    X1 = DO_FEATURE_SET_SUBSELECTION( XY, dfname="Interaction_Terms,  R:N",cmax=0.8, topn=6, nmax=NR, from_quantile=0.6, key="I" )
                XY = EXTEND_DF( HIGHER_DEGREE_TERMS, Y, colname="Y" )
                    X2 = DO_FEATURE_SET_SUBSELECTION( XY, dfname="Higher_Order_Terms, R:N",cmax=0.8, topn=6, nmax=NR, from_quantile=0.6, key="O" )
                XY = EXTEND_DF( INTERESTING_TERMS,   Y, colname="Y" )
                    X3 = DO_FEATURE_SET_SUBSELECTION( XY, dfname="Interesting_Terms,  R:N",cmax=0.8, topn=6, nmax=NR, from_quantile=0.6, key="P" )
                XY = EXTEND_DF( X,                   Y, colname="Y" )
                    X0 = DO_FEATURE_SET_SUBSELECTION( XY, dfname="Original    Terms,  R:N",cmax=0.7, topn=8, nmax=NR, from_quantile=0.6, key="X" )
                XX = cbind( X0, X1, X2, X3 )
            }

            FINAL_MAPPING = sprintf("FINAL_MAPPING: %16s-->%16s", colnames(XX), paste("S",1000+1:ncol(XX), sep=""))

            XY = EXTEND_DF( XX,                  Y, colname="Y" )
                 XX = DO_FEATURE_SET_SUBSELECTION( XY, dfname="Union Sel'd Subsets,R:N",cmax=0.8, topn=4, nmax=NR, from_quantile=0.6, key="S", 
                                                  debug=FALSE, refine=FALSE)

            MDF_STATS( XX )
        }

    SELECTED_FEATURES = PRINT_DESIGN_MATRIX_DETAILS( XX, FINAL_MAPPING )

    X = as.matrix(XX)
    Y = as.matrix(Y)

    retvals = list( 'X'=X, 'Y'=Y, 'MAPPING'=FINAL_MAPPING, 'SELECTED_FEATURES'=SELECTED_FEATURES, 'DO_FS'=FEATURE_SELECTION, 'DO_FE'=DO_FEATURE_EXPLORATION )
    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
BUILD_SYNTHETIC_DATASET = function( DO_FSELECTION=FALSE, DO_FEATURE_EXPLORATION=FALSE, do_log=TRUE ) {
    FEATURE_SELECTION = DO_FSELECTION
    NR = 1E4
    NF = 7
    NE = NR*NF
    X = matrix(rnorm(NE,0,10),NR)
        MODEL     = (1/t(as.matrix(seq(1,NF,1))))^2
        INTERCEPT = pi
        NOISE     = 5E-6 * matrix(rnorm(NR,0,10),NR)
        Y         = INTERCEPT + ( t( MODEL %*% t(X) ) + NOISE )

    if ( DO_FEATURE_EXPLORATION ) {
        INTERACTION_TERMS   = GENERATE_INTERACTION_TERMS( X, interaction="mult", op=abs )
        HIGHER_DEGREE_TERMS = cbind(  abs(X)^(1/2 ) )
        COMPLETE_TERMS_DF   = cbind(  X, INTERACTION_TERMS, HIGHER_DEGREE_TERMS )
        colnames(COMPLETE_TERMS_DF) = paste("X",1:ncol(COMPLETE_TERMS_DF), sep="")
    } else {
        COMPLETE_TERMS_DF = X
    }
    COMPLETE_TERMS_DF = X
    colnames(COMPLETE_TERMS_DF) = paste("X",1:ncol(COMPLETE_TERMS_DF), sep="")

    FULL_DF = EXTEND_DF( COMPLETE_TERMS_DF, Y, colname="Y" )
    WHICH_ROWS = complete.cases(FULL_DF)
    YCOL       = ncol(FULL_DF)
    Y          = FULL_DF[WHICH_ROWS,         YCOL]
    FULL_DF    = FULL_DF[WHICH_ROWS,        -YCOL]
    ORIG_X     = FULL_DF[WHICH_ROWS,c(1:ncol(FULL_DF))]
    XX         = ORIG_X

    FINAL_MAPPING = sprintf("FINAL_MAPPING: %4s-->%4s", colnames(XX), paste("S",1000+1:ncol(XX), sep=""))

        NR         = nrow(FULL_DF)
        if ( FEATURE_SELECTION ) {
            if ( DO_FEATURE_EXPLORATION ) {
                XY = EXTEND_DF( INTERACTION_TERMS,   Y, colname="Y" )
                    X1 = DO_FEATURE_SET_SUBSELECTION( XY, dfname="Interaction_Terms,  R:N",cmax=0.8, topn=6, nmax=NR, from_quantile=0.6, key="I" )
                XY = EXTEND_DF( HIGHER_DEGREE_TERMS, Y, colname="Y" )
                    X2 = DO_FEATURE_SET_SUBSELECTION( XY, dfname="Higher_Order_Terms, R:N",cmax=0.8, topn=6, nmax=NR, from_quantile=0.6, key="O" )
                XY = EXTEND_DF( X,                   Y, colname="Y" )
                    X0 = DO_FEATURE_SET_SUBSELECTION( XY, dfname="Original    Terms,  R:N",cmax=0.7, topn=8, nmax=NR, from_quantile=0.6, key="X" )
                XX = cbind( X0, X1, X2 )
            }

            FINAL_MAPPING = sprintf("FINAL_MAPPING: %16s-->%16s", colnames(XX), paste("S",1000+1:ncol(XX), sep=""))

            XY = EXTEND_DF( XX,                  Y, colname="Y" )
                 XX = DO_FEATURE_SET_SUBSELECTION( XY, dfname="Union Sel'd Subsets,R:N",cmax=0.7, topn=4, nmax=NR, from_quantile=0.6, key="S", 
                                                  debug=FALSE, refine=FALSE)

            MDF_STATS( XX )
        }

    SELECTED_FEATURES = PRINT_DESIGN_MATRIX_DETAILS( XX, FINAL_MAPPING )

    X = as.matrix(XX)
    Y = as.matrix(Y)

    retvals = list( 'X'=X, 'Y'=Y, 'MAPPING'=FINAL_MAPPING, 'SELECTED_FEATURES'=SELECTED_FEATURES, 'DO_FS'=FEATURE_SELECTION, 'DO_FE'=DO_FEATURE_EXPLORATION )
    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
# > housing = read.table( "http://www.jaredlander.com/data/housing.csv", sep=",", header=TRUE, stringsAsFactors=FALSE )
# > summary( housing )
#                   Neighborhood    Building.Classification  Total.Units     
#  FLUSHING-NORTH         : 133   R2-CONDOMINIUM: 441       Min.   :   1.00  
#  UPPER EAST SIDE (59-79): 123   R4-CONDOMINIUM:1883       1st Qu.:  15.00  
#  HARLEM-CENTRAL         :  94   R9-CONDOMINIUM: 237       Median :  30.00  
#  CHELSEA                :  88   RR-CONDOMINIUM:  65       Mean   :  70.18  
#  UPPER WEST SIDE (59-79):  87                             3rd Qu.:  75.00  
#  UPPER EAST SIDE (79-96):  78                             Max.   :3378.00  
#  (Other)                :2023                                              
#    Year.Built     Gross.SqFt      Estimated.Gross.Income Gross.Income.per.SqFt
#  Min.   :1825   Min.   :    478   Min.   :    6424       Min.   : 3.57        
#  1st Qu.:1926   1st Qu.:  18704   1st Qu.:  405180       1st Qu.:18.79        
#  Median :1986   Median :  38456   Median :  943901       Median :25.00        
#  Mean   :1967   Mean   :  82763   Mean   : 2640882       Mean   :27.57        
#  3rd Qu.:2005   3rd Qu.:  90626   3rd Qu.: 2725550       3rd Qu.:36.82        
#  Max.   :2010   Max.   :3364977   Max.   :56010967       Max.   :62.80        
#  NA's   :96                                                                   
#  Estimated.Expense  Expense.per.SqFt Net.Operating.Income Full.Market.Value  
#  Min.   :    1740   Min.   : 0.97    Min.   :    4684     Min.   :    30000  
#  1st Qu.:  155515   1st Qu.: 7.64    1st Qu.:  239700     1st Qu.:  1677750  
#  Median :  350264   Median : 9.18    Median :  581522     Median :  4026500  
#  Mean   :  840916   Mean   : 9.40    Mean   : 1799966     Mean   : 12977808  
#  3rd Qu.:  899084   3rd Qu.:11.05    3rd Qu.: 1805149     3rd Qu.: 13136752  
#  Max.   :21771401   Max.   :18.21    Max.   :40144686     Max.   :295182007  
#                                                                              
#  Market.Value.per.SqFt            Boro     
#  Min.   : 10.66        Bronx        :  69  
#  1st Qu.: 74.63        Brooklyn     : 717  
#  Median :112.22        Manhattan    :1380  
#  Mean   :131.19        Queens       : 434  
#  3rd Qu.:187.49        Staten Island:  26  
#  Max.   :399.38                    
# ######################################################################################################
BUILD_NYHOUSING_DATASET = function( DO_FSELECTION=FALSE, DO_FEATURE_EXPLORATION=FALSE, do_log=TRUE, PREDICT_VAR=10 ) {
    housing = read.table( "housing.csv", sep=",", stringsAsFactors=TRUE, header=TRUE )
    X = housing[-PREDICT_VAR]
    Y = housing[PREDICT_VAR]
    retvals = list( 'X'=X, 'Y'=Y )
    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
# The Jester Dataset (save to disk, then unzip to obtain Excel files):
# 
# jester-data-1.zip : (3.9MB) Data from 24,983 users who have rated 36 or more jokes, a matrix with dimensions 24983 X 101.
# jester-data-2.zip : (3.6MB) Data from 23,500 users who have rated 36 or more jokes, a matrix with dimensions 23500 X 101.
# jester-data-3.zip : (2.1MB) Data from 24,938 users who have rated between 15 and 35 jokes, a matrix with dimensions 24,938 X 101.
# Format:
# 
# 3 Data files contain anonymous ratings data from 73,421 users.
# Data files are in .zip format, when unzipped, they are in Excel (.xls) format
# Ratings are real values ranging from -10.00 to +10.00 (the value "99" corresponds to "null" = "not rated").
# One row per user
# The first column gives the number of jokes rated by that user. 
# The next 100 columns give the ratings for jokes 01 - 100.
# The sub-matrix including only columns {5, 7, 8, 13, 15, 16, 17, 18, 19, 20} is dense. 
# Almost all users have rated those jokes (see discussion of "universal queries" in the above paper).
# ######################################################################################################
BUILD_JESTER_DATASET = function( which_one=1 ) {
    jester = read.table( sprintf('jester-data-%s.csv', which_one), sep=",", header=FALSE )
    NCOL = ncol(jester)
    NROW = nrow(jester)
    colnames(jester) = c( 'NR', paste ('R', c(1:100), sep="" ) )
    J  = as.matrix( jester )

    NUM_RATINGS  = J[,1]
    JOKE_RATINGS = J[,2:NCOL]

    NCOL = NCOL-1

    UID = c(1:NROW)
    USER_RATING_AVG = apply( J, 1, mean )
    USER_RATING_SD  = apply( J, 1, sd )

    J = cbind( 'UID'=UID, 'NR'=NUM_RATINGS, 'AVG'=USER_RATING_AVG, 'SD'=USER_RATING_SD, J )
    RETVALS = list( 'J'=J, 'UID'=1, 'NR'=2, 'AVG'=3, 'SD'=4, 'RATINGS'=c(5:104) )
    return ( RETVALS )
}
# ######################################################################################################


# ######################################################################################################
# Herlocker, J., Konstan, J., Borchers, A., Riedl, J.. An Algorithmic
# Framework for Performing Collaborative Filtering. Proceedings of the
# 1999 Conference on Research and Development in Information
# Retrieval. Aug. 1999.
# 
# GroupLens Research currently operates a movie recommender based on
# collaborative filtering:
# 
#         http://www.movielens.org/
# 
# DETAILED DESCRIPTIONS OF DATA FILES
# ==============================================
# 
# Here are brief descriptions of the data.
# 
# ml-data.tar.gz   -- Compressed tar file.  To rebuild the u data files do this:
#                 gunzip ml-data.tar.gz
#                 tar xvf ml-data.tar
#                 mku.sh
# 
# u.data     -- The full u data set, 100000 ratings by 943 users on 1682 items.
#               Each user has rated at least 20 movies.  Users and items are
#               numbered consecutively from 1.  The data is randomly
#               ordered. This is a tab separated list of 
# 	         user id | item id | rating | timestamp. 
#               The time stamps are unix seconds since 1/1/1970 UTC   
# 
# u.info     -- The number of users, items, and ratings in the u data set.
# 
# u.item     -- Information about the items (movies); this is a tab separated
#               list of
#               movie id | movie title | release date | video release date |
#               IMDb URL | unknown | Action | Adventure | Animation |
#               Children's | Comedy | Crime | Documentary | Drama | Fantasy |
#               Film-Noir | Horror | Musical | Mystery | Romance | Sci-Fi |
#               Thriller | War | Western |
#               The last 19 fields are the genres, a 1 indicates the movie
#               is of that genre, a 0 indicates it is not; movies can be in
#               several genres at once.
#               The movie ids are the ones used in the u.data data set.
# 
# u.genre    -- A list of the genres.
# 
# u.user     -- Demographic information about the users; this is a tab
#               separated list of
#               user id | age | gender | occupation | zip code
#               The user ids are the ones used in the u.data data set
# ######################################################################################################
#     user_id         item_id           rating       timestamp        
#  Min.   :  1.0   Min.   :   1.0   Min.   :1.00   Min.   :874724710  
#  1st Qu.:254.0   1st Qu.: 175.0   1st Qu.:3.00   1st Qu.:879448710  
#  Median :447.0   Median : 322.0   Median :4.00   Median :882826944  
#  Mean   :462.5   Mean   : 425.5   Mean   :3.53   Mean   :883528851  
#  3rd Qu.:682.0   3rd Qu.: 631.0   3rd Qu.:4.00   3rd Qu.:888259984  
#  Max.   :943.0   Max.   :1682.0   Max.   :5.00   Max.   :893286638  
# 
# ######################################################################################################
BUILD_RATINGS_MATRIX = function( RATINGS, MOVIES, M, N, INIT_VAL=NA, debug=FALSE ) {
    print( sprintf( "BUILDING CROSS REFERENCE MATRIX [%dx%d]", M, N ) )

    mdat <- matrix(rep(INIT_VAL, M*N), nrow = M )
    for( i in 1:nrow(RATINGS) ) {
        uid = RATINGS[i,1]
        mid = RATINGS[i,2]
        mr  = RATINGS[i,3]
        t   = RATINGS[i,4]
        mdat[ uid, mid ] = as.numeric(mr)
    }
    if ( debug ) str(mdat)

    MDAT = as.data.frame( mdat ) 
    rownames(MDAT) = paste("U",c(1:nrow(MDAT)),sep="")
    colnames(MDAT) = paste("M",c(1:ncol(MDAT)),sep="")
    MDAT = as.matrix( MDAT )

    return ( MDAT )
}
# ######################################################################################################


# ######################################################################################################
BUILD_MOVIELENS_DATASET = function( ) {
    ratings   = read.csv( 'ml-100k/u.data', sep="\t", header=TRUE, fileEncoding="latin1" ) 
    genre     = read.csv( 'ml-100k/u.genre',sep="|",  header=TRUE, stringsAsFactors=TRUE, fileEncoding="latin1" )
    info      = read.csv( 'ml-100k/u.info', sep=" ",  header=TRUE, fileEncoding="latin1" )
    movies    = read.csv( 'ml-100k/u.item', sep="|",  header=TRUE, fileEncoding="latin1" )
    occupation= read.csv( 'ml-100k/u.occupation', sep=" ",  header=TRUE, fileEncoding="latin1" )
    users     = read.csv( 'ml-100k/u.user', sep="|",  header=TRUE, fileEncoding="latin1" )
    N_USERS   = nrow(users)
    N_MOVIES  = nrow(movies)
    N_GENRE   = nrow(genre)
    user_movies = BUILD_RATINGS_MATRIX( ratings, movies, N_USERS, N_MOVIES, INIT_VAL=NA )
    retvals  = list( 'ratings'=ratings,
                     'movies' =movies,
                     'users'  =users,
                     'genre'  =genre,
                     'counts' =info,
                     'user_movies'=user_movies,
                     'occupation'=occupation )
    return( retvals )
}
# ######################################################################################################


# ######################################################################################################
GET_RANDOM_XY = function( M=1000, N=5 ) {

    set.seed( as.integer( (proc.time()[3] * 4096 ) %% 16384 ) )

    u  = seq(1,4*N,4)
    d  = rep(0.1,N )

    XX = MATRIX(M, N)

    for ( i in 1:N) {
        m = u[i]
        s = d[i]
        XX[,i] = rnorm( M, m, s )
        print( sprintf( "V[%s] = rnorm( %s %s %s )", i, M, m, s ))
    }

    rownames(XX) = 1:M
    colnames(XX) = paste("X",1:N,sep="")

    return ( XX )
}
# ######################################################################################################


# ######################################################################################################
GET_CLUSTERED_X = function( M=100, NFEATURES=2, NCLUSTERS=5, MU_X=seq(0,NCLUSTERS-1,1), SD_X=rep(0.1, NCLUSTERS), PATTERN="LINE", IDX=c(), SD=0) {

    cat ( HEADER )
    print( sprintf( "X comprises: %s samples sets from %s normal random sources across a %s dimensional space", M*NCLUSTERS, NCLUSTERS, NFEATURES ) ) 
    print( sprintf( "MU[%s]=%.2f", 1:NCLUSTERS, MU_X ) )
    print( sprintf( "SD[%s]=%.2f", 1:NCLUSTERS, SD_X ) )
    cat ( HEADER )


    # a 2-dimensional example (taken from an R manpage?)
    if ( length(IDX)== 0 ) {
        if ( PATTERN == "LINE" )   
            IDX = rep(c(1,rep(1,NFEATURES-1),
                        2,rep(2,NFEATURES-1),
                        3,rep(3,NFEATURES-1),
                        4,rep(4,NFEATURES-1),
                        5,rep(5,NFEATURES-1)),10)
        if ( PATTERN == "SQUARE" ) 
            IDX = rep(c(1,rep(1,NFEATURES-1),
                        1,rep(5,NFEATURES-1),
                        3,rep(3,NFEATURES-1),
                        5,rep(1,NFEATURES-1),
                        5,rep(5,NFEATURES-1)),10)
        if ( PATTERN == "STAR" )   
            IDX = rep(c(2,rep(1,NFEATURES-1),
                        4,rep(1,NFEATURES-1),
                        3,rep(5,NFEATURES-1),
                        1,rep(5,NFEATURES_1),
                        5,rep(3,NFEATURES_1)),10)
    } else
        IDX = MU_X

    START = TRUE
    MX = MATRIX( M, NFEATURES )
    for ( idx in seq(1,(NFEATURES*NCLUSTERS),NFEATURES) ) {
        for ( j in 1:(NFEATURES) )  {
            MX[, (j)] = rnorm( M, mean = MU_X[IDX[idx+j-1]], sd = SD_X[IDX[idx+j-1]]+SD)
            if ( any( is.na( MX ) ) ) print( summary( MX ) )
        }

        cluster_idx = round((idx+1)/NFEATURES)
        print( sprintf( "CLUSTER #%s:    MU[%s]=%.2f      SD[%s]=%.2f", cluster_idx, cluster_idx, MU_X[cluster_idx], cluster_idx, SD_X[cluster_idx] ) )
        print( summary( MX ) )
        cat ( HEADER )

        if ( START ) {
            START = FALSE
            X = MX
        } else { 
            X = rbind( X, MX )
        }
    }
    str(X)
    colnames(X) <- paste( "X", 1:NFEATURES, sep="" )
    rownames(X) <- 1:nrow(X)
    X = scale( X )
    return ( X )
}
# #################################################################################################


# #################################################################################################
READ_C45_DATA = function( filename_stem ) {
    t = read.csv( paste(filename_stem,"data",sep="."), header=FALSE, stringsAsFactors=TRUE )
    h = read.csv2(paste(filename_stem,'c45-names', sep="."), sep=":")
    h = rownames(h)[c(-2,-3)]
    h = gsub("\\| ","", h )
    h = gsub(" ","_", h )
    h = c(h[2:length(h)], h[1])
    colnames(t) = h
    print( summary(t))
    return ( t )
}
# ######################################################################################################



