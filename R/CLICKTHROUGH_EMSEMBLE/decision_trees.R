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
# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
# library(partykit)				# Convert rpart object to BinaryTree
# ######################################################################################################
library(rpart)				    # Popular decision tree algorithm
library(rpart.plot)				# Enhanced tree plots
library(randomForest)           # randomForest for variable importance and error
#library(rattle)				# Fancy tree plot
#library(RColorBrewer)			# Color selection for fancy tree plot
#library(party)					# Alternative decision tree algorithm
#library(caret)					# Just a data source for this script # but probably one of the best R packages ever. 
# ######################################################################################################


# ######################################################################################################
source( 'utilities.R' )
source( 'datasets.R' )
# ######################################################################################################


# ###############################################################################
VERIFY_OPTARG ( OPTARG_NAME="TEST_ENABLED",  OPTARG_VALUE=TRUE)
# ###############################################################################


# ######################################################################################################
SCORE = function( Y, YP, i, YLEVELS_SCORING=list( 'unacc'=0, 'acc' =1, 'good'=2, 'vgood'=3 ), silent=FALSE, debug=FALSE ) {
    y  = as.character(Y[i])
    yp = as.character(YP[i])
    s_y  = YLEVELS_SCORING[[y]]
    s_yp = YLEVELS_SCORING[[yp]]
    MSE = (s_yp - s_y )^2
    print(MSE)
    # if ( debug ) print( sprintf( "%5s [%12s vs. %12s]  ==>  [%3s]    IMSE=%.4g", i, y, yp, s_yp - s_y, MSE/i  ) )
    # else if ( !silent ) { 
                # if ( debug ) print( paste( y, yp, s_yp, s_y ) )
                # if ( abs(s_yp - s_y) != 0 )
                    # print( sprintf( "%5s [%12s vs. %12s]  ==>  [%3s]    IMSE=%.4g", i, y, yp, s_yp - s_y, MSE/i  ) )
            # }
    return ( MSE )
}
# ######################################################################################################


# ######################################################################################################
# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
# ######################################################################################################
DO_DECISION_TREE = function( XY, Y, FORMULA="", DO_PRUNING=TRUE, Q=0.1, silent=TRUE, ... ) {
    if (nchar(FORMULA)==0) FORMULA = sprintf( "%s ~ .", colnames(Y) )
    DETAILED_MODEL   = rpart( as.formula(FORMULA), data=XY, ... )
    MODEL            = DETAILED_MODEL
    if ( DO_PRUNING ) {
        MODEL_CV         = printcp( DETAILED_MODEL )
        R_ERROR_THRESHOLD= quantile( MODEL_CV[,'rel error'], Q)
        while( length(R_ERROR_THRESHOLD)== 0 ) {
            Q = Q + 0.01
            R_ERROR_THRESHOLD= quantile( MODEL_CV[,'rel error'], Q)
        }
        CP_CHOICES       = MODEL_CV[,'rel error'] < R_ERROR_THRESHOLD
        CP_PIVOT         = MODEL_CV[CP_CHOICES,'CP'][1]
        MODEL            = prune( DETAILED_MODEL, cp=CP_PIVOT )
    }
    if ( !silent ) {
        NEWLINE(3)
        print( summary( MODEL ) )
        cat(HEADER)
        NEWLINE(3)
        printcp( MODEL )
        cat(HEADER)
    }
    return ( MODEL )
}
# ######################################################################################################


# ######################################################################################################
# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
# ######################################################################################################
PLOT_TREE = function( MODEL, INCREMENTAL_MSE, CONFUSION_MATRIX, LABEL="TRAIN" ) {
    def.par <- par(no.readonly = TRUE) # save default, for resetting...

    LABEL = sprintf("%s SET [%s SAMPLES]", LABEL, GET_SIZE(INCREMENTAL_MSE))

    nf <- layout(matrix(c(1,1,1,1,1,1,2,3,4), 3, 3, byrow = TRUE), respect = TRUE)

    # PLOT 1
    if ( length(grep( "rpart", class(MODEL))) ) {
        prp(MODEL, type=4, nn=TRUE, cex=0.7, extra=2, under=TRUE, branch=1, 
                        main=paste("RECURSIVE PARTITIONING TREE", LABEL))#, varlen=6)# Shorten variable names
    } else {
        plot( MODEL )
    }

    # PLOT 2
    plot( INCREMENTAL_MSE, t='l', main="INCREMENTAL MSE" )

    ACM = addmargins(CONFUSION_MATRIX)
    ACM = apply( ACM, 2, function(x) x/x[length(x)] )
    ACM = ACM[1:nrow(CONFUSION_MATRIX),1:ncol(CONFUSION_MATRIX)]

    # PLOT 3 and PLOT 4
    # colors = c("blue", "green", "brown", "red", "gray", "white", 20:40 )
    plot( CONFUSION_MATRIX, main="CONFUSION MATRIX (CM)", col=colors[1:max(ncol(CONFUSION_MATRIX),nrow(CONFUSION_MATRIX))] )
    plot( as.table(ACM),    main="CLASS FREQUENCIES",     col=colors[1:max(ncol(CONFUSION_MATRIX),nrow(CONFUSION_MATRIX))] )

    par( def.par )

    return ( ACM )
}
# ######################################################################################################


# ######################################################################################################
PREDICT_TREE = function( MODEL, XY, as_probas=FALSE ) {
    YP_PRED   = predict(MODEL, XY)
    if ( !as_probas ) {
        YP_MATRIX = matrix(YP_PRED, nrow(XY))
        YP        = apply( YP_MATRIX, 1, function( x ) colnames(YP_PRED)[which(x == max(x))] )
        return ( YP )
    } else {
        return ( YP_PRED )
    }
}
# ######################################################################################################


# ######################################################################################################
GET_TREE_IMSE = function( Y, YP, total=FALSE ) {
    SSE  = sapply( 1:GET_SIZE(Y),   function(i) SCORE(Y, YP, i ) ) 
    CSSE = cumsum( SSE )
    IMSE = sapply( 1:GET_SIZE(SSE), function( i ) CSSE[i]/i )
    cat( HEADER )
    if ( total ) return ( sum(IMSE) )
    return ( IMSE )
}
# ######################################################################################################


# ######################################################################################################
GET_CONFUSION_MATRIX = function( Y, YP ) {
    CM = table( Y, YP )
    print ( CM )
    cat( HEADER )
    return ( CM )
}
# ######################################################################################################


# ######################################################################################################
FIT_DECISION_TREE = function( XY, Y, FORMULA=FORMULA, DO_PRUNING=TRUE, Q=0.1, minbucket=20, ... ) {
    MODEL = DO_DECISION_TREE( XY, Y, FORMULA=FORMULA, DO_PRUNING=DO_PRUNING, Q=Q, minbucket=minbucket, ...)
    return ( MODEL )
}
# ######################################################################################################


# ######################################################################################################
APPLY_DECISION_TREE_MODEL = function( MODEL, XY, Y=c(), LABEL="TRAIN" ) {
    if (class(Y) == "factor") { Y = as.numeric(Y)-1 }
    if (class(Y) == "ordered factor") { Y = as.numeric(Y)-1 }
    YP   = PREDICT_TREE( MODEL, XY, as_probas=FALSE )
    YP = as.numeric(YP)
    if( GET_SIZE(Y) != 0 ) {
        IMSE  = GET_TREE_IMSE( Y, YP, total=FALSE )
        CM    = GET_CONFUSION_MATRIX( Y, YP )
        PLOT_TREE( MODEL, IMSE, CM, LABEL )
    } else {
        IMSE  = rep(NA, GET_SIZE(YP) )
        CM    = table( YP )
        PLOT_TREE( MODEL, IMSE, CM, LABEL )
    }
    RETVALS = list( MODEL=MODEL, IMSE=IMSE, CM=CM )
    return ( RETVALS )
}
# ######################################################################################################


# ######################################################################################################
# Random Forest prediction  # http://www.statmethods.net/advstats/cart.html
# ######################################################################################################
GET_RANDOM_FOREST_MODEL = function( FORMULA, XY, ... ) {
    RF_MODEL = randomForest(FORMULA, data=T_XY, ... )
    IMP      = importance(RF_MODEL)                             # importance of each predictor 
    cat( HEADER )
    print(FORMULA)
    print(RF_MODEL)
    cat( HEADER )
    print(IMP)
    cat( HEADER )
    return( RF_MODEL )
}
# ######################################################################################################


# ######################################################################################################
# Random Forest prediction  # http://www.statmethods.net/advstats/cart.html
# ######################################################################################################
APPLY_RANDOM_FOREST = function( MODEL, R_XY, R_Y=c(), MODE="TRAIN" ) {
    YP       = predict( RF_MODEL, R_XY )

    if ( GET_SIZE(R_Y)!= 0 ) {
        IMSE     = GET_TREE_IMSE( R_Y, YP, total=FALSE )
        CM       = GET_CONFUSION_MATRIX( R_Y, YP )
    } else {
        IMSE     = rep(NA,GET_SIZE(YP))
        CM       = table(YP)
    }

    PLOT_TREE( MODEL, IMSE, CM, "RANDOM FOREST MODEL" )
    RETVALS = list( MODEL=RF_MODEL, YP=YP, CM=CM, IMSE=IMSE )

    return ( RETVALS )
}
# ######################################################################################################




# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
    # ##################################################################################################
    # golf = read.csv( 'golf.txt', sep="\t", header=TRUE, stringsAsFactors=TRUE )
    # XX   = SLICE_DATAFRAME( golf, 4 )
    # YY   = SLICE_DATAFRAME( golf, 7 )
    # ##################################################################################################
    # PIVOT = FIND_MINIMUM_ENTROPY_CUT_VALUE( XX, YY, SILENT=FALSE, DO_PLOT=TRUE )
    # VERIFY_PIVOT( PIVOT, XX, debug=FALSE )
    # A = table( cbind( XX, YY ))
    # for ( i in 1:10 )  
    # print( paste( i, rownames(A)[i], 
    #                   CONCAT(XX[XX<as.numeric(rownames(A)[i])]), "||", 
    #                   CONCAT(A[1:i,1]), "||", CONCAT(A[1:i,2]), "||", 
    #                   GET_ENTROPY_XY_WRT( A[1:i,1], A[1:i,2], LOG=log10 )))
    # ##################################################################################################


# ######################################################################################################
if ( TEST_ENABLED ) {
    # ##################################################################################################
    if ( TRUE ) graphics.off()
    sink()
    sink( 'output_decision_trees.out', split=TRUE )
    opts = options( width=132 )
    # ##################################################################################################

    ORIGINAL_XY    = READ_C45_DATA( 'car' )
    M              = nrow( ORIGINAL_XY )
    TEST_SIZE      = as.integer(M * 0.3)
    TRAIN_SIZE     = M - TEST_SIZE
    ORDERING       = sample(  rownames(ORIGINAL_XY), M )
    TRAIN_ORDERING = ORDERING[1:TRAIN_SIZE]
    TEST_ORDERING  = ORDERING[ (TRAIN_SIZE+1):M]

    PDF_FILE      = "plot_decision_trees_summary.pdf" 

    # ##################################################################################################
    # TRAIN
    # ##################################################################################################
        T_XY = ORIGINAL_XY[TRAIN_ORDERING,]
        T_X  = SLICE_DATAFRAME(T_XY, c(1:6)) 
        T_Y  = SLICE_DATAFRAME(T_XY, 7)
        FORMULA = sprintf( "%s ~ .", colnames(T_Y) )

        MODEL      = FIT_DECISION_TREE( T_XY, T_Y[,1], FORMULA=FORMULA, DO_PRUNING=TRUE, minbucket=20 )
        TRAIN_MODEL_EVAL = APPLY_DECISION_TREE_MODEL( MODEL, T_XY, Y=T_Y[,1], LABEL="TRAIN" )
        dev.copy( pdf, PDF_FILE, 11, 8 )
    # ##################################################################################################

    # ##################################################################################################
    # TEST
    # ##################################################################################################
        P_XY = ORIGINAL_XY[TEST_ORDERING,]
        P_X  = SLICE_DATAFRAME(P_XY, c(1:6)) 
        P_Y  = SLICE_DATAFRAME(P_XY, 7)
    
        TEST_MODEL_EVAL  = APPLY_DECISION_TREE_MODEL( MODEL, P_XY, Y=P_Y[,1], LABEL="TEST" )
        dev.copy( pdf ) # continues writing to the still opened pdf device
    # ##################################################################################################

    # ##################################################################################################
    dev.off() # screen
    dev.off() # pdf1
    dev.off() # pdf2
    print( paste( 'PDF file', PDF_FILE, 'was generated summarizing an assessment of the generated decision tree' ))
    # ##################################################################################################

    # ##################################################################################################
    # RANDOM FOREST PREDICTION
    # ##################################################################################################
        PDF_FILE      = "plot_decision_trees_random_forest_summary.pdf" 
        pdf( PDF_FILE, 11, 8 )

        RF_MODEL = GET_RANDOM_FOREST_MODEL( as.formula(FORMULA), T_XY, imp=1, 
                                           mtry0=2, 
                                           ntree=1023, 
                                           # nodesize=20, 
                                           maxnodes=127 )

        RANDOM_FOREST_RETVALS = APPLY_RANDOM_FOREST( RF_MODEL, T_XY, T_Y[,1], MODE="TRAIN" )
        RF_CM    = RANDOM_FOREST_RETVALS$CM

        RANDOM_FOREST_RETVALS = APPLY_RANDOM_FOREST( RF_MODEL, P_XY, P_Y[,1], MODE="TEST" )
        RF_CM    = RANDOM_FOREST_RETVALS$CM

        dev.off()
        print( paste( 'PDF file', PDF_FILE, 'was generated summarizing an assessment of the random-forest selected tree' ))

    # ##################################################################################################


    # ##################################################################################################
    options( opts )
    sink()
    # ##################################################################################################
}
# ######################################################################################################



