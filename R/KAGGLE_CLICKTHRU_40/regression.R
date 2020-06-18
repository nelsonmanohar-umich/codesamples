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


# ###############################################################################
source( 'utilities.R' )
source( 'plot_functions.R' )
source( 'datasets.R' )
source( 't_tests.R' )
# ###############################################################################


# ###############################################################################
VERIFY_OPTARG ( OPTARG_NAME="TEST_ENABLED",  OPTARG_VALUE=TRUE)
# ###############################################################################


# ###############################################################################
# TODO
# pca implementation with verification (with subset and iterator if necessary for scalability)
# iterators for tests with variable sampling so as to address scalability and dominance of finding on repeated trials
# ###############################################################################


# ###############################################################################
# simple balanced anova with normality check
# ###############################################################################
# ANOVA( formula, data, 
# Call: aov(formula = tip ~ day - 1, data = data)
# Terms:                day Residuals
# Sum of Squares  2203.0066  455.6866
# Deg. of Freedom         4       240
# Residual standard error: 1.377931
# Estimated effects are balanced
# ###############################################################################
#            Df Sum Sq Mean Sq F value Pr(>F)    
# day         4 2203.0   550.8   290.1 <2e-16 ***
# Residuals 240  455.7     1.9                   
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   dayFri   daySat   daySun  dayThur 
# 2.734737 2.993103 3.255132 2.771452 
# ###############################################################################
DO_SIMPLE_ANOVA = function( formula_txt, data ) {
    formula_f = formula(eval(parse(text=formula_txt)))
    t = aov( tip ~ day -1, data=data )
    summary(t)
    t$coeff
    return( t )
}
# ###############################################################################


# ###############################################################################
COMPARE_EFFECTS_OF_FACTOR_LEVELS_FOR = function( x, y ) {
}
# ###############################################################################


# ######################################################################################################
DO_REGRESSION_DIAGNOSTICS = function( X_TEST_SCALED, 
                                      Y_TEST_SCALED, 
                                      SGD_Y_TEST, PREDICT_VARNAME="Y", W_INTERCEPT=TRUE, 
                                      silent=FALSE, debug=FALSE ) {
    cat(HEADER)
    op <- par( mfrow=c(2,3) )
        eterm = Y_TEST_SCALED-SGD_Y_TEST
        ci    = sd(eterm)
        plot( Y_TEST_SCALED,  SGD_Y_TEST, pch="+", cex=0.7, main="PREDICTED vs. TRUE VALUES" )
            text( SGD_Y_TEST     +rnorm(1,0,1E-3), 
                  Y_TEST_SCALED  +rnorm(1,0,1E-3), rownames(SGD_Y_TEST ), c="black", cex=0.5 )
            abline(c(0,1),          col="black",lw=2)
            abline(c(mean(eterm),1),col="blue", lw=1)
            abline(c(ci, 1),        col="red",  lw=2)
            abline(c(-ci,1),        col="red",  lw=2)

        DO_HIST(   Y_TEST_SCALED, nbins=32, ptitle="Histogram of Y" )
        DO_HIST(   SGD_Y_TEST,    nbins=32, ptitle="Histogram of YP" )
        DO_QQPLOT( Y_TEST_SCALED-SGD_Y_TEST, nbins=32, use_par=FALSE )
    par( op )

    # ######################################################################################################
    print( "WRT PREDICTED VALUES:" )
    print( "ARE PREDICTED AND TRUE VALUES REPRESENTATIVE OF A SIMILAR ENOUGH SOURCES?" )
    FINDINGS = ARE_THESE_REPRESENTATIVE_OF_SAME_SOURCE( Y_TEST_SCALED, 
                                                        SGD_Y_TEST,    nmax=0, confidence_level=0.999, silent=silent )
    cat( HEADER)
    # ######################################################################################################

    # ######################################################################################################
    # FIXME: put eval parse with theta and the columsn of x
    # ######################################################################################################
    XY_TEST = EXTEND_DF( as.data.frame(X_TEST_SCALED), as.data.frame(Y_TEST_SCALED), colname="Y")

    if ( !W_INTERCEPT )
        FIT = glm( Y ~ . - 1, data = XY_TEST )
    else
        FIT = glm( Y ~ .,     data = XY_TEST )

    sum_txt = summary( EXTEND_DF( as.data.frame(XY_TEST), as.data.frame(SGD_Y_TEST), colname="YP") )
    fit_txt = summary( FIT )

    if (!silent) {
        NEWLINE(3) ; cat(HEADER)
        print ( sum_txt )
        cat(HEADER)

        NEWLINE(3) ; cat(HEADER)
        print ( fit_txt )
        cat(HEADER)
    }

    retvals = list( 'FIT'=FIT, 'FINDINGS'=FINDINGS )
    return ( retvals )
}
# ######################################################################################################






# ######################################################################################################
# www.stats.ox.ac.uk/~burke/Linear%20Models/Linear%20Models%20Notes.pdf
# ######################################################################################################
# ASSUMPTIONS_TXT = "# The following assumptions must be satisfied for Ordinary Least Squares Regression:
# # 1 Model is linear in parameters
# 2 The data are a random sample of the population i.e. The errors are statistically independent from one another (well-behaved residuals)
# 3 The expected value of the errors is always zero
# 4 The independent variables are not too strongly collinear
# 5 The independent variables are measured precisely
# 6 The residuals have constant variance
# 7 The errors are normally distributed 
# "
# ######################################################################################################


# ######################################################################################################
VERIFY_LINEAR_MODEL_ASSUMPTIONS = function( RESIDUALS, X, Y, MODEL, XVARS, YVARS, silent=FALSE, debug=FALSE, ... ) {
    NEWLINE(1)
    cat( HEADER )
    print( "VERIFY: LINEAR_MODEL_ASSUMPTIONS" )
    cat( HEADER )

    NS = as.integer(length(RESIDUALS)/2)
    RESIDUALS_IDX = sample( 1:length(RESIDUALS), length(RESIDUALS) )

    cat(HEADER)
        print( "WRT SAMPLES:" )
        for ( i in 1:ncol(X) ) {
            print( colnames(X)[i] )
            ARE_SAMPLES_MEANINGFULLY_CORRELATED( X[,i], Y, plevel=0.999, silent=silent, ... )
        }

    cat(HEADER)
        print( "WRT RESIDUALS:" )
        RETVALS   = COULD_SAMPLE_MEAN_BE_REPRESENTATIVE_OF ( RESIDUALS, 0, silent=silent, confidence_level=0.999, ... )

    cat(HEADER)
        print( "WRT RESIDUALS:" )
        RETVALS   = IS_NORMALLY_DISTRIBUTED( RESIDUALS, silent=silent, ... )

    cat(HEADER)
        print( "WRT RESIDUALS:" )
        RETVALS   = OUTLIER_ID( RESIDUALS, silent=silent, ... )
        OUTLIERS = RETVALS$OUTLIERS

    cat(HEADER)
        print( "WRT RESIDUALS:" )
        ARE_VARIANCES_EQUAL( RESIDUALS[RESIDUALS_IDX[1:NS]],
                             RESIDUALS[RESIDUALS_IDX[(NS+1):(2*NS)]], silent=silent, ...)
    cat(HEADER)
        print( "WRT RESIDUALS:" )
        COULD_THERE_BE_PREDICTIVE_POWER_REMAINING_IN_THE_RESIDUALS( RESIDUALS )

    cat(HEADER)
        print( "WRT RESIDUALS:" )
        R = ARE_THERE_RUNLENGTH_PATTERNS_IN_THE_RESIDUALS( RESIDUALS, LENGTHMAX=3, nsl=2 )

    dev.new( X11, 11, 8 )
        YP       = predict(MODEL)
        RETVALS  = DO_REGRESSION_DIAGNOSTICS( X, Y, YP, PREDICT_VARNAME=YVARS, W_INTERCEPT=TRUE, silent=TRUE )
        GLM_FIT  = RETVALS$FIT
        # if ( do_plot ) dev.copy2pdf( file="plot_regression_basics_diagnostics.pdf" )
        dev.copy( pdf, file="plot_regression_basics_diagnostics.pdf" )
        print( "PDF file plot_regression_basis_diagnostics.pdf was generated w/ basic intuition about the regression" )
    dev.off()
    cat(HEADER)


    retvals = list( GLM_FIT=GLM_FIT, OUTLIERS=OUTLIERS )
    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
DO_LM_PLOT = function( MODEL, XY=data.frame(), XVARS=c(), YVARS=c(), debug=FALSE, ... ) {
    # layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
    op = par( mfrow=c(2,2) )
        plot( MODEL)
    par(op)
    retvals = list()
    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
DO_LM_FIT = function( FORMULA="", DATA=data.frame(), BASIC=TRUE, DO_PLOT=FALSE, METHOD="lm", silent=FALSE, debug=FALSE ) {
    # if (class(FORMULA) != "formula" ) as.formula( FORMULA )
    # METHOD = eval( parse( text=METHOD ) )
    FIT   = lm( FORMULA, data=DATA )
    TXT   = summary( FIT )
    THETA = coefficients(FIT, "coefficients") 

    if ( !silent ) {
        print( TXT )
        cat( HEADER )
    }

    RETVALS = list( FIT=FIT, THETA=THETA, FITTXT=TXT )

    return ( RETVALS )
}
# ######################################################################################################


# ######################################################################################################
GET_MSE = function( Y, YP ) {
    MSE = sum(Y - YP, na.rm=TRUE)^2/length(Y)
    return ( MSE )
}
# ######################################################################################################


# ######################################################################################################
PREDICT = function(XMAT, THETA, IDX=c() ) {
    if ( length(IDX) == 0 ) IDX = rownames(XMAT)[1:nrow(XMAT)]
    t(THETA %*% t(XMAT[IDX,]))
}
# ######################################################################################################


# ######################################################################################################
GET_VARS_FROM = function( XY, XVARS, debug=FALSE ) {
    X     = sapply( XVARS, function(x) XY[,x] )
    colnames(X) = colnames(X)
    rownames(X) = rownames(XY)
    if ( debug ) str(X)
    return ( X )
}
# ######################################################################################################


# ######################################################################################################
DO_CROSS_VALIDATION_FITTING = function( MODEL, THETA, XVARS, YVARS, XY, NFOLDS=10, debug=FALSE ) {
    X = GET_VARS_FROM( XY, XVARS )
    Y = GET_VARS_FROM( XY, YVARS )

    XP1   = cbind( rep(1,nrow(XY)), X  )
    colnames(XP1) = c("Intercept", colnames(X))
    rownames(XP1) = rownames(XY)

    M   = nrow(XP1)
    IDX = rownames(XP1)
    SUBSETS = list()
    Mcv = as.integer(M/NFOLDS)
    for ( i in 1:NFOLDS ) {
        lo =  (i-1) * Mcv + 1
        hi =  (i+0) * Mcv 
        SUBSETS[[i]] = IDX[lo:hi]
        if ( debug ) print( sprintf( "USING RANDOMIZED %5s-SAMPLE FOLDS[%3s]: %s - %s", length(SUBSETS[[i]]), i, lo, hi ) )
    }
    FITS = sapply( SUBSETS, function(x) PREDICT(XP1, THETA=THETA, IDX=x ) )
    if ( debug ) print ( FITS )

    MSE  = sapply( 1:NFOLDS, function(x) GET_MSE( Y[SUBSETS[[x]],], FITS[,x]) )
    if ( debug ) print ( sprintf( "MSE[FOLD=%s, NS=%s]=%.3g", 1:NFOLDS, rep(Mcv,NFOLDS), MSE ) )

    print ( sprintf( "AVG(MSE) [NFOLD=%s, NS=%s/FOLD]=%.3g +/- %.3g", NFOLDS, Mcv, mean(MSE, na.rm=TRUE), sd(MSE, na.rm=TRUE) ) )
    cat( HEADER )

    RETVALS = list( 'FOLDS'=SUBSETS, 'YP'=FITS, 'MSE'=MSE )
    return ( RETVALS )
}
# ######################################################################################################


# ######################################################################################################
GET_RESIDUALS = function( Y, MODEL, YP=c(), STANDARIZED=TRUE ) {
    if ( length(YP) == 0 ) 
        RESIDUALS = MODEL$residuals
    else
        RESIDUALS = YP - Y
    if ( STANDARIZED ) {
        MU = mean(RESIDUALS)
        SD = sd(RESIDUALS)
        if ( length(unique(RESIDUALS)) == 1 ) print(paste( "ZERO VARIANCE/SINGLE VALUE RESIDUALS", unique(RESIDUALS)))
        RESIDUALS = ( RESIDUALS - MU ) / SD
    }
    return ( RESIDUALS )
}
# ######################################################################################################


# ######################################################################################################
# FIXME: cooks distance and car::influencePlot alternative http://www.statmethods.net/stats/rdiagnostics.html
# ######################################################################################################
OUTLIER_ID = function( residuals, nsl=3, silent=FALSE ) {
    MU = mean(residuals, na.rm=TRUE)
    SD = sd(residuals, na.rm=TRUE)
    LO = MU - (nsl * SD)
    HI = MU + (nsl * SD)
    outliers = which( (residuals < LO) | (residuals > HI) )
    lo_outliers = which( residuals <= LO )
    hi_outliers = which( residuals >= HI )

    print( "RESIDUAL OUTLIER ANALYSIS" )
    print( sprintf( "NUMBER OF SIGMA LEVELS USED: %s SIGMAS (99.9%%)", nsl ) )
    print( sprintf( "NUMBER OF OUTLIER RESIDUALS: %s (%.2f%% of samples)", length(outliers), length(outliers)/length(residuals)*100 ))
    print( sprintf( "SAMPLES ASSOC. W/ OUTLIERS : %s", CONCAT(outliers) ) )

    retvals = list( 'OUTLIERS'=outliers, 'HIGH'=hi_outliers, 'LOW'=lo_outliers, 'NSL'=nsl, LCL=LO, UCL=HI, MU=MU )
    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
# http://www.rdatamining.com/examples/outlier-detection
# ######################################################################################################
GET_DENSITY_OUTLIERS = function( NUMERICAL_DATAFRAME=data.frame(), K_NEIGHBORS=5, TOP_N=3, DO_PLOT=FALSE) {
    require(DMwR)

    if ( nrow(NUMERICAL_DATAFRAME) == 0 )               # test case
        NUMERICAL_DATAFRAME = iris[,1:4]                # remove "Species", which is a categorical column

    OUTLIER.SCORES <- lofactor(NUMERICAL_DATAFRAME, k=K_NEIGHBORS)

    OUTLIERS <- order(OUTLIER.SCORES, decreasing=T)[1:TOP_N] # pick top 5 as outliers
    print(OUTLIERS)                                          # who are outliers

    if ( DO_PLOT ) PLOT_DENSITY_OUTLIERS( NUMERICAL_DATAFRAME=data.frame() )

    return( OUTLIERS )
}
# ######################################################################################################


# ######################################################################################################
ARE_THERE_RUNLENGTH_PATTERNS_IN_THE_RESIDUALS = function( RESIDUALS, LENGTHMAX=3, q=0.001, nsl=3 ) {
    print( 'ARE_THERE_RUNLENGTH_PATTERNS_IN_THE_RESIDUALS?' )

    MU = mean(RESIDUALS)
    SD = sd(RESIDUALS)
    UCL = MU + nsl * SD
    LCL = MU - nsl * SD
    DIFFP = ifelse( RESIDUALS > UCL,  1, 0 )
    DIFFN = ifelse( RESIDUALS < LCL, -1, 0 )
    RUNLENGTHS = DIFFP + DIFFN
    R = VECTOR(length(RESIDUALS))
    R[1] = 0
    for ( i in 2:length(RESIDUALS) ) {
        if ( RUNLENGTHS[i-1] != 0 ) {
            if ( RUNLENGTHS[i] != 0 ) {
                if ( sign(RUNLENGTHS[i]) == sign(RUNLENGTHS[i-1]) ) {
                    R[i]   = R[i-1] + 1
                    R[i-1] = 0
                } else {
                    R[i] = 1
                }
            } else {
                R[i] = 0
            }
        } else {
            R[i] = abs(RUNLENGTHS[i])
        }
    }

    W = which( R > LENGTHMAX )

    if ( length(W) > 0 )
        print ( sprintf( "(TRUE,  %s) RUNLENGTHS OF LENGTH GREATER THAN %s FOUND AT INDICES [%s]", length(W), LENGTHMAX, CONCAT( W ) ) )
    else
        print ( sprintf( "(FALSE, %s) NO RUNLENGTHS OF LENGTH GREATER THAN %s FOUND", length(W), LENGTHMAX ) )

    retvals = list( 'RUNLENGTH'=R, LENGTHMAX=LENGTHMAX, NSIGMAS=nsl, FINDINGS=W )

    return ( retvals )
}
# ######################################################################################################


# ######################################################################################################
# http://www.itl.nist.gov/div898/handbook/eda/section3/eda3311.htm 
# FIXME: there is a bug in the ucl/lcl limits at 95%
# FIXME: durbinwatson test
# ######################################################################################################
COULD_THERE_BE_PREDICTIVE_POWER_REMAINING_IN_THE_RESIDUALS = function( RESIDUALS, nsl=2 ) {
    print( "COULD_THERE_BE_PREDICTIVE_POWER_REMAINING_IN_THE_RESIDUALS?" )

    ACF = acf( RESIDUALS, lag.max=20, plot=FALSE )
    ACF = ACF$acf[2:length(ACF)]

    MU = mean(ACF, na.rm=TRUE)
    SD = sd  (ACF, na.rm=TRUE)

    UCL = MU + nsl * SD
    LCL = MU - nsl * SD

    predictive_lag_pattern = which( ACF < LCL | ACF > UCL )

    H0 = sprintf( "(%5s, %3s)", length(predictive_lag_pattern)>0, length(predictive_lag_pattern) )
    H1 = sprintf( "THERE IS A STATISTICALLY SIGNIFICANT RELATIONSHIP BETWEEN RESIDUALS ACROSS NEARBY LAGS" )

    if ( length(predictive_lag_pattern)>0 ) {
        print( sprintf( "%s: THAT IS, ACCEPT H1=[%s] %s", H0, H1, length(predictive_lag_pattern)) )
        print( ACF )
    } else
        print( sprintf( "%s: THAT IS, REJECT H1=[%s] %s", H0, H1, "" ) )

    return ( length(predictive_lag_pattern ) )
}
# ######################################################################################################



# ######################################################################################################
# compare models # http://www.statmethods.net/stats/regression.html
# ######################################################################################################
WHICH_MODEL_IS_BETTER = function( MFIT1, MFIT2, XY=data.frame() ) {
    # MFIT1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
    # MFIT2 <- lm(y ~ x1 + x2)
    anova(MFIT1, MFIT2)
}
# ######################################################################################################


# ######################################################################################################
DO_DATA_SUMMARY = function( XY ) {
    cat(HEADER)
    cat(HEADER)
    MDF_STATS(XY)
    cat(HEADER)
    print( summary( XY ) )
    cat(HEADER)
    NEWLINE(1)
    return ( XY )
}
# ######################################################################################################


# ######################################################################################################
GET_FORMULA = function( XVARS=c(), YVAR="", WRT=data.frame() ) {
    cat(HEADER)
    require( FSelector )
    FORMULA = CONCAT( sprintf( "%s +", XVARS ))
    FORMULA = substr( FORMULA, 1, nchar(FORMULA)-1 )
    FORMULA = sprintf( "%s ~ %s", YVARS, FORMULA )
    print( "FORMULA:" )
    print( FORMULA )
    # f = as.formula( FORMULA )
    f = as.simple.formula( XVARS, YVARS )
    return ( f )
}
# ######################################################################################################


# ######################################################################################################
PRINT_MODEL = function( FORMULA, MODEL, WITH_BASICS=TRUE ) {
    NEWLINE(3)
    if ( WITH_BASICS ) {
        cat(HEADER)
            print( paste( "FORMULA:" ))
            print( FORMULA )
        cat(HEADER)
            print ( "COEFFICIENTS" )
            print( coefficients(MODEL) )
        cat(HEADER)
            print( "CONFIDENCE LEVEL" )
            print( confint( MODEL ) )
    }
    cat(HEADER)
        print( summary( MODEL ) )
    NEWLINE(1)
}
# ######################################################################################################


# ######################################################################################################
DO_LINEAR_REGRESSION = function( XVARS, YVARS, XY, METHOD="lm", do_plot=TRUE ) {

    XY       = DO_DATA_SUMMARY( XY )
    FORMULA  = GET_FORMULA( XVARS, YVARS )

    LM_FIT   = DO_LM_FIT( FORMULA, DATA=XY, silent=TRUE )
    MODEL    = LM_FIT$FIT
    YP       = predict(MODEL)
    THETA    = coefficients(MODEL)
    RESIDUALS= GET_RESIDUALS( Y, MODEL )
    
    X        = GET_VARS_FROM( XY, XVARS )
    Y        = GET_VARS_FROM( XY, YVARS )
    
    RETVALS  = VERIFY_LINEAR_MODEL_ASSUMPTIONS( RESIDUALS, X, Y, MODEL, XVARS, YVARS, silent=TRUE )
    GLM_MODEL= RETVALS$GLM_FIT
    OUTLIERS = RETVALS$OUTLIERS

    dev.new( X11, 11, 8 )
        DO_LM_PLOT( MODEL )
        # if ( do_plot ) dev.copy2pdf( file="plot_regression_residuals_diagnostics.pdf" )
        dev.copy( pdf, file="plot_regression_residuals_diagnostics.pdf" )
        print( "PDF file plot_regression_residuals_diagnostics.pdf was generated w/ technical data about residuals" )
    dev.off()

    PRINT_MODEL( FORMULA, GLM_MODEL )
    PRINT_MODEL( FORMULA, MODEL )
    RETVALS  = DO_CROSS_VALIDATION_FITTING( MODEL, THETA, XVARS, YVARS, XY, NFOLDS=10)

    # LM_FIT_WO = DO_LM_FIT( FORMULA, DATA=XY[-OUTLIERS,] )
    # MODEL_WO= LM_FIT_WO$FIT
    # PRINT_MODEL( FORMULA, MODEL_WO )
    # WHICH_MODEL_IS_BETTER( MODEL, MODEL_WO, XY=XY )

    return ( MODEL )
}
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################











# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################
# ######################################################################################################

if( TEST_ENABLED ) {
    graphics.off()
    sink( 'output_linear_regression.out', split=T )

    # ###############################################################################
    opts = options( width=120, digits=3 )
    # ###############################################################################

    # ######################################################################################################
    H = BUILD_NYHOUSING_DATASET( DO_FSELECTION=FALSE, DO_FEATURE_EXPLORATION=FALSE, do_log=TRUE, PREDICT_VAR=10 )
        X = H$X
        Y = H$Y
    # ######################################################################################################
    
    library( MASS )
    data( cats )

    XY      = cats
    DATASET_NAME = "CATS"
    XVARS   = c("Bwt")
    YVARS   = c("Hwt")
    DO_LINEAR_REGRESSION( XVARS, YVARS, XY, METHOD="lm" )

    NEWLINE(20)

    M = 1000
    DATASET_NAME = "RANDOM"
    R       = function(x,d=1) { rnorm(M, x, x+d) }
    X1 = 1e-2*R(1);  X2 = 1e-3*R(2); X3 = 1e-1*R(3) 
    X4 = 2*X1 + 4*X2 + 1*X3 + 1e-3*R(1)
    XY      = as.data.frame( cbind( X1, X2, X3, X4 ))
    XY      = as.data.frame( as.matrix( scale( XY ) ) )
    XVARS   = c( "X1", "X2", "X3" )
    YVARS   = c( "X4" )
    colnames( XY) = c( XVARS, YVARS )
    DO_LINEAR_REGRESSION( XVARS, YVARS, XY, METHOD="lm" )

    NEWLINE(20)

    DATASET_NAME = "P004"
    XY      = read.csv( "P004.txt", sep="\t")
    XY      = as.data.frame( as.matrix( scale( XY ) ) )
    XVARS   = c("Protein",     "Days" )
    YVARS   = c("CurrentMilk" )
    X       = XY[,c(4:5)]
    Y       = XY[,1]
    DO_LINEAR_REGRESSION( XVARS, YVARS, XY, METHOD="lm" )

    sink()
    options( opts )
    # ######################################################################################################

}



