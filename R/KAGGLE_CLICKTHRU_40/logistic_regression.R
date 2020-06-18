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
sink( "output_logistics_regression.out", split=TRUE)
LICENSETEXT = "These R code samples (version Sep/2014), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License." 
message( LICENSETEXT )
message("")
# ######################################################################################################


# ######################################################################################################
# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
# ######################################################################################################
library(rpart)				    # Popular decision tree algorithm
library(rpart.plot)				# Enhanced tree plots
library(randomForest)           # randomForest for variable importance and error
library(vcd)                    # for visualizing association plots wrt their contigency tables
library(coefplot)
# ######################################################################################################


# ###############################################################################
source( 'utilities.R' )
OLD_CA = commandArgs()
commandArgs <- function() list(DO_TESTS=FALSE, TEST_ENABLED=FALSE, DO_DISTANCE_VALIDATION=FALSE, RUN_VALIDATE_SGD=FALSE )
VERIFY_OPTARG ( OPTARG_NAME="TEST_ENABLED",  OPTARG_VALUE=FALSE)
# ###############################################################################
source( 'datasets.R' )
source( 'fselect.R' )
source( 'decision_trees.R' )
# ############################################################################################


# ############################################################################################
# age: continuous.
# workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
# fnlwgt: continuous.
# education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
# education-num: continuous.
# marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
# occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
# relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
# race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
# sex: Female, Male.
# capital-gain: continuous.
# capital-loss: continuous.
# hours-per-week: continuous.
# native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.
# ############################################################################################
LOAD_ADULT_INCOME_CENSUS_DATASET = function() {
    COLNAMES = c( 'age', 'workclass', 'fnlwgt', 'education', 'educationnum', 
                  'maritalstatus', 'occupation', 'relationship', 'race', 
                  'sex', 'capitalgain', 'capitalloss', 'hoursperweek', 
                  'nativecountry', 'IG50K' )
    XY_TRAIN = read.csv( 'adult.data', header=FALSE, stringsAsFactors=TRUE )
    XY_TEST  = read.csv( 'adult.test', skip=1, header=FALSE, stringsAsFactors=TRUE )
    colnames(XY_TRAIN) = COLNAMES
    colnames(XY_TEST) = COLNAMES

    cat(HEADER)
    str(XY_TRAIN)
    cat(HEADER)
    summary(XY_TRAIN)
    cat(HEADER)
    cat(HEADER)
    NEWLINE(2)
    retvals = list( train=XY_TRAIN, test=XY_TEST)
    return ( retvals )
}
# ############################################################################################


# ############################################################################################
LOGIT = function( x ) {
    f = 1.0 / ( 1.0 + exp(-x))
    return ( f )
}
# ############################################################################################


# ############################################################################################
GET_Z = function( theta, x ) {
    if ( class(theta) == "numeric" || class(theta) == "integer" ) {
        z = theta * x
    } else {
        z = t(matrix(theta)) %*% x
    }
    return ( z )
}
# ############################################################################################


# ############################################################################################
H0 = function( x, theta=1.0 ) {
    z  = GET_Z( theta, x )
    yp = LOGIT( z )
    return ( yp )
}
# ############################################################################################


# ############################################################################################
LOGIT_PREDICT = function( theta=c(), x=c(), z=NA, THRESHOLD=0.5 ) {
    if ( is.na(z) ) {
        z = GET_Z( theta, x )
    }
    if ( z <= THRESHOLD ) {
        return ( 0 )
    }
    return ( 1 )
}
# ############################################################################################


# ############################################################################################
LOGIT_PREDICT_CLASS = function( X, Y=c(), model, theta, THRESHOLD=0.5 ) {
    YP_LABELS = as.matrix( sapply( X, function(x) { LOGIT_PREDICT( theta=theta, x=x, THRESHOLD=THRESHOLD ) } ) )
    return ( YP_LABELS )
}
# ############################################################################################


# ############################################################################################
LOGIT_PREDICT_PROBS = function( X, Y=c(), model, theta, THRESHOLD=0.5 ) {
    YP_PROBAS = as.matrix(sapply( X, function(x) { H0( x, theta) } ))
    YP_LABELS = LOGIT_PREDICT_CLASS( X, Y=Y, model, theta, THRESHOLD=THRESHOLD )
    YP = cbind( YP_PROBAS, YP_LABELS )
    return ( YP )
}
# ############################################################################################


# ############################################################################################
JCOST_Z = function( z, y_true ) {
    h0x = 1 /(1 + exp(-z))
    jcost = -y_true * log(h0x) + (1-y_true) * log(1-h0x)
    return (jcost)
}
# ############################################################################################


# ############################################################################################
JCOST_X = function( x_sample, y_true, theta ) {
    h0x = H0( x_sample, theta=theta )
    jcost = -y_true * log(h0x) + (1-y_true) * log(1-h0x)
    return (jcost)
}
# ############################################################################################


# ############################################################################################
# INCORRECT:BROKEN
# ############################################################################################
ITERATIVE_BINARY_THRESHOLD_EVALUATOR = function( DT_MODEL, X_TRAIN, Y_TRAIN, BETA=0.5, THRESHOLDS=c( 0.25, 0.333, 0.5, 0.667, 0.75 ) ) {
    FMAX = 0
    FMAX_THRESHOLD = NA
    FMAX_CMAT = NA

    YP = predict(DT_MODEL, X_TRAIN)

    for ( THRESHOLD in THRESHOLDS ) {
        YPP = as.data.frame( ifelse( YP <= THRESHOLD, 0, 1 ) )
        DT_CMAT = table(Y_TRAIN[[1]], YPP[[1]])
        F_RATIO = GET_F_RATIO( DT_CMAT, beta=BETA )
        if ( F_RATIO > FMAX ) {
            FMAX = F_RATIO
            FMAX_THRESHOLD = THRESHOLD
            FMAX_CMAT = DT_CMAT
        }
        print ( sprintf( "THRESHOLD=%.3f, FRATIO=%.3f\nCONFUSION TABLE:\n", THRESHOLD, F_RATIO ) )
        print ( DT_CMAT )
        cat ( HEADER )
    }
    cat( HEADER )
    RETVALS = list( F_RATIO=FMAX, CMAT=FMAX_CMAT, THRESHOLD=FMAX_THRESHOLD )
    return ( FMAX_THRESHOLD )
}
# ############################################################################################


# ############################################################################################
PLOT_METRICS = function( METRICS, THETA_MAT ) {
    par(mfrow=c(3,3))
    # plot 1
    plot( METRICS[,1], t='l', main="JCOST ACROSS FOLDS" )
    text( 1:nrow(METRICS), METRICS[,1], round(METRICS[,7]), col="brown", cex=0.8 )
    # plot 2
    hist(JCOST, main="HIST OF JCOST FOR LAST FOLD")
    # plot 3
    plot( METRICS[,2], t='l', main="F1 RATIO ACROSS FOLDS" )
    # plot 4
    plot( METRICS[,3], t='l', col="brown", pch=24, cex=0.8, main="FPR ACROSS FOLDS" )
    # plot 5
    plot( METRICS[,4], t='l', col="brown", pch=24, cex=0.8, main="FNR ACROSS FOLDS" )
    # plot 6
    plot( METRICS[,3], METRICS[,4], col="blue", pch=22, cex=1.2, main="FPR vs FNR ACROSS FOLDS" )
    # plot 7
    plot( METRICS[,5], t='l', main="ACCURACY ACROSS FOLDS" )
    # plot 8
    plot( METRICS[,6], t='l', main="BER ACROSS FOLDS" )
    # plot 9
    boxplot( t(TMAT), las=1, cex=0.7, cex.axis=0.7, main="COEFFICIENTS ACROSS FOLDS" )
}
# ############################################################################################

# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################




















# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
RETVALS = LOAD_ADULT_INCOME_CENSUS_DATASET()
    COLNAMES = c( 'age', 'workclass', 'fnlwgt', 'education', 'educationnum', 'maritalstatus', 'occupation', 'relationship', 'race', 'sex', 'capitalgain', 'capitalloss', 'hoursperweek', 'nativecountry', 'IG50K' )
    FORMULA0 = 'IG50K ~ age + workclass + fnlwgt + education + educationnum + maritalstatus + occupation + relationship + race + sex + capitalgain + hoursperweek + nativecountry'

    ORIG_XTRAIN = RETVALS$train
    ORIG_XTEST  = RETVALS$test
    M = nrow(ORIG_XTRAIN)
    N = ncol(ORIG_XTRAIN)

    PREDICT_VAR = "IG50K"
    PREDICT_COL = 15
    YCOL = "IG50K"
    CLASS0 = " <=50K."
    CLASS1 = " >50K."

    ORIG_YTRAIN = SLICE_DATAFRAME(ORIG_XTRAIN,PREDICT_COL)
# ############################################################################################

# ############################################################################################
# input conditioning, scaling as appropriate 
# ############################################################################################
XY = rbind( ORIG_XTRAIN, ORIG_XTEST )
COMPLETE_SAMPLES = complete.cases(XY)
XY = XY[COMPLETE_SAMPLES,]
COLNAMES = colnames(XY)
COLS_DATATYPES = mapply( class, XY)
COLS_FACTORS   = grep( 'factor',  COLS_DATATYPES )
COLS_INTEGERS  = grep( 'integer', COLS_DATATYPES )
COLS_NUMERICS  = grep( 'numeric', COLS_DATATYPES )
COLS_UNKNOWNS  = c(1:N, -COLS_FACTORS, -COLS_INTEGERS, -COLS_NUMERICS )
# #####################################################################################
if ( TRUE ) {
    print ( HEADER )
    print ( paste( "DATASET:COLUMN TYPE", COLNAMES, COLS_DATATYPES) )
    print ( paste( "CATEGORICAL COLUMNS", COLS_FACTORS ) )
    print ( paste( "NUMERICAL COLUMNS",   COLS_NUMERICS ) )
    print ( paste( "INTEGER COLUMNS",     COLS_INTEGERS ) )
    print ( HEADER )
}
# #####################################################################################
#UCI suggest cuts for continuous data
#age cut into Young(0-25), Middle(26-45), Senior(46-65), Old(66)
#hpw cut into Part(0-25),  Full(25-40), Over(40-60), TooMuch(60)
#cpglcut into None(0),  Low(<median!0), High(>mac40-60), TooMuch(60)
# #####################################################################################
XYr = XY[,setdiff(COLS_NUMERICS,PREDICT_COL)]
XYf = XY[,setdiff(COLS_FACTORS, PREDICT_COL)]
XYi = XY[,setdiff(COLS_INTEGERS,PREDICT_COL)]
# #####################################################################################
XYrnew = scale( XYr, center=TRUE, scale=TRUE )
XYrnew_centers = attr(XYrnew, "scaled:center" )
XYrnew_scales  = attr(XYrnew, "scaled:scale" )
colnames(XYrnew) = colnames(XYr)
rownames(XYrnew) = rownames(XYr)
# #####################################################################################
XYinew = scale( XYi, center=TRUE, scale=TRUE)
XYinew_centers = attr(XYinew, "scaled:center" )
XYinew_scales  = attr(XYinew, "scaled:scale" )
colnames(XYinew) = colnames(XYi)
rownames(XYinew) = rownames(XYi)
# #####################################################################################
XXi = data.frame( matrix( mapply( RECODE, XYi, xnbins=4), nrow=nrow(XYi)) )
colnames(XXi) = colnames(XYi)
rownames(XXi) = rownames(XYi)
# #####################################################################################
nativecountry = XYf[,"nativecountry"]
levels(nativecountry) <- list(US=" United-States", OVERSEAS=setdiff(levels(nativecountry)," United-States"))
XYf[,"nativecountry"] = nativecountry
# #####################################################################################
IG50K = as.data.frame(XY[,PREDICT_VAR])
colnames(IG50K) = PREDICT_VAR
rownames(IG50K) = rownames(XY)
# #####################################################################################
DATA = cbind( XYrnew, XYinew, XYf, IG50K )
DATA = cbind( XYr, XYi, XYf, IG50K )
ORIG_XTRAIN = DATA[1:M,]
ORIG_XTEST  = DATA[(M+1):nrow(DATA),]
# #####################################################################################


# ############################################################################################
# individual feature selection
# ############################################################################################
FSELECT_RETVALS = DO_GENERAL_SUBSET_SELECTION( ORIG_XTRAIN, dfname="AdultIncomes, C:F",
                                   using_approach=chi.squared, approach_ppname="chi.squared", 
                                   rtypes="SUBSAMPLE|COMPLETECASES",
                                   target_var="IG50K", top=10, nmax=1024, cmax=0.8 )
FSELECT_MODEL = FSELECT_RETVALS[[1]] # IG50K ~ educationnum + age + Srelationship + maritalstatus + occupation + education
FSELECT_XVARS = FSELECT_RETVALS[[3]]
FSELECT_YVAR  = FSELECT_RETVALS[[2]]
FSELECT_ATTRIMP = FSELECT_RETVALS[[4]]
# ############################################################################################

# ############################################################################################
# insight about potential non-linear and boolean interaction feature effects via decision trees 
# ############################################################################################
DT_MODEL = FIT_DECISION_TREE( ORIG_XTRAIN, SLICE_DATAFRAME(X_TRAIN,15), FORMULA=FORMULA0, DO_PRUNING=TRUE, minbucket=20 )
print ( DT_MODEL )
prp(DT_MODEL)
# ############################################################################################

# ############################################################################################
# feature exploration
# ############################################################################################
pdf( 'plot_logistic_regression_feature_exploration.pdf', 12, 9 )
par(mfrow=c(2,2))
NF=4
for ( i in 1:length(FSELECT_XVARS) ) {
    XVAR = FSELECT_XVARS[i]
    print(XVAR)
    cat(HEADER)
    XX = ORIG_XTRAIN[,XVAR]
    YY = ORIG_XTRAIN[,PREDICT_VAR]
    print(summary(XX))
    if ( class(XX) == "factor" ) {
        plot( XX, main=paste(XVAR) )
        plot( table( XX, YY), color=20:40 )
    } else {
        hist( XX, main=paste(XVAR) )
        plot( table( RECODE(XX, NF), YY), color=20:40 )
    }
}
dev.off()
graphics.off()
# ############################################################################################


# ############################################################################################
# exploratory model specifications
# ############################################################################################
FORMULA0 = 'IG50K ~ . + age*race + sex*maritalstatus + age*occupation'
FORMULA2 = 'IG50K ~ educationnum + (age+1) + relationship + maritalstatus + occupation'
FORMULA3 = 'IG50K ~ educationnum + (age+1) + relationship + maritalstatus'
FORMULA4 = 'IG50K ~ educationnum + (age+1) + relationship'
FORMULAS = list( FORMULA2, FORMULA3, FORMULA4 )
# ############################################################################################


# ############################################################################################
# investigation of class weights
# ############################################################################################


# ############################################################################################
# decision boundary thresholds
# ############################################################################################
THRESHOLD = 0.5


# ############################################################################################
# comparative evaluation of models
# ############################################################################################
    cat(HEADER)
    print( summary(ORIG_XTRAIN) )
    cat(HEADER)

    ORIG_XTRAIN[,YCOL] = as.integer( ORIG_XTRAIN[,YCOL] ) - 1
    Y_TRUE = ORIG_XTRAIN[,YCOL]

    model2 = glm( FORMULA2, data=ORIG_XTRAIN, family=binomial(link = "logit"))
    model3 = glm( FORMULA3, data=ORIG_XTRAIN, family=binomial(link = "logit"))
    model4 = glm( FORMULA4, data=ORIG_XTRAIN, family=binomial(link = "logit"))
    MODELS = list( model2, model3, model4 )

    for ( i in 1:length(MODELS)) {
        LAST_MODEL = MODELS[[i]]
        if ( length(MODELS) > 1 ) {
            print ( paste( "MODEL", i, sep="" ))
            print ( paste( "FORMULA", FORMULAS[[i]] ))
            cat (HEADER )
            ANOVA = anova( MODELS[[1]], LAST_MODEL )
            cat (HEADER )
            print ( ANOVA )
            cat (HEADER )
        }
    }
    #multiplot( model2, model3, model4 )
    cat (HEADER)


# ############################################################################################
# model performance metrics
# ############################################################################################
    for ( i in 1:length(MODELS)) {
        LAST_MODEL = MODELS[[i]]
        ANOVA_TEST = anova( LAST_MODEL, test="Chisq" )

        AIC_VAL = AIC(LAST_MODEL)
        ZVALS = predict(LAST_MODEL, ORIG_XTRAIN)
        JCOST = JCOST_Z( ZVALS, Y_TRUE )

        YP_PROBS = LOGIT(ZVALS)
        YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
        CMAT = table( Y_TRUE, YP_CLASS )

        cat (HEADER)
        print( summary(LAST_MODEL) )
        cat (HEADER )
        print( ANOVA_TEST )
        cat (HEADER )
        print( AIC_VAL )
        cat (HEADER )
        print( CMAT )
        cat(HEADER )
        cat(HEADER )
    }


# ############################################################################################
# Base model selection
# ############################################################################################
ANOVA = do.call("anova", as.list(MODELS))
print ( ANOVA )

BASE_FORMULA = FORMULA4
BASE_MODEL = glm( BASE_FORMULA, data=ORIG_XTRAIN, family=binomial(link = "logit"))
Y_TRUE = ORIG_XTRAIN[,YCOL]
ANOVA_TEST = anova( BASE_MODEL, test="Chisq" )
AIC_VAL = AIC(BASE_MODEL)
ZVALS = predict(BASE_MODEL, ORIG_XTRAIN)
JCOST = JCOST_Z( ZVALS, Y_TRUE )
YP_PROBS = LOGIT( ZVALS )
YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
CMAT = table( Y_TRUE, YP_CLASS )


# ############################################################################################
# summary of failed to predict samples wrt class/attributes and decision tree for those
# ############################################################################################
WHICH_ONES = which( YP_CLASS != Y_TRUE )
MISSED_SAMPLES = ORIG_XTRAIN[WHICH_ONES,]
print( summary( MISSED_SAMPLES ) )
cat(HEADER)

DECISION_TREE_INSIGHT = FALSE
if ( DECISION_TREE_INSIGHT ) {
    GENERAL_FORMULA = sprintf( "%s ~ .", PREDICT_VAR )
    MISSING_DT_MODEL = FIT_DECISION_TREE( MISSED_SAMPLES, SLICE_DATAFRAME(MISSED_SAMPLES,15), FORMULA=GENERAL_FORMULA, DO_PRUNING=TRUE, minbucket=20 )
    p = prp(MISSING_DT_MODEL)
    print ( MISSING_DT_MODEL )
    cat(HEADER)
}


# ############################################################################################
# incremental model refinements if any (add or step) 
# http://data.princeton.eu/R/glms.html
# ############################################################################################
ADD_MR = add1( BASE_MODEL, ~.^2, test="Chisq" )
print( ADD_MR )
cat(HEADER)


# ############################################################################################
# decision boundary thresholds
# ############################################################################################
ITERATIVE_BINARY_THRESHOLD_EVALUATOR( BASE_MODEL, ORIG_XTRAIN, ORIG_YTRAIN, BETA=1.0, THRESHOLDS=c( 0.333, 0.5, 0.667) )


# ############################################################################################
# bootstrap of model
# ############################################################################################
ORIG_ORDERING = rownames(ORIG_XTRAIN)
LEAVE_OUT = c()
NTRIALS = 25

FULL_BOOTSTRAP = FALSE
SUBSAMPLING_EXTENT = as.integer(0.80 * M)
if ( FULL_BOOSTRAP ) SUBSAMPLING_EXTENT = as.integer(1.00 * M)

START = TRUE
METRICS = MATRIX( NTRIALS, 7 )
for ( j in 1:NTRIALS ) {
    BOOT_ORDER = sample(ORIG_ORDERING, SUBSAMPLING_EXTENT, replace=TRUE)
    if (length(LEAVE_OUT)!=0) {
        X_TRAIN = ORIG_XTRAIN[BOOT_ORDER,-LEAVE_OUT]
    } else {
        X_TRAIN = ORIG_XTRAIN[BOOT_ORDER,]
    }
    Y_TRUE = as.matrix(ORIG_XTRAIN[BOOT_ORDER,PREDICT_VAR])

    BOOTBASE_MODEL = glm( BASE_FORMULA, data=ORIG_XTRAIN, family=binomial(link = "logit"))
    THETA = BOOTBASE_MODEL$coefficients
    if ( START ) {
        THETAS = as.data.frame( THETA )
        START = FALSE
    } else { 
        THETAS = cbind(THETAS, as.data.frame(THETA))
    }

    AIC_VAL = AIC(BOOTBASE_MODEL)
    ZVALS = predict(BOOTBASE_MODEL, X_TRAIN)
    JCOST = JCOST_Z( ZVALS, Y_TRUE )
    YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
    CV_CMAT = table( Y_TRUE, YP_CLASS )

    METRICS[j,1] = sum(JCOST, na.rm=TRUE)
    METRICS[j,2] = GET_F_RATIO( CV_CMAT)
    METRICS[j,3] = GET_FPR( CV_CMAT)
    METRICS[j,4] = GET_FNR( CV_CMAT)
    METRICS[j,5] = GET_ACC( CV_CMAT)
    METRICS[j,6] = ( GET_TPR( CV_CMAT) + GET_FPR( CV_CMAT ) ) / 2.0
    METRICS[j,7] = AIC_VAL
}


# ############################################################################################
# a look at the CV performance
# a look at the theta coefficients
# ############################################################################################
TMAT = as.matrix( THETAS )
colnames(TMAT) = paste( "BOOTSTRAP", 1:NTRIALS, sep="")
THETAS_COEFF_MEANS = apply( TMAT, 1, mean )
THETAS_COEFF_SDEV  = apply( TMAT, 1, sd )

PLOT_METRICS( METRICS, THETA_MAT )



# ############################################################################################
# performance on the test data
# ############################################################################################
ORIG_ORDERING = rownames(ORIG_XTEST)
LEAVE_OUT = c()
NTRIALS = 1

START = TRUE
METRICS = MATRIX( NTRIALS, 7 )
colnames(METRICS) = c( "JCOST", "F_RATIO", "FPR", "FNR", "ACCURACY", "BER", "MSE" )
for ( j in 1:NTRIALS ) {
    BOOT_ORDER = ORIG_ORDERING
    if (length(LEAVE_OUT)!=0) {
        X_TEST = ORIG_XTEST[BOOT_ORDER,-LEAVE_OUT]
    } else {
        X_TEST = ORIG_XTEST[BOOT_ORDER,]
    }
    Y_TRUE = as.matrix(as.factor(ORIG_XTEST[BOOT_ORDER,PREDICT_VAR]))
    Y_TRUE = ifelse( as.character(Y_TRUE) == " <=50K.", 0, 1 )

    ZVALS = predict(BOOTBASE_MODEL, X_TEST)
    JCOST = JCOST_Z( ZVALS, Y_TRUE )
    YP_CLASS = sapply( ZVALS, function(z) { ifelse(z <= THRESHOLD, 0, 1) } )
    CV_CMAT = table( Y_TRUE, YP_CLASS )

    METRICS[j,1] = sum(JCOST, na.rm=TRUE)
    METRICS[j,2] = GET_F_RATIO( CV_CMAT)
    METRICS[j,3] = GET_FPR( CV_CMAT)
    METRICS[j,4] = GET_FNR( CV_CMAT)
    METRICS[j,5] = GET_ACC( CV_CMAT)
    METRICS[j,6] = ( GET_TPR( CV_CMAT) + GET_FPR( CV_CMAT ) ) / 2.0
    METRICS[j,7] = sum( abs(YP_CLASS - Y_TRUE) )/ length(YP_CLASS)
}

cat(HEADER)
print( summary( ORIG_XTEST ) )
cat(HEADER)
print( METRICS )
cat(HEADER)
print( CV_CMAT )
cat(HEADER)

# ############################################################################################
sink()
