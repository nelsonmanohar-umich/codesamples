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



# #####################################################################################
# FOR CONVENIENCE in selecting proper feature selection algorithm with respect to the
# nature of feature mix in a dataset, this peripherally related list of available 
# feature selection/ranking algorithms was obtained from 
#               https://github.com/need47/fselector/blob/master/README.md
# #####################################################################################
# algorithm                        shortcut    algo_type  applicability  feature_type
# --------------------------------------------------------------------------------------------------
# Accuracy                         Acc         weighting  multi-class    discrete
# AccuracyBalanced                 Acc2        weighting  multi-class    discrete
# BiNormalSeparation               BNS         weighting  multi-class    discrete
# CFS_d                            CFS_d       searching  multi-class    discrete
# ChiSquaredTest                   CHI         weighting  multi-class    discrete
# CorrelationCoefficient           CC          weighting  multi-class    discrete
# DocumentFrequency                DF          weighting  multi-class    discrete
# F1Measure                        F1          weighting  multi-class    discrete
# FishersExactTest                 FET         weighting  multi-class    discrete
# FastCorrelationBasedFilter       FCBF        searching  multi-class    discrete
# GiniIndex                        GI          weighting  multi-class    discrete
# GMean                            GM          weighting  multi-class    discrete
# GSSCoefficient                   GSS         weighting  multi-class    discrete
# InformationGain                  IG          weighting  multi-class    discrete
# INTERACT                         INTERACT    searching  multi-class    discrete
# JMeasure                         JM          weighting  multi-class    discrete
# KLDivergence                     KLD         weighting  multi-class    discrete
# MatthewsCorrelationCoefficient   MCC, PHI    weighting  multi-class    discrete
# McNemarsTest                     MNT         weighting  multi-class    discrete
# OddsRatio                        OR          weighting  multi-class    discrete
# OddsRatioNumerator               ORN         weighting  multi-class    discrete
# PhiCoefficient                   PHI         weighting  multi-class    discrete
# Power                            Power       weighting  multi-class    discrete
# Precision                        Precision   weighting  multi-class    discrete
# ProbabilityRatio                 PR          weighting  multi-class    discrete
# Recall                           Recall      weighting  multi-class    discrete
# Relief_d                         Relief_d    weighting  two-class      discrete
# ReliefF_d                        ReliefF_d   weighting  multi-class    discrete
# Sensitivity                      SN, Recall  weighting  multi-class    discrete
# Specificity                      SP          weighting  multi-class    discrete
# SymmetricalUncertainty           SU          weighting  multi-class    discrete
# BetweenWithinClassesSumOfSquare  BSS_WSS     weighting  multi-class    continuous
# CFS_c                            CFS_c       searching  multi-class    continuous
# FTest                            FT          weighting  multi-class    continuous
# KS_CCBF                          KS_CCBF     searching  multi-class    continuous
# KSTest                           KST         weighting  two-class      continuous
# PMetric                          PM          weighting  two-class      continuous
# Relief_c                         Relief_c    weighting  two-class      continuous
# ReliefF_c                        ReliefF_c   weighting  multi-class    continuous
# TScore                           TS          weighting  two-class      continuous
# WilcoxonRankSum                  WRS         weighting  two-class      continuous
# LasVegasFilter                   LVF         searching  multi-class    discrete, continuous, mixed
# LasVegasIncremental              LVI         searching  multi-class    discrete, continuous, mixed
# Random                           Rand        weighting  multi-class    discrete, continuous, mixed
# RandomSubset                     RandS       searching  multi-class    discrete, continuous, mixed
# #####################################################################################


# #####################################################################################
library(rpart)
library(mlbench)
library(FSelector)
# #####################################################################################


# #####################################################################################
source( 'utilities.R' )
# #####################################################################################


# #####################################################################################
VERIFY_OPTARG ( OPTARG_NAME="TEST_ENABLED", OPTARG_VALUE=TRUE )
VERIFY_OPTARG ( OPTARG_NAME="DOUBLECHECK",  OPTARG_VALUE=FALSE )
VERIFY_OPTARG ( OPTARG_NAME="APPLY_EXTRA_METHODS",  OPTARG_VALUE=FALSE )
# #####################################################################################


# #####################################################################################
# ordered dataframe listing for which relevant information was containing in the row.names
# #####################################################################################
PRINT_WEIGHTS = function( weights ) {
    ordered_weights = cbind(weights,row.names(weights))[order(weights, decreasing=TRUE),]
    print( ordered_weights )
    return ( ordered_weights )
}
# #####################################################################################


# #####################################################################################
# APPLIES DATA SET REDUCTIONS BASED ON FEATURE SELECTION, SUBSAMPLING, MISSING VALUES, AND CORRELATION
# #####################################################################################
DO_GENERAL_SUBSET_SELECTION = function( mydf, dfname="C:N", 
                                              using_approach=rank.correlation, 
                                              approach_ppname="rank.correlation",
                                              rtypes="SUBSAMPLE|COMPLETECASES|CORRELATION", 
                                              target_var="Y", 
                                              top=3, 
                                              nmax=1024, 
                                              cmax=0.9,
                                              cutoff_approach=cutoff.biggest.diff,
                                              refine=TRUE, 
                                              percentile_threshold=0.6, 
                                              debug=FALSE ) {
    EXPLAIN_DATASET( mydf, dfname )
    ntop = min(top, ncol(mydf))
    target_predictor_formula = as.simple.formula( ".", target_var )
    subsampled_mydf = DO_DIMENSIONALITY_REDUCTION( mydf, target_var, rtypes, nmax, cmax )
    if ( debug ) str( subsampled_mydf )
    print( target_predictor_formula )
    weights <- using_approach( target_predictor_formula, subsampled_mydf )
    PRINT_WEIGHTS( weights )
    subset <- cutoff.k(weights, ntop)
    # subset <- cutoff_approach(weights)
    ntop_constrained = sum(weights>quantile(c(weights)[[1]],percentile_threshold)[[1]])
    if ( refine && ntop > ntop_constrained ) {
        print ( paste( "WARNING: apparently, too many variables selected, rerunning to reduce from", ntop, "to", ntop_constrained ))
        subset <- cutoff.k(weights, ntop_constrained)
    }
    resulting_f <- as.simple.formula(subset, target_var )
    GET_TIMESTAMP( sprintf("subset <- cutoff.k(weights <- %s(%s, d), %s)", approach_ppname, as.character(resulting_f), ntop ) )
    print(resulting_f)
    PRINT_HEADER()
    results = list( resulting_f, target_var, subset, weights )
    return ( results )
}
# #####################################################################################


# #####################################################################################
# Illustrative examples taken from the FSelector.R package on how to plug in their 
# feature selection variable importance subset functions to what data types (factors, 
# numeric) and problems (classification, regression). 
#
# Intended for cross comparision of performance with respect to reference datasets.
# Larger datasets are to be handled with implicit subsampling within the functions here.
# This is being added now.
#
# NOTE: Feature selection here is intended as DIMENSIONALITY REDUCTION with respect 
# to dataset subsample.
# #####################################################################################





# #####################################################################################
if ( APPLY_EXTRA_METHODS ) {
	# #####################################################################################
	data(iris)
	    evaluator <- function(subset) {
	        #k-fold cross validation
	        k <- 5
	        splits <- runif(nrow(iris))
	        results = sapply(1:k, function(i) {
	                test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
	                train.idx <- !test.idx
	                test <- iris[test.idx, , drop=FALSE]
	                train <- iris[train.idx, , drop=FALSE]
	                tree <- rpart(as.simple.formula(subset, "Species"), train)
	                error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
	                return(1 - error.rate)
	                })
	        print(subset)
	        print(mean(results))
	        return(mean(results))
	    }
	    EXPLAIN_DATASET( iris, "iris" )
	    subset <- best.first.search(names(iris)[-5], evaluator)
	    f <- as.simple.formula(subset, "Species")
	    
	    GET_TIMESTAMP( "subset <- best.first.search(names(iris)[-5], evaluator)")
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
	
	
	# #####################################################################################
	data(iris)
	    EXPLAIN_DATASET( iris, "iris" )
	    subset <- cfs(Species~., iris)
	    f <- as.simple.formula(subset, "Species")
	    
	    GET_TIMESTAMP( "subset <- cfs(Species~., iris)" )
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
	
	
	# #####################################################################################
	data(iris)
	    EXPLAIN_DATASET(iris, "iris" )
	    evaluator <- function(subset) {
	            #k-fold cross validation
	            k <- 5
	            splits <- runif(nrow(iris))
	            results = sapply(1:k, function(i) {
	            test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
	            train.idx <- !test.idx
	            test <- iris[test.idx, , drop=FALSE]
	            train <- iris[train.idx, , drop=FALSE]
	            tree <- rpart(as.simple.formula(subset, "Species"), train)
	            error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
	            return(1 - error.rate)
	        })
	        print(subset)
	        print(mean(results))
	        return(mean(results))
	    }
	    subset <- forward.search(names(iris)[-5], evaluator)
	    f <- as.simple.formula(subset, "Species")
	
	    GET_TIMESTAMP( "subset <- forward.search(names(iris)[-5], evaluator)" )
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
	
	
	# #####################################################################################
	data(iris)
	    EXPLAIN_DATASET(iris, "iris" )
	    evaluator <- function(subset) {
	            #k-fold cross validation
	            k <- 5
	            splits <- runif(nrow(iris))
	            results = sapply(1:k, function(i) {
	            test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
	            train.idx <- !test.idx
	            test <- iris[test.idx, , drop=FALSE]
	            train <- iris[train.idx, , drop=FALSE]
	            tree <- rpart(as.simple.formula(subset, "Species"), train)
	            error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
	            return(1 - error.rate)
	        })
	        print(subset)
	        print(mean(results))
	        return(mean(results))
	    }
	    subset <- hill.climbing.search(names(iris)[-5], evaluator)
	    f <- as.simple.formula(subset, "Species")
	
	    GET_TIMESTAMP( "subset <- hill.climbing.search(names(iris)[-5], evaluator)" )
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
	
	
	# #####################################################################################
	data(iris)
	    EXPLAIN_DATASET(iris, "iris")
	    weights <- relief(Species~., iris, neighbours.count = 5, sample.size = 20)
	    print(weights)
	    subset <- cutoff.k(weights, 2)
	    f <- as.simple.formula(subset, "Species")
	
	    GET_TIMESTAMP( "subset <- cutoff.k(weights <- relief(Species~., iris, neighbours.count = 5, sample.size = 20), 2) ")
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
	
	
	# #####################################################################################
	data(HouseVotes84)
	    EXPLAIN_DATASET(HouseVotes84, "HouseVotes84" )
	    subset <- consistency(Class~., HouseVotes84)
	    f <- as.simple.formula(subset, "Class")
	
	    GET_TIMESTAMP( "subset <- consistency(Class~., HouseVotes84)")
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
}
# #####################################################################################
	








# #####################################################################################
if ( DOUBLECHECK ) {
    # #####################################################################################
    data(iris)
        EXPLAIN_DATASET(iris, "iris" )
        weights <- symmetrical.uncertainty(Species~., iris)
        print(weights)
        subset <- cutoff.biggest.diff(weights)
        f <- as.simple.formula(subset, "Species")
    
        GET_TIMESTAMP( "subset <- cutoff.biggest.diff(weights <- symmetrical.uncertainty(Species~., iris)) " )
        print(f)
        PRINT_HEADER()
    # #####################################################################################


    # #####################################################################################
    data(HouseVotes84)
        EXPLAIN_DATASET(HouseVotes84, "HouseVotes84" )
        weights <- oneR(Class~., HouseVotes84)
        print(weights)
        subset <- cutoff.k(weights, 5)
        f <- as.simple.formula(subset, "Class")
    
        GET_TIMESTAMP( "subset <- cutoff.k(weights <- oneR(Class~., HouseVotes84), 5) ")
        print(f)
        PRINT_HEADER()
    # #####################################################################################


	# #####################################################################################
	data(iris)
	    EXPLAIN_DATASET(iris, "iris" )
	    weights <- information.gain(Species~., iris)
	    print(weights)
	    subset <- cutoff.k(weights, 1)
	    f <- as.simple.formula(subset, "Species")
	
	    GET_TIMESTAMP( "subset <- cutoff.k(weights <- information.gain(Species~., iris), 1)" )
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
	
	
	# #####################################################################################
	data(iris)
	    EXPLAIN_DATASET(iris, "iris" )
	    weights <- information.gain(Species~., iris)
	    print(weights)
	    subset <- cutoff.k.percent(weights, 0.75)
	    f <- as.simple.formula(subset, "Species")
	
	    GET_TIMESTAMP( "subset <- cutoff.k.percent(weights <- information.gain(Species~., iris), 0.75)" )
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
	
	
	# #####################################################################################
	data(iris)
	    EXPLAIN_DATASET(iris, "iris" )
	    weights <- information.gain(Species~., iris)
	    print(weights)
	    subset <- cutoff.k(weights, 2)
	    f <- as.simple.formula(subset, "Species")
	
	    GET_TIMESTAMP( "subset <- cutoff.k(weights <- information.gain(Species~., iris), 2)" )
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
	
	
	# #####################################################################################
	data(iris)
	    EXPLAIN_DATASET(iris, "iris" )
	    weights <- gain.ratio(Species~., iris)
	    print(weights)
	    subset <- cutoff.k(weights, 2)
	    f <- as.simple.formula(subset, "Species")
	
	    GET_TIMESTAMP( "subset <- cutoff.k(weights <- gain.ratio(Species~., iris), 2)" )
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
	
	
	# #####################################################################################
	data(HouseVotes84)
	    EXPLAIN_DATASET(HouseVotes84, "HouseVotes84" )
	    weights <- chi.squared(Class~., HouseVotes84)
	    print(weights)
	    subset <- cutoff.k(weights, 5)
	    f <- as.simple.formula(subset, "Class")
	
	    GET_TIMESTAMP( "subset <- cutoff.k(weights <- chi.squared(Class~., HouseVotes84), 5)" )
	    print(f)
	    PRINT_HEADER()
	# #####################################################################################
}
# #####################################################################################








# #####################################################################################
# #####################################################################################
SAMPLE = function( x, n=100 ) {
    sample( x, n )
}
# #####################################################################################
        
        

# #####################################################################################
# #####################################################################################
DO_PCA_PLOT = function( xX, ... ) {
    PCA = DO_PCA( xX )
    Z   = PCA$Z
    txt = rownames(xX)
    plot( Z[,1], Z[,2], pch="+", cex=0.8, ... )
    text( Z[,1], Z[,2], txt, cex=0.8 )
    return ( PCA )
}
# #####################################################################################



# #####################################################################################
# #####################################################################################
DO_PCA = function( x, k=0, vargoal=0.995, ntries=5, nmax=0, do_scale=TRUE, optimal=TRUE, silent=FALSE, debug=TRUE ) {
    if ( class(x) != "data.frame" ) {
        x = data.frame(x) 
        rownames(x) = 1:nrow(x)
    }

    if ( nmax == 0 ) nmax = nrow(x)

    k_opt = c()
    pca = list()
    for ( i in 1:ntries) {
        idx_set = sample( rownames(x), nmax )
        pca_analysis = princomp( x, subset=idx_set, scale=do_scale )
        pca[[i]] = pca_analysis
        txt = summary( pca_analysis )
        cummulative_variance_retained = cumsum( pca_analysis$sdev^2 / 
                                                sum(pca_analysis$sdev^2) )
        which_k = which( round(cummulative_variance_retained,5) > vargoal )[1]
        k_opt = append( k_opt, which_k ) 
        if ( !silent ) {
            cat("\n")
            print ("----------------------------")
            print ( i )
            print ( cummulative_variance_retained )
            print ("----------------------------")
            cat("\n")
            cat("\n")
        }
    }

    ## The signs of the columns are arbitrary
    which_k_to_use = median(k_opt) 
    opt_pca_idx = which(k_opt == which_k_to_use)[1] 
    opt_pca = pca[[ opt_pca_idx ]]
    loadings(opt_pca)   # note that blank entries are small but not zero

    # plot(opt_pca)       # shows a screeplot.
    # biplot(opt_pca)

    Ureduce = opt_pca$loadings[,1:which_k_to_use]

    pca_best_k = c( "ntries"=i, 
                    "min_k"=min(k_opt), 
                    "avg_k"=mean(k_opt), 
                    "med_k"=median(k_opt), 
                    "std_k"=sd(k_opt), 
                    "q"=quantile(k_opt, c(0.75)), 
                    "max_k"=max(k_opt))
    if ( debug ) print ( pca_best_k )


    Z = t(t(Ureduce) %*% t(as.matrix(x)))

    #START = TRUE
    #xp = apply(x,2,as.numeric)
    #xp = matrix(xp, nrow(x))
    #UT = t(Ureduce)
    #for (i in 1:nrow(x) ) {
        #z = UT %*% xp[i,]
        #if ( START ) {
            #Z = c(z)
            #colnames(Z) = colnames(z)
            #START = FALSE
        #}
        #else
            #Z = rbind(Z, c(z))
    #}

    rownames(Z) = rownames(x)

    retvals = list ("k"=which_k_to_use, 
                    "Ureduce"=Ureduce, 
                    "Z"=Z, 
                    "cumvar"=cummulative_variance_retained,
                    "k_search"=pca_best_k, 
                    "vargoal"=vargoal )
    return (retvals) 
}
# #####################################################################################







if ( TEST_ENABLED ) {
    sink( 'output.fselect.out', split=TRUE )
    # #####################################################################################
    GET_TIMESTAMP( "START" )
    # #####################################################################################
    NEWLINE(100)
    # #####################################################################################
    
    
    
    
    
    # #####################################################################################
    NEWLINE(20)
    data(BostonHousing) # mydf=BostonHousing[-4] # only numeric variables
    #####################################################################################
    F10 = DO_GENERAL_SUBSET_SELECTION( BostonHousing[-4], dfname="BostonHousing, R:N",
                                        using_approach=rank.correlation, approach_ppname="rank.correlation", 
                                        rtypes="SUBSAMPLE|COMPLETECASES|CORRELATION",
                                        target_var="medv", top=10, nmax=256, cmax=0.8 )
    # #####################################################################################
    F11 = DO_GENERAL_SUBSET_SELECTION( BostonHousing[-4], dfname="BostonHousing, R:N",
                                        using_approach=linear.correlation, approach_ppname="linear.correlation", 
                                        rtypes="SUBSAMPLE|COMPLETECASES|CORRELATION",
                                        target_var="medv", top=10, nmax=256, cmax=0.8 )
    # #####################################################################################
    
    
    
    
    # #####################################################################################
    NEWLINE(20)
    data(HouseVotes84)
    # #####################################################################################
    F20 = DO_GENERAL_SUBSET_SELECTION( HouseVotes84, dfname="HouseVotes84, C:F",
                                        using_approach=chi.squared, approach_ppname="chi.squared", 
                                        rtypes="SUBSAMPLE|COMPLETECASES",
                                        target_var="Class", top=10, nmax=256, cmax=0.8 )
    # #####################################################################################
    F21 = DO_GENERAL_SUBSET_SELECTION( HouseVotes84, dfname="HouseVotes84, C:F",
                                        using_approach=oneR, approach_ppname="oneR", 
                                        rtypes="SUBSAMPLE|COMPLETECASES",
                                        target_var="Class", top=10, nmax=256, cmax=0.8 )
    # #####################################################################################
    
    
    
    
    
    # #####################################################################################
    NEWLINE(20)
    data(iris)
    # #####################################################################################
    F31 = DO_GENERAL_SUBSET_SELECTION( iris, dfname="iris, C:N",
                                        using_approach=gain.ratio, approach_ppname="gain.ratio", 
                                        rtypes="SUBSAMPLE|COMPLETECASES|CORRELATION",
                                        target_var="Species", top=3, nmax=256, cmax=0.8, refine=FALSE )
    # #####################################################################################
    F32 = DO_GENERAL_SUBSET_SELECTION( iris, dfname="iris, C:N",
                                        using_approach=information.gain, approach_ppname="information.gain", 
                                        rtypes="SUBSAMPLE|COMPLETECASES|CORRELATION",
                                        target_var="Species", top=3, nmax=256, cmax=0.8, refine=FALSE )
    # #####################################################################################
    F33 = DO_GENERAL_SUBSET_SELECTION( iris, dfname="iris, C:N",
                                        using_approach=symmetrical.uncertainty, approach_ppname="symmetrical.uncertainty", 
                                        rtypes="SUBSAMPLE|COMPLETECASES|CORRELATION",
                                        target_var="Species", top=3, nmax=256, cmax=0.8, refine=FALSE )
    # #####################################################################################
    
    
    sink()
}

