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
require(moments)
require(nortest)
# ###############################################################################


# ###############################################################################
# t-test, 1 sample, two sided, high confidence: 
# could observed sample mean be representative of the historically sampled source?
# ###############################################################################
COULD_SAMPLE_MEAN_BE_REPRESENTATIVE_OF = function ( x, sample_mean, confidence_level=0.999, silent=FALSE, debug=FALSE ) {
    print( "COULD_THIS_VALUE_BE_A_REPRESENTATIVE_SAMPLE_MEAN_OF_THIS_SOURCE?" ) 

    rval = t.test( x, alternative="two.sided", mu=sample_mean, conf.level=confidence_level ) 

    if ( !silent ) print( rval )
    if ( debug )   str( rval )

    confint = rval$"conf.int"
    pval    = rval$"p.value"
    if ( is.na( pval ) ) {
        if ( !any( x != x ) ) print( "WARNING: x and y given are the exact same vector, trivial result follows" )
        pval = 1 
    }

    H1 = sprintf("that true mean of X is not equal to [%s]; mean is [%8.4f]?", sample_mean, mean(x))

    if ( pval > 2E-3 ) {
        accept_h0 = TRUE
        print ( sprintf( "(%s, %8.5g): REJECT H1: %s", accept_h0, pval, H1 ) )
    } else {
        accept_h0 = FALSE
        print ( sprintf( "(%s, %8.5g): ACCEPT H1: %s", accept_h0, pval, H1 ) )
    }
    rval = list( accept_h0, rval )
    return ( rval )
}
# ###############################################################################


# ###############################################################################
# shapiro normality test where:
#    H0 = "X is normally distributed"
#    H1 = "X is NOT normally distributed"
# i.e., large enough p-val asserts that alternative hypothesis H1 (x is not 
#       normally distributed) is NOT relevant
# ###############################################################################
IS_NORMALLY_DISTRIBUTED = function( x, nmin=0, plevel = 1E-1, silent=FALSE, debug=FALSE ) {
    print( "ARE_THEY_NORMALLY_DISTRIBUTED?" ) 

    if ( nmin != 0 ) {
        nmin = min(nmin, length(x))
        x = sample( x, nmin )
    }
    accept_h0 = FALSE
    accept_h1 = FALSE
    rval = shapiro.test( x )

    if ( !silent ) print( rval )
    if ( debug )   str( rval )

    pval = rval$"p.value"
    if ( is.na( pval ) ) {
        if ( !any( x != x ) ) print( "WARNING: p.value was NA" )
        print( rval )
        pval = 1 
    }
    if ( pval > plevel )  {
        accept_h0 = TRUE
        accept_h1 = FALSE
        print ( sprintf( "(%s, %8.5g): REJECT H1 -- which claims X is NOT normally distributed", accept_h0, pval ) )
    } else {
        accept_h0 = FALSE
        accept_h1 = TRUE
    }

    retval = list( 'decision'=accept_h0, 'pval'=pval )
    return ( retval )
}


# ###############################################################################
# http://stats.stackexchange.com/questions/52293/r-qqplot-how-to-see-whether-data-are-normally-distributed
# http://en.wikipedia.org/wiki/Kurtosis
# ###############################################################################
IS_NORMAL_WRT_ADDITIONAL_NORMALITY_TESTS = function( x, plevel=5E-3, nmax=4096, confidence_level=0.999, plot=FALSE, ... ) {
    print( "IS_NORMAL_WRT_ADDITIONAL_NORMALITY_TESTS?" ) 

    # skewness and kurtosis, they should be around (0,3)
    s = moments::skewness(x)           # the skew of a normal distribution should be 0 as it is pretty much symmetric
    k = moments::kurtosis(x)           # the kurtosis of a normal distributed variable is about 3 so that excess kurtosis is cancelled
    
    # maximum size is 5000
    nmax=min(nmax,length(x))
    x = sample( x, nmax )

    # Shapiro-Wilks test
    test_sw = shapiro.test(x)
    
    # Kolmogorov-Smirnov test
    test_ks = ks.test(x,"pnorm",mean(x),sqrt(var(x)))
    
    # Anderson-Darling test
    test_ad = ad.test(x)

    pvals = c( test_sw$p.value,         test_ks$p.value,        test_ad$p.value )
    tbits = pvals >= plevel

    if (sum(tbits)>=2) print ( sprintf( "(%s, %8.5g): REJECT H1 -- which claims X is NOT normally distributed", TRUE,  mean(pvals) ) )
    if (sum(tbits)<=1) print ( sprintf( "(%s, %8.5g): ACCEPT H1 -- which claims X is NOT normally distributed", FALSE, mean(pvals) ) )

    if ( plot ) DO_QQPLOT( x )

    retval = list( 'tbits'=tbits, 'pvals'=pvals, 'shapiro'=test_sw, 'kolmogorov'=test_ks, 'anderson'=test_ad, 'skew'=s, 'kurtosis'=k )

    return ( retval )
}
# ###############################################################################
    

# ###############################################################################
# F test to compare two variances
# data:  x and y
#
# List of 9
#  $ statistic  : Named num 1              #   ..- attr(*, "names")= chr "F"
#  $ parameter  : Named int [1:2] 243 243  #   ..- attr(*, "names")= chr [1:2] "num df" "denom df"
#  $ p.value    : num 1
#  $ conf.int   : atomic [1:2] 0.654 1.528 #   ..- attr(*, "conf.level")= num 0.999
#  $ estimate   : Named num 1              #   ..- attr(*, "names")= chr "ratio of variances"
#  $ null.value : Named num 1              #   ..- attr(*, "names")= chr "ratio of variances"
#  $ alternative: chr "two.sided"
#  $ method     : chr "F test to compare two variances"
#  $ data.name  : chr "x and y"            #     - attr(*, "class")= chr "htest"
# e.g, 
# F = 1, num df = 243, denom df = 243, p-value = 1
# alternative hypothesis:           true ratio of variances is not equal to 1
# 99.9 percent confidence interval: 0.6543049 1.5283395
# sample estimates:                 ratio of variances 1 
# ###############################################################################
ARE_VARIANCES_EQUAL = function( x, y, normal=TRUE, tmode="two.sided", nmin=0, plevel=5E-2, confidence_level=0.999, silent=FALSE, debug=FALSE, ... ) {
    print( "ARE_VARIANCES_EQUAL?" ) 

    H0 = "variances are as claimed"
    H1 = "variances are NOT as claimed"
    accept_h0 = FALSE
    accept_h1 = FALSE
    if ( nmin != 0 ) {
        nmin = min( nmin, length(x), length(y) )
        x = sample( x, nmin )
        y = sample( y, nmin )
    }
    if ( normal ) {
        rval = var.test(x, y, ratio = 1, alternative=tmode, conf.level = confidence_level, ... )
    } else {
        rval = ansari.test( x, y, alternative=tmode, conf.level = confidence_level, ... )
    }

    if (!silent ) print( rval )
    if ( debug )  str(rval)

    pval = rval$"p.value"
    if ( is.na( pval ) ) {
        if ( !any( x != x ) ) print( "WARNING: x and y given are the exact same vector, trivial result follows" )
        pval = 1 
    }
    if ( pval > plevel ) {
        accept_h0 = TRUE
        accept_h1 = FALSE
        print ( sprintf( "(%s, %8.5g): REJECT H1 -- which claims ratio of variances is NOT 1", accept_h0,  pval) )
    }
    retval = list( accept_h0, pval, rval )
    return ( retval )
}
# ###############################################################################


# ###############################################################################
# t-test, 2 sample, two sided, high confidence: 
# are these two sample means similar enough and/or representative of the same source?
# 
# Two Sample t-test
# data:  x and y
# 
# t = -1.3879, df = 242, p-value = 0.1665
# alternative hypothesis: true difference in means is not equal to 0
# 99.9 percent confidence interval:
#    -0.8710354   0.3586963
# sample estimates: 
#     mean of x   mean of y 
#    2.833448     3.089618 
# 
# List of 9
#  $ statistic  : Named num -1.39             ..- attr(*, "names")= chr "t"
#  $ parameter  : Named num 242               ..- attr(*, "names")= chr "df"
#  $ p.value    : num 0.166
#  $ conf.int   : atomic [1:2] -0.871 0.359   ..- attr(*, "conf.level")= num 0.999
#  $ estimate   : Named num [1:2] 2.83 3.09   ..- attr(*, "names")= chr [1:2] "mean of x" "mean of y"
#  $ null.value : Named num 0                 ..- attr(*, "names")= chr "difference in means"
#  $ alternative: chr "two.sided"
#  $ method     : chr " Two Sample t-test"
#  $ data.name  : chr "x and y"                 - attr(*, "class")= chr "htest"
# ###############################################################################
IS_MEAN_EFFECT_ON_THESE_THE_SAME = function( x=c(), y=c(), factor_formula="", confidence_level=0.999, var_equal=TRUE, nmax=0, do_test=TRUE, debug=FALSE, ... ) {
    print( "IS_MEAN_EFFECT_ON_THESE_THE_SAME?" ) 

    accept_h0 = FALSE
    accept_h1 = FALSE
    if ( factor_formula != "" && class(x)=="data.frame" ) {
        test_formula = formula(eval(parse(text=factor_formula)))
        fvars = all.vars(test_formula)
        data = x
        yvar = fvars[1]
        xvar = fvars[2]
        print( paste( "y", "x", yvar, xvar ) )
        x=eval(parse(text=paste("data$",yvar,"[data$",xvar,"== levels(data$",xvar,")[1]]",sep="")))
        y=eval(parse(text=paste("data$",yvar,"[data$",xvar,"== levels(data$",xvar,")[2]]",sep="")))
    }

    if ( nmax != 0 ) {
        nmax = min(nmax, length(x), length(y))
        x = sample( x, nmax )
        y = sample( y, nmax )
    }

    rval = t.test( x, y, alternative="two.sided", var.equal=var_equal, conf.level=confidence_level, ... ) 

    if ( debug ) {
        print( rval )
        str( rval )
    }

    confint = rval$"conf.int"
    pval    = rval$"p.value"
    if ( is.na( pval ) ) {
        if ( !any( x != x ) ) print( "WARNING: x and y given are the exact same vector, trivial result follows" )
        pval = 0 
    }
    if ( pval > 1E-1 ) {
        accept_h0 = TRUE
        accept_h1 = FALSE
        print ( sprintf( "(%s, %8.5g): REJECT H1: that is, that the true difference sampled means is NOT zero", accept_h0, pval ) )
    } 
    else 
    {
        if ( pval < 1E-6  || confint[1] < 0 && confint[2] > 0 && pval < 1E-3 ) {
            accept_h0 = FALSE
            accept_h1 = TRUE
            print ( sprintf( "(%s, %8.5g): ACCEPT H1: that is, that the true difference sampled means is NOT zero", accept_h0, pval ) )
        }
    }
    retval = list( accept_h0, pval, rval )
    return ( retval )
}
# ###############################################################################


# ###############################################################################
# Are these two sampled data-streams x1,x2 have similar enough distributions to possibly be 
# generated from the similar-enough random source <X>?
# ###############################################################################
# PAIRED_T_TEST
# H0: there is no difference between the means and they likely come from similar sources
# H1: there is a difference between the means and they likely come from different sources
# ###############################################################################
ARE_THESE_REPRESENTATIVE_OF_SAME_SOURCE = function( x, y, nmax=0, confidence_level=0.999, silent=FALSE, ... ) {
    print( "ARE_THESE_REPRESENTATIVE_OF_SAME_SOURCE?" ) 

    accept_h0 = FALSE
    accept_h1 = FALSE
    if ( nmax != 0 ) {
        nmax = min(max(nmax, 36), length(x), length(y))
        x = sample( x, nmax )
        y = sample( y, nmax )
    }
    rval = t.test( x, y, conf.level=confidence_level, paired=TRUE, ... ) 

    if (!silent) print ( rval )

    confint = rval$"conf.int"
    pval    = rval$"p.value"
    if ( is.na( pval ) ) {
        if ( !any( x != x ) ) print( "WARNING: x and y given are the exact same vector, trivial result follows" )
        pval = 1 
    }
    if ( pval > 1E-3 ) {
        accept_h0 = TRUE
        accept_h1 = FALSE
        print ( sprintf( "(%s, %8.5g): REJECT H1: where H1 is, that true difference in means is NOT zero & both from different source", accept_h0, pval ) )
    } 
    else 
    {
        if ( pval < 1E-6  || confint[1] < 0 && confint[2] > 0 && pval < 1E-3 ) {
            accept_h0 = FALSE
            accept_h1 = TRUE
            print ( sprintf( "(%s, %8.5g): ACCEPT H1: where H1 is, that difference in means is NOT zero & both from different source", accept_h0, pval ) )
        }
    }

    retval = list( accept_h0, pval, rval )
    return ( retval )
}
# ###############################################################################


# #####################################################################################
# iterates ntries over any of the tests above using the specified sampling size : best results if
# nmax is large enough
# ###############################################################################
ITERATE_ON_TEST = function( test_to_apply, x, y, ntries=10, nmax=0, confidence_level=0.999, debug=FALSE, ... ) {
    if ( nmax == 0 ) nmax = length(x)
    tbits = c()
    pvals = c()
    analyses = list()
    for ( i in 1:ntries) {
        idx_set = round(runif(nmax,1,min(length(x),length(y))))
        xx = x[ idx_set ]
        yy = y[ idx_set ]
        cur_analysis = test_to_apply( xx, yy, confidence_level=confidence_level, ... )
        test_sbit = cur_analysis[[1]]
        test_pval = cur_analysis[[2]]
        test_text = cur_analysis[[3]]
        analyses[[i]] = cur_analysis
        tbits = append( tbits, test_sbit ) 
        pvals = append( pvals, test_pval ) 
        txt = summary( test_text )
        if ( debug ) {
            cat (HEADER)
            print ( paste( i, test_sbit, test_pval ) )
        }
        cat (HEADER)
        NEWLINE(1)
    }

    NEWLINE(10)
    opt_analysis_idx = which( round(pvals,16) == round(median(pvals ),16) )[1]
    opt_analysis = analyses[[ opt_analysis_idx ]]
    cat(HEADER)
    print( opt_analysis )
    cat(HEADER)
    pval_range = c( "ntries"=i, 
                    "min_pval"=min(pvals), 
                    "avg_pval"=mean(pvals), 
                    "med_pval"=median(pvals), 
                    "std_pval"=sd(pvals), 
                    "q"=quantile(pvals, c(0.75)), 
                    "max_pval"=max(pvals) )
    print ( pval_range )
    cat(HEADER)

    retval = list(as.logical(median(tbits)), pval_range, tbits, pvals)
    return ( retval )
}
# ###############################################################################



# ###############################################################################
ARE_SAMPLES_MEANINGFULLY_CORRELATED = function( X, Y, plevel=0.999, silent=FALSE, ... ) {
    print( "ARE_SAMPLES_MEANINGFULLY_CORRELATED?" ) 
    accept_h0 = FALSE

    CXY = cor.test( X, Y, conf.level=plevel, ... )
    if (!silent) print (CXY)

    confint = CXY$conf.int
    pval    = CXY$p.value

    H1 = sprintf("that true correlation [%s,%s] is NOT equal to 0", CXY$method, CXY$alternative )

    if ( pval > 1E-3 ) {
        accept_h0 = TRUE
        accept_h1 = FALSE
        print ( sprintf( "(%s, %8.5g): REJECT H1: where H1 is, %s", accept_h1, pval, H1 ) )
    } 
    else 
    {
        if ( pval < 1E-6  || confint[1] < 0 && confint[2] > 0 && pval < 1E-3 ) {
            accept_h0 = FALSE
            accept_h1 = TRUE
            print ( sprintf( "(%s, %8.5g): ACCEPT H1: where H1 is, %s", accept_h1, pval, H1 ) )
        }
    }

    retval = list( accept_h0, pval )
    return ( CXY )
}
# ###############################################################################


