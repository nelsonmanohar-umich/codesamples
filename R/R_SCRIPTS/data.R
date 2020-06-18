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



# #####################################################################################
# load libraries
# loading csv
# clean csv: complete cases
# preprocessing csv: scaling (numerical features)
# preprocessing csv: binarize categorial data
# optional: stratified subsampling 
# optional: build feature subsets
# iterator: correlated features within feature subset
# describe features from csv 
# feature selection
# plot
# #####################################################################################


# #####################################################################################
# http://cran.r-project.org/doc/contrib/Lemon-kickstart/kr_scrpt.html
# #####################################################################################
INITIAL.DIR <- getwd()          # store the current directory
# setwd("/home/nrm/WORKSPACE/R")# change to the new directory
# detach("package:nlme")        # unload the libraries
# sink("R.out")                 # set the output file
# sink()                        # close the output file
# detach("package:nlme")        # unload the libraries
# setwd(INITIAL.DIR)            # change back to the original directory
# #####################################################################################


# #####################################################################################
# load the utility functions
# #####################################################################################
source( 'utilities.R' )
source( 'plot_functions.R' )
#source( 'aggregate.R' )
#source( 't_tests.R' )
#source( 'classifiers.R' )
#source( 'regression.R')


# #####################################################################################
#library(utils)
library(stats)
library(ggplot2)
library(ggthemes)
library(vcd)
# #####################################################################################


# #####################################################################################
DEBUG = TRUE
HEADER = paste( "==================================================" )
SUBHEADER = paste( "    ----------------------------------------------" )
# #####################################################################################


# #####################################################################################
# LOAD CSV
# #####################################################################################
DATAFILE = "mldata/autos/imports-85.data"
PREDICT = 25
DATAFILE = "adult.data"
PREDICT = 15
XY = read.csv ( DATAFILE, header=FALSE, na.strings="?", stringsAsFactors=TRUE, sep="," )
# #####################################################################################


# #####################################################################################
# M x N (M samples by N features)
# #####################################################################################
N = ncol(XY) 
M = nrow(XY)
P = M # as.integer(M/10)
# #####################################################################################


# #####################################################################################
# SELECT COMPLETE CASES, WITHOUT MISSING VALUES
# #####################################################################################
COMPLETE_SAMPLES = complete.cases(XY)[0:P]
XYnew = XY[COMPLETE_SAMPLES,]
print( summary(XYnew) )
print( HEADER )
# #####################################################################################


# #####################################################################################
COLNAMES = colnames(XYnew)
COLS_DATATYPES = mapply( class, XYnew )
COLS_FACTORS   = grep( 'factor',  COLS_DATATYPES )
COLS_INTEGERS  = grep( 'integer', COLS_DATATYPES )
COLS_NUMERICS  = grep( 'numeric', COLS_DATATYPES )
COLS_UNKNOWNS  = c(1:N, -COLS_FACTORS, -COLS_INTEGERS, -COLS_NUMERICS )
# #####################################################################################
if ( DEBUG ) {
    print ( HEADER )
    print ( paste( "DATASET:COLUMN TYPE", COLNAMES, COLS_DATATYPES) )
    print ( SUBHEADER )
    print ( paste( "CATEGORICAL COLUMNS", COLS_FACTORS ) )
    print ( SUBHEADER )
    print ( paste( "NUMERICAL COLUMNS",   COLS_NUMERICS ) )
    print ( SUBHEADER )
    print ( paste( "INTEGER COLUMNS",     COLS_INTEGERS ) )
    if ( !is.null ( COLS_UNKNOWNS ) ) {
        print ( SUBHEADER )
        print ( paste( "MISCELLANEOUS COLUMNS", COLS_UNKNOWNS ) )
    }
    print ( HEADER )
}
# #####################################################################################


# #####################################################################################
Y   = XYnew[,PREDICT]
XYr = XYnew[,setdiff(COLS_NUMERICS,PREDICT)]
XYf = XYnew[,setdiff(COLS_FACTORS, PREDICT)]
XYi = XYnew[,setdiff(COLS_INTEGERS,PREDICT)]
# #####################################################################################


# #####################################################################################
XYrnew = scale( XYr, center=TRUE, scale=TRUE )
XYrnew_centers = attr(XYrnew, "scaled:center" )
XYrnew_scales  = attr(XYrnew, "scaled:scale" )
colnames(XYrnew) = colnames(XYr)
rownames(XYrnew) = rownames(XYr)
utils::str(XYrnew)
# #####################################################################################


# #####################################################################################
XYinew = scale( XYi, center=TRUE, scale=TRUE )
XYinew_centers = attr(XYinew, "scaled:center" )
XYinew_scales  = attr(XYinew, "scaled:scale" )
colnames(XYinew) = colnames(XYi)
rownames(XYinew) = rownames(XYi)
utils::str(XYinew)
# #####################################################################################


# #####################################################################################
for (i in 1:ncol(XYinew)) {
    DO_BASIC_FEATURE_ANALYSIS_PLOT( XYinew, i, numbins=32 )
}
# #####################################################################################


# #####################################################################################
# example
# #####################################################################################
XXi = mapply( RECODE, XYi, xnbins=4)
XXi = data.frame( matrix( XXi, nrow=nrow(XYi)) )
colnames(XXi) = colnames(XYi)
rownames(XXi) = rownames(XYi)
# #####################################################################################


# #####################################################################################
# for (i in 1:ncol(XYinew)) { ggplot( as.data.frame(XYinew), aes( y=Y, x=as.factor(RECODE(XYinew[,i],xnbins=4) ))) + geom_violin() }
# #####################################################################################


# #####################################################################################
# #####################################################################################
SAMPLED_ROWS = RANDOM_ROWS( rownames(XYr), m=min(30,length(rownames(XYr))))
# #####################################################################################


# #####################################################################################
# #####################################################################################
if ( FALSE ) {
    library('GGally')
    pdf("ggpairs-numeric.pdf", onefile=TRUE, 10, 7)
        ggpairs(EXTEND_DF(XYr, Y)[SAMPLED_ROWS,], params=list(corSize=7, base_size=7))
    dev.off()
    pdf("ggpairs-integer.pdf", 10, 7)
        ggpairs(EXTEND_DF(XYi, Y)[SAMPLED_ROWS,], params=list(corSize=7, base_size=7))
    dev.off()
    pdf("ggpairs-integer-asfactors.pdf", 10, 7)
        ggpairs(EXTEND_DF(XXi, Y)[SAMPLED_ROWS,], params=list(corSize=7, base_size=7))
    dev.off()
    pdf("ggpairs-factors.pdf", 10, 7)
        ggpairs(EXTEND_DF(XYf, Y)[SAMPLED_ROWS,], params=list(corSize=7, base_size=7))
    dev.off()
    graphics.off()
}
# #####################################################################################


# #####################################################################################
# Because of scoping bug in ggplot, it is necessary that the data frame be at the global 
# scoping level, for this reason it is critical that the name matches to the local variable
# name; hence a new data frame is created here to make it work
# #####################################################################################
# XYdf = DO_STRATIFIED_SUBSAMPLING( XYinew, Y, 8, nmax=30 )
XYdf = EXTEND_DF( XYinew, Y )[SAMPLED_ROWS,]
pdf("feature_exploration_discrete_features.pdf")
    for (i in 1:ncol(XYdf)) {
        plist = VIOLIN_DENSITY_PLOT( XYdf, i )
    }
dev.off()
graphics.off()
# #####################################################################################


# #####################################################################################
# #####################################################################################
# XYdf = DO_STRATIFIED_SUBSAMPLING( XYrnew, Y, 8, nmax=30 )
XYdf = EXTEND_DF( XYrnew, Y )[SAMPLED_ROWS,]
pdf("feature_exploration_continous_features.pdf")
    for (i in 1:ncol(XYdf)) {
        plist = VIOLIN_DENSITY_PLOT( XYdf, i )
    }
dev.off()
graphics.off()
# #####################################################################################


# #####################################################################################
# #####################################################################################
ASSOC_PLOT( XYf, Y, 4, plot_output="feature_exploration_factor_features.pdf" )
# #####################################################################################


