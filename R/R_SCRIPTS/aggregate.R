
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
DO_AGGREGATE = function( formula_text="Y~.", XYdf, function_name="mean" ) {
    my_formula  = formula(eval(parse(text=formula_text)))
    my_function = eval(parse(text=function_name))
    agg_result = aggregate( my_formula, data=XYdf, my_function )
    return ( agg_result )
}
# #####################################################################################


# #####################################################################################
DO_AGGREGATE_COUNT = function( formula_text="Y~.", XYdf ) {
    DO_AGGREGATE( formula_text, XYdf, "length" )
}
# #####################################################################################


# #####################################################################################
DO_AGGREGATE_MEAN  = function( formula_text="Y~.", XYdf ) {
    DO_AGGREGATE( formula_text, XYdf, "mean" )
}
# #####################################################################################


# #####################################################################################
DO_AGGREGATE_STDEV = function( formula_text="Y~.", XYdf ) {
    DO_AGGREGATE( formula_text, XYdf, "sd" )
}
# #####################################################################################


# #####################################################################################
DO_MERGE_DFCOLS= function( ... ) {
    ERROR = NULL
    df_list = list( ... )
    argtypes = mapply( class, df_list )
    argnrows = mapply( nrow,  df_list )
    if ( any( argtypes!= "data.frame" ) ) {
        print ( "ERROR: arguments are not data.frames; operation NOT performed" )
        ERROR = TRUE
    }
    if ( min(argnrows) != max(argnrows) ) {
        print ( "ERROR: data.frames have not the same number of columns" )
        ERROR = TRUE
    }
    if ( !ERROR  ) return ( cbind( ... ) )
}
# #####################################################################################


# #####################################################################################
DO_MERGE_DFROWS= function( ... ) {
    ERROR = NULL
    df_list = list( ... )
    argtypes = mapply( class, df_list )
    argncols = mapply( ncol,  df_list )
    if ( any( argtypes!= "data.frame" ) ) {
        print ( "ERROR: arguments are not data.frames; operation NOT peformed" )
        ERROR = TRUE
    }
    if ( min(argncols) != max(argncols) ) {
        print ( "ERROR: data.frames have not the same number of columns" )
        ERROR = TRUE
    }
    if ( !ERROR  ) return ( rbind( ... ) )
}
# #####################################################################################


# #####################################################################################
MERGE_DF = function( X1, X2, xvars=c("Y"), yvars=c("Y") ) {
    merge( X1, X2, by.x=xvars, by.y=yvars )
}
# #####################################################################################



