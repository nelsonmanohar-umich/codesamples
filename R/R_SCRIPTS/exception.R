# #################################################################################################
# http://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r 
# #################################################################################################
EXCEPTION_BASED_FUNCTION <- function( COMMON_FUNCTION_F="", ALTERNATIVE_F="", ... ) {
    FINAL_RETVALS <- tryCatch(
        {
            message("CONDITIONAL EXECUTION: TRY")
            # print( ... )

            # Just to highlight: if you want to use more than one 
            # R expression in the "try" part then you'll have to 
            # use curly brackets.
            # 'tryCatch()' will return the last evaluated expression 
            # in case the "try" part was completed successfully

            RETVALS = COMMON_FUNCTION_F( ... )
            print ( RETVALS )
            RETVALS

            # The return value of `readLines()` is the actual value 
            # that will be returned in case there is no condition 
            # (e.g. warning or error). 

            # You don't need to state the return value via `return()` as code 
            # in the "try" part is not wrapped insided a function (unlike that
            # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
            message("CONDITIONAL EXECUTION: ERROR")
            message(cond)
            cat("\n")
            RETVALS = ALTERNATIVE_F( ... )
            print( RETVALS )
            # Choose a return value in case of error
            return(RETVALS)
        },
        warning=function(cond) {
            message("CONDITIONAL EXECUTION: WARNING")
            message(cond)
            cat("\n")
            RETVALS = ALTERNATIVE_F( ... )
            print( RETVALS )
            # Choose a return value in case of error
            return(RETVALS)
        }
    )    
    return(FINAL_RETVALS)
}
# #################################################################################################


# #################################################################################################
# d = EXCEPTION_BASED_FUNCTION( COMMON_FUNCTION_F=cumsum, ALTERNATIVE_F=function ( x ) { sum( x, na.rm=TRUE ) }, FINALLY_F=str, c(2:10)) 
# #################################################################################################

