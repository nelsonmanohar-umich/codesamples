# #######################################################################################
source('utilities.R')
source('clt_basic_functions.R')
source('clt_options.R')
# #######################################################################################


# #######################################################################################
PYTHON              = "/usr/bin/python"
RJE_INTERFACE_SHIM  = "./clt_aws_client_send.py"
BMIN                = CLT_OPTIONS$min_subset_size_to_train_on
DEFAULT_RDATA       = "T4.RData"
NMAX                = 1000
ALL_XVAR_LEVELS     = "\'\'"
OPMODE_TRAIN        = "train"
OPMODE_TEST         = "test"
OPMODE_PREDICT      = "test"
# #######################################################################################


# #######################################################################################
REQUEST_REMOTE_OPMODE_FOR = function(cname, fl=ALL_XVAR_LEVELS, opmode=OPMODE_TRAIN, rdata=DEFAULT_RDATA, cvfold=1, until_cvfold=0, delay=5) {
    logging_data = ""
    if ( length(grep( OPMODE_TRAIN, opmode))>0 )
        BANNER(sprintf("TRAIN THE FACTOR-LEVEL MODEL OF: %s LEVEL: %s", cname, fl))
    else
        BANNER(sprintf("PREDICT WITH FACTOR-LEVEL MODELS OF: %s FOLD: %s", cname, cvfold))

    try({
            logcmd=sprintf("%s %s --xvar %s --xlevel %s --opmode %s --nmax %s --rdata %s --cvfold %s --until_cvfold %s --delay %s", 
                            PYTHON, RJE_INTERFACE_SHIM, cname, fl, opmode, NMAX, rdata, cvfold, until_cvfold, delay )
            print( logcmd )
            logging_data <- system(logcmd, intern=TRUE)
            print( logging_data )
    })
    return (logging_data)
}
# #######################################################################################


# #######################################################################################
# PREPROCESSING FACTOR LEVEL MODELS: GATHERING CSVs SETTING REMOTE MODEL BUILDING
# #######################################################################################
RJE_BASED_MODEL_BUILDER = function(individual_builds=FALSE, rdata="", all=FALSE ) {
    NEWLINE(20)
    BANNER(sprintf("REMOTE FACTOR_LEVEL MODEL BUILDING"))

    RJE_WHICH_COLUMNS = ACTIVATED_COLNAMES
    if ( !all )
        RJE_WHICH_COLUMNS = setdiff(names(sapply(CLT_OPTIONS$predictors, function(x) x)), "")

    if (individual_builds) {
        for (cname in RJE_WHICH_COLUMNS) {
            FL = table(ORIG_XTRAIN[, cname])
            w  = which( FL>BMIN )
            FL = names(FL[w])
            for (fl in FL) {
                ret = REQUEST_REMOTE_OPMODE_FOR(cname, fl, opmode='train')
            }
        }
    } else {
        for (cname in RJE_WHICH_COLUMNS) {
             ret = REQUEST_REMOTE_OPMODE_FOR(cname, fl=ALL_FACTOR_LEVELS_FOR_THIS_XVAR, opmode=OPMODE_TRAIN)
        }
    }
}
# #######################################################################################


# #######################################################################################
RJE_BASED_FACTOR_BASED_PREDICTOR = function(cvfold=1, until_cvfold=0, rdata=CLT_OPTIONS$'precomputed_aws_image', delay=5, all=FALSE) {
    NEWLINE(20)
    BANNER(sprintf("REMOTE FACTOR_LEVEL PREDICTION"))

    RJE_WHICH_COLUMNS = ACTIVATED_COLNAMES
    if ( !all )
        RJE_WHICH_COLUMNS = setdiff(names(sapply(CLT_OPTIONS$predictors, function(x) x)), "")

    for (cname in RJE_WHICH_COLUMNS ) {
         ret = REQUEST_REMOTE_OPMODE_FOR(cname,
                                         fl=ALL_FACTOR_LEVELS_FOR_THIS_XVAR, 
                                         opmode=OPMODE_TEST, 
                                         rdata=rdata, 
                                         cvfold=cvfold,
                                         until_cvfold=until_cvfold )
    }
}
# #######################################################################################

