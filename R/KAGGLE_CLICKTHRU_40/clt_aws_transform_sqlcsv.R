# ############################################################################################
BANNER(sprintf( "LOADING FACTOR LEVEL TRAINING FILE %s", CLT_OPTIONS$'factor_level_training_file'))
# ############################################################################################
# ,ad_id,click,yymmddhh,c01,banner_pos,site_id,site_domain,site_category,app_id,app_domain,app_category,device_id,device_ip,device_model,device_type,device_conn_type,c14,c15,c16,c17,c18,c19,c20,c21,id,minhash,ridx
# colClasses=c('integer','character','numeric','integer','numeric','numeric',rep('factor',9), rep('numeric',13)))
# ############################################################################################
    ORIG_XTRAIN_PLUS = read.csv( CLT_OPTIONS$'factor_level_training_file', header=TRUE, stringsAsFactors=TRUE, 
                                colClasses=c('integer','character','numeric','integer','numeric','numeric',rep('factor',9), rep('numeric',13)))
    colnames(ORIG_XTRAIN_PLUS) = c('rowid', 'ad_id', 'click', 'YYMMDDHH', 'C01', 'banner_pos', 'site_id', 'site_domain', 'site_category', 'app_id', 'app_domain', 'app_category', 'device_id', 'device_ip', 'device_model', 'device_type', 'device_conn_type', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21', 'id', 'minhash', 'ridx')
    ORIG_XTRAIN_PLUS = ORIG_XTRAIN_PLUS[,COLNAMES]
    colnames(ORIG_XTRAIN_PLUS) = COLNAMES

    # ###########################################################################
    ORIG_XTRAIN_PLUS[,"origin"] = "W"
    ORIG_XTRAIN_PLUS[,"origin"] = as.factor( ORIG_XTRAIN_PLUS[,"origin"] )
    # ###########################################################################
    M = nrow(ORIG_XTRAIN_PLUS)
    N = ncol(ORIG_XTRAIN_PLUS)
    rownames(ORIG_XTRAIN_PLUS) = 1:M

    PREDICT_VAR = "click"
    PREDICT_COL = 2
    YCOLNAME = "click"
    YCOLNUM  = PREDICT_COL
    CLASS0 = 0
    CLASS1 = 1

    ORIG_XTRAIN_PLUS[YCOLNAME] = as.factor(ORIG_XTRAIN_PLUS[,'click'])

    BANNER("FINISHED LOADING DATA")
# ############################################################################################


# ############################################################################################
BANNER( 'TRANSFORMING SPECIALIZATION TRAINING DATASET TO CONFORM TO EXISTING ONE' )
# ############################################################################################
    print(summary(ORIG_XTRAIN_PLUS))
    str(ORIG_XTRAIN_PLUS)
    ORIG_XTEST = ORIG_XTRAIN_PLUS
    source('clt_basic_functions.R')
    source('clt_testcoding.R')
    ORIG_XTRAIN_PLUS = ORIG_XTEST
    print( summary( ORIG_XTRAIN_PLUS ) )
    str(ORIG_XTRAIN_PLUS)
# ############################################################################################


# ############################################################################################
BANNER( 'TRANSFORMATIONS DONE, READYING THE DATA' )
# ############################################################################################
    CVTEST = TRUE
    source('clt_activated_cols.R')
    # ########################################################################################
    ORIG_XTRAIN_PLUS = ORIG_XTRAIN_PLUS[,ACTIVATED_COLNAMES]
    ORIG_XTRAIN      = ORIG_XTRAIN[,ACTIVATED_COLNAMES]
    ORIG_XTRAIN      = rbind(ORIG_XTRAIN, ORIG_XTRAIN_PLUS)
    # ########################################################################################

# ############################################################################################
BANNER( 'DATASET READY' )
# ############################################################################################
    ORIG_XTRAIN      = ORIG_XTRAIN[,ACTIVATED_COLNAMES]
    Y_TRUE           = ORIG_XTRAIN[,YCOLNAME]
    PREDICT_COL      = WHICH_COL_FOR( ORIG_XTRAIN, PREDICT_VAR )
    # ########################################################################################
    cat(HEADER)
    print(summary(ORIG_XTRAIN))
    cat(HEADER)
    str(ORIG_XTRAIN)
    cat(HEADER)
# ############################################################################################
