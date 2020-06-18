sink ( 'output_clt_tester.out', split=T )


# ############################################################################################
source( 'clt_intro.R' )
source( 'clt_basic_functions.R' )
source( 'clt_columns.R' )
source( 'clt_prob_enhancer.R' )
source( 'clt_nb_formula.R' )
# ############################################################################################


# ############################################################################################
# SUBSUMED MODELS
# ############################################################################################
AWS = FALSE
LAPTOP = TRUE
USING_SUBSUMED_MODELS = TRUE 

SUBSUME_MODELS = function() {
    if ( USING_SUBSUMED_MODELS ) {
        print(sprintf( "REMARK: MODEL %s BEEN REPLACED BY MODEL %s", c(5,6,7,8), c(3,3,9,1)))
        PROB_CORRECTION_MODEL_5 <<- PROB_CORRECTION_MODEL_3   # MATCHES SUBSUBE_COLS
        PROB_CORRECTION_MODEL_6 <<- PROB_CORRECTION_MODEL_3   # MATCHES SUBSUBE_COLS
        PROB_CORRECTION_MODEL_7 <<- PROB_CORRECTION_MODEL_9   # MATCHES SUBSUBE_COLS
        PROB_CORRECTION_MODEL_8 <<- PROB_CORRECTION_MODEL_1   # MATCHES SUBSUBE_COLS
        return()
    }
}

NOT_A_SUBSUMED_MODEL = function(i) {
    if ( i %in% c(5,6,7,8) ) return (FALSE)                 # MATCHES ABOVE/BELOW
    return (TRUE)
}

SUBSUME_COLUMNS = function(i) {
    if ( USING_SUBSUMED_MODELS ) {
        if ( i == 5 ) return ( 3 ) # must exact match SUBSUME_MODELS SPECIFICATION
        if ( i == 6 ) return ( 3 ) # must exact match SUBSUME_MODELS SPECIFICATION
        if ( i == 7 ) return ( 9 ) # must exact match SUBSUME_MODELS SPECIFICATION
        if ( i == 8 ) return ( 1 ) # must exact match SUBSUME_MODELS SPECIFICATION
    }
    return ( i )
}
# ############################################################################################
# IMPORTANT, THE ABOVE MUST BE EDITED TO MATCH REQUIREMENTS OF THE EMSEMBLE COMPUTE 
# CAPACITY, RESOURCES, AND MODELING AND REPRESENTS GLOBAL STATE
# ############################################################################################




# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
ACP = NA
SCP = NA
EPS = NA
F_APP_CAT  = NA
F_SITE_CAT = NA
A2C = NA
S2C = NA
A_ids = NA
S_ids = NA
S2CHP = NA
A2CHP = NA
source('clt_probs.R')
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################





# ############################################################################################
# load datasets
# ############################################################################################
DO_FAST_TEST                    = TRUE
FRACTION_TO_USE                 = 0.01 # increasing this decreases lloss (as data does not reflect true data but decreases overfitting)
NROWS                           = 500 # the 1.5M subsample spans 10 days (accuracy increases if full span used)
# ############################################################################################
# total should be around: 6000 + 20000 + 200000 + 100000 = 330000
# ############################################################################################

# ############################################################################################
NROWS_2                         = 40000  # just 20k
NROWS_10                        = 100000 # 1/500 of 100K longitudinal samples from balanced set
NROWS_W                         = 20000  # they are between 15k and 26k #NROWS_W = 10000; NROWS_W = 20000
NROWS_B                         = 10  # they are between 15k and 26k #NROWS_W = 10000; NROWS_W = 20000
# ############################################################################################

# add origin flag to each row
# retrain c5 with those w/ origin b
# all others with the other origin
# actually bad bursts come at midday and wee hours, just like prob matrix said, then retrain by selecting those hours on c5 or with origin
# also add hour and/or shift to all classifiers

USE_NAIVE_BAYES                 = TRUE
USE_EXPLORATORY_TRANSFORMS      = FALSE

PROBABILITY_ENHANCE             = TRUE
CENTERS = NA
SCALES  = NA
PROB_CORRECTION_MODEL_A = NA
PROB_CORRECTION_MODEL_B = NA
PROB_CORRECTION_MODEL_1 = NA
PROB_CORRECTION_MODEL_2 = NA
PROB_CORRECTION_MODEL_3 = NA
PROB_CORRECTION_MODEL_4 = NA
PROB_CORRECTION_MODEL_5 = NA
PROB_CORRECTION_MODEL_0 = NA
PROB_CORRECTION_MODEL_6 = NA
PROB_CORRECTION_MODEL_7 = NA
PROB_CORRECTION_MODEL_8 = NA
PROB_CORRECTION_MODEL_9 = NA

PROB_CORRECTION_THRESHOLD= 0.62

USE_PRECOMPUTED_MODELS  = FALSE
if (USE_PRECOMPUTED_MODELS) { load('pcm.rdata') }

BIGW                     = 1/3
LILW                     = 0.10
NB_W                     = 1.00
LAPLACE                  = 2048
NB_THRESHOLD                    = 1E-18 ; NB_EPS                          = 1E-18
NB_THRESHOLD                    = 1E-16 ; NB_EPS                          = 1E-16

YPF                     = NA
CVTEST                  = TRUE

# previous run was without laplace but ???check on nb threshold instead

COLNAMES = c( 'ad_id', 'click', 'YYMMDDHH', 'C01', 'banner_pos', 'site_id', 'site_domain', 'site_category', 'app_id', 'app_domain', 'app_category', 'device_id', 'device_ip', 'device_model', 'device_type', 'device_conn_type', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21' )

NEWLINE(20)
    BALANCED_TRAINSET_9  = 'TRAINING_FILES/training_set9.csv'
    ORIGINAL_TRAINSET_2  = 'TRAINING_FILES/subsampled_train_set_2.csv'
    ORIGINAL_TRAINSET_10 = 'TRAINING_FILES/subsampled_train_set_10.csv'
    BAD_PERFORMANCE_X    = 'TRAINING_FILES/bad_performance.csv'
    PREFIX0="TRAINING_FILES/"
    PREFIX1="TRAINING_FILES/t-"
    PREFIX2="TRAINING_FILES/t2-"
    PREFIX3="TRAINING_FILES/t3-"
    PREFIX6="TRAINING_FILES/t6-"

    PREFIX = PREFIX6
    DAYOWEEK_TRAINSET = c(sprintf("%s%s",PREFIX,'subsampled_141021.csv'), # 21 - 20 T
                          sprintf("%s%s",PREFIX,'subsampled_141022.csv'), # 22 - 20 W
                          sprintf("%s%s",PREFIX,'subsampled_141023.csv'), # 23 - 20 T
                          sprintf("%s%s",PREFIX,'subsampled_141024.csv'), # 24 - 20 F
                          sprintf("%s%s",PREFIX,'subsampled_141025.csv'), # 25 - 20 S
                          sprintf("%s%s",PREFIX,'subsampled_141026.csv'), # 26 - 20 S
                          sprintf("%s%s",PREFIX,'subsampled_141027.csv'), # 27 - 20 M
                          sprintf("%s%s",PREFIX,'subsampled_141028.csv'), # 28 - 20 T
                          sprintf("%s%s",PREFIX,'subsampled_141029.csv'), # 29 - 20 W
                          sprintf("%s%s",PREFIX,'subsampled_141030.csv')) # 30 - 20 T

    source('clt_dataloader.R')
# ############################################################################################


# #####################################################################################
BANNER( 'TRANSFORMING TRAINING DATASET' )
source('clt_traincoding.R')
# #####################################################################################


# #####################################################################################
# drop ad_id, device_id, and device_id as exogeneous patterns
# #####################################################################################
BANNER( 'DATASET READY' )
    ACTIVATED_COLNAMES  = c( "click", 'sitecatclickprobs2', "origin", "adix", "adix_ts", "adix_ts2", "adix_ts6", "banner_pos", 
                            "site_domain", "site_category", 'site_id', "app_domain", "app_category", 'app_id', 
                            "device_type", 'device_ip', "device_conn_type", 'device_model', 
                            "H01","H14","H15","H16","H17","H18","H19","H20","H21","H44",'H54',
                             "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1", "shift", "hour", "day" )
    ORIG_XTRAIN = ORIG_XTRAIN[,ACTIVATED_COLNAMES]
    PREDICT_COL = WHICH_COL_FOR( ORIG_XTRAIN, PREDICT_VAR )
    cat(HEADER)
    print( summary( ORIG_XTRAIN ) )
    cat(HEADER)
    str(ORIG_XTRAIN)
    cat(HEADER)
    if ( NROWS > 100000 )
        save.image(file="DATASET.RData")

    if( FALSE )
    if ( nrow(ORIG_XTRAIN) > 20000 ) {
        Y = sapply( 1:ncol(ORIG_XTRAIN), function(x) as.numeric(ORIG_XTRAIN[,x]))
        colnames(Y) = colnames(ORIG_XTRAIN)
        C = cor(Y)
        t = apply(cor(Y), 1, function(x) sum(is.na(x))>1)
        t = which(t)
        C = C[-t,-t]
        plot( hclust( dist( abs( C ))))
    }
    
    NTREE=2048; CLWT=c(0.5,0.5) ; NSZ = 11
    SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) <= 7 )

    if ( TRUE )
    for( i in c(1:4,9) ) {
        cols = GET_COLS( i ) ; FORMULA = GET_FORMULAE(cols)
        BANNER( sprintf("MODEL %s", i ) )
        PROB_CORRECTION_MODEL_1 <<- randomForest( ORIG_XTRAIN[SUBSET,cols], y=Y_TRUE[SUBSET], 
                                    ntree=NTREE, mtry=1, nodesize=NSZ, subset=SUBSET, classwt=CLWT, importance=TRUE )
        print ( importance( PROB_CORRECTION_MODEL_1 ) )
        YP = predict(PROB_CORRECTION_MODEL_1, type='prob')
        COMPUTE_LOG_LOSS( ORIG_XTRAIN[SUBSET,'click'], YP[,2] )
    }

    cols = c( 'sitecatclickprobs2', "adix", "adix_ts", "adix_ts2", "adix_ts6", "banner_pos", 
                            "site_domain", "site_category", 'site_id', "app_domain", "app_category", 'app_id', 
                            "device_type", 'device_ip', "device_conn_type", 'device_model', 
                            "H01","H14","H15","H16","H17","H18","H19","H20","H21","H44",'H54',
                             "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1", "shift", "hour", "day" )

    for ( K in seq(5,30,10)) {
      for ( i in seq(1,9,1)) {
        cols = GET_COLS( i ) ; FORMULA = GET_FORMULAE(cols)
        BANNER( sprintf("K=%s w/ MODEL %s; %s", K, i, FORMULA) )
        SUBSET = as.numeric( sample( rownames(ORIG_XTRAIN), min(nrow(ORIG_XTRAIN),300000) ) )
        SUBSET = 1:nrow(ORIG_XTRAIN)
        WHICH_ROWS = SUBSET[1:20000]
        DF = as.data.frame( sapply(cols, function(x) as.numeric( ORIG_XTRAIN[SUBSET,x]) ))
        DF = scale(DF)
        PP = knn( DF[WHICH_ROWS,], DF, Y_TRUE[WHICH_ROWS], k=K, prob=TRUE )
        print( table( as.numeric(PP)-1, Y_TRUE[SUBSET]) )
        probs = 1 - attr(PP, "prob")
        COMPUTE_LOG_LOSS( Y_TRUE, probs )
        print( table( Y_TRUE[SUBSET], round(probs,1) ) )
      } 
    }
# #####################################################################################

