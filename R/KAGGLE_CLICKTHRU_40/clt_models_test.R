sink('output_clt_t.out', split=T)
source('clt_intro.R')

load('T10/t7-MEDFACT-DATASET.RData')

source('clt_intro.R')
source('clt_basic_functions.R')
source('clt_prob_enhancer.R')

CP = 1E-12

BANNER( 'MODEL1' )
if ( FALSE ) {
        BANNER( "EMSEMBLE-1: TREE < 7" )
        MINBCKT = 11 #17 # 12
        if ( AWS ) MINBCKT = 9
        SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) <= 7 )
        tree_controls = rpart.control(xval=10, minbucket=MINBCKT, minsplit=as.integer(MINBCKT*3.5), cp=CP, split="information")
        BUILD_EMSEMBLE_MODEL( 1, SUBSET, tree_controls, PRUNE=FALSE, FIT_METHOD="FIT" )
        if( AWS ) save(PROB_CORRECTION_MODEL_1, file=GET_MODEL_NAME("B1.RData" ))
} else {
	load('T10/t7-B1.RData')
}

BANNER( 'MODEL2' )
if ( FALSE ) {
        BANNER( "EMSEMBLE-2: TREE < 7" )
        MINBCKT = 29 # 31
        if ( AWS ) MINBCKT = 23
        SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) <= 7 )
        tree_controls = rpart.control(xval=10, minbucket=MINBCKT, minsplit=as.integer(MINBCKT*2.8), cp=CP, split="information")
        BUILD_EMSEMBLE_MODEL( 2, SUBSET, tree_controls, PRUNE=FALSE, FIT_METHOD="FIT" )
        if( AWS ) save(PROB_CORRECTION_MODEL_2, file=GET_MODEL_NAME("B2.RData" ))
} else {
	load('T10/t7-B2.RData')
}

BANNER( 'MODEL3' )
if ( FALSE ) {
        BANNER( "EMSEMBLE-3: TREE < 7" )
        MINBCKT = 13 # 17
        if ( AWS ) MINBCKT = 13
        SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) <= 7 )
        tree_controls = rpart.control(xval=10, minbucket=MINBCKT, minsplit=as.integer(MINBCKT*2.5), cp=CP, split="information")
        BUILD_EMSEMBLE_MODEL( 3, SUBSET, tree_controls, PRUNE=FALSE, FIT_METHOD="FIT" )
        if( AWS ) save(PROB_CORRECTION_MODEL_3, file=GET_MODEL_NAME("B3.RData" ))
} else {
	load('T10/t7-B3.RData')
}

BANNER( 'MODEL4' )
if ( FALSE ) {
        BANNER( "EMSEMBLE-4: TREE DAY < 7" )
        MINBCKT = 11 # 13 # 17
        if ( AWS ) MINBCKT = 11
        SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) <= 7 )
        tree_controls = rpart.control(xval=10, minbucket=MINBCKT, minsplit=as.integer(MINBCKT*3.0), cp=CP, split="information")
        BUILD_EMSEMBLE_MODEL( 4, SUBSET, tree_controls, PRUNE=FALSE, FIT_METHOD="FIT" )
        if( AWS ) save(PROB_CORRECTION_MODEL_4, file=GET_MODEL_NAME("B4.RData" ))
} else {
	load('T10/t7-B4.RData')
}


BANNER( 'MODEL9' )
if ( FALSE ) {
        BANNER( "EMSEMBLE-9: TREE DAY < 7" )
        MINBCKT = 11 # 23 # 29 # 23
        if ( AWS ) MINBCKT = 9
        SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) <= 7 )
        tree_controls = rpart.control(xval=10, maxdepth=21, minbucket=MINBCKT, cp=CP, split="information")
        BUILD_EMSEMBLE_MODEL( 9, SUBSET, tree_controls, PRUNE=TRUE, FIT_METHOD="FIT" )
        if( AWS ) save(PROB_CORRECTION_MODEL_9, file=GET_MODEL_NAME("B9.RData" ))
} else {
	load('T10/t7-B9.RData')
}

BANNER( 'SUBSUME MODELS' )
SUBSUME_MODELS()

BANNER( 'RELEASING TRAINING DATASET' )
    if ( nrow(ORIG_XTRAIN) > 200000 ) {
        gc(T)
        t=ORIG_XTRAIN[1:1000,]
        gc(T)
        rm(ORIG_XTRAIN)
        gc(T)
        ORIG_XTRAIN=t
        gc(T)
    }


BANNER( 'EVALUATION' )
source('clt_prob_enhancer.R')
# CVTEST = TRUE
# source('clt_cv.R')


BANNER( 'EVALUATION' )
CVTEST = FALSE
source('clt_test.R')

BANNER( 'DONE' )

