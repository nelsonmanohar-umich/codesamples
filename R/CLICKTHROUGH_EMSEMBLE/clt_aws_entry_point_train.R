#!/usr/bin/Rscript


# ############################################################################################
setwd( '~/WORKSPACE/R/' )
opts = options(width=206, digits=2, error = function() traceback(2))
# ############################################################################################


# ############################################################################################
require(rpart)
require(randomForest)
require(FSelector)
require(stringr)
require(class)
# ############################################################################################


# ############################################################################################
# Individualized Predictor
# ############################################################################################
TARGETED_INDIVIDUALIZED_PREDICTOR_BUILDER = function(cname='', fl='', 
                                                     CP=1E-6, NMAX=512000, MINBCKT=11, NTREE=511, DO_EXTRA_PRUNING=FALSE, ALGORITHM="DT" ) {

    BANNER( sprintf( "EMSEMBLE-%s: TARGETED/CUSTOMIZED PREDICTOR FOR: %s", cname, fl ) )

    BMIN = CLT_OPTIONS$min_subset_size_to_train_on
    UNDERFIT_THRESHOLD = CLT_OPTIONS$max_acceptable_training_logloss

    FL = table(ORIG_XTRAIN[, cname])
    w  = which( FL>BMIN )
    FL = names(FL[w])
    new_fl = fl; if ( !(fl %in% FL) ) new_fl = 'other'

    PROB_CORRECTION_MODEL = NA
    if ( new_fl %in% FL ) {
        SUBSET = which( as.character(ORIG_XTRAIN[, cname]) == new_fl )
        if ( length(SUBSET) < BMIN )  next
        if ( length( unique(ORIG_XTRAIN[SUBSET,"click"])) == 1 ) next

        if ( ALGORITHM == "DT" ) {
            PROB_CORRECTION_MODEL = OPTIMIZE_DT_PREDICTOR(cname, SUBSET, MINBCKT, UNDERFIT_THRESHOLD, DO_EXTRA_PRUNING, CP, fl=fl)
            if (!is.null(PROB_CORRECTION_MODEL)) {
                save(PROB_CORRECTION_MODEL, file=sprintf("P%s-%s.RData", cname, fl ))
            }
        }
        if ( ALGORITHM == "RF" ) {
            PMODEL = BUILD_RANDOM_FOREST_MODEL( NUM=0, SUBSET=SUBSET, NTREE=NTREE, MINBCKT=MINBCKT, MTRY=2, IMPORTANCE=TRUE, FL=fl )
            yp = predict(PMODEL, ORIG_XTRAIN[SUBSET,], type='prob')
            ll = COMPUTE_LOG_LOSS(Y_TRUE[SUBSET], yp[,2])
        }
    }

    if ( !is.na(PROB_CORRECTION_MODEL)) BANNER("DONE: GENERATED MODEL")
    return ( PROB_CORRECTION_MODEL )
}
# ############################################################################################


# ############################################################################################
INDIVIDUALIZED_PREDICTOR = function( P5, XT, cname='', defaultprob=0.175 ) {
    print( sprintf( "CUSTOMIZED PREDICTOR FOR %s", cname ))
    dirty_bit = FALSE

    yp_probs  = COMPUTE_INITIAL_YP_PROBS(XT)
    lyp_probs = yp_probs
    lyp_probs[,1] = 1

    Pmodels = system('ls P*RData', intern=TRUE)
    w = grep( cname, Pmodels)

    FL = levels(ORIG_XTRAIN[, cname])
    ow = options("warn")
    for ( fl in FL ) {
        #if ( !DOES_MODEL_EXIST_FOR( cname, fl) ) next
        if ( length(w)==0) next 
        if ( length(Pmodels) == 0) next
        if ( length(Pmodels[w]) == 0) next

        which_one = grep( fl, Pmodels[w])
        if ( length(which_one)== 0 ) next
        which_one = Pmodels[w[which_one]]

        SUBSET = which( as.character(XT[, cname]) == fl)
        if ( length(SUBSET) > CLT_OPTIONS$min_subset_size_to_predict_on ) {
            try( {
                # PROB_CORRECTION_MODEL = LOAD_FACTORLEVEL_MODEL( cname, fl )
                load(file=sprintf("P%s-%s.RData", cname, fl )) 
                XNAMES = names(attr(PROB_CORRECTION_MODEL,"xlevels"))
                for ( xvar in XNAMES ) {
                    A = attr(PROB_CORRECTION_MODEL,"xlevels")$xvar 
                    B = levels(XT$xvar)
                    C = union(A, B)
                    if ( length(C) != length(B) | length(C) != length(A) ) {
                        cat(HEADER)
                        print( paste( "XVAR LEVELS BEING ADJUSTED", xvar) )
                        cat(HEADER)
                        attr(PROB_CORRECTION_MODEL,"xlevels")$xvar = union(A, B)
                        levels(XT$xvar) = union(A, B)
                    }
                }

                dt_probs = predict(PROB_CORRECTION_MODEL, XT[SUBSET,], type="prob" )
                print( paste( which_one, cname, fl ) )
                if ( TRUE )  {
                    a1 = lyp_probs[SUBSET,2] + dt_probs[,2]
                    b1 = lyp_probs[SUBSET,1] + 1
                    yp = data.frame( b1, a1 ); colnames(yp) = colnames(yp_probs)
                    lyp_probs[SUBSET,] = as.matrix(yp)
                    D = data.frame( lyp_probs[SUBSET,2], yp_probs[SUBSET,2], lyp_probs[SUBSET,2] - yp_probs[SUBSET,2] )
                    colnames(D) = c("MODF", "ORIG", "DIFF")
                    cat(HEADER); print( summary( D  ) )
                    dirty_bit = TRUE
                }
                if ( CVTEST ) { 
                    cat(HEADER)
                    llval_modf = COMPUTE_LOG_LOSS(XT$click, lyp_probs[,2]/lyp_probs[,1])
                    llval_orig = COMPUTE_LOG_LOSS(XT$click, yp_probs[,2])
                    llval_subs_modf = COMPUTE_LOG_LOSS(XT$click[SUBSET], lyp_probs[SUBSET,2]/lyp_probs[SUBSET,1])
                    llval_subs_orig = COMPUTE_LOG_LOSS(XT$click[SUBSET], yp_probs[SUBSET,2])
                    print ( sprintf( "FACTOR MODEL ITER [%s] YIELD [%s] LEVEL [%s] N [%s] LOGLOSS = [subset=%.4f/%.4f, total=%.4f/%.4f]", iter, cname, as.character(fl), length(SUBSET), llval_subs_modf, llval_subs_orig, llval_modf, llval_orig ) )
                }
                cat(HEADER)
            }, silent=TRUE )
        }
    }
    options(ow)

    # ============================================================================
      a1 = lyp_probs[,2]/lyp_probs[,1]
      yp = data.frame( 1.-a1, a1 ) 
      colnames(yp) = colnames(yp_probs)
      if( dirty_bit) {
          D = data.frame( yp_probs[,2], yp[,2], yp_probs[,2] - yp[,2] )
          colnames(D) = c("MODF", "ORIG", "DIFF")
          cat(HEADER); print( summary( D ) )
          cat(HEADER)
          if( CVTEST ) {
              llval = COMPUTE_LOG_LOSS(XT$click, yp_probs[,2])
              llval = COMPUTE_LOG_LOSS(XT$click, yp[,2])
          }
      }
      yp_probs = as.matrix(yp)
      cat(HEADER)
    # ============================================================================
    return ( yp_probs )
}
# ############################################################################################


# ############################################################################################
# Individualized predictions
# ############################################################################################
TARGETED_INDIVIDUALIZED_PREDICTOR = function( XT, cname='', fl="", defaultprob=0.175 ) {
    print( sprintf( "CUSTOMIZED PREDICTOR FOR %s", cname ))
    dirty_bit = FALSE

    yp_probs  = COMPUTE_INITIAL_YP_PROBS(XT)
    lyp_probs = yp_probs
    lyp_probs[,1] = 1

    Pmodels = system('ls P*RData', intern=TRUE)
    w = grep( cname, Pmodels)

    FL = levels(ORIG_XTRAIN[, cname])
    ow = options("warn")
    if ( fl %in% FL ) {
        #if ( !DOES_MODEL_EXIST_FOR( cname, fl) ) next
        if ( length(w)==0) next 
        if ( length(Pmodels) == 0) next
        if ( length(Pmodels[w]) == 0) next

        which_one = grep( fl, Pmodels[w])
        if ( length(which_one)== 0 ) next
        which_one = Pmodels[w[which_one]]

        SUBSET = which( as.character(XT[, cname]) == fl)
        if ( length(SUBSET) > CLT_OPTIONS$min_subset_size_to_predict_on ) {
            try( {
                # PROB_CORRECTION_MODEL = LOAD_FACTORLEVEL_MODEL( cname, fl )
                load(file=sprintf("P%s-%s.RData", cname, fl )) 
                XNAMES = names(attr(PROB_CORRECTION_MODEL,"xlevels"))
                for ( xvar in XNAMES ) {
                    A = attr(PROB_CORRECTION_MODEL,"xlevels")$xvar 
                    B = levels(XT$xvar)
                    C = union(A, B)
                    if ( length(C) != length(B) | length(C) != length(A) ) {
                        cat(HEADER)
                        print( paste( "XVAR LEVELS BEING ADJUSTED", xvar) )
                        cat(HEADER)
                        attr(PROB_CORRECTION_MODEL,"xlevels")$xvar = union(A, B)
                        levels(XT$xvar) = union(A, B)
                    }
                }

                dt_probs = predict(PROB_CORRECTION_MODEL, XT[SUBSET,], type="prob" )
                print( paste( which_one, cname, fl ) )
                if ( TRUE )  {
                    a1 = lyp_probs[SUBSET,2] + dt_probs[,2]
                    b1 = lyp_probs[SUBSET,1] + 1
                    yp = data.frame( b1, a1 ); colnames(yp) = colnames(yp_probs)
                    lyp_probs[SUBSET,] = as.matrix(yp)
                    D = data.frame( lyp_probs[SUBSET,2], yp_probs[SUBSET,2], lyp_probs[SUBSET,2] - yp_probs[SUBSET,2] )
                    colnames(D) = c("MODF", "ORIG", "DIFF")
                    cat(HEADER); print( summary( D  ) )
                    dirty_bit = TRUE
                }
                if ( CVTEST ) { 
                    cat(HEADER)
                    llval_modf = COMPUTE_LOG_LOSS(XT$click, lyp_probs[,2]/lyp_probs[,1])
                    llval_orig = COMPUTE_LOG_LOSS(XT$click, yp_probs[,2])
                    llval_subs_modf = COMPUTE_LOG_LOSS(XT$click[SUBSET], lyp_probs[SUBSET,2]/lyp_probs[SUBSET,1])
                    llval_subs_orig = COMPUTE_LOG_LOSS(XT$click[SUBSET], yp_probs[SUBSET,2])
                    print ( sprintf( "FACTOR MODEL ITER [%s] YIELD [%s] LEVEL [%s] N [%s] LOGLOSS = [subset=%.4f/%.4f, total=%.4f/%.4f]", iter, cname, as.character(fl), length(SUBSET), llval_subs_modf, llval_subs_orig, llval_modf, llval_orig ) )
                }
                cat(HEADER)
            }, silent=TRUE )
        }
    }
    options(ow)

    # ============================================================================
      a1 = lyp_probs[,2]/lyp_probs[,1]
      yp = data.frame( 1.-a1, a1 ) 
      colnames(yp) = colnames(yp_probs)
      if( dirty_bit) {
          D = data.frame( yp_probs[,2], yp[,2], yp_probs[,2] - yp[,2] )
          colnames(D) = c("MODF", "ORIG", "DIFF")
          cat(HEADER); print( summary( D ) )
          cat(HEADER)
          if( CVTEST ) {
              BANNER('CVTEST LOG LOSS DERIVED FROM CUSTOMIZED MODEL')
              llval = COMPUTE_LOG_LOSS(XT$click, yp_probs[,2])
              llval = COMPUTE_LOG_LOSS(XT$click, yp[,2])
          }
      }
      yp_probs = as.matrix(yp)
      cat(HEADER)
    # ============================================================================
    return ( yp_probs )
}
# ############################################################################################


# ############################################################################################
PARSE_ARGS = function( args ) {
    n = length(args)
    v_idx = seq(2,n,2)
    n_idx = seq(1,n,2)
    optional_args  = args[v_idx]
    print ( paste( args[v_idx], args[n_idx] )) 
    names(optional_args) = args[n_idx]
    print ( (optional_args))
    return( optional_args )
}
# ############################################################################################


# ############################################################################################
GET_ARGVAL = function( args, argname, default="" ) {
    argval = default
    n = length(args)
    if ( n == 0 ) return ( "" )
    idx = grep( argname, args )[1]
    if (length(idx) == 0) return ("")
    if ( length(idx) & (idx+1)<=n ) {
        argval = args[idx+1]
    }
    if (length(argval)==0) return( "None")
    if ( argval == "None" ) return("None")
    return ( argval )
}
# ############################################################################################









# ############################################################################################
# ############################################################################################
# ############################################################################################
#                                THE REMOTE COMPUTATION
# ############################################################################################
# ############################################################################################
# ############################################################################################



# ############################################################################################
source('utilities.R')
source('clt_basic_functions.R')
# ############################################################################################


# ############################################################################################
t1 = proc.time()
cmdargs     = commandArgs()
OPMODE      = ""
if ( length(cmdargs)>0) {
    XVAR        = GET_ARGVAL(cmdargs, "XVAR")
    XLEVEL      = GET_ARGVAL(cmdargs, "XLEVEL")
    OPMODE      = GET_ARGVAL(cmdargs, "OPMODE")
    CVFOLD      = "1"        ; try({ CVFOLD=GET_ARGVAL(cmdargs, "CVFOLD") }, silent=TRUE)
    UNTIL_CVFOLD= "0"        ; try({ UNTIL_CVFOLD=GET_ARGVAL(cmdargs, "UNTIL") }, silent=TRUE)
    try({
        RDATA_IMAGE = "T4.RData" ; try({ RDATA_IMAGE=GET_ARGVAL(cmdargs, "RDATA") }, silent=TRUE)
        BANNER(sprintf("LOADING IMAGE %s", RDATA_IMAGE))
        load(RDATA_IMAGE)
    })
    print(paste(OPMODE, XVAR, XLEVEL, RDATA_IMAGE))
}
# ############################################################################################


# ############################################################################################
# ############################################################################################


# ############################################################################################
source('utilities.R')
source('clt_basic_functions.R')
source('clt_options.R')
source('clt_prob_enhancer.R')
# ############################################################################################


# ############################################################################################
BANNER("REMOTE COMPUTATION STARTED")
# ############################################################################################


# ############################################################################################
# COMPUTE TASK 1
# ############################################################################################
if ( length(grep( "train", OPMODE)) > 0 ) {
    # ----------------------------------------------------------------------------------------
    if ( FALSE ) {
        try( {
            BANNER("DATA COMPATIBILITY TRANSFORMS")
            if ( XLEVEL != "other" ) {
                CLT_OPTIONS$factor_level_training_file = sprintf("P%s_%s_xy_.csv", XVAR, XLEVEL)
                source('clt_aws_transform_sqlcsv.R')
            }
        })
    }
    # ----------------------------------------------------------------------------------------
    try( {
        BANNER("MODEL GENERATION")
        log_filename = sprintf( "log_P%s-%s-%s.log", XVAR, XLEVEL, OPMODE )
        sink(log_filename, split=TRUE)
            if ( XLEVEL != 'None' ) {
                BANNER( sprintf( "CUSTOMIZED FACTOR LEVEL PREDICTOR TREE FOR: %s %s", XVAR, XLEVEL ) )
                cat(HEADER)
                cat(HEADER)
                TARGETED_INDIVIDUALIZED_PREDICTOR_BUILDER(cname=XVAR, fl=XLEVEL)
            } else {
                BANNER( sprintf( "INDIVIDUALIZED FACTOR LEVEL PREDICTOR FOREST FOR: %s", XVAR) )
                cat(HEADER)
                cat(HEADER)
                INDIVIDUALIZED_PREDICTOR_BUILDER(cname=XVAR)
            }
        sink()
    })
    # ----------------------------------------------------------------------------------------
}
# ############################################################################################


# ############################################################################################
# COMPUTE TASK 2
# ############################################################################################
if ( length(grep( "predict", OPMODE)) > 0 | length(grep( "test", OPMODE)) > 0 ) {
    log_filename = sprintf( "log_P%s-%s-%s.log", XVAR, XLEVEL, CVFOLD )
    print( log_filename )
    sink(log_filename, split=TRUE)

    try({
        BANNER( sprintf( "DOWNLOADING PMODELS FROM S3 FOR: %s", XVAR ) )
        FL = levels(ORIG_XTRAIN[, XVAR])
        i = 0
        j = 0
        n = length(FL)
        for ( fl in FL ) {
            cat(HEADER)
            REMOVE_INVALID_MODELS(XVAR)
            if (DOES_MODEL_EXIST_FOR(XVAR, fl)) next
            i = i + 1
            ok = S3_DOWNLOAD (filename=sprintf("P%s-%s.RData", XVAR, fl ))
            if (DOES_MODEL_EXIST_FOR(XVAR, fl)) next
            j=j+1
        }
        REMOVE_INVALID_MODELS(XVAR)
        BANNER( sprintf( "STATS: TOTAL [%s], ALREADY_LOCAL [%s], S3_REQUESTED [%s], S3_DOWNLOADED [%s].", n, n-i, i, j ))
        LIST_MODELS(cname=XVAR)
        cat(HEADER)
    })

    try( {
         # --------------------------------------------------------------------------------
         print( XLEVEL )
         if ( XLEVEL == 'None' ) {
              BANNER( sprintf( "PREDICTING FOR INDIVIDUALIZED FACTOR LEVEL PREDICTOR FOREST FOR: %s", XVAR ) )
              # #####################################################################################
              MIN_CIDX = as.integer(CVFOLD)
              MAX_CIDX = max(as.integer(UNTIL_CVFOLD),MIN_CIDX)
              RANGE_CIDX = MIN_CIDX:MAX_CIDX
              # -------------------------------------------------------------------------------------
              CVTEST = TRUE
              L = list()
              H = list() # cv metrics tabulated according to each hour
              for ( i in seq(0,24) ) { H[[paste(i)]] = c() }
              PROB_METRICS_VECTOR = VECTOR( NUM_MODELS )
              METRICS             = MATRIX( length(RANGE_CIDX), 12 )
              colnames(METRICS)   = c( "JCOST", "F_RATIO", "FPR", "FNR", "ACCURACY", "BER", "MSE", "PRECISION", "RECALL", "TPR", "TNR", "LOG_LOSS" )
              iter = 0
              k = 1
              Q = 10
              R = 50000 
              # #####################################################################################

              for( cidx in MIN_CIDX:MAX_CIDX ) {
                iter = iter + 1
                # #####################################################################################
                WRT = sprintf( "training_chunk_%s.csv", 1000+as.integer(cidx))
                # -------------------------------------------------------------------------------------
                S3_DOWNLOAD(filename=WRT)
                # -------------------------------------------------------------------------------------
                ORIG_XTEST  = read.csv( WRT, skip=Q*k, nrows=R, header=FALSE, stringsAsFactors=TRUE, colClasses=c('character',rep('integer',4), rep('factor',9), rep('integer',10)))
                colnames(ORIG_XTEST) = COLNAMES
                # -------------------------------------------------------------------------------------
                ORIG_XTEST[YCOLNAME] = as.factor(ORIG_XTEST[,'click'])
         	    Y_TRUE               = as.matrix(ORIG_XTEST[,PREDICT_VAR])
                print( summary( ORIG_XTEST ) )
                # #####################################################################################
            
                # #####################################################################################
                #                                       IMPORTANT
                # #####################################################################################
                source('clt_testcoding.R')                   # generates transcoded ORIG_XTEST
                # #####################################################################################
            
                # #####################################################################################
                ORIG_XTEST[,'origin'] = 'W'
                ORIG_XTEST  = ORIG_XTEST[, ACTIVATED_COLNAMES]
                # #####################################################################################
            
                # #####################################################################################
                #                                       IMPORTANT
                # #####################################################################################
                USE_INCREMENTAL_MODELING = CLT_OPTIONS$'use_incremental_probability_builder'
                if ( USE_INCREMENTAL_MODELING ) {
                    YP_PROBS = COMPUTE_INITIAL_YP_PROBS(ORIG_XTEST)
                    YP_CLASS = WHICH_CLASS_KERNEL(YP_PROBS, 0.5)
	                ll_orig = COMPUTE_LOG_LOSS( Y_TRUE, YP_PROBS[,2] )
                    # #################################################################################
                    YP_PROBS = CLT_AWS_INDIVIDUALIZED_PREDICTOR( ORIG_XTEST, YP_PROBS, cname=XVAR, CVTEST=TRUE )
                    YP_CLASS = WHICH_CLASS_KERNEL(YP_PROBS, 0.5)
	                ll_modf = COMPUTE_LOG_LOSS( Y_TRUE, YP_PROBS[,2] )
                    # #################################################################################

                    # #################################################################################
	                CV_CMAT          = table( Y_TRUE, YP_CLASS )
	                METRICS[iter,1]  = SUM_JCOST
	                METRICS[iter,2]  = GET_F_RATIO( CV_CMAT)
	                METRICS[iter,3]  = GET_FPR( CV_CMAT)
	                METRICS[iter,4]  = GET_FNR( CV_CMAT)
	                METRICS[iter,5]  = GET_ACC( CV_CMAT)
	                METRICS[iter,6]  =(GET_TPR( CV_CMAT) + GET_TNR( CV_CMAT ) ) / 2.0
	                METRICS[iter,7]  = sum( abs(as.numeric(YP_CLASS)-1 - as.numeric(Y_TRUE)-1)^2 )/ length(YP_CLASS)
	                METRICS[iter,8]  = GET_PRECISION( CV_CMAT )
	                METRICS[iter,9]  = GET_RECALL( CV_CMAT )
	                METRICS[iter,10] = GET_TPR( CV_CMAT )
	                METRICS[iter,11] = GET_TNR( CV_CMAT )
	                METRICS[iter,12] = COMPUTE_LOG_LOSS( Y_TRUE, YP_PROBS[,2] )
                    # #################################################################################

                    # #################################################################################
                    BANNER( 'METRICS' )
                    # #################################################################################
                    cat(HEADER)
                    print( summary( ORIG_XTEST ) )
                    cat(HEADER)
                    if ( FALSE ) str(ORIG_XTEST)
                    cat(HEADER)
                    NEWLINE(1)
                    cat(HEADER)
	                colnames(METRICS) = c( "JCOST", "F_RATIO", "FPR", "FNR", "ACCURACY", "BER", "MSE", "PRECISION", "RECALL", "TPR", "TNR", "LOG_LOSS" )
                    print( summary( METRICS[1:iter,] ) )
                    cat(HEADER)
                    print( METRICS[iter,] )
                    # #################################################################################

                    # #################################################################################
                    BANNER( 'LOG LOSS CV PERFORMANCE (CURRENTLY)' )
                    # #################################################################################
                    L[[paste(end)]]  = METRICS[iter,12]
                    L2 = unlist(L)
                    cat(HEADER)
                    print ( L2 )
                    cat(HEADER)
                    print ( summary( L2 )) 
                    cat(HEADER)
                    print ( paste( "MEAN LOG LOSS: ", mean(L2) ) )
                    print ( paste( "STDV LOG LOSS: ", sd(L2) ) )
                    # #################################################################################
                } else {
                    source('clt_cv_prediction_kernel.R')         # computes YP_PROBS and METRICS, reuses P9
                }
                # #####################################################################################

                # #####################################################################################
                yp_probs_csv = sprintf( "P%s_cvchunk_%s.probs", XVAR, 1000+as.integer(cidx) )
                BANNER( sprintf( "WRAPPING UP RESULTS (YP PROBS): %s", yp_probs_csv))
                write.table(data.frame(ORIG_XTEST$ad_id, ORIG_XTEST$click, YP_PROBS), file=yp_probs_csv, row.names=FALSE, quote=FALSE, col.names=TRUE, sep=",")
                # -------------------------------------------------------------------------------------
                S3_UPLOAD(filename=yp_probs_csv)
                MQ_SEND(filename=yp_probs_csv, cname=XVAR, opmode=OPMODE, fl=XLEVEL, status="CMD_SUCCEEDED" )
                # -------------------------------------------------------------------------------------
                try ({ system( sprintf('/bin/rm %s', WRT )) })
                try ({ system( sprintf('/bin/rm %s', yp_probs_csv )) })
                rm(ORIG_XTEST,YP_PROBS,Y_TRUE,ORIG_YTEST)
                gc()
                # #####################################################################################
             }
        }
        # --------------------------------------------------------------------------------
    })
    sink()
}
# ############################################################################################


# ############################################################################################
print(sprintf( "%s seconds elapsed", (proc.time() - t1)[1] ))
BANNER("REMOTE COMPUTATION COMPLETED")
BANNER( "OPTIONAL MQ TRANSFER/STORAGE TO SERVER TRANSFER FOLLOWS:" )
# ############################################################################################
