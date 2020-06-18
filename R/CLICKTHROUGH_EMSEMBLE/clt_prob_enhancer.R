# ############################################################################################
FACTOR_XVARS=c(
    "appcatclickprobs0", "sitecatclickprobs1",
    "banner_pos",
    "device_ip", "device_type", "device_conn_type", "device_model", "device_id",
    "site_domain", "site_category", "site_id",
    "app_domain", "app_category", "app_id",
    "H01", "H14", "H15", "H16", "H17", "H18", "H19", "H20", "H21",
    "adix_ts6", "hour", "day")
# ############################################################################################


# ############################################################################################
COMPUTE_INITIAL_YP_PROBS = function(XT) {
    if ( CLT_OPTIONS$'use which model as default model' == 1 ) yp_probs = predict(PROB_CORRECTION_MODEL_1, XT, type="prob" )
    if ( CLT_OPTIONS$'use which model as default model' == 2 ) yp_probs = predict(PROB_CORRECTION_MODEL_2, XT, type="prob" )
    if ( CLT_OPTIONS$'use which model as default model' == 3 ) yp_probs = predict(PROB_CORRECTION_MODEL_3, XT, type="prob" )
    if ( CLT_OPTIONS$'use which model as default model' == 4 ) yp_probs = predict(PROB_CORRECTION_MODEL_4, XT, type="prob" )
    if ( CLT_OPTIONS$'use which model as default model' == 5 ) yp_probs = predict(PROB_CORRECTION_MODEL_5, XT, type="prob" )
    if ( CLT_OPTIONS$'use which model as default model' == 6 ) yp_probs = predict(PROB_CORRECTION_MODEL_6, XT, type="prob" )
    if ( CLT_OPTIONS$'use which model as default model' == 7 ) yp_probs = predict(PROB_CORRECTION_MODEL_7, XT, type="prob" )
    if ( CLT_OPTIONS$'use which model as default model' == 8 ) yp_probs = predict(PROB_CORRECTION_MODEL_8, XT, type="prob" )
    if ( CLT_OPTIONS$'use which model as default model' == 9 ) yp_probs = predict(PROB_CORRECTION_MODEL_9, XT, type="prob" )
    if ( CLT_OPTIONS$'use which model as default model' == 0 ) { N = nrow(XT); yp_probs = MATRIX(N, 2, initval=defaultprob); yp_probs[,1] = 1- defaultprob }
    if ( CLT_OPTIONS$'use which model as default model' == -1) yp_probs = predict(BASE_MODEL,              XT, type="prob" )
    return (yp_probs)
}
# ############################################################################################


# ############################################################################################
GET_SUBSET = function( x, do_replace=F, NMAX=0) {
    NMAX_TRAIN = NMAX
    if ( NMAX == 0 ) NMAX_TRAIN=as.integer( CLT_OPTIONS$'max number of samples per cust model' )
    s = sample( x, min(length(x),NMAX_TRAIN), replace=do_replace )
    return ( s )
}
# ############################################################################################


# ############################################################################################
PUBLISH_MODEL = function(WHICH_MODEL, PMODEL) {
    if (WHICH_MODEL == 1) PROB_CORRECTION_MODEL_1 <<- PMODEL
    if (WHICH_MODEL == 2) PROB_CORRECTION_MODEL_2 <<- PMODEL
    if (WHICH_MODEL == 3) PROB_CORRECTION_MODEL_3 <<- PMODEL
    if (WHICH_MODEL == 4) PROB_CORRECTION_MODEL_4 <<- PMODEL
    if (WHICH_MODEL == 5) PROB_CORRECTION_MODEL_5 <<- PMODEL
    if (WHICH_MODEL == 6) PROB_CORRECTION_MODEL_6 <<- PMODEL
    if (WHICH_MODEL == 7) PROB_CORRECTION_MODEL_7 <<- PMODEL
    if (WHICH_MODEL == 8) PROB_CORRECTION_MODEL_8 <<- PMODEL
    if (WHICH_MODEL == 9) PROB_CORRECTION_MODEL_9 <<- PMODEL
    return
}
# ############################################################################################


# ############################################################################################
COMPUTE_INITIAL_MODEL = function(WHICH_MODEL) {
    MAXDEPTH = 24
    MINBCKT  = 17
    MINSPLT  = as.integer(1.1*(MINBCKT+1))
    CP       = 1E-6
    DO_PRUNE = FALSE
    SPLIT_CRITERION = "information"
    PMODEL   = NA
    UNDERFIT_THRESHOLD = CLT_OPTIONS$max_acceptable_training_logloss * 1.0175
    NMAX     = CLT_OPTIONS$'max number of samples per main model'

    if ( NOT_A_SUBSUMED_MODEL(WHICH_MODEL) ) {
        PREDICTOR = CLT_OPTIONS$'predictors'[WHICH_MODEL]
        if ( PREDICTOR!='' & CLT_OPTIONS$'use individualized predictor' ) {
            BANNER( sprintf( "EMSEMBLE-%s: INDIVIDUALIZED PREDICTOR TREE FOR: %s", WHICH_MODEL, PREDICTOR) )
            PMODEL = INDIVIDUALIZED_PREDICTOR_BUILDER(cname=PREDICTOR)
            PUBLISH_MODEL(WHICH_MODEL, PMODEL)
        } else {
            BANNER( sprintf( "EMSEMBLE-%s: DAY < 7", WHICH_MODEL ) )
            SUBSET_W = GET_SUBSET( which( as.character(ORIG_XTRAIN[,'origin']) == 'W' ) )
            SUBSET = which( as.integer(ORIG_XTRAIN[SUBSET_W,'day']) <= 7 )
            TEST_SUBSET  = SUBSET 
            # tree_controls = rpart.control(xval=10, maxdepth=MAXDEPTH, minbucket=MINBCKT, minsplit=MINSPLT, cp=CP, split=SPLIT_CRITERION)
            # PMODEL = BUILD_EMSEMBLE_MODEL( WHICH_MODEL, SUBSET, tree_controls, PRUNE=DO_PRUNE, FIT_METHOD="FIT" )

            EXPLORATION_MODELS = c(10, 17, 24, 30)
            llvals = list()
            for ( stump_depth in EXPLORATION_MODELS ) {
                cat(HEADER)
                tree_controls = rpart.control(xval=10, maxdepth =as.integer(stump_depth), 
                                                       minbucket=as.integer(MINBCKT*1.0), 
                                                       minsplit =as.integer(MINBCKT*2.0), 
                                                       cp=CP, split=SPLIT_CRITERION)
                PMODEL = NA
                try({ 
                    gc()
                    TRAIN_SUBSET = GET_VALID_TRAINSET( SUBSET, PERCENTAGE=0.75 )
                    TRAIN_SUBSET = sample( TRAIN_SUBSET, min(NMAX, length(TRAIN_SUBSET)))
                    PMODEL = BUILD_EMSEMBLE_MODEL(WHICH_MODEL, TRAIN_SUBSET, tree_controls, PRUNE=FALSE, FIT_METHOD="FIT", RESET=FALSE, CNAME='')
                    })
                if (!( stump_depth == 30 | IS_VALID_DT_MODEL(PMODEL) )) next 
                llvals[[stump_depth]] = NA
                try( { 
                    cprows = c(1, which.min(PMODEL$cptable[,"xerror"]), nrow(PMODEL$cptable))
                    cat(HEADER); print(PMODEL$cptable[cprows,])
                    cat(HEADER); print(PMODEL$variable.importance)
                    cat(HEADER)
                    yp = predict(PMODEL, ORIG_XTRAIN[TEST_SUBSET,], type='prob')
                    ll =  COMPUTE_LOG_LOSS(ORIG_XTRAIN[TEST_SUBSET,PREDICT_VAR], yp[,2])
                    cat(HEADER)
        
                    llvals[[stump_depth]] = ll
                    if ( ll < UNDERFIT_THRESHOLD ) {                          
                         print ( sprintf( "OPTIMIZED DECISION TREE N [%s] TREE_DEPTH [%s] LOGLOSS = [%s]", 
                                           length(SUBSET), stump_depth, ll) )
                         print(unlist(llvals))
                         break
                    }
        
                } )
            }
            PUBLISH_MODEL(WHICH_MODEL, PMODEL)
        }
    }
    return (PMODEL)
}
# ############################################################################################


# ############################################################################################
PRECOMPUTED_MODEL_PREFETCHER = function(cname) {
    if( CLT_OPTIONS$'prefetch_remote_precomputed_models' ) {
        FL = levels(ORIG_XTRAIN[, cname])
        BANNER( sprintf( 'PREFETCHING FACTOR LEVEL MODELS FOR %s', cname ))
        for ( fl in FL ) {
            FL = levels(ORIG_XTRAIN[, cname])
            FETCH_MODEL_FOR(cname, fl)
        }
    }
    return ( LIST_MODELS() )
}
# ############################################################################################


# ############################################################################################
LIST_MODELS = function(cname="") {
    Pmodels = system(sprintf('ls P%s*.RData', cname), intern=TRUE)
    print( Pmodels )
    return ( Pmodels )
}
# ############################################################################################


# ############################################################################################
REMOVE_INVALID_MODELS = function(cname="") {
    ok = -1
    ow = options("warn")
    try({
         rm_cmd = sprintf( "ls -ls P%s*.RData  | awk '{print $6, $10}' | grep '^0 '", cname )
         ok = system( rm_cmd )
            if ( ok == 0 ) {
                rm_cmd = sprintf( "ls -ls P%s*.RData  | awk '{print $6, $10}' | grep '^0 ' | awk '{ print $2 }' | xargs rm", cname )
                print( rm_cmd )
                ok = system( rm_cmd )
                # if ( ok == 0) print ( "DONE" )
            }
        }, silent=TRUE)
    options(ow)
    return ( ok )
}
# ############################################################################################


# ############################################################################################
MQ_SEND = function(filename="", cname="", opmode="test", fl="", status="CMD_SUCCEEDED", 
                   PYTHON="/usr/bin/python", 
                   CLIENT_MQ_REPLYER="clt_aws_client_send.py") {
    try({
        print( paste( "MQ_SENDING %s", filename ) )
        mq_cmd = sprintf("%s %s --opmode %s --xvar %s --xlevel %s --status %s --rdata %s --data %s --filename %s --dryrun 0", 
                         PYTHON, 
                         CLIENT_MQ_REPLYER, 
                         opmode, cname, fl, status, 
                         RDATA_IMAGE, filename, filename)
        print( mq_cmd )
        ok = system(mq_cmd, intern=TRUE)
    })
    return (filename)
}
# ############################################################################################


# ############################################################################################
S3_UPLOAD = function(filename="", S3CMD="/usr/bin/s3cmd" ) {
    try({ 
        print( paste( "UPLOADING %s", filename ) )
        #s3_cmd  = sprintf("/usr/bin/aws s3 cp %s s3://%s/%s", filename, CLT_OPTIONS$aws_bucketkey, filename)
        s3_cmd  = sprintf("%s put --force %s s3://%s/%s", S3CMD, filename, CLT_OPTIONS$aws_bucketkey, filename)
        print( s3_cmd )
        ok = system(s3_cmd, intern=TRUE)
    })
    return (filename)
}
# ############################################################################################


# ############################################################################################
S3_DOWNLOAD = function(filename="" ) {
    ok = NA
    try({ 
        print( paste( "DOWNLOADING %s", filename ) )
        #s3_cmd  = sprintf("/usr/bin/aws s3 cp s3://%s/%s %s", CLT_OPTIONS$aws_bucketkey, filename, filename)
        s3_cmd  = sprintf("/usr/bin/s3cmd get --force s3://%s/%s %s", CLT_OPTIONS$aws_bucketkey, filename, filename)
        print( s3_cmd )
        ok = system(s3_cmd, intern=TRUE)
    })
    return (ok)
}
# ############################################################################################


# ############################################################################################
STORE_MODEL_FOR = function(cname="", fl="", PYTHON="/usr/bin/python", CLIENT_MQ_REPLYER="clt_aws_client_send.py") {
    Pmodel = sprintf("P%s-%s.RData", cname, fl)
    S3_UPLOAD(filename=Pmodel)
    MQ_SEND(filename=Pmodel, cname=cname, opmode="train", fl=fl, status="CMD_SUCCEEDED:MODEL_GENERATED")
    return (Pmodel)
}
# ############################################################################################


# ############################################################################################
FETCH_MODEL_FOR = function(cname, fl) {
    Pmodel = sprintf("P%s-%s.RData", cname, fl)
    try({ 
          s3_cmd  = sprintf("/usr/bin/s3cmd get --force s3://%s/%s %s", CLT_OPTIONS$aws_bucketkey, Pmodel, Pmodel)
          print( s3_cmd )
          ok = system(s3_cmd, intern=TRUE)
    })
    return (Pmodel)
}
# ############################################################################################


# ############################################################################################
GET_MODEL_NUMBER_FOR = function( cname ) {
    WHICH_MODEL = 0
    for (WHICH_MODEL in 1:9) {
        PREDICTOR = CLT_OPTIONS$'predictors'[WHICH_MODEL]
        if ( PREDICTOR == cname ) return(WHICH_MODEL)
    }
    return ( WHICH_MODEL )
}
# ############################################################################################


# ############################################################################################
DOES_MODEL_EXIST_FOR = function(cname, fl) {
    REMOVE_INVALID_MODELS(cname=sprintf("%s-%s", cname, fl)) 
    Pmodel = sprintf("P%s-%s.RData", cname, fl)
    fs_cmd  = sprintf("ls %s", Pmodel)
    ok = FALSE
    ow = options("warn")
    try({ 
        ls_stat = system(fs_cmd) 
        if ( ls_stat == 0 ) ok = TRUE
        print(sprintf("CHECKING FOR EXISTING MODEL: %s (%s --> %s) MODEL EXISTS? = %s", Pmodel, fs_cmd, ls_stat, ok))
        }, silent=TRUE)
    options(ow)
    return(ok)
}
# ############################################################################################


# ############################################################################################
CLT_AWS_PROB_ENHANCER = function( yp_probs, NVARS=24 ) {
    WHICH_COLUMNS = minus( ACTIVATED_COLNAMES, c('origin'))
    for ( cname in WHICH_COLUMNS ) { 
        yp_probs = CLT_AWS_INDIVIDUALIZED_PREDICTOR( ORIG_XTEST, yp_probs, cname=cname, CVTEST=TRUE )
    }
    return ( yp_probs )
}
# ############################################################################################


# ############################################################################################
CLT_AWS_INDIVIDUALIZED_PREDICTOR = function( XT, yp_probs, cname='', defaultprob=0.175, CVTEST=TRUE ) {
    FL = levels(ORIG_XTRAIN[, cname])
    ow = options("warn")
    dirty_bit = FALSE

    Pmodels = system('ls P*RData', intern=TRUE)
    w = grep( cname, Pmodels)

    lyp_probs = yp_probs
    lyp_probs[,1] = 1

    for ( fl in FL ) {
        if ( length(w)==0) next 
        if ( length(Pmodels) == 0) next
        if ( length(Pmodels[w]) == 0) next

        which_one = character(0)
        which_one = grep( fl, Pmodels[w])
        if ( length(which_one)== 0 ) next
        which_one = Pmodels[w[which_one]]

        SUBSET = which( as.character(XT[, cname]) == fl)
        P = sprintf("P%s-%s.RData", cname, fl )
        print ( sprintf( "FACTOR MODEL [%s] ITER [%s] EVAL [%s] LEVEL [%s] N [%s]", P, iter, cname, as.character(fl), length(SUBSET)) )
        if ( length(SUBSET) > CLT_OPTIONS$min_subset_size_to_predict_on ) {
            try( {
                # PROB_CORRECTION_MODEL = LOAD_FACTORLEVEL_MODEL( cname, fl )
                load(file=sprintf("P%s-%s.RData", cname, fl )) 
                XNAMES = names(attr(PROB_CORRECTION_MODEL,"xlevels"))
                for ( xvar in XNAMES ) {
                    print( xvar )
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
                # ============================================================================
                #           VERY IMPORTANT: CONTINUOUS REFINEMENT SMOOTHER 
                #           VERY IMPORTANT: CONTINUOUS REFINEMENT SMOOTHER 
                # ============================================================================
                    a1 = lyp_probs[SUBSET,2] + dt_probs[,2]
                    b1 = lyp_probs[SUBSET,1] + 1
                    yp = data.frame( b1, a1 ); colnames(yp) = colnames(yp_probs)
                    lyp_probs[SUBSET,] = as.matrix(yp)
                    D = data.frame( lyp_probs[SUBSET,2], yp_probs[SUBSET,2], lyp_probs[SUBSET,2] - yp_probs[SUBSET,2] )
                    colnames(D) = c("MODF", "ORIG", "DIFF")
                    cat(HEADER); print( summary( D  ) )
                    dirty_bit = TRUE
                # ============================================================================
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

    # ============================================================================
      a1 = lyp_probs[,2]/(lyp_probs[,1])
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

    if ( CVTEST ) { 
        if ( dirty_bit ) {
            llval = COMPUTE_LOG_LOSS(XT$click, yp_probs[,2])
            BANNER( sprintf( "DONE AT ITER [%s], FACTOR MODEL YIELDING [%s] LOGLOSS = [%s] (WAS IT APPLIED?: %s)", iter, cname, llval, dirty_bit) )
        }
    }
    options(ow)
    return ( yp_probs )
}
# ############################################################################################


# ############################################################################################
MIXER = function( h1, h2, h3, h4, h5, h6, h7, h8, h9, h0, idxs ) {
    w = c()
    if ( 0 %in% idxs ) { w = append(w,h0) }
    if ( 1 %in% idxs ) { w = append(w,h1) }
    if ( 2 %in% idxs ) { w = append(w,h2) }
    if ( 3 %in% idxs ) { w = append(w,h3) }
    if ( 4 %in% idxs ) { w = append(w,h4) }
    if ( 5 %in% idxs ) { w = append(w,h5) }
    if ( 6 %in% idxs ) { w = append(w,h6) }
    if ( 7 %in% idxs ) { w = append(w,h7) }
    if ( 8 %in% idxs ) { w = append(w,h8) }
    if ( 9 %in% idxs ) { w = append(w,h9) }
    # w = unique(round(w,12))
    w = sum(w)/as.numeric(length(w))
    if (is.na(w)) w = 0
    return ( w )
}
# ############################################################################################


# ############################################################################################
MIX = function( D1, D2, D3, D4, D5, D6, D7, D8, D9, YP, EPS=-0.000, TRAIN=FALSE ) {
    BANNER('PROBABILITY MIXTURE')

    H0 = YP[,2]; H1 = D1[,2]; H2 = D2[,2]; H3 = D3[,2]; H4 = D4[,2]; H5 = D5[,2]; H6 = D6[,2]; H7 = D7[,2]; H8 = D8[,2]; H9 = D9[,2]

    if ( FALSE ) {
        n = length(H0)
        HOPT9 = VECTOR(n); for (i in 1:n) HOPT9[i] = MIXER( H1[i],H2[i],H3[i],H4[i],H5[i],H6[i],H7[i],H8[i],H9[i],H0[i], c(1,3,9) )
        HOPT8 = VECTOR(n); for (i in 1:n) HOPT8[i] = MIXER( H1[i],H2[i],H3[i],H4[i],H5[i],H6[i],H7[i],H8[i],H9[i],H0[i], c(1,3,4,7,9) )
        HOPT  = VECTOR(n); for (i in 1:n) HOPT [i] = MIXER( H1[i],H2[i],H3[i],H4[i],H5[i],H6[i],H7[i],H8[i],H9[i],H0[i], c(1,3,4,6,8,9) )
        HOPT0 = VECTOR(n); for (i in 1:n) HOPT0[i] = MIXER( H1[i],H2[i],H3[i],H4[i],H5[i],H6[i],H7[i],H8[i],H9[i],H0[i], c(1,3,4,5,7,8,9) )
        HOPT1 = VECTOR(n); for (i in 1:n) HOPT1[i] = MIXER( H1[i],H2[i],H3[i],H4[i],H5[i],H6[i],H7[i],H8[i],H9[i],H0[i], c(1,2,3,4,6,7,8,9) )
        HOPT2 = VECTOR(n); for (i in 1:n) HOPT2[i] = MIXER( H1[i],H2[i],H3[i],H4[i],H5[i],H6[i],H7[i],H8[i],H9[i],H0[i], c(1,2,3,4,6,7,8,9,0) )
        HOPT7 = VECTOR(n); for (i in 1:n) HOPT7[i] = MIXER( H1[i],H2[i],H3[i],H4[i],H5[i],H6[i],H7[i],H8[i],H9[i],H0[i], c(1,3,4,7,8,9) )
        HOPTA = VECTOR(n); for (i in 1:n) HOPTA[i] = MIXER( H1[i],H2[i],H3[i],H4[i],H5[i],H6[i],H7[i],H8[i],H9[i],H0[i], c(1,2,3,4,9) )
        HOPTB = VECTOR(n); for (i in 1:n) HOPTB[i] = MIXER( H1[i],H2[i],H3[i],H4[i],H5[i],H6[i],H7[i],H8[i],H9[i],H0[i], c(1,2,3,4,5,6,7,8,9) )
    } else {
        HOPT9=(H1+H3+H9)/3.
        HOPT8=(H1+H3+H4+H7+H9)/5.
        HOPT =(H1+H3+H4+H6+H8+H9)/6.
        HOPT0=(H1+H5+H3+H4+H7+H8+H9)/7.
        HOPT1=(H1+H2+H3+H4+H6+H7+H8+H9)/8.
        HOPT2=(H1+H2+H3+H4+H6+H7+H8+H9+H0)/9.
        HOPT7=(H1+H3+H4+H7+H8+H9)/6.
        HOPTA=(H1+H3+H4+H2+H9)/5.
        HOPTB=(H1+H2+H3+H4+H5+H6+H7+H8+H9)/9.
    }

    DT =  0.05*H1 + 0.05*H2 + 0.05*H3 + 0.05*H4 + 0.05*H5 + 0.05*H6 + 0.05*H7 + 0.05*H8 + 0.05*H9 + 
                 0.05*(H3+H9)/2. + 0.05*(H1+H3)/2. + 0.15*HOPT + 0.30*HOPT1

    HOPT3=(H1+H2+H3+H4+H6+H7+H8+H9+H0+DT)/10.
    HOPT4=(H1+H2+H3+H4+H6+H5+H7+H8+H9+H0+DT)/15. # diluted probability
    HOPT5=(H1+H2+H3+H4+H6+H5+H7+H8+H9+H0+DT)/11.
    HOPT6=(H1+H2+H3+H4+H6+H5+H7+H8+H9+DT)/9.  # inflated probability #HOPT6[HOPT6>1] = 0.999999999999999

    if ( CVTEST ) {
        # -------------------------------
        COMPUTE_LOG_LOSS( Y_TRUE, HOPTB )
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT ) 
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT0 )
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT1 )
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT2 )
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT3 )
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT4 )
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT5 )
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT6 )
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT7 )
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT8 ) 
        COMPUTE_LOG_LOSS( Y_TRUE, HOPT9 )
        COMPUTE_LOG_LOSS( Y_TRUE, HOPTA )
        # -------------------------------
        COMPUTE_LOG_LOSS( Y_TRUE, DT )
    }

    if ( TRUE ) {
        DT = HOPTB
        DT_PROBS = data.frame( 1.-DT, DT) 
        colnames(DT_PROBS) = colnames(D1)
        BANNER( "PROBABILITY CORRECTION DONE")
        return ( DT_PROBS )
    } else {
        #HA2CP1=A2C_PREDICT(TRAIN) # 0.5
        #HA2CP2=(HA2CP1+HOPT)/2. # 0.48
        source('clt_probability_predictor.R')
        return ( DT_PROBS )
    }
}
# ############################################################################################


# ############################################################################################
GET_COLS = function( i ) {
    cols = COLS(i)
    return ( cols )
}
# ############################################################################################


# ############################################################################################
SAMPLE = function ( C0, N, day=NA, withr=FALSE ) {
    if ( !withr )
        N = min( N, nrow(ORIG_XTRAIN)) 
    C0 = ifelse( C0 > 1, 0.5, C0 )
    N0 = as.integer(C0 * N)
    N1 = N - N0 
    W0 = which(ORIG_XTRAIN$click==0)
    W1 = which(ORIG_XTRAIN$click==1)
    if ( length(W0) < N0 ) withr=TRUE
    SD0 = sample( W0, N0, replace=withr )
    if ( length(W1) < N1 ) withr=TRUE
    SD1 = sample( W1, N1, replace=withr )
    SUBSET = c( SD0, SD1 )
    return ( SUBSET )
}
# ############################################################################################


# ############################################################################################
IS_VALID_DT_MODEL = function( PROB_CORRECTION_MODEL ) {
    if ( is.null(PROB_CORRECTION_MODEL) ) return (FALSE)
    if ( length(PROB_CORRECTION_MODEL) == 0 ) return (FALSE)
    if ( class(PROB_CORRECTION_MODEL) != "rpart" ) return (FALSE)
    if ( nrow(PROB_CORRECTION_MODEL$cptable) == 0 ) return (FALSE)
    if ( length(PROB_CORRECTION_MODEL$variable.importance) == 0 ) return (FALSE) 
    return ( TRUE )
}
# ############################################################################################


# ############################################################################################
BUILD_EMSEMBLE_MODEL = function( NUM, SUBSET, tree_controls, PRUNE=TRUE, FIT_METHOD="FIT", RESET=TRUE, CNAME="", NUMFSVARS=9 ) {
    if ( NUM > 0 ) {
        cols = GET_COLS(NUM)[GET_COLS(NUM) %in% colnames(ORIG_XTRAIN)]
        cols = setdiff( cols, CNAME )
        FORMULA = GET_FORMULAE(cols) ; cols = c('click', cols)
    } else {
        if ( TRUE ) {
            done = FALSE
            try( {
                BANNER("CUSTOMIZED MODEL BUILDING: CHISQ FSELECT")
                cols = GET_COLS(NUM)[GET_COLS(NUM) %in% colnames(ORIG_XTRAIN)]
                cols = setdiff( cols, CNAME )
                cols = c('click', cols)

                Y = sapply( cols, function(x) as.factor(ORIG_XTRAIN[SUBSET,x]))
                Y = as.data.frame(Y)
 
                NMAX = CLT_OPTIONS$'max number of samples per cust. model'
                FSELECT_RETVALS = DO_GENERAL_SUBSET_SELECTION( Y, dfname="CLT, C:F",
                                    using_approach=chi.squared,
                                    approach_ppname="chi.squared", 
                                    rtypes="COMPLETECASES",
                                    target_var="click",
                                    top=NUMFSVARS, refine=TRUE, #nmax=NMAX, 
                                    cmax=0.8 )
                cols    = FSELECT_RETVALS[[3]]
                FORMULA = FSELECT_RETVALS[[1]]
                FORMULA = GET_FORMULAE(cols) ; cols = c('click', cols)
                print( FORMULA)
                done = TRUE
            } )
            if ( !done | length(cols)==0 ) {
                # if an exception happens above, default to known fselect features
                DNUM = CLT_OPTIONS$'use which model as default model'
                cols = GET_COLS(DNUM)[GET_COLS(DNUM) %in% colnames(ORIG_XTRAIN)]
                cols = setdiff( cols, CNAME )
                FORMULA = GET_FORMULAE(cols) ; cols = c('click', cols)
            }
        } else {
            # entropy/correlation xvar wrt click 
            cols = cfs( click~., ORIG_XTRAIN[SUBSET,])
            cols = setdiff( cols, CNAME )
            FORMULA = GET_FORMULAE(cols) ; cols = c('click', cols)
        }
    }

    cat(HEADER); print(cols)
    cat(HEADER); print(unlist(tree_controls))
    cat(HEADER)

    if( RESET ) sink( sprintf("output_rpart_tree_model%s_summary.out", NUM), split=FALSE)
        if (FIT_METHOD == "FIT")
            PROB_CORRECTION_MODEL = FIT_DECISION_TREE( ORIG_XTRAIN[SUBSET,cols], 
                                                   SLICE_DATAFRAME(ORIG_XTRAIN[SUBSET,],PREDICT_COL), 
                                                   FORMULA=FORMULA, 
                                                   DO_PRUNING=PRUNE, # was set TRUE before 
                                                   control=tree_controls)
        if (FIT_METHOD == "RPART")
            PROB_CORRECTION_MODEL = rpart(FORMULA, ORIG_XTRAIN[SUBSET,], control=tree_controls)
        if (FIT_METHOD == "OPTIM")
            PROB_CORRECTION_MODEL = OPTIMIZE_DT_PREDICTOR( sprintf("M%s",NUM), SUBSET, MINBCKT, 0.40, PRUNE, CP, fl="" )
            #PROB_CORRECTION_MODEL = OPTIMIZE_DT_PREDICTOR( '', SUBSET, MINBCKT, 0.40, PRUNE, CP, fl="" )
    if ( RESET ) sink()

    if( !IS_VALID_DT_MODEL(PROB_CORRECTION_MODEL) ) return ()

    cat(HEADER); gc(TRUE); cat(HEADER)
    if ( PRUNE ) PROB_CORRECTION_MODEL <<- prune( PROB_CORRECTION_MODEL, cp=PROB_CORRECTION_MODEL$cptable[which.min(PROB_CORRECTION_MODEL$cptable[,"xerror"]),"CP"])

    if (CLT_OPTIONS$print_rje_computed_model) 
        print(PROB_CORRECTION_MODEL)

    BANNER("GENERATED MODEL STATS")
    cprows = c(1, which.min(PROB_CORRECTION_MODEL$cptable[,"xerror"]), nrow(PROB_CORRECTION_MODEL$cptable))
    cat(HEADER); print(PROB_CORRECTION_MODEL$cptable[cprows,])
    cat(HEADER); print(PROB_CORRECTION_MODEL$variable.importance)
    cat(HEADER)
    if ( NUM > 0 ) {
        yp = predict(PROB_CORRECTION_MODEL, ORIG_XTRAIN, type='prob')
        ll = COMPUTE_LOG_LOSS(ORIG_XTRAIN[,PREDICT_VAR], yp[,2])
        cat(HEADER)
    }

    if ( RESET ) {
        if ( NUM == 1 ) PROB_CORRECTION_MODEL_1 <<- PROB_CORRECTION_MODEL
        if ( NUM == 2 ) PROB_CORRECTION_MODEL_2 <<- PROB_CORRECTION_MODEL
        if ( NUM == 3 ) PROB_CORRECTION_MODEL_3 <<- PROB_CORRECTION_MODEL
        if ( NUM == 4 ) PROB_CORRECTION_MODEL_4 <<- PROB_CORRECTION_MODEL
        if ( NUM == 5 ) PROB_CORRECTION_MODEL_5 <<- PROB_CORRECTION_MODEL
        if ( NUM == 6 ) PROB_CORRECTION_MODEL_6 <<- PROB_CORRECTION_MODEL
        if ( NUM == 7 ) PROB_CORRECTION_MODEL_7 <<- PROB_CORRECTION_MODEL
        if ( NUM == 8 ) PROB_CORRECTION_MODEL_8 <<- PROB_CORRECTION_MODEL
        if ( NUM == 9 ) PROB_CORRECTION_MODEL_9 <<- PROB_CORRECTION_MODEL
    } else {
        return (PROB_CORRECTION_MODEL)
    }

    BANNER("STORING MODEL")
    save(PROB_CORRECTION_MODEL, file=sprintf("B%s.RData", NUM ))

    return ( PROB_CORRECTION_MODEL )
}
# ############################################################################################


# ############################################################################################
GET_TOP_ATTRIBUTES = function( DT_MODEL, THRESHOLD=49 ) {
    A = DT_MODEL$variable.importance
    A = A[A > THRESHOLD]
    DTVARS = attr(A,"names") 
    # DTFORM = GET_FORMULAE( DTVARS )
    return (DTVARS )
}
# ############################################################################################


# ############################################################################################
GET_VALID_TRAINSET = function( SUBSET, PERCENTAGE=CLT_OPTIONS$factorlevel_training_percentage_split) {
     TRAIN_SUBSET = sample(SUBSET, as.integer(length(SUBSET) * PERCENTAGE))
     while ( length( unique(ORIG_XTRAIN[TRAIN_SUBSET,"click"])) == 1 ) 
         TRAIN_SUBSET = sample(SUBSET, as.integer(length(SUBSET) * PERCENTAGE))
     return ( TRAIN_SUBSET )
}
# ############################################################################################


# ############################################################################################
BUILD_RANDOM_FOREST_MODEL = function( NUM=0, SUBSET=c(), NTREE=512, MINBCKT=11, MTRY=1, IMPORTANCE=TRUE, FL="" ) {
    cols = GET_COLS( NUM );
    PROB_CORRECTION_MODEL = randomForest( ORIG_XTRAIN[SUBSET, cols], y=ORIG_XTRAIN[SUBSET, PREDICT_VAR], ntree=NTREE, mtry=MTRY, nodesize=MINBCKT, importance=IMPORTANCE)
    print ( importance( PROB_CORRECTION_MODEL ) )
    if ( FL != "" ) {
        save(PROB_CORRECTION_MODEL, file=sprintf("P5-%s.RData", FL ))
    }
    yp = predict(PROB_CORRECTION_MODEL, ORIG_XTRAIN, type='prob')
    COMPUTE_LOG_LOSS(ORIG_XTRAIN[,PREDICT_VAR], yp[,2])
    return ( PROB_CORRECTION_MODEL )
}
# ############################################################################################


# ############################################################################################
# need to tally train vs cv to know SUBSEQ which models to drop or could take a cv test slice 
# to do so during training
# ############################################################################################
OPTIMIZE_DT_PREDICTOR = function( cname, SUBSET, MINBCKT, UNDERFIT_THRESHOLD, DO_EXTRA_PRUNING, CP, fl="" ) {
    NMAX = CLT_OPTIONS$'max number of samples per cust. model'

    BANNER( sprintf( "FACTOR [%s] LEVEL [%s] N [%s]", cname, as.character(fl), length(SUBSET)) )

    OPTIMIZATION_ABORT_THRESHOLD = CLT_OPTIONS$logloss_threshold_to_abort_optimizing

    TEST_SUBSET  = SUBSET 

    EXPLORATION_MODELS = CLT_OPTIONS$'exploration_depths_for_optimization'

    llvals = list()
    for ( stump_depth in EXPLORATION_MODELS ) {
        cat(HEADER)
        tree_controls = rpart.control(xval=10, maxdepth =as.integer(stump_depth), 
                                               minbucket=as.integer(MINBCKT*1.0), 
                                               minsplit =as.integer(MINBCKT*2.0), 
                                               cp=CP, 
                                               split=SPLIT_CRITERION)
        cat(HEADER); print(unlist(tree_controls))

        PMODEL = NA
        sink('tmp', split=CLT_OPTIONS$display_intermediary_trees)
        try({ 
            TRAIN_SUBSET = GET_VALID_TRAINSET( SUBSET, PERCENTAGE=CLT_OPTIONS$'factorlevel_training_percentage_split' )
            TRAIN_SUBSET = sample( TRAIN_SUBSET, min(NMAX, length(TRAIN_SUBSET)))
            PMODEL = BUILD_EMSEMBLE_MODEL(0, TRAIN_SUBSET, tree_controls, PRUNE=DO_EXTRA_PRUNING, FIT_METHOD="FIT", RESET=FALSE, CNAME=cname)
            })
        sink()

        if ( !IS_VALID_DT_MODEL(PMODEL) ) next 

        llvals[[stump_depth]] = NA
        try( { 
            cprows = c(1, which.min(PMODEL$cptable[,"xerror"]), nrow(PMODEL$cptable))
            cat(HEADER); print(PMODEL$cptable[cprows,])
            cat(HEADER); print(PMODEL$variable.importance)
            cat(HEADER)

            ll = 0
            N = as.integer(min(NMAX,length(TEST_SUBSET)) * 0.5)
            for( i in 1:CLT_OPTIONS$'num_crossval_folds_during_training' ) {
                W = sample(TEST_SUBSET,N)
                yp = predict(PMODEL, ORIG_XTRAIN[W,], type='prob')
                ll =  ll + COMPUTE_LOG_LOSS(ORIG_XTRAIN[W, PREDICT_VAR], yp[,2])
            }
            ll = ll / as.numeric(CLT_OPTIONS$'num_crossval_folds_during_training')
            print( sprintf("MEAN (CV LOGLOSS) = %s", ll))
            llvals[[stump_depth]] = ll

            if ( ll > OPTIMIZATION_ABORT_THRESHOLD ) {  
                 print(unlist(llvals))
                 return ()
            }

            #delta = CLT_OPTIONS$'logloss_threshold_to_abort_optimizing' - CLT_OPTIONS$'max_acceptable_training_logloss'
            #mid_checkpoint = CLT_OPTIONS$'max_acceptable_training_logloss' + delta
            #if ( stump_depth > 18 & ll > mid_checkpoint ) {  
                 #print(unlist(llvals))
                 #return ()
            #}

            if ( ll < UNDERFIT_THRESHOLD ) {                          
                 print ( sprintf( "OPTIMIZED FACTOR MODEL [%s] LEVEL [%s] N [%s] TREE_DEPTH [%s] LOGLOSS = [%s]", 
                                   cname, as.character(fl), length(SUBSET), stump_depth, ll) )
                 print(unlist(llvals))
                 return ( PMODEL )
            }

        } )
    }
    print(unlist(llvals))
    return ()
}
# ############################################################################################


# ############################################################################################
# Individualized Predictor
# ############################################################################################
INDIVIDUALIZED_PREDICTOR_BUILDER = function(cname='', CP=1E-6, NMAX=512000, MINBCKT=11, NTREE=511, DO_EXTRA_PRUNING=FALSE, ALGORITHM="DT" ) {
    BMIN = CLT_OPTIONS$min_subset_size_to_train_on
    UNDERFIT_THRESHOLD = CLT_OPTIONS$max_acceptable_training_logloss

    cat(HEADER)
    cat(HEADER)

    PROB_CORRECTION_MODELS = c()
    FL = table(ORIG_XTRAIN[, cname])
    w  = which( FL>BMIN )
    FL = names(FL[w])
    for ( fl in FL ) {
        print ( fl )
        SUBSET = which( as.character(ORIG_XTRAIN[, cname]) == fl )
        if ( length(SUBSET) < BMIN )  next
        if ( length( unique(ORIG_XTRAIN[SUBSET,"click"])) == 1 ) next
        if ( !CLT_OPTIONS$'rebuild_models_already_existing' ) {
            if (DOES_MODEL_EXIST_FOR(cname, fl)) next
        }

        if ( ALGORITHM == "DT" ) {
            PROB_CORRECTION_MODEL = OPTIMIZE_DT_PREDICTOR( cname, SUBSET, MINBCKT, 
                                            UNDERFIT_THRESHOLD,
                                            DO_EXTRA_PRUNING, 
                                            CP, 
                                            fl=fl )
            if (!is.null(PROB_CORRECTION_MODEL)) {      #if( !IS_VALID_DT_MODEL(PROB_CORRECTION_MODEL)) {
                save(PROB_CORRECTION_MODEL, file=sprintf("P%s-%s.RData", cname, fl ))
                STORE_MODEL_FOR(cname, fl)
                fluniqname = sprintf( "P%s-%s.RData", cname, fl )
                PROB_CORRECTION_MODELS = c(PROB_CORRECTION_MODELS, fluniqname)
            }
        } else if ( ALGORITHM == "RF" ) {
            PMODEL = BUILD_RANDOM_FOREST_MODEL( NUM=0, SUBSET=SUBSET, NTREE=NTREE, MINBCKT=MINBCKT, MTRY=2, IMPORTANCE=TRUE, FL=fl )
            yp = predict(PMODEL, ORIG_XTRAIN[SUBSET,], type='prob')
            ll = COMPUTE_LOG_LOSS(ORIG_XTRAIN[TEST_SUBSET, PREDICT_VAR], yp[,2])
            if ( ll < UNDERFIT_THRESHOLD ) PROB_CORRECTION_MODELS[[fl]] = fl 
        }
    }
    return ( PROB_CORRECTION_MODELS )
}
# ############################################################################################


# ############################################################################################
LOAD_FACTORLEVEL_MODEL = function( cname, fl ) {
    load(file=sprintf("P%s-%s.RData", cname, fl )) 
    XNAMES = names(attr(PROB_CORRECTION_MODEL,"xlevels"))
    for ( xvar in XNAMES ) {
        A = attr(PROB_CORRECTION_MODEL,"xlevels")$xvar 
        B = levels(ORIG_XTEST$xvar)
        C = setdiff(A, B)
        D = setdiff(B, A)
        if ( length(C)>0  | length(D)>0 ) {
            print( xvar)
            attr(PROB_CORRECTION_MODEL,"xlevels")$xvar = union(A, B)
            # levels(ORIG_XTEST$xvar) <<- union(A, B)
            L = union(A, B)
        }
    }
    return( list(P=PROB_CORRECTION_MODEL, L=L))
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
      }
      if( CVTEST ) {
          llval_modf = COMPUTE_LOG_LOSS(XT$click, yp[,2])
          llval_orig = COMPUTE_LOG_LOSS(XT$click, yp_probs[,2])
          num = GET_MODEL_NUMBER_FOR( cname )
          PROB_METRICS_VECTOR[num] <<- llval_modf
      }
      yp_probs = as.matrix(yp)
      cat(HEADER)
    # ============================================================================
    return ( yp_probs )
}
# ############################################################################################


# ############################################################################################
PROBABILITY_ENHANCER_PREDICTOR = function( MODELNUM=1, TRAIN=FALSE ) {
    cols = GET_COLS( MODELNUM )

    PREDICTOR = CLT_OPTIONS$'predictors'[MODELNUM]

    if ( MODELNUM == 1 )
            if ( PREDICTOR !='' & CLT_OPTIONS$'use individualized predictor' )  
                DT_PROBS_0 = INDIVIDUALIZED_PREDICTOR(PROB_CORRECTION_MODEL_1, ORIG_XTEST, cname=PREDICTOR)
            else  
                DT_PROBS_0 = predict(PROB_CORRECTION_MODEL_1, ORIG_XTEST[,cols], type="prob" ) 
    if ( MODELNUM == 2 )
            if ( PREDICTOR !='' & CLT_OPTIONS$'use individualized predictor' )  
                DT_PROBS_0 = INDIVIDUALIZED_PREDICTOR(PROB_CORRECTION_MODEL_2, ORIG_XTEST, cname=PREDICTOR)
            else  
                DT_PROBS_0 = predict(PROB_CORRECTION_MODEL_2, ORIG_XTEST[,cols], type="prob" ) 
    if ( MODELNUM == 3 )
            if ( PREDICTOR !='' & CLT_OPTIONS$'use individualized predictor' )  
                DT_PROBS_0 = INDIVIDUALIZED_PREDICTOR(PROB_CORRECTION_MODEL_3, ORIG_XTEST, cname=PREDICTOR)
            else  
                DT_PROBS_0 = predict(PROB_CORRECTION_MODEL_3, ORIG_XTEST[,cols], type="prob" ) 
    if ( MODELNUM == 4 )
            if ( PREDICTOR !='' & CLT_OPTIONS$'use individualized predictor' )  
                DT_PROBS_0 = INDIVIDUALIZED_PREDICTOR(PROB_CORRECTION_MODEL_4, ORIG_XTEST, cname=PREDICTOR)
            else  
                DT_PROBS_0 = predict(PROB_CORRECTION_MODEL_4, ORIG_XTEST[,cols], type="prob" ) 
    if ( MODELNUM == 5 )
            if ( PREDICTOR !='' & CLT_OPTIONS$'use individualized predictor' )  
                DT_PROBS_0 = INDIVIDUALIZED_PREDICTOR(PROB_CORRECTION_MODEL_5, ORIG_XTEST, cname=PREDICTOR)
            else  
                DT_PROBS_0 = predict(PROB_CORRECTION_MODEL_5, ORIG_XTEST[,cols], type="prob" ) 
    if ( MODELNUM == 6 )
            if ( PREDICTOR !='' & CLT_OPTIONS$'use individualized predictor' )  
                DT_PROBS_0 = INDIVIDUALIZED_PREDICTOR(PROB_CORRECTION_MODEL_6, ORIG_XTEST, cname=PREDICTOR)
            else  
                DT_PROBS_0 = predict(PROB_CORRECTION_MODEL_6, ORIG_XTEST[,cols], type="prob" ) 
    if ( MODELNUM == 7 )
            if ( PREDICTOR !='' & CLT_OPTIONS$'use individualized predictor' )  
                DT_PROBS_0 = INDIVIDUALIZED_PREDICTOR(PROB_CORRECTION_MODEL_7, ORIG_XTEST, cname=PREDICTOR)
            else  
                DT_PROBS_0 = predict(PROB_CORRECTION_MODEL_7, ORIG_XTEST[,cols], type="prob" ) 
    if ( MODELNUM == 8 )
            if ( PREDICTOR !='' & CLT_OPTIONS$'use individualized predictor' )  
                DT_PROBS_0 = INDIVIDUALIZED_PREDICTOR(PROB_CORRECTION_MODEL_8, ORIG_XTEST, cname=PREDICTOR)
            else  
                DT_PROBS_0 = predict(PROB_CORRECTION_MODEL_8, ORIG_XTEST[,cols], type="prob" ) 
    if ( MODELNUM == 9 )
            if ( PREDICTOR !='' & CLT_OPTIONS$'use individualized predictor' )  
                DT_PROBS_0 = INDIVIDUALIZED_PREDICTOR(PROB_CORRECTION_MODEL_9, ORIG_XTEST, cname=PREDICTOR)
            else  
                DT_PROBS_0 = predict(PROB_CORRECTION_MODEL_9, ORIG_XTEST[,cols], type="prob" ) 
    # ############################################################################################

    return ( DT_PROBS_0 )
}
# ############################################################################################


# ############################################################################################
RANDOM_FOREST_METRICS = function( PROB_CORRECTION_MODEL ) {
    t1 = treesize( PROB_CORRECTION_MODEL )
    print (t1 )
    print ( importance( PROB_CORRECTION_MODEL ) )
    return(t1)
}
# ############################################################################################


# ############################################################################################
GET_GLOBAL_MODEL_FOR = function( NUM ) {
    PROB_CORRECTION_MODEL = NA
    if ( NUM == 1 ) PROB_CORRECTION_MODEL = PROB_CORRECTION_MODEL_1
    if ( NUM == 2 ) PROB_CORRECTION_MODEL = PROB_CORRECTION_MODEL_2
    if ( NUM == 3 ) PROB_CORRECTION_MODEL = PROB_CORRECTION_MODEL_3
    if ( NUM == 4 ) PROB_CORRECTION_MODEL = PROB_CORRECTION_MODEL_4
    if ( NUM == 5 ) PROB_CORRECTION_MODEL = PROB_CORRECTION_MODEL_5
    if ( NUM == 6 ) PROB_CORRECTION_MODEL = PROB_CORRECTION_MODEL_6
    if ( NUM == 7 ) PROB_CORRECTION_MODEL = PROB_CORRECTION_MODEL_7
    if ( NUM == 8 ) PROB_CORRECTION_MODEL = PROB_CORRECTION_MODEL_8
    if ( NUM == 9 ) PROB_CORRECTION_MODEL = PROB_CORRECTION_MODEL_9
    return ( PROB_CORRECTION_MODEL )
}
# ############################################################################################


# ############################################################################################
CREATE_DEFAULT_MODELS = function( MAXDEPTH=NA, MINBCKT=NA, MINSPLT=NA, CP=NA, SPLIT_CRITERION=NA, DO_PRUNE=NA, FORCED=FALSE) {
    if (is.na(MAXDEPTH))        MAXDEPTH = 24
    if (is.na(MINBCKT))         MINBCKT  = 17
    if (is.na(MINSPLT))         MINSPLT  = as.integer(1.1*(MINBCKT+1))
    if (is.na(CP))              CP       = 1E-6
    if (is.na(SPLIT_CRITERION)) SPLIT_CRITERION = "information"
    if (is.na(DO_PRUNE))        DO_PRUNE = FALSE

    PREDICTORS = CLT_OPTIONS$'predictors'
    for (WHICH_MODEL in 1:length(PREDICTORS))  {
        if ( PREDICTORS[WHICH_MODEL] == "")  {
            if (FORCED | is.na(GET_GLOBAL_MODEL_FOR(WHICH_MODEL))) {
                CREATE_MODEL_FOR( WHICH_MODEL, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE )
            }
        }
    }
}
# ############################################################################################


# ############################################################################################
CREATE_MODEL_FOR = function( WHICH_MODEL, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE ) {
    PREDICTOR = CLT_OPTIONS$'predictors'[WHICH_MODEL]
    if (PREDICTOR!='' & CLT_OPTIONS$'use individualized predictor' ) {
        BANNER( sprintf( "EMSEMBLE-%s: INDIVIDUALIZED PREDICTOR TREE FOR: %s", WHICH_MODEL, PREDICTOR) )
        return (INDIVIDUALIZED_PREDICTOR_BUILDER(cname=PREDICTOR))
    } else {
        BANNER( sprintf("EMSEMBLE-%s: TREE (DAY < 7)", WHICH_MODEL  ) )
        tree_controls = rpart.control(xval=10, maxdepth=MAXDEPTH, minbucket=MINBCKT, minsplit=MINSPLT, cp=CP, split=SPLIT_CRITERION)
        SUBSET_W = GET_SUBSET( which( as.character(ORIG_XTRAIN[,'origin']) == 'W' ), 
                               NMAX=min(nrow(ORIG_XTRAIN), CLT_OPTIONS$'max number of samples per main model'))
        SUBSET = which( as.integer(ORIG_XTRAIN[SUBSET_W,'day']) <= 7 )
        return (BUILD_EMSEMBLE_MODEL( WHICH_MODEL, SUBSET, tree_controls, PRUNE=DO_PRUNE, FIT_METHOD="FIT" ))
    }
} 
# ############################################################################################


# ############################################################################################
PROBABILITY_ENHANCER_PREDICTOR_TRAINING = function() {
    NTREE    = 1600
    MAXDEPTH = 24
    MINBCKT  = 17
    MINSPLT  = as.integer(1.1*(MINBCKT+1))
    CP       = 1E-6
    DO_PRUNE = FALSE
    SPLIT_CRITERION = "information"


    ### MODEL ############################################################
    if ( NOT_A_SUBSUMED_MODEL(1) ) {
        PROB_CORRECTION_MODEL_1 <<- CREATE_MODEL_FOR( 1, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE )
    } else {
        #BANNER( "EMSEMBLE-1: RANDOM FOREST <= 3" )
        #BAL = 47/100 ; CLWT=c(0.47,0.53) ; NSZ = 10
        #cols = GET_COLS( 1 ) ; FORMULA = GET_FORMULAE(cols)
        #SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) <= 3 )
        #PROB_CORRECTION_MODEL_1 <<- randomForest( ORIG_XTRAIN[SUBSET,cols], y=YT[SUBSET], ntree=NTREE, mtry=1, nodesize=NSZ, subset=SUBSET, classwt=CLWT, importance=TRUE )
        #t1 = RANDOM_FOREST_METRICS( PROB_CORRECTION_MODEL_1 )
    }

    ### MODEL ############################################################
    if ( NOT_A_SUBSUMED_MODEL(2) ) {
        PROB_CORRECTION_MODEL_2 <<- CREATE_MODEL_FOR( 2, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE )
    } else {
        #BANNER( "EMSEMBLE-2: TREE ALL" )
        #cols = GET_COLS( 2 ) ; FORMULA = GET_FORMULAE(cols) ; cols = c('click', cols); print(cols)
        #BAL = 50/100 ; CP = 0.00000001; MINBCKT = 31
        #SUBSET = 1:nrow(ORIG_XTRAIN)
        #PROB_CORRECTION_MODEL_2 <<- rpart(FORMULA, ORIG_XTRAIN[SUBSET,], cp=CP, parms = list(prior = c(5/6,1/6), split = SPLIT_CRITERION), minbucket=MINBCKT)
        #PROB_CORRECTION_MODEL_2 <<- prune( PROB_CORRECTION_MODEL_2, cp=CP )
        #print(summary(print(PROB_CORRECTION_MODEL_2)))
    }

    ### MODEL ############################################################
    if ( NOT_A_SUBSUMED_MODEL(3) ) {
        PROB_CORRECTION_MODEL_3 <<- CREATE_MODEL_FOR( 3, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE )
    } else {
        #BANNER( "EMSEMBLE-3: RANDOM FOREST > 3" )
        #cols = GET_COLS( 3 ) ; FORMULA = GET_FORMULAE(cols)
        #BAL = 52/100 ; CLWT=c(0.50,0.50) ; NSZ = 11
        #SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) > 3 )
        #PROB_CORRECTION_MODEL_3 <<- randomForest( ORIG_XTRAIN[SUBSET,cols], y=YT[SUBSET], ntree=NTREE, mtry=1, nodesize=NSZ, subset=SUBSET, classwt=CLWT, importance=TRUE )
        #t3 = RANDOM_FOREST_METRICS( PROB_CORRECTION_MODEL_3 )
    }

    ### MODEL ############################################################
    if ( NOT_A_SUBSUMED_MODEL(4) ) {
        PROB_CORRECTION_MODEL_4 <<- CREATE_MODEL_FOR( 4, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE )
    } else {
        #BANNER( "EMSEMBLE-4: RANDOM FOREST <= 3" )
        #cols = GET_COLS( 4 ) ; FORMULA = GET_FORMULAE(cols)
        #BAL = 38/100 ; CLWT=c(0.49,0.51) ; NSZ = 11
        #SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) <= 3 )
        #PROB_CORRECTION_MODEL_4 <<- randomForest( ORIG_XTRAIN[SUBSET,cols], y=YT[SUBSET], ntree=NTREE, mtry=1, nodesize=NSZ, subset=SUBSET , classwt=CLWT, importance=TRUE )
        #t4 = RANDOM_FOREST_METRICS( PROB_CORRECTION_MODEL_4 )
    }

    ### MODEL ############################################################
    if ( NOT_A_SUBSUMED_MODEL(5) ) {
        PROB_CORRECTION_MODEL_5 <<- CREATE_MODEL_FOR( 5, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE )
    } else {
        #BANNER( "EMSEMBLE-5: RANDOM FOREST > 3" )
        #cols = GET_COLS( 5 ) ; FORMULA = GET_FORMULAE(cols)
        #BAL = 50/100 ; CLWT=c(0.50,0.50) ; NSZ = 15
        #SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) <= 3 )
        #PROB_CORRECTION_MODEL_5 <<- randomForest( ORIG_XTRAIN[SUBSET,cols], y=YT[SUBSET], ntree=NTREE, mtry=2, nodesize=NSZ, subset=SUBSET, classwt=CLWT, importance=TRUE )
        #t5 = RANDOM_FOREST_METRICS( PROB_CORRECTION_MODEL_5 )
    }

    ### MODEL ############################################################
    if ( NOT_A_SUBSUMED_MODEL(6) ) {
        PROB_CORRECTION_MODEL_6 <<- CREATE_MODEL_FOR( 6, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE )
    } else {
        #BANNER( "EMSEMBLE-6: TREE: DAY <= 3" )
        #cols = GET_COLS( 6 ) ; FORMULA = GET_FORMULAE(cols) ; cols = c('click', cols); print(cols)
        #BAL = 50/100 ; CP = 0.00000005; MINBCKT = 28
        #SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) %in% c(6,0,1,2) )
        #PROB_CORRECTION_MODEL_6 <<- FIT_DECISION_TREE( ORIG_XTRAIN[SUBSET,cols], SLICE_DATAFRAME(ORIG_XTRAIN[SUBSET,],PREDICT_COL), FORMULA=FORMULA, DO_PRUNING=TRUE, cp=CP, minbucket=MINBCKT)
    }

    ### MODEL ############################################################
    if ( NOT_A_SUBSUMED_MODEL(7) ) {
        PROB_CORRECTION_MODEL_7 <<- CREATE_MODEL_FOR( 7, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE )
    } else {
        #BANNER( "EMSEMBLE-7: RANDOM FOREST > 3" )
        #cols = GET_COLS( 7 ) ; FORMULA = GET_FORMULAE(cols)
        #BAL = 40/100 ; CLWT=c(0.50,0.50) ; NSZ = 11
        #SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) > 3 )
        #PROB_CORRECTION_MODEL_7 <<- randomForest( ORIG_XTRAIN[SUBSET,cols], y=YT[SUBSET], ntree=NTREE, mtry=2, nodesize=NSZ, subset=SUBSET, classwt=CLWT, importance=TRUE )
        #t7 = RANDOM_FOREST_METRICS( PROB_CORRECTION_MODEL_7 )
    }

    ### MODEL ############################################################
    if ( NOT_A_SUBSUMED_MODEL(8) ) {
        PROB_CORRECTION_MODEL_8 <<- CREATE_MODEL_FOR( 8, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE )
    } else {
        #BANNER( "EMSEMBLE-8: TREE DAY <= 3" )
        #cols = GET_COLS( 8 ) ; FORMULA = GET_FORMULAE(cols) ; cols = c('click', cols); print(cols)
        #MINBCKT = 21
        #SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) <= 3 )
        #tree_controls = rpart.control(xval=10, minbucket=MINBCKT, minsplit=as.integer(MINBCKT*1.8), prior=c(5/6,1/6), cp=CP, split=SPLIT_CRITERION)
        #PROB_CORRECTION_MODEL_8 <<- BUILD_EMSEMBLE_MODEL( 8, SUBSET, tree_controls, PRUNE=TRUE, FIT_METHOD="RPART" )
    }

    ### MODEL ############################################################
    if ( NOT_A_SUBSUMED_MODEL(9) ) {
        PROB_CORRECTION_MODEL_9 <<- CREATE_MODEL_FOR( 9, MAXDEPTH, MINBCKT, MINSPLT, CP, SPLIT_CRITERION, DO_PRUNE )
    } else {
        #BANNER( "EMSEMBLE-9: RANDOM FOREST <= 3" )
        #cols = GET_COLS( 9 ) ; FORMULA = GET_FORMULAE(cols)
        #BAL = 50/100 ; CLWT=c(0.49,0.51) ; NSZ = 11 
        #SUBSET = which( as.integer(ORIG_XTRAIN[,'day']) > 3 )
        #PROB_CORRECTION_MODEL_9 <<- randomForest( ORIG_XTRAIN[SUBSET,cols], y=YT[SUBSET], ntree=NTREE, mtry=2, nodesize=NSZ, subset=SUBSET, classwt=CLWT, importance=TRUE )
        #t9 = RANDOM_FOREST_METRICS( PROB_CORRECTION_MODEL_9 )
    }

    SUBSUME_MODELS()

    return ()
}
# ############################################################################################


# ############################################################################################
PRINT_LL = function( LABEL, YT, DT ) {
    M0 = capture.output( round(COMPUTE_LOG_LOSS( YT, DT ),4))
    idx = regexpr( " ([0-9 ].[0-9]+)", M0 )
    len = attr(idx, "match.length")
    logloss = as.numeric(substr(M0,idx,idx+len-1))[1]

    if      ( LABEL == "YP" ) num = 10
    else if ( LABEL == "M1" ) num = 1
    else if ( LABEL == "M2" ) num = 2
    else if ( LABEL == "M3" ) num = 3
    else if ( LABEL == "M4" ) num = 4
    else if ( LABEL == "M5" ) num = 5
    else if ( LABEL == "M6" ) num = 6
    else if ( LABEL == "M7" ) num = 7
    else if ( LABEL == "M8" ) num = 8
    else if ( LABEL == "M9" ) num = 9
    else if ( LABEL == "DT" ) num = 11
    else if ( LABEL == "YF" ) num = 12
    else paste( "UNKNOWN LABEL", logloss)
    PROB_METRICS_VECTOR[num] <<- logloss
    
    M1 = sprintf( "%s: %s", LABEL, substr(M0,22,27) )
    print( M1 )
    return ( M1 )
}
# ############################################################################################


# ############################################################################################
DO_COMPARATIVE_HISTOGRAM = function( YT, YP, DT_PROBS_1, DT_PROBS_2, DT_PROBS_3, DT_PROBS_4, DT_PROBS_5, DT_PROBS_6, DT_PROBS_7, DT_PROBS_8, DT_PROBS_9, DT_PROBS, YP2, PLOT=TRUE ) {
    BANNER( "COMPARATIVE SUMMARY OF GENERATED PROBABILITIES" )
    #print( summary( data.frame( YP=YP[,2], D1=DT_PROBS_1[,2], D2=DT_PROBS_2[,2], D3=DT_PROBS_3[,2], D4=DT_PROBS_4[,2], D5=DT_PROBS_5[,2], D6=DT_PROBS_6[,2], D7=DT_PROBS_7[,2], D8=DT_PROBS_8[,2], D9=DT_PROBS_9[,2], DT=DT_PROBS[,2], YP2=YP2[,2] ) ) )

    BANNER( "MODELING LOG LOSS" )
    MP = PRINT_LL( "YP", YT, YP[,2] )
    cat(HEADER)
    M1 = PRINT_LL( "M1", YT, DT_PROBS_1[,2] )
    M2 = PRINT_LL( "M2", YT, DT_PROBS_2[,2] )
    M3 = PRINT_LL( "M3", YT, DT_PROBS_3[,2] )
    M4 = PRINT_LL( "M4", YT, DT_PROBS_4[,2] )
    M5 = PRINT_LL( "M5", YT, DT_PROBS_5[,2] )
    M6 = PRINT_LL( "M6", YT, DT_PROBS_6[,2] )
    M7 = PRINT_LL( "M7", YT, DT_PROBS_7[,2] )
    M8 = PRINT_LL( "M8", YT, DT_PROBS_8[,2] )
    M9 = PRINT_LL( "M9", YT, DT_PROBS_9[,2] )
    cat(HEADER)
    MT = PRINT_LL( "DT", YT, DT_PROBS[,2]   )
    MP2= PRINT_LL( "YF", YT, YP2[,2]        )

    if ( PLOT ) {
        par(mfrow=c(4,3))
        hist(YP[,2],         xlim=c(0,1), breaks=100) ; text( 0.5, 1000, MP, cex=1.5, col='red')
        hist(DT_PROBS_1[,2], xlim=c(0,1), breaks=100) ; text( 0.5, 1000, M1, cex=1.5, col='red')
        hist(DT_PROBS_2[,2], xlim=c(0,1), breaks=100) ; text( 0.5, 1000, M2, cex=1.5, col='red')
        hist(DT_PROBS_3,     xlim=c(0,1), breaks=100) ; text( 0.5, 1000, M3, cex=1.5, col='red')
        hist(DT_PROBS_4[,2], xlim=c(0,1), breaks=100) ; text( 0.5, 1000, M4, cex=1.5, col='red')
        hist(DT_PROBS_5[,2], xlim=c(0,1), breaks=100) ; text( 0.5, 1000, M5, cex=1.5, col='red')
        hist(DT_PROBS_6[,2], xlim=c(0,1), breaks=100) ; text( 0.5, 1000, M6, cex=1.5, col='red')
        hist(DT_PROBS_7[,2], xlim=c(0,1), breaks=100) ; text( 0.5, 1000, M7, cex=1.5, col='red')
        hist(DT_PROBS_8[,2], xlim=c(0,1), breaks=100) ; text( 0.5, 1000, M8, cex=1.5, col='red')
        hist(DT_PROBS_9[,2], xlim=c(0,1), breaks=100) ; text( 0.5, 1000, M9, cex=1.5, col='red')
        hist(DT_PROBS[,2],   xlim=c(0,1), breaks=100) ; text( 0.5, 1000, MT, cex=1.5, col='red')
        hist(YP2[,2], xlim=c(0,1), breaks=100)        ; text( 0.5, 1000, MP2,cex=1.5, col='red')
    }

    return()
}
# ############################################################################################


# ############################################################################################
# NB probabilities generated differently and when mixed with theses via W usually lower the lloss. 
# ############################################################################################
PROBABILITY_ENHANCER = function( YP, YT, TRAIN=TRUE, W=1.000, FAST=FALSE) {
    BANNER( 'APPLYING PROBABILITY CLEANER' )
    if ( TRAIN ) {
        gc(TRUE); PROBABILITY_ENHANCER_PREDICTOR_TRAINING( ); gc(TRUE)

        if ( FALSE ) write.table( ORIG_XTRAIN, file="TRANSFORMED_ORIG_XTRAIN.csv", row.names=FALSE, quote=TRUE, col.names=FALSE, sep="\t")
        if ( FALSE ) save.image(file="MODELS_DONE.RData")

        BANNER( "COMPUTING PROBABILITIES" )
        DT_PROBS_1 = predict(PROB_CORRECTION_MODEL_1, ORIG_XTRAIN, type="prob" )
        print ( sprintf( "DONE W/ MODEL %s", 1 ))
        DT_PROBS_2 = predict(PROB_CORRECTION_MODEL_2, ORIG_XTRAIN, type="prob" )
        print ( sprintf( "DONE W/ MODEL %s", 2 ))
        DT_PROBS_3 = predict(PROB_CORRECTION_MODEL_3, ORIG_XTRAIN, type="prob" )
        print ( sprintf( "DONE W/ MODEL %s", 3 ))
        DT_PROBS_4 = predict(PROB_CORRECTION_MODEL_4, ORIG_XTRAIN, type="prob" )
        print ( sprintf( "DONE W/ MODEL %s", 4 ))
        DT_PROBS_5 = predict(PROB_CORRECTION_MODEL_5, ORIG_XTRAIN, type="prob" )
        print ( sprintf( "DONE W/ MODEL %s", 5 ))
        DT_PROBS_6 = predict(PROB_CORRECTION_MODEL_6, ORIG_XTRAIN, type="prob" )
        print ( sprintf( "DONE W/ MODEL %s", 6 ))
        DT_PROBS_7 = predict(PROB_CORRECTION_MODEL_7, ORIG_XTRAIN, type="prob" )
        print ( sprintf( "DONE W/ MODEL %s", 7 ))
        DT_PROBS_8 = predict(PROB_CORRECTION_MODEL_8, ORIG_XTRAIN, type="prob" )
        print ( sprintf( "DONE W/ MODEL %s", 8 ))
        DT_PROBS_9 = predict(PROB_CORRECTION_MODEL_9, ORIG_XTRAIN, type="prob" )
        print ( sprintf( "DONE W/ MODEL %s", 9 ))

        # ############################################################################################
        if ( CLT_OPTIONS$'do release training dataset' ) {
            BANNER( 'RELEASING TRAINING DATASET' )
            gc(TRUE)
            t=ORIG_XTRAIN[1:1000,]
            ORIG_XTRAIN <<- t
            gc(TRUE)
        }
        # ############################################################################################

        if ( USE_PRECOMPUTED_MODELS ) 
            return ( YP )

        DT_PROBS = MIX( DT_PROBS_1, DT_PROBS_2, DT_PROBS_3, DT_PROBS_4, DT_PROBS_5, DT_PROBS_6, DT_PROBS_7, DT_PROBS_8, DT_PROBS_9, YP, TRAIN=TRUE )
        DT_PROBS = as.matrix(DT_PROBS )
        YP2 = (1.-NB_W) * YP[,2] + NB_W * as.matrix(DT_PROBS[,2])
        YP2 = data.frame( 1.-YP2, YP2 ) 
        summary(DT_PROBS)
        summary(YP2)
        colnames(YP2) = colnames(YP)
        YP2 = as.matrix(YP2)
        YP2 = as.data.frame(YP2)

	    #print ( COMPUTE_LOG_LOSS( YT, YP2[,2] ))

        gc(TRUE)
        if ( AWS ) return ( YP2 )

        DO_COMPARATIVE_HISTOGRAM( YT, YP, DT_PROBS_1, DT_PROBS_2, DT_PROBS_3, DT_PROBS_4, DT_PROBS_5, DT_PROBS_6, DT_PROBS_7, DT_PROBS_8, DT_PROBS_9, DT_PROBS, YP2, PLOT=TRUE )

        if ( LAPTOP ) return ( YP2 )
        if ( FAST ) return ( YP2 )

        BANNER( "MODELING LOG LOSS" )
        YC0 = WHICH_CLASS(YP)
        cat(HEADER)
        YC1 = WHICH_CLASS(DT_PROBS_1) ; YC2 = WHICH_CLASS(DT_PROBS_2) ; YC3 = WHICH_CLASS(DT_PROBS_3)
        YC4 = WHICH_CLASS(DT_PROBS_4) ; YC5 = WHICH_CLASS(DT_PROBS_5) ; YC6 = WHICH_CLASS(DT_PROBS_6)
        YC7 = WHICH_CLASS(DT_PROBS_7) ; YC8 = WHICH_CLASS(DT_PROBS_8) ; YC9 = WHICH_CLASS(DT_PROBS_9)
        cat(HEADER)
        YCT = WHICH_CLASS(DT_PROBS)
        YCP = WHICH_CLASS(YP2)

        BANNER( "MODELING CONFUSION TABLES" )
        BANNER( "ORIGINAL" )
        print( table( YT, YC0) )
        cat(HEADER)
        print( table( YT, YC1) ) ; print( table( YT, YC2) ) ; print( table( YT, YC3) )
        print( table( YT, YC4) ) ; print( table( YT, YC5) ) ; print( table( YT, YC6) )
        print( table( YT, YC7) ) ; print( table( YT, YC8) ) ; print( table( YT, YC9) )
        cat(HEADER)
        print( table( YT, YCT) )
        print( table( YT, YCP) )

        BANNER( "BEFORE AND AFTER" )
	    print ( COMPUTE_LOG_LOSS( YT, YP[,2] ))
	    print ( COMPUTE_LOG_LOSS( YT, YP2[,2] ))
        print( summary( YP2-YP ))

        return ( YP2 )
    } else {
        BANNER( "COMPUTING PROBABILITIES" )
        YP = as.data.frame(YP)
        DT_PROBS_1 = PROBABILITY_ENHANCER_PREDICTOR( MODELNUM=1 )
        print ( sprintf( "DONE W/ MODEL %s", 1 ))
        DT_PROBS_2 = PROBABILITY_ENHANCER_PREDICTOR( MODELNUM=2 )
        print ( sprintf( "DONE W/ MODEL %s", 2 ))
        DT_PROBS_3 = PROBABILITY_ENHANCER_PREDICTOR( MODELNUM=3 )
        print ( sprintf( "DONE W/ MODEL %s", 3 ))
        DT_PROBS_4 = PROBABILITY_ENHANCER_PREDICTOR( MODELNUM=4 )
        print ( sprintf( "DONE W/ MODEL %s", 4 ))
        DT_PROBS_5 = PROBABILITY_ENHANCER_PREDICTOR( MODELNUM=5 )
        print ( sprintf( "DONE W/ MODEL %s", 5 ))
        DT_PROBS_6 = PROBABILITY_ENHANCER_PREDICTOR( MODELNUM=6 )
        print ( sprintf( "DONE W/ MODEL %s", 6 ))
        DT_PROBS_7 = PROBABILITY_ENHANCER_PREDICTOR( MODELNUM=7 )
        print ( sprintf( "DONE W/ MODEL %s", 7 ))
        DT_PROBS_8 = PROBABILITY_ENHANCER_PREDICTOR( MODELNUM=8 )
        print ( sprintf( "DONE W/ MODEL %s", 8 ))
        DT_PROBS_9 = PROBABILITY_ENHANCER_PREDICTOR( MODELNUM=9 )
        print ( sprintf( "DONE W/ MODEL %s", 9 ))
        DT_PROBS = MIX( DT_PROBS_1, DT_PROBS_2, DT_PROBS_3, DT_PROBS_4, DT_PROBS_5, DT_PROBS_6, DT_PROBS_7, DT_PROBS_8, DT_PROBS_9, YP )
        YP2 = (1.-NB_W) * YP[,2] + NB_W * as.matrix(DT_PROBS[,2])
        YP2 = data.frame( 1.-YP2, YP2 ) 
        colnames(YP2) = colnames(YP)
        YP2 = as.matrix(YP2)
        YP2 = as.data.frame(YP2)

        DO_COMPARATIVE_HISTOGRAM( YT, YP, DT_PROBS_1, DT_PROBS_2, DT_PROBS_3, DT_PROBS_4, DT_PROBS_5, DT_PROBS_6, DT_PROBS_7, DT_PROBS_8, DT_PROBS_9, DT_PROBS, YP2, PLOT=FALSE )

        BANNER( "BEFORE AND AFTER" )
	    print ( COMPUTE_LOG_LOSS( YT, YP[,2] ))
	    print ( COMPUTE_LOG_LOSS( YT, YP2[,2] ))
        print( summary( YP2-YP ))

        return ( YP2 )
    }
    return ( YP )
}
# ############################################################################################


