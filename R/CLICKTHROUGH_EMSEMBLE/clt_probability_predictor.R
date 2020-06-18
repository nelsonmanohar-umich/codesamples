    BANNER( "APPLYING MIXTURE MANIFOLD" )
    Q = c(seq(0.00,1,0.06),1)
    Q = c(seq(0.00,1,0.07),1)
    P = sapply( 1:length(H1), 
               function(i) 
                quantile( c(H1=H1[i],
                   H2=H2[i],
                   H3=H3[i],
                   H4=H4[i],
                   H5=H5[i],
                   H6=H6[i], 
                   H7=H7[i],
                   H8=H8[i],
                   H9=H9[i],
                   DT=DT[i],
                   HT=(DT[i]+H1[i])/2,
                   HAa=DT[i],
                   HAb=DT[i],
                   HAc=(DT[i]+HOPT1[i])/2,
                   HAd=(DT[i]+HOPT1[i])/2,
                   HAe=HOPT1[i],
                   HAf=HOPT1[i],
                   HAg=HOPT1[i],
                   HAh=HOPT5[i],
                   HAi=HOPT5[i],
                   H1a=H1[i], 
                   H1b=H1[i], H1c=H1[i], 
                   H1d=H1[i], H1e=H1[i],
                   H5a=H5[i],
                   HDk=(H1[i]+DT[i])/2,
                   HDl=(H2[i]+DT[i])/2,
                   HDm=(H3[i]+DT[i])/2,
                   HDn=(H4[i]+DT[i])/2,
                   HDp=(H5[i]+DT[i])/2,
                   HDq=(H7[i]+DT[i])/2,
                   HDr=(H8[i]+DT[i])/2,
                   HDs=(H9[i]+DT[i])/2,
                   H1t=(H3[i]+H1[i])/2,
                   H1u=(H4[i]+H1[i])/2,
                   H1u=(H8[i]+H1[i])/2,
                   H52=(H1[i]+H5[i])/2,
                   H52a=(H3[i]+H5[i])/2,
                   H52b=(H1[i]+H5[i])/2,
                   H57a=(H8[i]+H5[i])/2,
                   H57b=(H0[i]+H5[i])/2,
                   H57c=(H1[i]+H5[i])/2,
                   H30=(H1[i]+H4[i]+H7[i])/3, 
                   H34=(H3[i]+H4[i]+H9[i])/3, 
                   H35=(H4[i]+H8[i]+H7[i])/3, 
                   H81=(H9[i]+H8[i])/2, 
                   H87=(H9[i]+H8[i])/2,
                   H86=(H9[i]+H8[i])/2.,
                   H82=(H7[i]+H8[i])/2, 
                   H83=(H1[i]+H8[i])/2,
                   H84=(H5[i]+H8[i])/2,
                   H85=(H8[i]+H8[i])/2,
                   H9l=(H9[i]+H1[i])/2,
                   H9m=(H9[i]+H5[i])/2,
                   H9n=(H9[i]+H4[i])/2,
                   H9p=(H9[i]+H3[i])/2,
                   H86=(H8[i]+H1[i])/2, 
                   H88=(H8[i]+DT[i])/2.,
                   H13=(H9[i]+H1[i]+H5[i])/3., 
                   H19=(H8[i]+H1[i]+H9[i])/3, 
                   H18a=(H9[i]+H3[i]+H4[i])/3.,
                   H18b=(H9[i]+H3[i]+H8[i])/3.,
                   H18c=(H4[i]+H3[i]+H8[i])/3.,
                   H18d=(H4[i]+H3[i]+H9[i])/3.,
                   H20a=(H3[i]+H4[i]+H8[i]+H9[i])/4,
                   H21a=(H3[i]+H4[i]+H5[i]+H8[i]+H9[i])/5,
                   H21b=(H3[i]+H4[i]+H5[i]+H7[i]+H8[i]+H9[i])/6,
                   H22a=(H1[i]+H3[i]+H4[i]+H5[i]+H8[i]+H9[i]+H0[i])/7,
                   H24a=HOPT0[i],
                   H24b=HOPT0[i],
                   H24c=HOPT0[i],
                   H24d=HOPT0[i],
                   H24e=HOPT0[i],
                   H23a=HOPT1[i],
                   H23b=HOPT1[i],
                   H23c=HOPT1[i],
                   H23d=HOPT1[i],
                   H23e=HOPT1[i],
                   H24a=HOPT2[i],
                   H24b=HOPT2[i],
                   H24c=HOPT2[i],
                   H25a=HOPT3[i],
                   H25b=HOPT3[i],
                   H25c=HOPT3[i],
                   H26a=HOPT4[i],
                   H26b=HOPT5[i],
                   H26c=HOPT6[i],
                   H26d=HOPT7[i],
                   #HA22=HA2CP2[i],
                   H26e=HOPT[i]), Q))

    rm(H1); rm(H2); rm(H3); rm(H4); rm(H5); rm(H6); rm(H7); rm(H8); rm(H9); rm(DT)

    P = t(P)

    if ( TRAIN ) {
        BANNER( "LOGLOSS COMPONENTS P" )
        for( i in 1:ncol(P))
            COMPUTE_LOG_LOSS( Y_TRUE, P[,i])

        BANNER( "EMSEMBLE PREDICTOR-COMPONENTS ROWS" )
        PY = data.frame( P, Y_TRUE )
        write.table( PY, file="PREDICTORS_P.csv", row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
        rm(P)
        if ( FALSE )  {
            rm(PY)
            PY = read.csv( "PREDICTORS_P.csv", header=FALSE )
        }
        colnames(PY) = paste( "X", 1:ncol(PY), sep="")
        colnames(PY)[ncol(PY)] = "Y_TRUE"

        BANNER( "PROBABILITY PREDICTOR ATTRIBUTE IMPORTANCE" )
        N = ncol(PY) ; M = nrow(PY)

        if ( USE_PRECOMPUTED_MODELS ) {
            BANNER("USING PRECOMPUTED PREDICTORS")
        } else {
            BANNER("TRAINING PRUNED PREDICTOR")
            tree_controls = rpart.control(xval=20, minbucket=9, minsplit=45, cp=1E-9, split="information")
            PROB_CORRECTION_MODEL_A <<- rpart( 'Y_TRUE ~ .', PY[,1:N], control=tree_controls )
            print(PROB_CORRECTION_MODEL_A$variable.importance)
            if ( FALSE ) {
                cat(HEADER)
                PROB_CORRECTION_MODEL_A <<- prune(PROB_CORRECTION_MODEL_A, 
                                                cp=PROB_CORRECTION_MODEL_A$cptable[which.min(PROB_CORRECTION_MODEL_A$cptable[,"xerror"]),"CP"])
                print(PROB_CORRECTION_MODEL_A$variable.importance)
            }
            print(summary(residuals(PROB_CORRECTION_MODEL_A, type="pearson")))
            cat(HEADER)
    
            tree_controls = rpart.control(xval=30, minbucket=5, minsplit=27, cp=1E-12, split="information")
            PROB_CORRECTION_MODEL_B <<- rpart( 'Y_TRUE ~ .', PY[,1:N], control=tree_controls )
            print(summary(residuals(PROB_CORRECTION_MODEL_B, type="pearson")))
            print(PROB_CORRECTION_MODEL_B$variable.importance)
                cat(HEADER)
                PROB_CORRECTION_MODEL_B <<- prune( PROB_CORRECTION_MODEL_B, 
                                                cp=PROB_CORRECTION_MODEL_B$cptable[which.min(PROB_CORRECTION_MODEL_B$cptable[,"xerror"]),"CP"])
                print(PROB_CORRECTION_MODEL_B$variable.importance)
            print(summary(residuals(PROB_CORRECTION_MODEL_B, type="pearson")))
            cat(HEADER)
        }

        N = ncol(PY)
        GRID_SEARCH = FALSE
        if ( GRID_SEARCH ) {
            ll = list()
            for ( a in seq(21,28,7)) {
                for ( b in seq(5,min(a-5,13),8)) {
                    for( c in c(5e-6,1e-5,5e-5)) {
                        tree_controls = rpart.control(xval=2, minbucket=b, minsplit=a, cp=c, split="information")
                        print(paste(a,b,c))
                        PROB_CORRECTION_MODEL <<- rpart( 'Y_TRUE ~ .', PY[,1:N], control=tree_controls )
                        ll = append( ll, COMPUTE_LOG_LOSS(Y_TRUE, predict(PROB_CORRECTION_MODEL, PY )))
                        print(PROB_CORRECTION_MODEL$variable.importance)
                        print(summary(residuals(PROB_CORRECTION_MODEL, type="pearson")))
                        cat(HEADER)
                    }
                }
            }
            print (ll)
        }

		#------------------------------------------------------------------------
		#[1] "21 5 1e-07" [1] "LOG_LOSS_VALUE: 0.2576" #[1] "LOG_LOSS_VALUE: 0.3968"
		#[1] "21 5 1e-06" [1] "LOG_LOSS_VALUE: 0.2587" #[1] "LOG_LOSS_VALUE: 0.3978"
		#[1] "21 5 1e-05" [1] "LOG_LOSS_VALUE: 0.2872" #[1] "LOG_LOSS_VALUE: 0.3961"
		#[1] "21 5 1e-04" [1] "LOG_LOSS_VALUE: 0.3934" #[1] "LOG_LOSS_VALUE: 0.3965"
		#------------------------------------------------------------------------
		#[1] "21 13 1e-07" [1] "LOG_LOSS_VALUE: 0.289" #[1] "LOG_LOSS_VALUE: 0.3967"
		#[1] "21 13 1e-06" [1] "LOG_LOSS_VALUE: 0.2905" #[1] "LOG_LOSS_VALUE: 0.3961"
		#[1] "21 13 1e-05" [1] "LOG_LOSS_VALUE: 0.3139" #[1] "LOG_LOSS_VALUE: 0.3968"
		#[1] "21 13 1e-04" [1] "LOG_LOSS_VALUE: 0.3935" #[1] "LOG_LOSS_VALUE: 0.3968"
		#------------------------------------------------------------------------
		#[1] "28 5 1e-07" [1] "LOG_LOSS_VALUE: 0.2707" #[1] "LOG_LOSS_VALUE: 0.3968"
		#[1] "28 5 1e-06" [1] "LOG_LOSS_VALUE: 0.2718" #[1] "LOG_LOSS_VALUE: 0.3963"
		#[1] "28 5 1e-05" [1] "LOG_LOSS_VALUE: 0.2976" #[1] "LOG_LOSS_VALUE: 0.3972"
		#[1] "28 5 1e-04" [1] "LOG_LOSS_VALUE: 0.3934" #[1] "LOG_LOSS_VALUE: 0.3967"
		#------------------------------------------------------------------------

        BANNER("MIXING OF PRUNED AND NON PRUNED PREDICTORS")
        DT_PROBS_PRED_A = predict( PROB_CORRECTION_MODEL_A)#, PY)#, type='prob' )
        DT_PROBS_PRED_B = predict( PROB_CORRECTION_MODEL_B)#, PY)#, type='prob' )  
        if ( class(DT_PROBS_PRED_A) == "matrix" ) { if (ncol(DT_PROBS_PRED_A)==2) DT_PROBS_PRED_A = DT_PROBS_PRED_A[,2] }
        if ( class(DT_PROBS_PRED_B) == "matrix" ) { if (ncol(DT_PROBS_PRED_B)==2) DT_PROBS_PRED_B = DT_PROBS_PRED_B[,2] }
        if ( length(DT_PROBS_PRED_A) == (2*N) ) { DT_PROBS_PRED_A = DT_PROBS_PRED_A[,2] }
        if ( length(DT_PROBS_PRED_B) == (2*N) ) { DT_PROBS_PRED_B = DT_PROBS_PRED_B[,2] }
        DT_PROBS_PRED = BIGW * DT_PROBS_PRED_A   + (1-BIGW) * DT_PROBS_PRED_B
        COMPUTE_LOG_LOSS(Y_TRUE, DT_PROBS_PRED_A)
        COMPUTE_LOG_LOSS(Y_TRUE, DT_PROBS_PRED_B)
        COMPUTE_LOG_LOSS(Y_TRUE, DT_PROBS_PRED)

        if ( FALSE ) {
            for ( w in seq(0.1,0.9,0.1)) {
                print ( w )
                DT_PROBS_PREDa = w * DT_PROBS_PRED_A   + (1-w) * DT_PROBS_PRED_B
                COMPUTE_LOG_LOSS(Y_TRUE, DT_PROBS_PRED_A)
                COMPUTE_LOG_LOSS(Y_TRUE, DT_PROBS_PRED_B)
                COMPUTE_LOG_LOSS(Y_TRUE, DT_PROBS_PREDa)
                cat( HEADER)
            }
            rm( DT_PROBS_PREDa )
        }

        cat(HEADER)
        sink( "output_rpart_tree_model0_summary.out", split=FALSE)
            BANNER( "PROBABILITY CORRECTION MODEL A" )
            print(PROB_CORRECTION_MODEL_A$variable.importance)
            print( summary( PROB_CORRECTION_MODEL_A ) )
            print(summary(residuals(PROB_CORRECTION_MODEL_A, type="pearson")))

            BANNER( "PROBABILITY CORRECTION MODEL B" )
            print(PROB_CORRECTION_MODEL_B$variable.importance)
            print( summary( PROB_CORRECTION_MODEL_B ) )
            print(summary(residuals(PROB_CORRECTION_MODEL_B, type="pearson")))
        sink()
        cat(HEADER)
    } else {
        cat(HEADER)
        PY = data.frame( P, round(YP) )        # Y_TRUE is just the NAIVE BAYES estimator here
        colnames(PY) = paste( "X", 1:ncol(PY), sep="")
        colnames(PY)[ncol(PY)] = "Y_TRUE"
        if ( CVTEST ) {
            BANNER( "LOGLOSS COMPONENTS P" )
            for( i in 1:ncol(P))
                COMPUTE_LOG_LOSS( Y_TRUE, P[,i])
        }
        rm(P)
        write.table( PY, file="PREDICTORS_P-temp.csv", row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

        if ( FALSE ) {
            rm(PY)
            PY = read.csv( "PREDICTORS_P-temp.csv", header=FALSE )
            colnames(PY) = paste( "X", 1:ncol(PY), sep="")
            colnames(PY)[ncol(PY)] = "Y_TRUE"
        }

        BANNER("MIXING OF PRUNED AND NON-PRUNED PREDICTORS")
        DT_PROBS_PRED_A = predict( PROB_CORRECTION_MODEL_A, PY)#, type='prob' ) 
        DT_PROBS_PRED_B = predict( PROB_CORRECTION_MODEL_B, PY)#, type='prob' )   
        if ( class(DT_PROBS_PRED_A) == "matrix" ) { if (ncol(DT_PROBS_PRED_A)==2) DT_PROBS_PRED_A = DT_PROBS_PRED_A[,2] }
        if ( class(DT_PROBS_PRED_B) == "matrix" ) { if (ncol(DT_PROBS_PRED_B)==2) DT_PROBS_PRED_B = DT_PROBS_PRED_B[,2] }
        if ( length(DT_PROBS_PRED_A) == (2*N) ) { DT_PROBS_PRED_A = DT_PROBS_PRED_A[,2] }
        if ( length(DT_PROBS_PRED_B) == (2*N) ) { DT_PROBS_PRED_B = DT_PROBS_PRED_B[,2] }
        print(summary(DT_PROBS_PRED_A))
        print(summary(DT_PROBS_PRED_B))
        DT_PROBS_PRED = BIGW * DT_PROBS_PRED_A   + (1-BIGW) * DT_PROBS_PRED_B
        print(summary(DT_PROBS_PRED))
        COMPUTE_LOG_LOSS(Y_TRUE, DT_PROBS_PRED)
        if ( FALSE ) {
            for ( w in seq(0.1,0.9,0.1)) {
                print ( w )
                DT_PROBS_PREDa = w * DT_PROBS_PRED_A   + (1-w) * DT_PROBS_PRED_B
                COMPUTE_LOG_LOSS(Y_TRUE, DT_PROBS_PRED_A)
                COMPUTE_LOG_LOSS(Y_TRUE, DT_PROBS_PRED_B)
                COMPUTE_LOG_LOSS(Y_TRUE, DT_PROBS_PREDa)
                cat( HEADER)
            }
            rm( DT_PROBS_PREDa )
        }
    }

    if ( CVTEST ) TRAIN = TRUE

    BANNER( "PREDICTED PROBABILITY FINAL PHASE" )
    print(summary(DT_PROBS_PRED_A))
    print(summary(DT_PROBS_PRED_B))
    print(summary(DT_PROBS_PRED))
    if ( TRAIN ) {
        COMPUTE_LOG_LOSS( Y_TRUE, DT_PROBS_PRED_A )
        COMPUTE_LOG_LOSS( Y_TRUE, DT_PROBS_PRED_B )
        COMPUTE_LOG_LOSS( Y_TRUE, DT_PROBS_PRED )
    }

    NQ   = ncol(PY)-1
    PMAX = (PY[,NQ-7] + PY[,NQ-8])/2
    PMIN = (PY[,9]  + PY[,10])/2

    BANNER( "MIXTURE MANIFOLD THRESHOLD SEARCH" )
    if ( TRAIN ) {
        if ( TRUE ) {
            THRESHOLDS = seq(0.3,0.7,0.04)
            T0 = sapply( THRESHOLDS, function(t) { DT_PROBSa = ifelse(DT_PROBS_PRED < t, PMIN, DT_PROBS_PRED ); COMPUTE_LOG_LOSS( Y_TRUE, DT_PROBSa) } )
            T0 = unlist( T0 )
            T0 = round(T0,4)
            T1 = which( T0 == min(T0) ) 
            NT1 = length(T1)
            if (NT1 > 1) NT1 = as.integer((NT1+1)/2)
            if ( FALSE ) PROB_CORRECTION_THRESHOLD <<- THRESHOLDS[T1][NT1]
            print( paste ( "PROB_CORRECTION_THRESHOLD", PROB_CORRECTION_THRESHOLD ))  
        }
    }

    BANNER( "PROBABILITY CORRECTION MATRIX")
    if ( FALSE ) {
        DT_PROBS_PRED = ifelse( DT_PROBS_PRED < PROB_CORRECTION_THRESHOLD, PMIN, DT_PROBS_PRED )
        if (TRAIN) COMPUTE_LOG_LOSS( Y_TRUE, DT_PROBS_PRED )
        DT_PROBS_PRED = ifelse( DT_PROBS_PRED > PROB_CORRECTION_THRESHOLD, PMAX, DT_PROBS_PRED )
        if (TRAIN) COMPUTE_LOG_LOSS( Y_TRUE, DT_PROBS_PRED )
    } else {
        cat(HEADER)
        if (TRAIN) COMPUTE_LOG_LOSS( Y_TRUE, ifelse( DT_PROBS_PRED < PROB_CORRECTION_THRESHOLD, PMIN, DT_PROBS_PRED ))
        if (TRAIN) COMPUTE_LOG_LOSS( Y_TRUE, ifelse( DT_PROBS_PRED > PROB_CORRECTION_THRESHOLD, PMAX, DT_PROBS_PRED ))
        cat(HEADER)
    }

    DT_PROBS_PRED = ifelse( DT_PROBS_PRED <= PROB_CORRECTION_THRESHOLD, PMIN, DT_PROBS_PRED )
    if (TRAIN) COMPUTE_LOG_LOSS( Y_TRUE, DT_PROBS_PRED )

    cat(HEADER)
    if ( FALSE ) DT_PROBS_PRED = ifelse( DT_PROBS_PRED > PROB_CORRECTION_THRESHOLD, max(PMIN, DT_PROBS_PRED), DT_PROBS_PRED )
    if (TRAIN) COMPUTE_LOG_LOSS( Y_TRUE, ifelse( DT_PROBS_PRED > PROB_CORRECTION_THRESHOLD, max(PMIN, DT_PROBS_PRED), DT_PROBS_PRED ))

    if ( FALSE ) DT_PROBS_PRED = ifelse( DT_PROBS_PRED > PROB_CORRECTION_THRESHOLD, min(PMAX, DT_PROBS_PRED), DT_PROBS_PRED )
    if (TRAIN) COMPUTE_LOG_LOSS( Y_TRUE, ifelse( DT_PROBS_PRED > PROB_CORRECTION_THRESHOLD, max(PMAX, DT_PROBS_PRED), DT_PROBS_PRED ))

    if ( FALSE ) DT_PROBS_PRED = ifelse( DT_PROBS_PRED > PROB_CORRECTION_THRESHOLD, max(PMAX, DT_PROBS_PRED), DT_PROBS_PRED )
    if (TRAIN) COMPUTE_LOG_LOSS( Y_TRUE, ifelse( DT_PROBS_PRED > PROB_CORRECTION_THRESHOLD, min(PMAX, DT_PROBS_PRED), DT_PROBS_PRED ))
    cat(HEADER)

    if ( TRUE  ) DT_PROBS_PRED = ifelse( DT_PROBS_PRED > PROB_CORRECTION_THRESHOLD, PMAX, DT_PROBS_PRED )
    if (TRAIN) COMPUTE_LOG_LOSS( Y_TRUE, DT_PROBS_PRED )

    if ( FALSE ) {
        DT_PROBS = (LILW/2)*HOPT2 + (LILW/2)*HOPT1 + (1-LILW)*DT_PROBS_PRED
    } else {
        DT_PROBS = DT_PROBS_PRED
    }
    if ( TRAIN ) COMPUTE_LOG_LOSS( Y_TRUE, DT_PROBS )

    if ( TRAIN ) print( data.frame( PY[1:10,], DT0=DT_PROBS_PRED[1:10], DT2=DT_PROBS[1:10], YT=Y_TRUE[1:10] ) )
    DT_PROBS = data.frame( 1.-DT_PROBS, DT_PROBS ) 
    colnames(DT_PROBS) = colnames(D1)
    BANNER( "PROBABILITY CORRECTION DONE")
