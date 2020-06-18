
# ############################################################################################
# exploratory/bootstrapping decision tree for insight about potential non-linear and factor/subset interaction effects via 
# ############################################################################################
ADDITIONAL_INSIGHT = TRUE
if ( ADDITIONAL_INSIGHT ) {
    BANNER( 'BOOTSTRAPPING GRID FOR DECISION TREE INSIGHT' )
    FORMULA = GET_FORMULAE( FSELECT_XVARS[1:16] )
    WHAT = sample(1:nrow(ORIG_XTRAIN), min(500000, nrow(ORIG_XTRAIN)))
    gridlogloss = list()
    gridcmat    = list()
    gridout     = list()
    i = 0
    M = MATRIX( 1000, 8 ) 
    M = as.data.frame( M )
    colnames(M) = c( "maxdepth", "minbucket", "minsplit", "cp", "split", "c1w", "c2w", "logloss" )
    for( maxdepth in c(12, 18, 24)) {
        for( minbucket in c(11, 17, 23, 29)) {
            for( minsplit in c(1.1*(minbucket+1), 2.1*(minbucket+1), 3.1*(minbucket+1))) {
                cp = 1e-6
                # for ( cp in c(1e-05, 1e-010) ) {
                    split = "information"
                    # for ( split in c("information", "gini") ) {
                        c1w = 0.5 ; c2w = 0.5
                        # for( c1w in c( 0.2, 0.5, 0.8 )) {
                            # for( c2w in c( 0.2, 0.5, 0.8 )) {
                                # ############################################################################################
                                i = i + 1
    	                        out = sprintf( "TUPLE: %s,  %s,  %s,  %s,  %6.3g,  %s,  %.2f,  %.2f", i, maxdepth, minbucket, minsplit, cp, split, c1w, c2w )
                                BANNER( sprintf( "%s", out ))
                                CLW = c( c1w, c2w )
                                # ############################################################################################
                                tree_controls = rpart.control(xval=10, minbucket=minbucket, minsplit=minsplit, maxdepth=maxdepth, cp=CP, clsplit=split, prior = c(c1w, c2w))
                                GRID_DT_MODEL = FIT_DECISION_TREE( model.frame(FORMULA, ORIG_XTRAIN)[WHAT,], 
                                                                   SLICE_DATAFRAME(ORIG_XTRAIN[WHAT,PREDICT_COL]),
                                                                   FORMULA=FORMULA, DO_PRUNING=TRUE, control=tree_controls )
                                    DT_PROBS = predict(GRID_DT_MODEL, ORIG_XTRAIN)
                                    DT_CLASS = WHICH_CLASS_KERNEL(DT_PROBS, 0.5)
                                    cmat = table( Y_TRUE, DT_CLASS )
    	                            llval = COMPUTE_LOG_LOSS( Y_TRUE, DT_PROBS[,2] )
                                    cmatvals = as.character(array(cmat))
    	                            out = sprintf( "TUPLE: %s,  %s,  %s,  %s,  %6.3g,  %s,  %.2f,  %.2f,  %.6f", i, maxdepth, minbucket, minsplit, cp, split, c1w, c2w, llval )
                                        gridlogloss[[i]] = llval 
                                        gridcmat[[i]] = cmat
                                        gridout [[i]] = out
                                    M[i,1] = maxdepth
                                    M[i,2] = minbucket
                                    M[i,3] = minsplit
                                    M[i,4] = cp 
                                    M[i,5] = split
                                    M[i,6] = c1w
                                    M[i,7] = c2w
                                    M[i,8] = llval
                                A = GRID_DT_MODEL$variable.importance
                                # A = A[A>100]
                                DTVARS = attr(A,"names") 
                                DTFORM = GET_FORMULAE( DTVARS )
                                cat(HEADER)
                                print ( unlist( tree_controls ) )
                                cat(HEADER)
                                print( cmat )
                                cat(HEADER)
                                print( out )
                                cat(HEADER)
                                print ( A )
                                cat(HEADER)
                                print(DTFORM)
                                cat(HEADER)
                                print( aggregate( M$logloss ~ M$maxdepth + round(M$minsplit) + M$minbucket, M[1:i,], mean) )
                                # ############################################################################################
                            # }
                        # }
                    # }
                # }
            }
        }
    }
}

M = M[1:i,]
colnames(M) = c( "maxdepth", "minbucket", "minsplit", "cp", "split", "c1w", "c2w", "logloss" )
M[,"split" ] = as.factor( M[, "split"] )
M = as.data.frame( M )

BANNER( "GRID SEARCH RESULTS" )
print ( M )
cat(HEADER)
g = aggregate( M$logloss ~ M$maxdepth + round(M$minsplit) + M$minbucket, M, mean)
print ( g )

graphics.off()
png('plot_clt_grid_search.png', 800, 600 )
par(mar=c(13,4,6,4))
labels = sapply( 1:nrow(g), function( idx ) sprintf( "(D=%s,S=%s,B=%s):[%.3f]", g[idx,1], g[idx,2], g[idx,3], g[idx,4]))
ordering = order(g[,4])
barplot( g[ordering,4], names=labels[ordering], horiz=F, las=2, col=terrain.colors(nrow(g)), cex=1.0, cex.axis=1.2, cex.lab=1.2, cex.main=2, xlab="", ylab="Log Loss For Parameter Set", main="GRID SEARCH OVER DECISION TREE PARAMETERS\nD=MAXDEPTH, S=MINSPLIT, B=MINBUCKET" )
dev.off()
