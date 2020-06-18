# ############################################################################################
# summary of failed to predict samples wrt class/attributes
# ############################################################################################
BANNER( 'SUMMARY OF MISSED CASES' )
ADDITIONAL_INSIGHT = TRUE
if ( ADDITIONAL_INSIGHT ) {
    WHICH_ONES = which( YP_CLASS != Y_TRUE )
    MISSED_SAMPLES = ORIG_XTRAIN[WHICH_ONES,]
    print( summary( MISSED_SAMPLES ) )
    cat(HEADER)
}
# ############################################################################################


# ############################################################################################
# decision tree insight into failed to predict samples
# ############################################################################################
ADDITIONAL_INSIGHT = TRUE
if ( ADDITIONAL_INSIGHT ) {
    BANNER( 'DECISION TREE INSIGHT INTO MISSED CASES' )
    GENERAL_FORMULA = sprintf( "%s ~ .", PREDICT_VAR )
    MISSING_DT_MODEL = FIT_DECISION_TREE( MISSED_SAMPLES, 
                                          SLICE_DATAFRAME(MISSED_SAMPLES,PREDICT_COL), 
                                          FORMULA=GENERAL_FORMULA, 
                                          DO_PRUNING=TRUE, 
                                          minbucket=max(20,as.integer(nrow(ORIG_XTRAIN)/2048)))
    p = prp(MISSING_DT_MODEL)
    print ( MISSING_DT_MODEL )
    cat(HEADER)
}
# ############################################################################################


# #####################################################################################
# tables of factor to predict var  
# #####################################################################################
BANNER( 'SUMMARY OF FACTOR RELATION TO PREDICTOR FOR MISSED CASES' )
ADDITIONAL_INSIGHT = TRUE
TOPN=10
if ( ADDITIONAL_INSIGHT ) {
    for ( i in 1:ncol(MISSED_SAMPLES))  {
        if ( colnames(ORIG_XTRAIN)[i] == "ad_id" ) { next }
        print ( colnames(ORIG_XTRAIN)[i] )
        GET_TOPN_TABLE_FROM( MISSED_SAMPLES[,i], MISSED_SAMPLES[,PREDICT_COL], TOPN, WRT=c(1,2) )
    }
}
# #####################################################################################


