    # #####################################################################################
    # input conditioning, complete cases
    # #####################################################################################
    COMPLETE_SAMPLES = complete.cases(ORIG_XTEST)
    ORIG_XTEST  = ORIG_XTEST[COMPLETE_SAMPLES,]
    # #####################################################################################

    # #####################################################################################
    # scale uneven numeric fields
    # #####################################################################################
    ORIG_XTEST['H01'] = CUT(ORIG_XTEST[,'C01'], S01)#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST['H14'] = CUT(ORIG_XTEST[,'C14'], S14)#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST['H15'] = CUT(ORIG_XTEST[,'C15'], S15)#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST['H16'] = CUT(ORIG_XTEST[,'C16'], S16)#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST['H17'] = CUT(ORIG_XTEST[,'C17'], S17)#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST['H18'] = CUT(ORIG_XTEST[,'C18'], S18)#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST['H19'] = CUT(ORIG_XTEST[,'C19'], S19)#, labels=FALSE)#, ordered_result=TRUE)
    #ORIG_XTEST$C20 = ifelse( ORIG_XTEST$C20 != -1, ORIG_XTEST$C20, 0 )
    ORIG_XTEST['H20'] = CUT(ORIG_XTEST[,'C20'], S20)#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST['H21'] = CUT(ORIG_XTEST[,'C21'], S21)#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST['H44'] = CUT(ORIG_XTEST[,'C14'], S44)#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST['H54'] = CUT(ORIG_XTEST[,'C14'] + ORIG_XTEST[,'banner_pos']*1000., S44)#, labels=FALSE)#, ordered_result=TRUE )

    # #####################################################################################

    # #####################################################################################
    # recode meaningless timestamp into two new time variables
    # #####################################################################################
    #ORIG_XTEST[,'app_category']   = factor( ORIG_XTEST[,"app_category"],  F_APP_CAT,  ordered=TRUE )
    #ORIG_XTEST[,'site_category']  = factor( ORIG_XTEST[,"site_category"], F_SITE_CAT, ordered=TRUE )
    #ORIG_XTEST[,"s2c"] = sapply( ORIG_XTEST$site_id, function(x) S2C[[x]] ) ; ORIG_XTEST[is.null(ORIG_XTEST$s2c),"s2c"] = 0
    #ORIG_XTEST[,"a2c"] = sapply( ORIG_XTEST$app_id,  function(x) A2C[[x]] ) ; ORIG_XTEST[is.null(ORIG_XTEST$a2c),"a2c"] = 0
    #ORIG_XTEST[,"s2c"] = CUT(ORIG_XTEST[,"s2c"] , S_S2C, ordered=TRUE)
    #ORIG_XTEST[,"a2c"] = CUT(ORIG_XTEST[,"a2c"] , S_A2C, ordered=TRUE)

    MATCH_APPLIED_TRANSFORMS_FOR( 'app_category', 'app_category' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'site_category', 'site_category' )

    ORIG_XTEST[,"hour"] = factor(as.integer(substr(as.character(ORIG_XTEST$YYMMDDHH),7,8)), 0:23)
    ORIG_XTEST[,"day"] = factor((as.integer(substr(as.character(ORIG_XTEST$YYMMDDHH),5,6))-20)%%7, 0:7, ordered=TRUE)
    ORIG_XTEST[,"sitecatclickprobs0"] =  SAPPLY( ORIG_XTEST[,"site_category"], function(x) ifelse(is.null(SCP[[x]]), 0, SCP[[x]][1]))
    ORIG_XTEST[,"sitecatclickprobs0"] =  CUT( (as.numeric(ORIG_XTEST[,"hour"]) * ORIG_XTEST[,"sitecatclickprobs0"]+1), SCP0)#, labels=FALSE)#, ordered_result=TRUE )

    ORIG_XTEST[,"appcatclickprobs0"]  =  SAPPLY( ORIG_XTEST[,"app_category"],  function(x) ifelse(is.null(ACP[[x]]), 0, ACP[[x]][1]))
    ORIG_XTEST[,"appcatclickprobs0"]  =  CUT( (as.numeric(ORIG_XTEST[,"hour"]) * ORIG_XTEST[,"appcatclickprobs0"]+1), ACP0)#, labels=FALSE)#, ordered_result=TRUE )

    ORIG_XTEST[,"sitecatclickprobs1"] =  SAPPLY( ORIG_XTEST[,"site_category"], function(x) ifelse(is.null(SCP[[x]]), 0, SCP[[x]][2]))
    ORIG_XTEST[,"sitecatclickprobs1"] =  CUT( (ORIG_XTEST[,"sitecatclickprobs1"]), SCP1)#, labels=FALSE)#, ordered_result=TRUE )

    ORIG_XTEST[,"appcatclickprobs1"]  =  SAPPLY( ORIG_XTEST[,"app_category"],  function(x) ifelse(is.null(ACP[[x]]), 0, ACP[[x]][2]))
    ORIG_XTEST[,"appcatclickprobs1"]  =  CUT( (ORIG_XTEST[,"appcatclickprobs1"]), ACP1)#, labels=FALSE)#, ordered_result=TRUE )

    ORIG_XTEST[,"sitecatclickprobs2"] =  paste( ORIG_XTEST[,"site_category"], ORIG_XTEST[,"hour"], sep=":" )
    ORIG_XTEST[,"sitecatclickprobs2"] =  sapply( ORIG_XTEST[,"sitecatclickprobs2"], function(x) ifelse(is.null(S2CHP[[x]]), 0, S2CHP[[x]]))
    ORIG_XTEST[,"sitecatclickprobs2"] =  CUT( ORIG_XTEST[,"sitecatclickprobs2"], SCP2)#, labels=FALSE)#, ordered_result=TRUE )
    #ORIG_XTEST[,"appcatclickprobs2"] =  paste( ORIG_XTEST[,"app_category"], ORIG_XTEST[,"hour"], sep=":" )
    #ORIG_XTEST[,"appcatclickprobs2"] =  sapply( ORIG_XTEST[,"appcatclickprobs2"], function(x) ifelse(is.null(A2CHP[[x]]), 0, A2CHP[[x]]))
    #ORIG_XTEST[,"appcatclickprobs2"] =  CUT( ORIG_XTEST[,"appcatclickprobs2"], ACP2 )

    ORIG_XTEST[,"device_model"] = as.factor(substr(ORIG_XTEST$device_model,1,8))
    ORIG_XTEST[,"device_ip"]    = as.factor(substr(ORIG_XTEST$device_ip,1,8))
    #ORIG_XTEST[,"adix"]        = log(as.numeric(ORIG_XTEST$ad_id))
    #ORIG_XTEST[,'adix']        = CUT(ORIG_XTEST[,'adix'], c(-Inf,0,seq(35,45,0.25), Inf))
    ORIG_XTEST[,"adix"]         = CUT( as.numeric(str_sub(ORIG_XTEST$ad_id, 1, 2)), c(-Inf,seq(0,99,2), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST[,"adix_ts"]      = CUT( as.numeric(str_sub(ORIG_XTEST$ad_id, -2, -1)), c(-Inf,seq(0,99,2), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST[,"adix_ts2"]     = CUT( as.numeric(str_sub(ORIG_XTEST$ad_id, -4, -3)), c(-Inf,seq(0,99,2), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTEST[,"adix_ts6"]     = CUT( log(as.numeric(str_sub(ORIG_XTEST$ad_id, -6, -1))+1), c(-Inf,seq(0,20,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)

    ORIG_XTEST[,"shift"]        = factor(as.integer(ORIG_XTEST[,"day"])*4 + as.integer(as.integer(substr(as.character(ORIG_XTEST$YYMMDDHH),7,8))/8), 0:30)
    #ORIG_XTEST[,"shift"]       = factor(as.integer(as.integer(substr(as.character(ORIG_XTEST$YYMMDDHH),7,8))/4), 0:6)
    # #####################################################################################

    # #####################################################################################
    # CLICKPROBS
    # #####################################################################################
    hours = as.integer(ORIG_XTEST[,"hour"]); 
    ORIG_XTEST[,"hourclickprobs"] =  round( TRAINED_HOURCLICKPROBS[hours,3], NDIGITS )
    shifts = as.integer(ORIG_XTEST[,"shift"]); 
    ORIG_XTEST[,"shiftclickprobs"]=  round( TRAINED_SHIFTCLICKPROBS[shifts,3], NDIGITS )
    days = as.integer(ORIG_XTEST[,"day"]); 
    ORIG_XTEST[,"dayclickprobs"]  =  round( TRAINED_DAYCLICKPROBS[days,3], NDIGITS )
    # #####################################################################################

    # #####################################################################################
    # recode some inconvenient factors
    # #####################################################################################
    #MATCH_APPLIED_TRANSFORMS_FOR( 'ad_id', 'ad_id' )
    #ORIG_XTEST['probs'] = factor(round(A2C_PREDICT(TRAIN=FALSE),1), seq(0,1,0.1), ordered=TRUE)
    #MATCH_APPLIED_TRANSFORMS_FOR( "probs", "probs" )
    # #####################################################################################
    BANNER( 'COMPUTING APPID CLICK PROBABILITIES' )
    # ORIG_XTEST['probs'] = round(A2C_PREDICT(TRAIN=FALSE),1)
    # ORIG_XTEST['probs'] = CUT( ORIG_XTEST[,'probs'], SPROBS )
    # #####################################################################################

    # #####################################################################################
    BANNER( 'MATCHING TRANSFORMS' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'device_id', 'device_id' )
    # MATCH_APPLIED_TRANSFORMS_FOR( 'YYMMDDHH', 'YYMMDDHH' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'app_domain', 'app_domain' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'app_id', 'app_id' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'site_domain', 'site_domain' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'site_id', 'site_id' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'device_type', 'device_type' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'device_model', 'device_model' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'device_conn_type', 'device_conn_type' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'banner_pos', 'banner_pos' )
    MATCH_APPLIED_TRANSFORMS_FOR( 'device_ip', 'device_ip' )

    ADDITIONAL_INSIGHT  = FALSE
    if ( ADDITIONAL_INSIGHT ) {
        ORIG_XTEST['H22'] = CUT(sqrt(exp(as.numeric(ORIG_XTEST$site_domain))   -  exp(-as.numeric(ORIG_XTEST$site_category)) / 
                                  (as.numeric(ORIG_XTEST$device_conn_type))),  c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
        ORIG_XTEST['H23'] = CUT(sqrt(exp(as.numeric(ORIG_XTEST$site_category)) -  exp(-as.numeric(ORIG_XTEST$site_domain)) / 
                                  (as.numeric(ORIG_XTEST$device_conn_type))),  c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
        ORIG_XTEST['H24'] = CUT(sqrt(exp(as.numeric(ORIG_XTEST$app_category))  *  exp(-as.numeric(ORIG_XTEST$banner_pos)) / 
                                  as.numeric(ORIG_XTEST$site_category)),       c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
        ORIG_XTEST['H25'] = CUT(sqrt(exp(as.numeric(ORIG_XTEST$app_id))        +  exp(-as.numeric(ORIG_XTEST$banner_pos)) / 
                                  (as.numeric(ORIG_XTEST$app_category))),      c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
        ORIG_XTEST['H26'] = CUT(sqrt(exp(as.numeric(ORIG_XTEST$app_domain))    /  exp(-as.numeric(ORIG_XTEST$app_category)) / 
                                  (as.numeric(ORIG_XTEST$H18))),               c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
        ORIG_XTEST['H27'] = CUT(sqrt(exp(as.numeric(ORIG_XTEST$app_category))  -  exp(-as.numeric(ORIG_XTEST$device_conn_type)) / 
                                  (as.numeric(ORIG_XTEST$site_id))),           c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
        ORIG_XTEST['H28'] = CUT(sqrt(exp(as.numeric(ORIG_XTEST$site_domain))   /  exp(-as.numeric(ORIG_XTEST$app_category)) / 
                                  (as.numeric(ORIG_XTEST$H18))),               c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
        ORIG_XTEST['H29'] = CUT(sqrt(exp(-as.numeric(ORIG_XTEST$app_category))  +  exp(as.numeric(ORIG_XTEST$H18)) /
                                  (as.numeric(ORIG_XTEST$H14))),               c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
        ORIG_XTEST['H30'] = CUT(sqrt(exp(as.numeric(ORIG_XTEST$site_id))       +  exp(-as.numeric(ORIG_XTEST$H21)) / 
                                  (as.numeric(ORIG_XTEST$H19))),               c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    }


# ############################################################################################
# input conditioning, complete cases
# ############################################################################################
BANNER( 'CLEANING: COMPLETE CASES' )
    COMPLETE_SAMPLES = complete.cases(ORIG_XTEST)
    ORIG_XTEST = ORIG_XTEST[COMPLETE_SAMPLES,]
    rm( COMPLETE_SAMPLES)
# #####################################################################################
