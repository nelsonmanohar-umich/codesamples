# ############################################################################################
# input conditioning, complete cases
# ############################################################################################
BANNER( 'CLEANING: COMPLETE CASES' )
    COMPLETE_SAMPLES = complete.cases(ORIG_XTRAIN)
    ORIG_XTRAIN = ORIG_XTRAIN[COMPLETE_SAMPLES,]
    rm( COMPLETE_SAMPLES)
# #####################################################################################


# #####################################################################################
# IMPUTATION HANDLING OF MISSING DATA COLUMNS
# #####################################################################################
BANNER( 'CLEANING: MISSING DATA' )
    # ORIG_XTRAIN = RECODE_MISSING_VALUES_WITH_MEAN( ORIG_XTRAIN, 'C20', NA_SYMBOL=-1 )
# #####################################################################################


# #####################################################################################
# add preliminary auto-factor versions of the numeric fields
# #####################################################################################
BANNER( 'CLEANING: INTRODUCING FACTOR VERSIONS OF NUMERIC FIELDS VIA PERCENTILE RECODING' )
if ( USE_EXPLORATORY_TRANSFORMS ) {
    NUMERIC_FIELDS_OF_CONCERN =            c('C14','C15','C16','C17','C18','C19','C21')
    NUMERIC_FIELDS_OF_CONCERN_AS_FACTORS = c('F14','F15','F16','F17','F18','F19','F21')
    ORIG_XTRAIN[, 'F14' ] = CUT(ORIG_XTRAIN[, 'C14'], 24)
    ORIG_XTRAIN[, 'F15' ] = CUT(ORIG_XTRAIN[, 'C15'], 24)
    ORIG_XTRAIN[, 'F16' ] = CUT(log(ORIG_XTRAIN[, 'C16']+1), 8)
    ORIG_XTRAIN[, 'F17' ] = CUT(ORIG_XTRAIN[, 'C17'], 24)
    ORIG_XTRAIN[, 'F18' ] = CUT(ORIG_XTRAIN[, 'C18'], 8)
    ORIG_XTRAIN[, 'F19' ] = CUT(log(ORIG_XTRAIN[, 'C19']+1), 24)
    ORIG_XTRAIN[, 'F21' ] = CUT(ORIG_XTRAIN[, 'C21'], 24)
}
# #####################################################################################


# #####################################################################################
BANNER( 'COMPUTING APPID CLICK PROBABILITIES' )
    SPROBS = c(-Inf,seq(0,0.40,0.05),seq(0.5,1,0.1))
    # ORIG_XTRAIN['probs'] = round(A2C_PREDICT(TRAIN=TRUE),1)
    # ORIG_XTRAIN['probs'] = CUT( ORIG_XTRAIN[,"probs"], SPROBS )
# #####################################################################################


# #####################################################################################
# scale uneven numeric fields
# #####################################################################################
BANNER( 'CLEANING: INTRODUCING FACTOR VERSIONS OF NUMERIC FIELDS USING THEIR LABELS' )
    S01 = c(-100,seq(1000,1020,1),Inf)
# #####################################################################################
    S14 = c(-100,1000, 2500, seq(5000,15000,50),seq(15500,2.225e+04,250),Inf)
    S14 = c(-100,1000, 2500, seq(5000,15000,100),seq(15500,2.225e+04,250),Inf)
# #####################################################################################
    S15 = c(-100, 100, 250, 310, 400, 700, 900, 1100, Inf )
    S16 = c(-100, 50, 100, 150, 250,  500, 750, 1000, Inf ) 
    S17 = c(-100, 100, 150, 250, 450, 500, 575, 600, 650, 750, 800, 950, 1150, 1250, 1450, 1550, seq(1700,3000,40),Inf)
    S18 = c(-100, -0.5, 0.5, 1.5, 2.5, 3.5, Inf )
    S19 = c(-100, 5, 10, 15, 20, 25, 50, 200, 350, 500, 650, 750, 850, 975, seq(1000,2000,100), Inf)
    S20 = c(-100,0,seq(100000,100250,10), Inf)
    S21 = c(-100,0,10,20, 25, 35, 45, 50, 55, 65, 70, 75, 85, 95, 100, 105, 115, 125, 150, 170, 215, 225, 250, 400, Inf)
    S44 = c(-100,1000, 2500, seq(5000,15000,250),seq(15500,2.2e+04,1000),Inf)

    ORIG_XTRAIN['H01'] = CUT(ORIG_XTRAIN[,'C01'], S01)#, labels=FALSE)#, ordered_result=TRUE )
    ORIG_XTRAIN['H14'] = CUT(ORIG_XTRAIN[,'C14'], S14)#, labels=FALSE)#, ordered_result=TRUE )
    ORIG_XTRAIN['H15'] = CUT(ORIG_XTRAIN[,'C15'], S15)#, labels=FALSE)#, ordered_result=TRUE )
    ORIG_XTRAIN['H16'] = CUT(ORIG_XTRAIN[,'C16'], S16)#, labels=FALSE)#, ordered_result=TRUE )
    ORIG_XTRAIN['H17'] = CUT(ORIG_XTRAIN[,'C17'], S17)#, labels=FALSE)#, ordered_result=TRUE )
    ORIG_XTRAIN['H18'] = CUT(ORIG_XTRAIN[,'C18'], S18)#, labels=FALSE)#, ordered_result=TRUE )
    ORIG_XTRAIN['H19'] = CUT(ORIG_XTRAIN[,'C19'], S19)#, labels=FALSE)#, ordered_result=TRUE )
    #ORIG_XTRAIN$C20 = IFELSE( as.character(ORIG_XTRAIN$C20) == "-1", ORIG_XTRAIN$C20, 0 )
    ORIG_XTRAIN['H20'] = CUT(ORIG_XTRAIN[,'C20'], S20)#, labels=FALSE)#, ordered_result=TRUE )
    ORIG_XTRAIN['H21'] = CUT(ORIG_XTRAIN[,'C21'], S21)#, labels=FALSE)#, ordered_result=TRUE )
    ORIG_XTRAIN['H44'] = CUT(ORIG_XTRAIN[,'C14'], S44)#, labels=FALSE)#, ordered_result=TRUE )
    ORIG_XTRAIN['H54'] = CUT(ORIG_XTRAIN[,'C14'] + ORIG_XTRAIN[,'banner_pos']*1000., S44)#, labels=FALSE)#, ordered_result=TRUE )
# #####################################################################################


# #####################################################################################
# scale uneven numeric fields
# #####################################################################################
BANNER( 'CLEANING: SCALING NUMERIC FIELDS' )
if ( USE_EXPLORATORY_TRANSFORMS ) {
    NUMERIC_FIELDS_OF_CONCERN =            c('C14','C15','C16','C17','C18','C19','C21')
    XYi = ORIG_XTRAIN[, NUMERIC_FIELDS_OF_CONCERN ]
    XYinew = scale( XYi, center=FALSE, scale=TRUE)
    XYinew_centers = attr(XYinew, "scaled:center" )
    XYinew_scales  = attr(XYinew, "scaled:scale" )
    colnames(XYinew) = colnames(XYi)
    rownames(XYinew) = rownames(XYi)
    for ( n1 in NUMERIC_FIELDS_OF_CONCERN ) {
        ORIG_XTRAIN[, n1 ] = XYinew[, n1]
    }
}
# #####################################################################################


# #####################################################################################
# recode meaningless timestamp into two new time variables
# #####################################################################################
BANNER( 'FEATURES: RECODING TIMESTAMPS' )
    ORIG_XTRAIN[,"hour"] = factor(as.integer(substr(as.character(ORIG_XTRAIN$YYMMDDHH),7,8)), 0:23)
    ORIG_XTRAIN[,"day"] = factor((as.integer(substr(as.character(ORIG_XTRAIN$YYMMDDHH),5,6))-20)%%7, 0:7)#, ordered=TRUE)
    ORIG_XTRAIN[,"shift"] = factor(as.integer(ORIG_XTRAIN[,"day"])*4 + as.integer(as.integer(substr(as.character(ORIG_XTRAIN$YYMMDDHH),7,8))/8), 0:30)
    #ORIG_XTRAIN[,"shift"] = factor(as.integer(as.integer(substr(as.character(ORIG_XTRAIN$YYMMDDHH),7,8))/4), 0:6)

    ORIG_XTRAIN[,"device_model"] = as.factor(substr(ORIG_XTRAIN$device_model,1,8))
    ORIG_XTRAIN[,"device_ip"] = as.factor(substr(ORIG_XTRAIN$device_ip,1,8))
    ORIG_XTRAIN[,"adix"] = CUT( as.numeric(str_sub(ORIG_XTRAIN$ad_id, 1, 2)), c(-Inf,seq(0,99,2), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN[,"adix_ts"] = CUT( as.numeric(str_sub(ORIG_XTRAIN$ad_id, -2, -1)), c(-Inf,seq(0,99,2), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN[,"adix_ts2"] = CUT( as.numeric(str_sub(ORIG_XTRAIN$ad_id, -4, -3)), c(-Inf,seq(0,99,2), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN[,"adix_ts6"] = CUT( log(as.numeric(str_sub(ORIG_XTRAIN$ad_id, -6, -1))+1), c(-Inf,seq(0,20,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
# #####################################################################################


# #####################################################################################
BANNER( 'FEATURES: PROBABILITY PRIOR FEATURES' )
    #ORIG_XTRAIN[,"s2c"] = SAPPLY( ORIG_XTRAIN$site_id, function(x) S2C[[x]] ) #; ORIG_XTRAIN[is.null(ORIG_XTRAIN$s2c),"s2c"] = 0
    #ORIG_XTRAIN[,"a2c"] = SAPPLY( ORIG_XTRAIN$app_id,  function(x) A2C[[x]] ) #; ORIG_XTRAIN[is.null(ORIG_XTRAIN$a2c),"a2c"] = 0
    #ORIG_XTRAIN[,"s2c"] = CUT(ORIG_XTRAIN[,"s2c"] , S_S2C)#, ordered=TRUE)
    #ORIG_XTRAIN[,"a2c"] = CUT(ORIG_XTRAIN[,"a2c"] , S_A2C)#, ordered=TRUE)

    ORIG_XTRAIN[,'app_category']   = factor( ORIG_XTRAIN[,"app_category"],  F_APP_CAT,  ordered=TRUE )
    ORIG_XTRAIN[,'site_category']  = factor( ORIG_XTRAIN[,"site_category"], F_SITE_CAT, ordered=TRUE )
    ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "site_category",N=23, other="other" )
    ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "app_category", N=32, other="other" )

    #SCP0 = c(-Inf,seq(0,15,1/3),Inf)
    #ACP0 = c(-Inf,seq(0,15,1/3),Inf)
    #SCP1 = c(-Inf, seq(-5,0,0.1), Inf)
    #ACP1 = c(-Inf, seq(-5,0,0.1), Inf)
    SCP0 = c(-Inf,seq(0,0.1,0.005),seq(0.11,4,1/5),Inf)
    ACP0 = c(-Inf,seq(0,0.05,0.0025),seq(0.51,3,1/10),Inf)
    SCP1 = c(-Inf, seq(0,0.07,0.0015), Inf)
    ACP1 = c(-Inf, seq(0,0.01,0.0005), .15, .25, Inf)
    SCP2 = c(-Inf, seq(0,1,0.02), Inf)
    ACP2 = c(-Inf, seq(0,0.1,0.005), Inf)

    print( "sitecatclickprobs0" )
    ORIG_XTRAIN[,"sitecatclickprobs0"] =  SAPPLY( ORIG_XTRAIN[,"site_category"], function(x) IFELSE(is.null(SCP[[x]]), 0, SCP[[x]][1]))
    ORIG_XTRAIN[,"sitecatclickprobs0"] =  CUT( (as.numeric(ORIG_XTRAIN[,"hour"]) * ORIG_XTRAIN[,"sitecatclickprobs0"]+1), SCP0)#, labels=FALSE)#, ordered_result=TRUE )

    print( "appcatclickprobs0" )
    ORIG_XTRAIN[,"appcatclickprobs0"] =  SAPPLY( ORIG_XTRAIN[,"app_category"],  function(x) IFELSE(is.null(ACP[[x]]), 0, ACP[[x]][1]))
    ORIG_XTRAIN[,"appcatclickprobs0"] =  CUT( (as.numeric(ORIG_XTRAIN[,"hour"]) * ORIG_XTRAIN[,"appcatclickprobs0"]+1), ACP0)#, labels=FALSE)#, ordered_result=TRUE )

    print( "sitecatclickprobs1" )
    ORIG_XTRAIN[,"sitecatclickprobs1"] =  SAPPLY( ORIG_XTRAIN[,"site_category"], function(x) IFELSE(is.null(SCP[[x]]), 0, SCP[[x]][2]))
    ORIG_XTRAIN[,"sitecatclickprobs1"] =  CUT( (ORIG_XTRAIN[,"sitecatclickprobs1"]), SCP1)#, labels=FALSE)#, ordered_result=TRUE )

    print( "appcatclickprobs1" )
    ORIG_XTRAIN[,"appcatclickprobs1"] =  SAPPLY( ORIG_XTRAIN[,"app_category"],  function(x) IFELSE(is.null(ACP[[x]]), 0, ACP[[x]][2]))
    ORIG_XTRAIN[,"appcatclickprobs1"] =  CUT( (ORIG_XTRAIN[,"appcatclickprobs1"]), ACP1)#, labels=FALSE)#, ordered_result=TRUE )

    print( "sitecatclickprobs2" )
    ORIG_XTRAIN[,"sitecatclickprobs2"] =  paste( ORIG_XTRAIN[,"site_category"], ORIG_XTRAIN[,"hour"], sep=":" )
    ORIG_XTRAIN[,"sitecatclickprobs2"] =  SAPPLY( ORIG_XTRAIN[,"sitecatclickprobs2"], function(x) IFELSE(is.null(S2CHP[[x]]), 0, S2CHP[[x]]))
    ORIG_XTRAIN[,"sitecatclickprobs2"] =  CUT( ORIG_XTRAIN[,"sitecatclickprobs2"], SCP2)#, labels=FALSE)#, ordered_result=TRUE )

    print( "appcatclickprobs2 skipped" )
    #ORIG_XTRAIN[,"appcatclickprobs2"] =  paste( ORIG_XTRAIN[,"app_category"], ORIG_XTRAIN[,"hour"], sep=":" )
    #ORIG_XTRAIN[,"appcatclickprobs2"] =  SAPPLY( ORIG_XTRAIN[,"appcatclickprobs2"], function(x) IFELSE(is.null(A2CHP[[x]]), 0, A2CHP[[x]]))
    #ORIG_XTRAIN[,"appcatclickprobs2"] =  CUT( ORIG_XTRAIN[,"appcatclickprobs2"], ACP2 )
# #####################################################################################


# #####################################################################################
# CLICKPROBS
# #####################################################################################
p = function(fl,y,ft) { ft[fl,y]/(ft[fl,1]+ft[fl,2]) * (ft[fl,1]+ft[fl,2])/sum(ft[1:nrow(ft),1:2]) }
NDIGITS = 4
clicktable = table(ORIG_XTRAIN$hour, ORIG_XTRAIN$click)
CLICKPROBS = as.data.frame(MATRIX(nrow(clicktable), 4))
CLICKPROBS[,1] = clicktable[,1] 
CLICKPROBS[,2] = clicktable[,2] 
#CLICKPROBS[,3] = clicktable[,2]/(clicktable[,1]+clicktable[,2])
NCLICKEVENTS = sum(CLICKPROBS[,1:2])
CLICKPROBS[,3] = p(1:nrow(clicktable),2,clicktable) # CLICKPROBS[,3] = clicktable[,2]/NCLICKEVENTS
CLICKPROBS[,4] = names(clicktable[,1])
hours = as.integer(ORIG_XTRAIN[,"hour"]); 
ORIG_XTRAIN[,"hourclickprobs"] =  round( CLICKPROBS[hours,3], NDIGITS)
TRAINED_HOURCLICKPROBS = CLICKPROBS
# #####################################################################################
clicktable = table(ORIG_XTRAIN$shift, ORIG_XTRAIN$click)
CLICKPROBS = as.data.frame(MATRIX(nrow(clicktable), 4))
CLICKPROBS[,1] = clicktable[,1] 
CLICKPROBS[,2] = clicktable[,2] 
NCLICKEVENTS = sum(CLICKPROBS[,1:2])
#CLICKPROBS[,3] = clicktable[,2]/(clicktable[,1]+clicktable[,2])
CLICKPROBS[,3] = p(1:nrow(clicktable),2,clicktable) # CLICKPROBS[,3] = clicktable[,2]/NCLICKEVENTS
CLICKPROBS[,4] = names(clicktable[,1])
shifts = as.integer(ORIG_XTRAIN[,"shift"]); 
ORIG_XTRAIN[,"shiftclickprobs"] =  round( CLICKPROBS[shifts,3], NDIGITS)
TRAINED_SHIFTCLICKPROBS = CLICKPROBS
# #####################################################################################
clicktable = table(ORIG_XTRAIN$day, ORIG_XTRAIN$click)
CLICKPROBS = as.data.frame(MATRIX(nrow(clicktable), 4))
CLICKPROBS[,1] = clicktable[,1] 
CLICKPROBS[,2] = clicktable[,2] 
NCLICKEVENTS = sum(CLICKPROBS[,1:2])
#CLICKPROBS[,3] = clicktable[,2]/(clicktable[,1]+clicktable[,2])
CLICKPROBS[,3] = p(1:nrow(clicktable),2,clicktable) # CLICKPROBS[,3] = clicktable[,2]/NCLICKEVENTS
CLICKPROBS[,4] = names(clicktable[,1])
days = as.integer(ORIG_XTRAIN[,"day"]); 
ORIG_XTRAIN[,"dayclickprobs"] =  round( CLICKPROBS[days,3], NDIGITS)
TRAINED_DAYCLICKPROBS = CLICKPROBS
rm(clicktable)
# #####################################################################################


# #####################################################################################
# recode some inconvenient factors
# #####################################################################################
BANNER( 'FEATURES: RECODING FACTORS' )
    ORIG_XTRAIN[,'site_id']          = factor(ORIG_XTRAIN[,'site_id'],   S_ids[,1],  ordered=TRUE)
    ORIG_XTRAIN[,'app_id']           = factor(ORIG_XTRAIN[,'app_id'],    A_ids[,1],  ordered=TRUE)
    ORIG_XTRAIN[,"banner_pos"]       = factor(ORIG_XTRAIN[,"banner_pos"],            ordered=TRUE)
    ORIG_XTRAIN[,"device_type"]      = factor(ORIG_XTRAIN[,"device_type"],           ordered=TRUE)
    ORIG_XTRAIN[,"device_conn_type"] = factor(ORIG_XTRAIN[,"device_conn_type"],      ordered=TRUE)

    #ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "ad_id",           N=1024, other="other" )
    ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "device_type",      N=7,    other="other" )
    ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "device_conn_type", N=7,    other="other" )
    ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "banner_pos",       N=7,    other="other" )

    # FROM BOOTSTRAP DECISION TREE 
    # "click ~   site_id + device_model + site_domain + app_id + C16 + C15 + site_category + app_domain + banner_pos + C01 + app_category + C17 + C14 + device_type + C20 + C19 + device_conn_type + C21 + YYMMDDHH "

    if ( TRUE ) {
        # ---------
        N_1Q = 1023;    N_2Q = 255 ;    N_3Q = 127 ;  N_4Q = 63
        N_1Q = 127 ;    N_2Q = 127 ;    N_3Q = 127 ;  N_4Q = 127
        N_1Q = 63;      N_2Q = 63;      N_3Q = 63;    N_4Q = 63
        N_1Q = 64;      N_2Q = 32;      N_3Q = 16;    N_4Q = 16
        N_1Q = 64;      N_2Q = 32;      N_3Q = 20;    N_4Q = 12
        N_1Q = 254 ;    N_2Q = 127 ;    N_3Q = 63 ;   N_4Q = 31
        N_1Q = 253 ;    N_2Q = 126 ;    N_3Q = 62 ;   N_4Q = 30
        # ---------
        # ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "YYMMDDHH",         N=GET_NUMLEVELS('YYMMDDHH',255),  other="other" )
        ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "site_id",          N=GET_NUMLEVELS('site_id',N_1Q),      other="other" )
        ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "site_domain",      N=GET_NUMLEVELS('site_domain',N_2Q),  other="other" )
        ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "device_model",     N=GET_NUMLEVELS('device_model',N_2Q), other="other" )
        ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "device_ip",        N=GET_NUMLEVELS('device_ip',N_2Q),    other="other" )
        ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "app_id",           N=GET_NUMLEVELS('app_id',N_3Q),       other="other" )
        ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "app_domain",       N=GET_NUMLEVELS('app_domain',N_4Q),   other="other" )
        ORIG_XTRAIN = RECODE_FACTOR( ORIG_XTRAIN, "device_id",        N=GET_NUMLEVELS('device_id',N_4Q),    other="other" )
    }

    if ( LAPTOP & FALSE ) {
        graphics.off()
        par(mfrow=c(3,3))
        plot( table(ORIG_XTRAIN$H22, ORIG_XTRAIN$click))
        plot( table(ORIG_XTRAIN$H23, ORIG_XTRAIN$click))
        plot( table(ORIG_XTRAIN$H24, ORIG_XTRAIN$click))
        plot( table(ORIG_XTRAIN$H25, ORIG_XTRAIN$click))
        plot( table(ORIG_XTRAIN$H26, ORIG_XTRAIN$click))
        plot( table(ORIG_XTRAIN$H27, ORIG_XTRAIN$click))
        plot( table(ORIG_XTRAIN$H28, ORIG_XTRAIN$click))
        plot( table(ORIG_XTRAIN$H29, ORIG_XTRAIN$click))
        plot( table(ORIG_XTRAIN$H30, ORIG_XTRAIN$click))
    }


# #####################################################################################
# generate some new numerical feature
# #####################################################################################
ADDITIONAL_INSIGHT  = FALSE
if ( ADDITIONAL_INSIGHT ) {
    ORIG_XTRAIN['H22'] = CUT(sqrt(exp(as.numeric(ORIG_XTRAIN$site_domain))   -  exp(-as.numeric(ORIG_XTRAIN$site_category)) / 
                                  (as.numeric(ORIG_XTRAIN$device_conn_type))),  c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN['H23'] = CUT(sqrt(exp(as.numeric(ORIG_XTRAIN$site_category)) -  exp(-as.numeric(ORIG_XTRAIN$site_domain)) / 
                                  (as.numeric(ORIG_XTRAIN$device_conn_type))),  c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN['H24'] = CUT(sqrt(exp(as.numeric(ORIG_XTRAIN$app_category))  *  exp(-as.numeric(ORIG_XTRAIN$banner_pos)) / 
                                  as.numeric(ORIG_XTRAIN$site_category)),       c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN['H25'] = CUT(sqrt(exp(as.numeric(ORIG_XTRAIN$app_id))        +  exp(-as.numeric(ORIG_XTRAIN$banner_pos)) / 
                                  (as.numeric(ORIG_XTRAIN$app_category))),      c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN['H26'] = CUT(sqrt(exp(as.numeric(ORIG_XTRAIN$app_domain))    /  exp(-as.numeric(ORIG_XTRAIN$app_category)) / 
                                  (as.numeric(ORIG_XTRAIN$H18))),               c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN['H27'] = CUT(sqrt(exp(as.numeric(ORIG_XTRAIN$app_category))  -  exp(-as.numeric(ORIG_XTRAIN$device_conn_type)) / 
                                  (as.numeric(ORIG_XTRAIN$site_id))),           c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN['H28'] = CUT(sqrt(exp(as.numeric(ORIG_XTRAIN$site_domain))   /  exp(-as.numeric(ORIG_XTRAIN$app_category)) / 
                                  (as.numeric(ORIG_XTRAIN$H18))),               c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN['H29'] = CUT(sqrt(exp(-as.numeric(ORIG_XTRAIN$app_category))  +  exp(as.numeric(ORIG_XTRAIN$H18)) /
                                  (as.numeric(ORIG_XTRAIN$H14))),               c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
    ORIG_XTRAIN['H30'] = CUT(sqrt(exp(as.numeric(ORIG_XTRAIN$site_id))       +  exp(-as.numeric(ORIG_XTRAIN$H21)) / 
                                  (as.numeric(ORIG_XTRAIN$H19))),               c(-100, seq(0,7,0.5), Inf))#, labels=FALSE)#, ordered_result=TRUE)
}
# #####################################################################################


# #####################################################################################
# generate some new numerical feature
# #####################################################################################
BANNER( 'FEATURES: POLINOMIAL FEATURES' )
if ( USE_EXPLORATORY_TRANSFORMS ) {
    C22 = apply(ORIG_XTRAIN[,c("C14","C15","C16","C17","C18", "C19", "C21")],1, function(x) ( log(x[1]^2 * x[4]^2 + 1 ) ))
    C23 = apply(ORIG_XTRAIN[,c("C14","C15","C16","C17","C18", "C19", "C21")],1, function(x) ( sqrt(sum(x)) ))
    C24 = apply(ORIG_XTRAIN[,c("C14","C15","C16","C17","C18", "C19", "C21")],1, function(x) ( log(x[1]^2 + x[4]^2 + x[5]^2 + 1 )))
    ORIG_XTRAIN[,"C22"] = C22 
    ORIG_XTRAIN[,"C23"] = C23 
    ORIG_XTRAIN[,"C24"] = C24 
}
# #####################################################################################


# ############################################################################################
# input conditioning, complete cases
# ############################################################################################
BANNER( 'CLEANING: COMPLETE CASES' )
    COMPLETE_SAMPLES = complete.cases(ORIG_XTRAIN)
    ORIG_XTRAIN = ORIG_XTRAIN[COMPLETE_SAMPLES,]
    rm( COMPLETE_SAMPLES)
# #####################################################################################


