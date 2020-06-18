# ############################################################################################
A2C_PREDICT = function(TRAIN=FALSE) {
    DEFAULT_PROB = 1/20

    if ( AWS ) {
        if ( TRAIN ) {
            DT0 = VECTOR(nrow(ORIG_XTRAIN), initval=DEFAULT_PROB)
        } else {
            DT0 = VECTOR(nrow(ORIG_XTEST), initval=DEFAULT_PROB)
        }
        DT0 = c(DT0)
        DT0 = unlist(DT0)
        print( summary( DT0 ) )
        return ( DT0 )
    }

    if ( TRAIN ) {
        if ( nrow(ORIG_XTRAIN)>100000 ) {
            DT0 = VECTOR(nrow(ORIG_XTRAIN))
            for ( i in 1:nrow(ORIG_XTRAIN)) {
                DT0[i] = DEFAULT_PROB
                app_id = ORIG_XTRAIN[i,"app_id"]
                if ( !is.null(A2C[[app_id]]) ) DT0[i] = A2C[[app_id]]
            }
            DT0 = c(DT0)
        } else {
            DT0 = sapply( ORIG_XTRAIN[,"app_id"], function(x) IFELSE(is.null(A2C[[x]]), DEFAULT_PROB, A2C[[x]]))
        }
    } else {
        #DT0 = sapply( ORIG_XTEST [,"app_id"], function(x) IFELSE(is.null(A2C[[x]]), DEFAULT_PROB, A2C[[x]]))
        DT0 = VECTOR(nrow(ORIG_XTEST))
        for ( i in 1:nrow(ORIG_XTEST)) {
            DT0[i] = DEFAULT_PROB
            app_id = ORIG_XTEST[i,"app_id"]
            if ( !is.null(A2C[[app_id]]) ) DT0[i] = A2C[[app_id]]
        }
    }

    DT0 = unlist(DT0)
    print( summary( DT0 ) )
    return ( DT0 )
}
# ############################################################################################




# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
    EPS <<- 1E-6

    ACP <<- list(
    "07d7df22"=c(5.118353e-01,1.310169e-01),
    "09481d60"=c(2.847333e-03,3.962222e-04),
    "0bfbc358"=c(2.466667e-05,6.666667e-07),
    "0f2161f8"=c(2.017882e-01,2.804244e-02),
    "0f9a328c"=c(1.464444e-04,2.222222e-05),
    "18b1e0be"=c(4.444444e-06,6.666667e-07),
    "2281a340"=c(7.044444e-05,1.333333e-06),
    "2fc4f2aa"=c(4.444444e-07,0.000000e+00),
    "4681bb9d"=c(8.311111e-05,1.666667e-05),
    "4b7ade46"=c(6.666667e-07,0.000000e+00),
    "4ce2e9fc"=c(5.937778e-04,7.488889e-05),
    "52de74cf"=c(2.222222e-07,0.000000e+00),
    "5326cf99"=c(6.666667e-07,0.000000e+00),
    "7113d72a"=c(6.000000e-06,2.222222e-07),
    "71af18ce"=c(2.222222e-07,0.000000e+00),
    "75d80bbe"=c(9.986667e-04,9.755556e-05),
    "79f0b860"=c(3.088889e-05,8.888889e-07),
    "86c1a5a3"=c(4.444444e-07,0.000000e+00),
    "879c24eb"=c(2.604444e-04,4.177778e-05),
    "8ded1f7a"=c(2.602667e-02,2.477778e-03),
    "8df2e842"=c(2.933333e-05,7.333333e-06),
    "a3c42688"=c(3.224444e-04,1.711111e-05),
    "a7fd01ec"=c(6.000000e-06,4.444444e-07),
    "a86a3e89"=c(9.333333e-05,1.022222e-05),
    "bd41f328"=c(2.222222e-07,0.000000e+00),
    "cef3e649"=c(4.662756e-02,7.188889e-03),
    "d1327cf5"=c(4.644000e-03,4.137778e-04),
    "dc97ec06"=c(4.711111e-04,1.120000e-04),
    "ef03ae90"=c(2.222222e-07,0.000000e+00),
    "f95efa07"=c(2.688200e-02,5.451556e-03),
    "fc6fa53d"=c(7.864444e-04,2.666667e-05),
    "other"   =c(EPS,EPS))
    
    SCP <<- list(
    '0569f928'=c(1.781111e-03,5.955556e-05),
    '110ab22d'=c(2.222222e-07,0.000000e+00),
    '28905ebd'=c(1.741616e-01,4.387978e-02),
    '335d28a8'=c(4.097333e-03,4.651111e-04),
    '3e814130'=c(5.154733e-02,1.880289e-02),
    '42a36e14'=c(2.400000e-05,5.111111e-06),
    '50e219e0'=c(3.505209e-01,5.587667e-02),
    '5378d028'=c(1.444444e-05,4.444444e-07),
    '70fb0e29'=c(1.063111e-03,1.515556e-04),
    '72722551'=c(1.800889e-03,1.504444e-04),
    '74073276'=c(4.444444e-07,0.000000e+00),
    '75fa27f6'=c(2.735778e-03,4.708889e-04),
    '76b2941d'=c(2.892000e-03,8.466667e-05),
    '8fd0aea4'=c(1.177778e-05,8.888889e-07),
    '9ccfa2ea'=c(1.555556e-06,0.000000e+00),
    'a818d37a'=c(3.128889e-04,6.666667e-07),
    'bcf865d9'=c(1.577778e-05,1.555556e-06),
    'c0dd3be3'=c(6.017778e-04,1.057778e-04),
    'dedf689d'=c(3.093333e-04,2.466667e-05),
    'e787de0e'=c(6.888889e-05,8.444444e-06),
    'f028772b'=c(2.236716e-01,5.509911e-02),
    'f66779e6'=c(8.949111e-03,2.300000e-04),
    'other'=c(EPS,EPS))

    #t = read.csv( "clt_app_id_cat.csv", header=TRUE, stringsAsFactors=FALSE, colClasses=c('character',rep('numeric',2)))
    #SCP  <<- list(); for (i in 1:nrow(t)) SCP[[t[i,1]]] = c(t[i,3], t[i,2]/ 4.6E6)
    #SCP[['other']] = c(EPS,EPS)
    #t = read.csv( "clt_site_id_cat.csv", header=TRUE, stringsAsFactors=FALSE, colClasses=c('character',rep('numeric',2)))
    #ACP  <<- list(); for (i in 1:nrow(t)) ACP[[t[i,1]]] = c(t[i,3], t[i,2]/ 4.6E6)
    #ACP[['other']] = c(EPS,EPS)

    F_APP_CAT  <<- c('07d7df22','0f2161f8','f95efa07','cef3e649','8ded1f7a','d1327cf5','dc97ec06','09481d60','75d80bbe','4ce2e9fc','879c24eb','fc6fa53d','4681bb9d','0f9a328c','a3c42688','8df2e842','a86a3e89','a7fd01ec','79f0b860','2281a340','18b1e0be','0bfbc358','0d82db25','2fc4f2aa','7113d72a','71af18ce','5326cf99','other')
    F_SITE_CAT  <<- c( 'f028772b','50e219e0','28905ebd','3e814130','75fa27f6','335d28a8','dedf689d','f66779e6','c0dd3be3','70fb0e29','76b2941d','72722551','0569f928','42a36e14','8fd0aea4','e787de0e','bcf865d9','5378d028','a818d37a','9ccfa2ea','74073276', 'other')

    # #######################################################################################
    # name-fields were reversed in extractor
    # #######################################################################################
    t = read.csv( "clt_site_id.csv", header=TRUE, stringsAsFactors=FALSE, colClasses=c('character',rep('numeric',2)))
    A2C  <<- list(); for (i in 1:nrow(t)) A2C[[t[i,1]]] = t[i,3]
    A_ids1  <<- as.data.frame(t)
    rownames(A_ids1) = t[,1]
    A_ids  <<- A_ids1

    t = read.csv( "clt_app_id.csv",  header=TRUE, stringsAsFactors=FALSE, colClasses=c('character',rep('numeric',2)))
    S2C  <<- list(); for (i in 1:nrow(t)) S2C[[t[i,1]]] = t[i,3]
    S_ids1  <<- as.data.frame(t)
    rownames(S_ids1) = t[,1]
    S_ids  <<- S_ids1
    rm(t)

    # #######################################################################################
    #S2C  <<- unlist(S2C)
    #A2C  <<- unlist(A2C)
    #S_S2C   <<- c(-Inf,seq(0,.20,0.005),seq(0.3,1,0.1),Inf)
    #S_A2C   <<- c(-Inf,seq(0,.20,0.005),seq(0.3,1,0.1),Inf)
    # #######################################################################################
    
    t = read.csv( "clt_probs_site_cat.csv", header=TRUE, stringsAsFactors=FALSE, sep=",", colClasses=c('character',rep('numeric',3)))
    S2CHP  <<- list(); for (i in 1:nrow(t)) S2CHP[[t[i,1]]] = t[i,4]
    t = read.csv( "clt_probs_app_cat.csv", header=TRUE, stringsAsFactors=FALSE, sep=",", colClasses=c('character',rep('numeric',3)))
    A2CHP  <<- list(); for (i in 1:nrow(t)) A2CHP[[t[i,1]]] = t[i,2]/6865066.
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
