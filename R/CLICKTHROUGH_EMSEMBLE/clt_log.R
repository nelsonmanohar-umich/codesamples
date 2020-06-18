## # ############################################################################################
## COLS = function( i ) {
## 
##     if ( USING_SUBSUMED_MODELS ) { 
##         if ( i == 7 ) i = 1
##         if ( i == 8 ) i = 3
##         if ( i == 9 ) i = 5
##     }
## 
##     if ( i == 1 )  {
##         cols = c('H44', 'H15', 'H16', 'H18', 'H19', 'H20', 'H21',
##                     'banner_pos', 'adix', 'hour',
##                     'app_id', 'app_domain', 'app_category',
##                     'site_domain', 'site_category')
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'hour',
##                     'app_id', 'app_domain', 'app_category',
##                     'site_domain', 'site_category')
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'hour',
##                     'app_id', 'app_domain', 'app_category',
##                     'site_domain', 'site_category',
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'hour',
##                     'app_id', 'app_domain', 'app_category',
##                     'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'hour',
##                     'app_id', 'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix',
##                     'app_id', 'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'adix_ts',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix_ts',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'adix_ts', 'hour',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', 'H54',
##                     'banner_pos', 'adix_ts', 'hour',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', 'H54',
##                     'banner_pos', 'adix', 'adix_ts', 'hour',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', 'H54',
##                     'banner_pos', 'adix', 'adix_ts',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', 'H54',
##                     'banner_pos', 'adix', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'adix', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'adix',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'adix',
##                     'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos',
##                     'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos',
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', #H01 poss
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', #H01 poss
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day')
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', #H01 poss
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day')
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H17', 'H20',
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_category',     
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'day')
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H17', 'H20', 'H14',
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_category',     
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'sitecatclickprobs2', 'appcatclickprobs2',
##                     'day')
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H17', 'H20',
##                     'adix_ts2',
##                     'app_category',
##                     'site_id', 'site_category',     
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'sitecatclickprobs2', 'appcatclickprobs2',
##                     'day')
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H18', 'H20', #H01 poss
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day')
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H18', ##'H20', #H01 poss
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', #####'site_domain', 
##                     'site_category',
##                      #appcatclickprobs0,
##                      "sitecatclickprobs1",
##                      #####'probs', ####
##                      'hour', ####
##                      'day')
##     }
##     if ( i == 2 ) {
##         cols = c(
##                     'H14', 'H44',
##                     'banner_pos',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_conn_type' )
##         cols = c(
##                     'H14', 'H44',
##                     'banner_pos', 'adix_ts',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_conn_type' )
##         cols = c(
##                     'H14', 'H44',
##                     'banner_pos', 'adix', 'adix_ts',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_conn_type' )
##         cols = c(
##                     'H14', 'H44',
##                     'banner_pos', 'adix', 'adix_ts',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_conn_type' )
##         cols = c(
##                     'H14', 'H44',
##                     'banner_pos', 'adix', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1", 
##                     'device_type', 'device_conn_type' ) #day
##         cols = c( # attempted the cp param otherwise it needs to revert to two above
##                     'H14', 'H44',
##                     'banner_pos', 'adix', 'adix_ts', #'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1", 
##                     'device_type', 'device_conn_type' ) #day
##         cols = c( # attempted the cp param otherwise it needs to revert to two above        # SUBMITTED TO 0.41
##                     'H14', 'H44', "H21", "H20",
##                     'banner_pos', 'adix', 'adix_ts', #'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1", 
##                     'device_type', 'device_conn_type' ) #day
##         cols = c( # attempted the cp param otherwise it needs to revert to two above        # seeking lowering to 0.39
##                     'H14', 'H44', "H21", "H20", "H19",
##                     'banner_pos', 'adix', 'adix_ts', #'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1", 
##                     'device_type', 'device_conn_type' ) #day
##         cols = c( # 
##                     'H14', "H21", "H20", "H19",
##                     'banner_pos', 'adix_ts2',
##                     'app_domain',
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1", 
##                     'day',
##                     'device_type', 'device_conn_type' )
##         cols = c( # 
##                     "H21", "H20", "H19", "H15", "H17", 'H01', 'H14',
##                     'banner_pos', 'hour', 'adix_ts2',
##                     'app_category',
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1", 
##                     'sitecatclickprobs2', 'appcatclickprobs2',
##                     'day'
##                     )
##         cols = c( # 
##                     "H21", "H20", "H19", "H15", "H17", 'H01',
##                     'banner_pos', 'adix_ts2',
##                     'app_category',
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1", 
##                     'appcatclickprobs2',
##                     'day')
##         cols = c( # 
##                     "H21", "H20", "H19", "H15", "H17", 'H01',
##                     'banner_pos', 'adix_ts2',
##                     'app_category',
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1", 
##                     'appcatclickprobs2',
##                     'hour',
##                     'day')
##         cols = c( "H21", "H20", "H19", "H15", "H17", 'H01',
##                     ##'banner_pos', 
##                     'adix_ts2',
##                     'app_category',
##                     'site_category', 'site_id',
##                     # appcatclickprobs0,
##                     "sitecatclickprobs1", 
##                     #'appcatclickprobs2',
##                     'hour') #'day'
##     }
##     if ( i == 3 ) {
##         cols = c( 'H44', 'H19', 'H21', 'H01',
##                     'banner_pos', 'adix', 'hour',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                     'device_type', 'device_ip' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'hour',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                     'device_type', 'device_ip' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'hour', 'adix_ts',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                     'device_type', 'device_ip' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                     'device_type', 'device_ip' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                     'device_type', 'device_ip' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix', 'adix_ts',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                     'device_type', 'device_ip' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos',
##                     'adix_ts6',
##                     'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', #H20 poss 
##                     'adix_ts6',
##                     'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18',
##                     'adix_ts6',
##                     'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H18',
##                     'adix_ts6',
##                     'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_category', 'app_id',
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'day')
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H20', 'H17', 'H14',
##                     'banner_pos', 'adix_ts6',
##                     'app_id',
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'sitecatclickprobs2', 'appcatclickprobs2',
##                     'day')
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H20', 'H17',
##                     'banner_pos', 'adix_ts6',
##                     'app_id',
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'appcatclickprobs2',
##                     'day')
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H20', 'H17',
##                     'banner_pos', 'adix_ts6',
##                     'app_id',
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'appcatclickprobs2',
##                     #'hour',
##                     'day')
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H17', ##'H20', 
##                     #'banner_pos', 
##                     'adix_ts6',
##                     'app_id',
##                     'site_category', 'site_id',
##                     #"appcatclickprobs0", 
##                     "sitecatclickprobs1",
##                     #'appcatclickprobs2',
##                     #'hour',
##                     'day')
##     }
##     if ( i == 4 ) {
##         cols = c( 'H44', 'H19', 'H21', 'H01',
##                     'banner_pos',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H20', 'H18',
##                     'banner_pos', 'adix',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H20', 'H18', 'H16',
##                     'banner_pos', 'adix', 'hour',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'hour',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'hour',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'hour',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'banner_pos', 'adix', 'hour',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20', 'H54',
##                     'banner_pos', 'hour', 'adix_ts', 'shift',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20', 'H54',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20', 'H54',
##                     'banner_pos', 'hour', 'adix', 'adix_ts',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', 'H54',
##                     'banner_pos', 'hour', 'adix', 'adix_ts',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', 'H54',
##                     'banner_pos', 'hour', 'adix', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix', 'adix_ts',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'hour',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',  #device_conn_type poss
##                     'adix_ts6',
##                     'app_category', 
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',  #H20 will go click will be added
##                     'adix_ts6',
##                     'app_category', 
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18',
##                     'adix_ts6',
##                     'app_category', 
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_model' )
##         cols = c( 'H19', 'H21', 'H15', 'H16', 'H18',
##                     'adix_ts6',
##                     'app_category', 
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_model' )        # maybe H20 with device model out
##         cols = c( 'H19', 'H21', 'H16', 'H20',
##                     'adix_ts6',
##                     'app_category', 
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'day',
##                     'device_type' )        # maybe H20 with device model out
##         cols = c( 'H19', 'H21', 'H16', 'H20', 'H17',
##                     'adix_ts2',
##                     'app_id', 
##                     'site_id',
##                     "appcatclickprobs0",
##                     'appcatclickprobs2',
##                     'device_type' )        # maybe H20 with device model out
##         cols = c( 'H19', 'H21', 'H01', 'H15',
##                     'adix_ts6',
##                     'app_category', 
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_model' )
##         cols = c( 'H19', 'H21', 'H01', #'H15',
##                     'adix_ts6',
##                     'app_category', 
##                     #'site_category', 
##                     'probs', #####
##                     'site_id',
##                      #"appcatclickprobs0", 
##                      "sitecatclickprobs1",
##                      #'day',
##                     'device_model' )
##     }
##     if ( i == 5 ) {
##         cols = c('H16', 'H18', 'H20', 'H21','H44',
##                     'app_domain',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_conn_type' )
##         cols = c('H16', 'H18', 'H20', 'H21','H44', 'H17',
##                     'app_domain', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'app_domain', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_model', 'device_conn_type' )
##         cols = c( 'H44', 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H17', 'H20',
##                     'adix', 'adix_ts',
##                     'app_domain', 'app_id',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_model', 'device_conn_type' )
##         cols = c(
##                     'H14', 'H44', 'H54',
##                     'banner_pos', 'adix', 'adix_ts',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_conn_type' )
##         cols = c(
##                     'H14', 'H44',
##                     'banner_pos', 'adix_ts',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                     'device_type', 'device_conn_type' )
##         cols = c( #something happened here
##                     'H14', 'H44',
##                     'banner_pos', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type', 'device_conn_type' )
##         cols = c(
##                     'H14', 'H44',           #maybe H54 instead H44
##                     'banner_pos', 'adix_ts',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id', # maybe H19,H15,H18 instead of site domain
##                     'device_type', 'device_conn_type' )
##         cols = c(
##                     'H14', 'H54', 'H18', 'H16', 'H17', 'H19',
##                     'banner_pos', 'adix_ts',
##                     'app_domain', 'app_category',
##                     'site_category', 'site_id',
##                     'device_type', 'device_conn_type' )
##         cols = c(
##                     'H18', 'H16', 'H17', 'H19',
##                     'banner_pos', 'adix_ts2',
##                     'site_category', 'site_domain',
##                     'day',
##                     'sitecatclickprobs2', 'appcatclickprobs2',
##                     'device_type', 'device_conn_type' )
##         cols = c( 'H18', 'H16', 'H17', 'H19',
##                     'banner_pos', 'adix_ts2',
##                     'site_category', 'site_domain',
##                     'day',
##                     'sitecatclickprobs2', 'appcatclickprobs2',
##                     'device_type', 'device_conn_type' )
##         cols = c( 'H18', 'H16', 'H17', 'H19',
##                     #'banner_pos', 
##                     'adix_ts2',
##                     #'site_category', 
##                     'site_domain',
##                     'day',
##                     'sitecatclickprobs2', 
##                     #'appcatclickprobs2',
##                     #'device_type', 
##                     'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H18', ##'H20', #H01      # taken from best fit to top appid being model #1
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      #appcatclickprobs0,
##                      "sitecatclickprobs1",
##                      'day')
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H18', ##'H20', #H01      # taken from best fit to top appid being model #1
##                     ####'probs',#new added
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      #appcatclickprobs0,
##                      "sitecatclickprobs1",
##                      'day')
##     }
##     if ( i == 6 ) {
##         cols = c('H17','H44','H14','H21','H54','adix_ts2','adix_ts6','site_domain','device_ip', "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1" )
##         cols = c('H17','H14','adix','adix_ts2','device_ip', "appcatclickprobs0", "sitecatclickprobs1" )
##         cols = c(
##                     'H14', 'H44',
##                     'banner_pos', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'app_domain', 'app_category',
##                     'site_domain', 'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type', 'device_conn_type' )
##         cols = c( #something was wrong here
##                     'H14','H17','H21','H44','H54','H20','H18',
##                     'banner_pos', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_domain', 'site_category',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H18', 'H20', 'H14',
##                     'adix_ts', 'adix_ts2',
##                     'app_category',
##                     'site_category', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H18', 'H20', 'H14',
##                     'adix_ts6',
##                     'app_category',
##                     'site_category', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         #cols = c( # going back to original if changes fail
##                     #'H14', 'H44',
##                     #'banner_pos', 'adix_ts',
##                     #'adix_ts2', 'adix_ts6',
##                     #'app_domain', 'app_category',
##                     #'site_domain', 'site_category', 'site_id',
##                     #"appcatclickprobs0", "sitecatclickprobs1",
##                     #'day',
##                     #'device_type', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H18', 'H20', 'H14', #'H44',      # site_id is very relevant on > 3 and H44 usually helps
##                     'adix_ts6',
##                     'app_category',
##                     'site_category', 'site_domain', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H18', 'H20', 'H14', 'H44',      # site_id is very relevant on > 3 and H44 usually helps
##                     'adix_ts6',
##                     'app_category',
##                     'site_category', 'site_domain', #'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day', #'hour',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H18', 'H20', 'H14', 'H44',      # site_id is very relevant on > 3 and H44 usually helps
##                     'adix_ts6',                                     # H54 instead of H44 or H14
##                     'app_category',
##                     'site_category', 'site_domain', #'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day', #'hour',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H20', 'H14', 'H17',
##                     'hour','banner_pos','adix_ts',          # added all three for adix_ts6
##                     'app_domain', 'app_category',
##                     'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'day', 
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H20', 'H14', 'H17',
##                     'banner_pos', 'adix_ts',          # added all three for adix_ts6
##                     'app_domain', 'app_category',
##                     'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'appcatclickprobs2',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H20', 'H14', 'H17',
##                     'banner_pos', 'adix_ts',          # added all three for adix_ts6
##                     #'app_domain', 
##                     'app_category',
##                     'site_id',
##                     #"appcatclickprobs0", 
##                     "sitecatclickprobs1",
##                     #'appcatclickprobs2',
##                     'device_type' )
##     }
##     if ( i == 7 ) {
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts6',
##                     'app_domain', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_id', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts6',
##                     'app_domain', 'app_id', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts6',
##                     'app_domain', 'app_id', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                      #'adix_ts6',
##                     'app_domain', 'app_id', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type', 'device_ip', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour',
##                     'app_domain', 'app_id', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour',
##                     'app_id', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip', 'device_conn_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour',
##                     'app_id', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      "day", #day was added as all significant mdels have it and wondering it
##                     'device_type', 'device_ip' ) #device_conn_type may go
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H18', 'H20', 'H14', 'H44', # 14,54,44 added to see
##                     'hour',
##                     'app_id', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      "day", #day was added as all significant mdels have it and wondering it
##                     'device_type', 'device_ip' ) #device_conn_type may go
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H18', 'H20', 'H14', 'H44', # 14,54,44 added to see
##                     'adix_ts6', # adix_ts6 usually is more powerful than hour whch is simply neutral
##                     'app_id', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      "day", #day was added as all significant mdels have it and wondering it
##                     'device_type', 'device_ip' ) #device_conn_type may go
##                     # maybe H54 for H44 H01/device_type H20/device_ip, site_domain goes/H17?
##         cols = c( 'H19', 'H21', 'H16', 'H20', 'H14', 'H17',
##                     'adix_ts2',
##                     'app_id', 'app_domain',
##                     'site_id', 
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     "day", 
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H20', 'H14', 'H17',
##                     'adix_ts2',
##                     'app_id', 'app_category',
##                     'site_id', 
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     "day", 
##                     'sitecatclickprobs2', 'appcatclickprobs2',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H20',
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     "day", 
##                     'sitecatclickprobs2', 'appcatclickprobs2',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H20',
##                     ###'adix_ts6',
##                     'app_category',
##                     'site_id', 
##                     'probs', ##new
##                     #"appcatclickprobs0", 
##                     "sitecatclickprobs1", #####
##                     "day", 
##                     "hour",
##                     'sitecatclickprobs2') ###, 
##                     #'appcatclickprobs2',
##                     ###'device_type' )
##                     #'day')
##     }
##     if ( i == 8 ) {
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18',
##                     'banner_pos', 'hour',
##                     'adix_ts6',
##                     'app_domain',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H21', 'H01', 'H15', 'H16',
##                     'banner_pos',
##                     'adix_ts6',
##                     'app_domain',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H21', 'H01', 'H15', 'H16',
##                     'banner_pos',
##                     'adix_ts6',
##                     'app_domain',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type' )
##         cols = c( 'H21', 'H01', 'H15', 'H16',
##                     'banner_pos',
##                     'adix_ts6',
##                     'app_domain',
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type' )
##         cols = c( 'H21', 'H01', 'H15', 'H16',
##                     'banner_pos',
##                     'adix_ts6',
##                     'app_domain',
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs1",
##                     'device_type' )
##         cols = c( 'H21', 'H01', 'H15', 'H16',
##                     'banner_pos',
##                     'adix_ts6',
##                     'app_domain',
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H21', 'H01', 'H15', 'H16',
##                     #'banner_pos', unclear H15 H16 poss
##                     'adix_ts6',
##                     'app_domain',
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H21', 'H01', 'H15', 'H16',
##                     'adix_ts6',
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H21', 'H01', #maybe H19
##                     'adix_ts6',
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H21', 'H01', 'H19', 'H20', 'H15',
##                     'adix_ts6',
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'day',
##                     'device_type' )
##         cols = c( 'H21', 'H01', 'H19', 'H20', 'H15',
##                     'banner_pos', 'adix_ts2',
##                     'site_category', 'site_id',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'sitecatclickprobs2', 'appcatclickprobs2'
##                      )
##         cols = c( 'H21', 'H01', 'H15', 'H19',
##                     'adix_ts6',
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H21', 'H01', 'H15', 'H19',
##                     'adix_ts6', #'adix_ts2',
##                     'site_category', 'site_id',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                      'sitecatclickprobs2', #'shift', #'hour',
##                     'device_type' )
##         cols = c( 'H21', 'H01', 'H15', 'H19',
##                     'adix_ts6', #'adix_ts2',
##                     'site_category', 
##                     'site_id',
##                      #"appcatclickprobs0", 
##                      "sitecatclickprobs1",
##                      #'day',
##                      'sitecatclickprobs2') #, #'shift', #'hour', #'device_type' )
##         cols = c( "H21", "H20", "H19", "H15", "H17", 'H01',         # features seem not to work, so replace by #2
##                     ##'banner_pos', 
##                     'adix_ts2',
##                     #'app_category',
##                     'site_category', 
##                     ####'probs', ### ####'probs', ###
##                     'site_id',
##                     # appcatclickprobs0,
##                     "sitecatclickprobs1", 
##                     #'appcatclickprobs2',
##                     'hour') #'day'
##                     ##'day')
##     }
##     if ( i == 9 ) {
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour', 'adix_ts',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour',
##                     'adix_ts2', 'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'banner_pos', 'hour',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'hour',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category', 'app_id',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_category', 'site_id', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_category', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type', 'device_ip' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_category', 'site_domain',
##                      "appcatclickprobs0", "appcatclickprobs1", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_category', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs0", "sitecatclickprobs1",
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_domain', 'app_category',
##                     'site_category', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H16', 'H18', 'H20', #H15 is so-so
##                     'adix_ts6',
##                     'app_category',
##                     'site_category', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H18', 'H20', #H15 is so so and adix_ts6 and adix_ts2 come up as important on hard ones
##                     'adix_ts6',
##                     'app_category',
##                     'site_category', 'site_domain',
##                      "appcatclickprobs0", "sitecatclickprobs1",
##                      'day',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H18', 'H20',
##                     'adix_ts6',
##                     'app_category',
##                     'site_category', 'site_domain',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'day',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H16', 'H20',
##                     'banner_pos', 'adix_ts2',
##                     'site_category', 'site_domain',
##                     "appcatclickprobs0",
##                     'appcatclickprobs2',
##                     'device_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H20', #H15 is so so and adix_ts6 and adix_ts2 come up as important on hard ones
##                     'adix_ts6',
##                     'app_category',
##                     'site_category', 'site_domain',
##                     "appcatclickprobs0", "sitecatclickprobs1",
##                     'day',
##                     'device_type' )
##         cols = c( 'H17', 'H19', 'H21', 'H20', #H15 is so so and adix_ts6 and adix_ts2 come up as important on hard ones
##                     'adix_ts6', 'adix', 'adix_ts2',
##                     'app_category',
##                     'site_category', 'site_domain',
##                     #"appcatclickprobs0", 
##                     'sitecatclickprobs2', "sitecatclickprobs1") #, #'day', 'device_type' )
##         cols = c( 'H17', 'H19', 'H21', #H15 is so so and adix_ts6 and adix_ts2 come up as important on hard ones
##                     'adix_ts6', 'adix', 'adix_ts2',
##                     'app_id',
##                     'site_id', 'device_ip',
##                     #"appcatclickprobs0", 
##                     'sitecatclickprobs2', "sitecatclickprobs1") #, #'day', 'device_type' )
##         cols = c( #'H17', 
##                  #'H19', 'H21', #H15 is so so and adix_ts6 and adix_ts2 come up as important on hard ones
##                     'adix_ts6', 'adix', 'adix_ts2',
##                     'app_id',
##                     'site_id', 'device_ip',
##                     #"appcatclickprobs0", 
##                     'sitecatclickprobs2', "sitecatclickprobs1") #, #'day', 'device_type' )
##         cols = c( 'H19', 'H21', 'H01', 'H15', 'H18', ##'H20', #H01 poss     # replaced with #1 features as these appear non-significant 
##                     'adix_ts6',
##                     'app_category',
##                     'site_id', 'site_domain', 'site_category',
##                      #appcatclickprobs0,
##                      "sitecatclickprobs1",
##                      'day')
##         cols = c( "H21", "H20", "H19", "H15", "H17", 'H01',         # replaced wth #2
##                     ##'banner_pos', 
##                     'adix_ts2',
##                     'app_category',
##                     'site_category', 'site_id',
##                     # appcatclickprobs0,
##                     "sitecatclickprobs1", 
##                     #'appcatclickprobs2',
##                     'hour') #'day'
##         cols = c( 'H19', 'H21', 'H01', 'H16', 'H17', ##'H20',       # replaced with #3
##                     #'banner_pos', 
##                     'adix_ts2',
##                     'app_id',
##                     ######'probs', ###
##                     'site_category', 
##                     'site_id',
##                     #"appcatclickprobs0", 
##                     "sitecatclickprobs1",
##                     #'appcatclickprobs2',
##                     #'hour', 
##                     'hour', ###,
##                     'day'
##                     )
##         ###cols = c( 'H19', 'H21', 'H01', 'H16', 'H17', ##'H20', #'banner_pos', #"appcatclickprobs0", #'appcatclickprobs2', #'hour',
##                     ###'adix_ts6', 'app_id', 'site_category', 'site_id',
##                     ###"sitecatclickprobs1",
##                     ###'day')
##     }
##     return ( cols )
## }
## # ############################################################################################
## 
## 
