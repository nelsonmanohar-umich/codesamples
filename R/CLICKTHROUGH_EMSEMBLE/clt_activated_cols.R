if ( CVTEST ) {
    COLNAMES = c( 'ad_id', 'click', 'YYMMDDHH', 'C01', 'banner_pos', 'site_id', 'site_domain', 'site_category', 'app_id', 'app_domain', 'app_category', 'device_id', 'device_ip', 'device_model', 'device_type', 'device_conn_type', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21' )

    ACTIVATED_COLNAMES  = c("click", #"s2c", "a2c", #"device_id", #'appcatclickprobs2', #'sitecatclickprobs2', #"origin", #'device_ip', # "appcatclickprobs0", "sitecatclickprobs0", #"adix", "adix_ts", "adix_ts2", #"probs", 
                            'origin',
                            'device_ip',
                            # "YYMMDDHH",
                            "appcatclickprobs0", "sitecatclickprobs1", 
                            'shiftclickprobs', 'dayclickprobs', 'hourclickprobs', 
                            "banner_pos", "device_type", "device_conn_type", 'device_model', 'device_id',
                            "site_domain", "site_category", 'site_id', 
                            "app_domain", "app_category", 'app_id', 
                            "H01","H14","H15","H16","H17","H18","H19","H20","H21",
                            "C01", 
                            'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21',
                            "adix_ts6", "shift", "hour", "day" )
} else {
    COLNAMES = c( 'ad_id', 'YYMMDDHH', 'C01', 'banner_pos', 'site_id', 'site_domain', 'site_category', 'app_id', 'app_domain', 'app_category', 'device_id', 'device_ip', 'device_model', 'device_type', 'device_conn_type', 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21' )

    ACTIVATED_COLNAMES  = c("ad_id", #NOT click but INSTEAD ad_id:
                            'origin',
                            # "YYMMDDHH",
                            'device_ip',
                            "appcatclickprobs0", "sitecatclickprobs1", 
                            'dayclickprobs', 'shiftclickprobs', 'hourclickprobs', 
                            "banner_pos", "device_type", "device_conn_type", 'device_model', 'device_id',
                            "site_domain", "site_category", 'site_id', 
                            "app_domain", "app_category", 'app_id', 
                            "H01","H14","H15","H16","H17","H18","H19","H20","H21",
                            "C01", 
                            'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21',
                            "adix_ts6", "shift", "hour", "day" )
}
cat( HEADER )
print ( COLNAMES )
cat( HEADER )
print ( paste(1:length(ACTIVATED_COLNAMES), ACTIVATED_COLNAMES ) )
cat( HEADER )
