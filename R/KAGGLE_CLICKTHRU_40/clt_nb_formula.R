GET_NB_FORMULA = function () {
FORMULA6 = 'click ~ site_domain + site_category + app_id + app_category + device_model + device_conn_type + banner_pos + H17 + H19 + H21 + H18 + H28 + H29'
FORMULA6 = 'click ~ site_domain + app_id + app_category + device_conn_type + banner_pos + H17 + H19 + H21 + H28 '
FORMULA6 = 'click ~ site_domain + app_id + app_category + device_conn_type + banner_pos + H17 + H19 + H21 + H28 '
FORMULA6 = 'click ~ site_domain + app_category + device_conn_type + banner_pos + H17 + H19 + H21 + H28 '
FORMULA6 = 'click ~ site_domain + app_category + H17 + H19 + H21 + H28 '
FORMULA6 = 'click ~ site_domain + H17 + H19 + H21 + H28 '
FORMULA6 = 'click ~ H14 + H17 + H21 + H18 + H22 + H30 + site_domain + app_category' 
FORMULA6 = 'click ~ H14 + H17 + H19 + H21' # .86
FORMULA6 = 'click ~ H14 + H17 + H21 + H18 + H19 + site_domain' # 1.0
FORMULA6 = 'click ~ H14 + H17 + H21 + H18 + H19' # 0.93
FORMULA6 = 'click ~ H14 + H17 + H21 + H28 + H19' # 1.3
FORMULA6 = 'click ~ H14 + H17 + H21 + H19' # .86
FORMULA6 = 'click ~ H14 + H17 + H21 + H18 + H22 + H30' # 1.0
FORMULA6 = 'click ~ H14 + H17 + H21 + H18' # .88
FORMULA6 = 'click ~ H14 + H17 + H21' # .82
FORMULA6 = 'click ~ H14 + H17 + site_domain' # 0.8
FORMULA6 = 'click ~ H14 + H17 ' # .75
FORMULA6 = 'click ~ H14 + H17 + site_id'  #0.81
FORMULA6 = 'click ~ H14 + site_id'  #0.74 LAPLACE=1024
FORMULA6 = 'click ~ H14 + H17'  #0.71 LAPLACE=4092
FORMULA6 = 'click ~ H14 + site_id'  #0.78 LAPLACE=255
FORMULA6 = 'click ~ H14 + site_id + banner_pos'  #0.71 LAPLACE=4092
FORMULA6 = 'click ~ H14 + site_id + banner_pos'  #0.72 LAPLACE=4092, SITE_ID_F=2000
FORMULA6 = 'click ~ H14 + site_id + banner_pos + H31'  #0.73 LAPLACE=4092, SITE_ID_F=30 (but .81 on test)
FORMULA6 = 'click ~ H14 + site_id + banner_pos'  #0.73 LAPLACE=4092, SITE_ID_F=30 (but .79 on test)
FORMULA6 = 'click ~ H14 + site_id + banner_pos + device_model'  #0.70 LAPLACE=4092, SITE_ID_F=30 (but .76 on test) w/ two digit dev model
FORMULA6 = 'click ~ H14 + site_id + device_model'  #0.70 LAPLACE=4092, SITE_ID_F=30 (but .76 on test) w/ two digit dev model
FORMULA6 = 'click ~ H14 + site_id + device_model'  #0.70 LAPLACE=4092, SITE_ID_F=30 (but .75 on test) w/ two digit dev model w/ reverse
FORMULA6 = 'click ~ H14 + site_id + device_model'  #0.70 LAPLACE=4092, SITE_ID_F=255 (but .74 on test) w/ two digit dev model w/ reverse
FORMULA6 = 'click ~ H14 + H17 + site_id + device_conn_type + device_model'  
FORMULA6 = 'click ~ H14 + site_id + device_ip '  # best so far
FORMULA6 = 'click ~ H14 + site_id + site_domain + H17 + device_ip '  
FORMULA6 = 'click ~ site_id + H17 + H21 + app_id '  
FORMULA6 = 'click ~ site_id + H17 + H14 + H18 + H19 + H21 + app_id '  
FORMULA6 = 'click ~ H17 + H14 + H18 + H19 + H21 + app_id '   # submitted
FORMULA6 = 'click ~ H17 + H14 + H18 + H19 + H21 + app_id + device_model + site_category + device_ip'  
FORMULA6 = 'click ~ H17 + H14 + H18 + H19 + H21 + app_id + device_model + site_category + device_ip'  
FORMULA6 = 'click ~ H17 + H44 + H14 + H18 + H19 + H21 + adix + app_id + device_model + site_category + device_ip'  
FORMULA6 = 'click ~ H17 + H44 + H14 + H21 + adix + app_id + site_domain + device_ip'  
FORMULA6 = 'click ~ H17 + H44 + H14 + adix + adix_ts + adix_ts2 + adix_ts6 + site_domain + device_ip'  
FORMULA6 = 'click ~ H17 + H44 + H14 + adix + adix_ts + adix_ts2 + adix_ts6 + site_domain + device_ip + appcatclickprobs0 + appcatclickprobs1 + sitecatclickprobs0 + sitecatclickprobs1'
FORMULA6 = 'click ~ H17 + H44 + H14 + adix_ts + adix_ts2 + adix_ts6 + site_domain + device_ip + appcatclickprobs1 + sitecatclickprobs0 '
FORMULA6 = 'click ~ H17 + H14 + adix_ts6 + site_domain + device_ip + appcatclickprobs1 + sitecatclickprobs0 + site_id + device_conn_type '
FORMULA6 = 'click ~ H17 + H14 + H21 + adix + adix_ts2 + site_domain + appcatclickprobs1 + sitecatclickprobs0 + site_id + banner_pos'
FORMULA6 = 'click ~ H17 + H14 + H21 + adix + site_domain + appcatclickprobs1 + sitecatclickprobs0 + site_id'
FORMULA6 = 'click ~ H19 + H21 + H01 + H18 + H20 + site_id + site_domain + site_category + appcatclickprobs1 + sitecatclickprobs1 + device_model + banner_pos'
FORMULA6 = 'click ~ H14 + H17 + H19 + H21 + H01 + H18 + H20 + site_id + site_domain + site_category + appcatclickprobs1 + sitecatclickprobs1 + device_model'
FORMULA6 = 'click ~ H44 + H17 + H19 + H21 + H18 + site_id + site_domain + site_category + sitecatclickprobs1 + adix'
FORMULA6 = 'click ~ H14 + H17 + H19 + H21 + site_id + sitecatclickprobs1 + adix + device_ip'
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + adix + device_ip'
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + adix + device_ip + day'
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + adix + day + H21 + adix_ts2 + H20'
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + adix + day + H21 + adix_ts2'
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + adix + day + H21 + H20'
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + adix + day + H21'
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + adix + day + H21 + H20 + adix_ts2'    # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + H21 + H20 + adix_ts2'    # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H20 + app_id'  # .60   # 
FORMULA6 = 'click ~ H44 + H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + H21 + H20 + app_id'    #64 # 
FORMULA6 = 'click ~ H44 + H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + H21 + H20 + site_category + adix_ts2'    # 64# 
FORMULA6 = 'click ~ H44 + H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + H21 + H20 + site_category'    # 65# 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + H21 + H20 + adix_ts2'    # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H20 + app_id'  # .60   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H20 + app_id + shift'  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H20 + app_id + hour'  # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H20 + app_id + shift'  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H20 + app_id + shift + s2c + a2c'  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H20 + app_id + shift'  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H20 + app_id + shift + s2c '  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H20 + app_id + shift + s2c + a2c'  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H20 + app_id + shift '  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + H21 + H01 + H15'  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + app_id + H21 + H01 + H15'  # 1.1   # 
FORMULA6 = 'click ~ H01 + H20 + H15 + H19 + day + site_category + site_id + sitecatclickprobs1 + H21 + H14 + H17'  # 1.1   # 
FORMULA6 = 'click ~ H01 + H20 + H15 + H19 + day + site_category + site_id + sitecatclickprobs1 + H21 + H14 + H17 + sitecatclickprobs2'  # 1.1   # 
FORMULA6 = 'click ~ H01 + H20 + H15 + H19 + day + site_category + site_id + banner_pos + H21 + H14 + H17 + sitecatclickprobs2'  # 1.1   # 
FORMULA6 = 'click ~ H01 + H20 + H15 + H19 + day + site_category + site_id + banner_pos + H21 + H14 + H17 + sitecatclickprobs2 + H16'  # 1.1   # 
FORMULA6 = 'click ~ H01 + H20 + H15 + H19 + day + site_category + site_id + banner_pos + H21 + H17 + adix + appcatclickprobs2 + H14'  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs0 + adix + day + H21 + adix_ts6 + banner_pos + hour'
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs0 + adix + day + H21 + banner_pos + sitecatclickprobs2 + adix_ts2 + hour'
FORMULA6 = 'click ~ site_id + sitecatclickprobs0 + adix + day + banner_pos + sitecatclickprobs2 + adix_ts2 + adix_ts6 + hour + device_ip + app_id'
FORMULA6 = 'click ~ site_id + sitecatclickprobs0 + adix + day + banner_pos + sitecatclickprobs2 + adix_ts2 + adix_ts6 + hour + device_ip + app_id'
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + adix_ts2 + H21 + H01 + H15'  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + adix_ts2 + H21 + H01 + H15 + probs '  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + adix_ts6 + H21 + H01 + H15 + probs + hour'  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + day + site_category + adix_ts6 + H21 + H01 + H15 + probs '  # 1.1   # 
FORMULA6 = 'click ~ H14 + H17 + H19 + site_id + sitecatclickprobs1 + H21 + H01 + shiftclickprobs'  # 1.1   # 
FORMULA6 = 'click ~ H19 + H21 + H01 + H16 + H17 + adix_ts6 + app_id + site_category + site_id + sitecatclickprobs1 + day'
}
