# #############################################################################
import boto
import boto.ec2
from boto.s3.key import Key
from boto.s3.connection import S3Connection
# #############################################################################
import time
import datetime
import dateutil.parser
# #############################################################################
import sys
import re
import math
# #############################################################################
import traceback
from collections import defaultdict
# #############################################################################


# #############################################################################
# GLOBALS FOR THIS COMPUTATION
# #############################################################################
from clt_aws_boto_config import *
from clt_aws_s3_common import *
# #############################################################################


# #############################################################################
def HEADER(what="", n=1, debug=0):
    print '-' * 80
    print "%.2f %s" % (time.time(), what)
    if debug:
        traceback.print_stack()
    print '-' * 80
    for i in range(n):
        print
    return
# #############################################################################


# #############################################################################
def DEBUG(dtype="INFO", msg="", val="", code=-1):
    print "[%16.2f] [%6s]: %48s %s" % (time.time(), dtype, msg, val)
    if dtype == "ERROR":
        HEADER()
        traceback.print_stack()
        HEADER()
        sys.exit(code)
    return val
# #############################################################################


# #############################################################################
def get_spot_prices(ec2):
    ec2_prices = ec2.get_spot_price_history()
    return ec2_prices
# #############################################################################


# #############################################################################
def parse_spot_ec2types(ec2types_list):
    ec2types = defaultdict(int)
    for e in ec2types_list:
        try:
            ec2types[e] += 1
        except:
            pass
    ec2types = sorted(ec2types.items(), key=lambda x : x[1], reverse=True)
    return ec2types
# #############################################################################


# #############################################################################
def _parse_spot_prices(ec2_prices=(), threshold=None,
                       itypes=('large', 'medium'), debug=0):
    p = re.compile('\(([a-z].+)\)')
    try:
        ts, sumcum, n, ec2types = [], 0., 0, []
        for spotpriceitem in ec2_prices:
            item, price = str(spotpriceitem).split(':')
            item = p.findall(item)[0]
            price = float(price)
            if threshold and price < threshold and any([item.find(itype) > 0 for itype in itypes]):
                if debug:
                    DEBUG("INFO", "SUCCESSFUL PREVIOUS BID: %10s:" % item, price)
                sumcum += price
                ec2types.append(item)
                ts.append(price)
                n += 1
    except:
        DEBUG("ERROR", "PRICING LIKELY NOT FEASIBLE", len(ec2_prices), code=-3)

    ec2types = parse_spot_ec2types(ec2types)
    results = [ec2types, n, sumcum/(n+1), min(ts), max(ts)]
    if len(ts):
        DEBUG("INFO", "PRICING SUCCESS HISTORY:", ec2types)
    return results
# #############################################################################


# #############################################################################
def parse_spot_prices(ec2_prices=(), threshold=None,
                      min_samples=100, max_attempts=5, update=0.01,
                      itypes=('large', 'medium'), debug=0):
    num_samples, i = 0, 0
    while num_samples < min_samples:
        results = _parse_spot_prices(ec2_prices, threshold=threshold,
                                     itypes=('medium', 'large'))
        DEBUG("INFO", "AT %s" % threshold, results)
        [ec2types, num_samples, avgprice, minprice, maxprice] = results
        threshold = threshold + update
        i = i + 1
        if i > max_attempts:
            break
    return results
# #############################################################################


# #############################################################################
def timenow(format="string", tz=AWS2UTC_TZ, debug=0):
    epoch = time.time()
    if format == "epoch":
        return epoch
    else:
        return time.strftime('%Y-%m-%dT%H:%M:%S', time.localtime(epoch))+tz
# #############################################################################


# #############################################################################
# http://stackoverflow.com/questions/12400256
# #############################################################################
def timenow_plusdelta(plusdelta=15, format="string", tz=AWS2UTC_TZ, debug=0):
    epoch = time.time() + plusdelta * 60.0
    if format == "epoch":
        return epoch
    else:
        return time.strftime('%Y-%m-%dT%H:%M:%S', time.localtime(epoch))+tz
# #############################################################################


# #############################################################################
# http://stackoverflow.com/questions/969285
# #############################################################################
def time_as_awstime(datestring, debug=0):
    awsdate = dateutil.parser.parse(datestring)
    if debug > 0:
        DEBUG("DEBUG", "time", awsdate)
    return awsdate
# #############################################################################


# #############################################################################
def request_spot_instances(price=AWS_SPOT_PRICE_MAX, image_id=COMPUTATION_AMI, ec2=None, n=1):
    if not ec2: ec2 = boto.ec2.connect_to_region('us-east-1')
    if not image_id: DEBUG("ERROR", "AMI IMAGE ID IS NEEDED", image_id, code=-2)

    ec2_prices = get_spot_prices(ec2)

    results = parse_spot_prices(ec2_prices, threshold=price, itypes=('large', 'medium'))
    [ec2types, num_samples, avgprice, minprice, maxprice] = results
    idx = min(2, int(len(ec2types)/2-1))
    which_type = ec2types[idx][0]
    which_type = 'm3.medium'
    which_price = avgprice + (maxprice-avgprice)/2.
    from_time, until_time = timenow_plusdelta(5), timenow_plusdelta(20)
    DEBUG("INFO", "SEEKING (%s, %s, %s) AT:" % (which_type, from_time, until_time), which_price)

    try:
        spot_req = ec2.request_spot_instances(which_price, image_id, count=n, type='one-time', instance_type=which_type,
                                              valid_from=from_time, valid_until=until_time, key_name=AWS_KEYNAME,
                                              security_groups=SECURITY_GROUPNAME, security_group_ids=SECURITY_GROUPID,
                                              #ebs_optimized=False,
                                              #network_interfaces=None,
                                              #instance_profile_arn=None,
                                              #instance_profile_name=None,
                                              #launch_group=None,
                                              #placement=None,
                                              #placement_group=None,
                                              #availability_zone_group=None,
                                              #user_data=None,
                                              #addressing_type=None,
                                              #kernel_id=None,
                                              #ramdisk_id=None,
                                              #monitoring_enabled=False,
                                              #subnet_id=None,
                                              #block_device_map=None,
                                              dry_run=False)
    except boto.exception.EC2ResponseError:
        DEBUG('WARNING', "PROBLEM WITH EC2 REQUEST", (which_price, image_id, which_type, timenow(), timenow_plusdelta()))
        spot_req = None

    return spot_req
# #############################################################################


# #############################################################################
def check_bids_on_spot_instances(spot_reqs=None):
    if not spot_reqs:
        spot_reqs = ec2.get_all_spot_instance_requests()

    for spot_req in spot_reqs:
        pass

    return spot_reqs
# #############################################################################
