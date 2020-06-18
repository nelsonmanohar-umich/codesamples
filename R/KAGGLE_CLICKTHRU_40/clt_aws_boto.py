import boto
import boto.ec2
from boto.s3.connection import S3Connection
from boto.s3.key import Key
import time
import dateutil.parser
import datetime
import math
import sys
import re
import traceback
from collections import defaultdict


# #############################################################################
# GLOBALS FOR THIS COMPUTATION
# #############################################################################
AWS_AMI_OWNER = "526570712761"
AWS_KEYNAME = "NRM2"
COMPUTATION_AMI = "ami-a589a095"
SECURITY_GROUPID = ["sg-9da69df8", ]
SECURITY_GROUPNAME = ["compute_node", ]
AWS2UTC_TZ = "-07:00"
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
def get_proj_bucketkey(user="nelsonmanohar", area="machlearn", project="clt"):
    bucketkey = "%s-%s-%s-%s" % (user, area, project, 3141519)
    print bucketkey
    return bucketkey
# #############################################################################


# #############################################################################
def aws_shipper(bucket, filename=None, data=None, delay=2):
    if filename:
        bkey = bucket.new_key(filename)
        bkey.set_contents_from_filename(filename)
        time.sleep(delay)
        return bkey
    elif data:
        bkey = bucket.new_key(filename)
        bkey.set_contents_from_string(data)
        time.sleep(delay)
        return bkey
# #############################################################################


# #############################################################################
def aws_contents_reader(filing_key):
    return filing_key.get_contents_as_string()
# #############################################################################


# #############################################################################
def aws_file_downloader(s3_bucket_filename, filing_key=None):
    if not filing_key:
        s3 = boto.connect_s3()
        bucket_key = get_proj_bucketkey()
        filing_key = s3.get_bucket(bucket_key).get_key(s3_bucket_filename)
    filing_key_contents = filing_key.get_contents_to_filename(s3_bucket_filename)
    return (filing_key, filing_key_contents)
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
def request_spot_instances(price=0.05, image_id=COMPUTATION_AMI, n=1):
    if not image_id:
        DEBUG("ERROR", "AMI IMAGE ID IS NEEDED", image_id, code=-2)

    ec2_prices = get_spot_prices(ec2)

    results = parse_spot_prices(ec2_prices, threshold=price, itypes=('large', 'medium'))
    [ec2types, num_samples, avgprice, minprice, maxprice] = results
    which_type = ec2types[int(len(ec2types)/2-1)][0]
    which_price = avgprice + (maxprice-avgprice)/2.
    from_time, until_time = timenow(), timenow_plusdelta()
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
        DEBUG('ERROR', "PROBLEM WITH EC2 REQUEST", (which_price, image_id, which_type, timenow(), timenow_plusdelta()), -4)

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


# #############################################################################
def s3_check_bucket(s3, bucket_key):
    try:
        bucket = s3.get_bucket(bucket_key)
    except boto.exception.S3ResponseError:
        bucket = None
    DEBUG("INFO", "BUCKET", bucket)
    return bucket
# #############################################################################


# #############################################################################
def test_s3(s3="", throwaway_bucket_key=time.time()*math.pi):
    HEADER("S3 BUCKET")
    bucket = s3_check_bucket(s3, throwaway_bucket_key)
    if not bucket:
        bucket = s3.create_bucket(throwaway_bucket_key)

    HEADER("S3 BUCKET LISTING")
    rs = bucket.list()
    for key in rs:
        print key.name

    HEADER("S3 FILE SHIPMENT")
    filing_key = aws_shipper(bucket, 'old_plotting_cv.R')
    print "START:", filing_key.get_contents_as_string()[:60]
    print "END  :", filing_key.get_contents_as_string()[-60:]

    HEADER("S3 DELETE")
    filing_key.delete()
    bucket.delete()

    HEADER("S3 CHECK")
    bucket = s3_check_bucket(s3, throwaway_bucket_key)
    if not bucket:
        print "DONE"
        return 1
    return 0
# #############################################################################


# #############################################################################
if __name__ == "__main__":
    s3 = boto.connect_s3()
    ec2 = boto.ec2.connect_to_region('us-west-2')

    if 0:
        HEADER("S3 TESTING")
        bucket_key = get_proj_bucketkey()
        test_s3(s3, bucket_key)

    if 1:
        HEADER("EC2 TESTING")
        req = request_spot_instances(price=0.05, image_id=COMPUTATION_AMI, n=1)

    if 1: # http://stackoverflow.com/questions/12801966
        job_instance_id = None
        while job_instance_id == None:
            print "checking job instance id for this spot request"
            job_sir_id = req[0].id # spot instance request = sir, job_ is the relevant aws item for this job
            reqs = ec2.get_all_spot_instance_requests()
            for sir in reqs:
                if sir.id == job_sir_id:
                    job_instance_id = sir.instance_id
                    print "job instance id: " + str(job_instance_id)
                    break
                time.sleep(60)

    if 0:
        ids = ec2.run_instances(COMPUTATION_AMI, key_name=AWS_KEYNAME, instance_type='c1.xlarge', security_groups=SECURITY_GROUPID)

    if 0:
        res = ec2.stop_instances(instance_ids=ids)

    if 0:
        res = ec2.terminate_instances(instance_ids=ids)

    if 1:
        reservations = ec2.get_all_reservations()
        for k,resv in enumerate(reservations):
            HEADER()
            instances = reservations[k].instances
            for i, inst in enumerate(instances):
                DEBUG("INFO", "INSTANCES", "%s %s %s %s" % (k, i, inst.instance_type, inst.placement))
            HEADER()
