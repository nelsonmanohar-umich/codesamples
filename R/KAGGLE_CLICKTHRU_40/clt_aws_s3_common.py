# #############################################################################
import boto
import time
import os.path
# #############################################################################


# #############################################################################
# GLOBALS FOR THIS COMPUTATION
# #############################################################################
from clt_aws_boto_config import *
# #############################################################################


# #############################################################################
global S3
S3 = None
# #############################################################################


# #############################################################################
def get_proj_bucketkey(user="nelsonmanohar", area="machlearn", project="clt"):
    bucketkey = "%s-%s-%s-%s" % (user, area, project, 3141519)
    return bucketkey
# #############################################################################


# #############################################################################
def copy_to_s3(filename, s3=None):
    if not s3: s3 = boto.connect_s3()
    bucket_key = get_proj_bucketkey()
    filing_key = aws_shipper(filename, bucket=get_proj_bucket(s3), s3=s3)
    print filing_key
    return (bucket_key, filing_key)
# #############################################################################


# #############################################################################
def aws_shipper(filename, bucket=None, data=None, delay=3, s3=None):
    if not filename: return None
    if not bucket: bucket = get_proj_bucket()
    if not s3: s3 = boto.connect_s3()
    print "PUT", s3, filename,
    s3_filename = os.path.basename(filename)
    #local_filesystem_filename = "%s/%s" % (get_prefix(), s3_filename)
    filing_key = bucket.new_key(s3_filename)
    if not filing_key: return None
    if data:
        filing_key.set_contents_from_string(data)
    else:
        filing_key.set_contents_from_filename(filename)
    time.sleep(delay)
    return filing_key
# #############################################################################


# #############################################################################
def get_from_s3(filename, s3=None):
    if not filename: return None
    if not s3: s3 = boto.connect_s3()
    print "GET", s3, filename,
    s3_filename = os.path.basename(filename)
    #local_filesystem_filename = "%s/%s" % (get_prefix(), s3_filename)
    filing_key = get_proj_filingkey_for(s3_filename, s3=s3)
    print filing_key
    if filing_key: filing_key.get_contents_to_filename(filename)
    return filing_key
# #############################################################################


# #############################################################################
def aws_contents_reader(filing_key):
    return filing_key.get_contents_as_string()
# #############################################################################


# #############################################################################
def get_proj_filingkey_for(filename, s3=None):
    bucket = get_proj_bucket(s3=s3)
    filing_key = bucket.get_key(filename)
    return filing_key
# #############################################################################


# #############################################################################
def get_proj_bucket(s3=None):
    if not s3: s3 = boto.connect_s3()
    bucket_key = get_proj_bucketkey()
    bucket = s3_check_bucket(bucket_key, s3=s3)
    return bucket
# #############################################################################


# #############################################################################
def s3_check_bucket(bucket_key, s3=None):
    if not s3: s3 = boto.connect_s3()
    try:
        bucket = s3.get_bucket(bucket_key)
    except:
        print "EXCEPTION", s3, bucket_key
        bucket = None
    if not bucket:
        bucket = s3.create_bucket(bucket_key)
    return bucket
# #############################################################################


# #############################################################################
def aws_file_downloader(filename, filing_key=None, s3=None):
    if not s3: s3 = boto.connect_s3()
    if not filing_key:
        s3_filename = os.path.basename(filename)
        #local_filesystem_filename = "%s/%s" % (get_prefix(), s3_filename)
        filing_key = get_proj_filingkey_for(s3_filename, s3=s3)
    if not filing_key:
        RJE_DEBUG("WARN", "S3 ERROR, FILE NOT IMPORTED, NOT FOUND", filename, s3, filing_key) 
        return (None, "")
    filing_key_contents = filing_key.get_contents_to_filename(filename)
    print "GET", s3, filename,
    return (filing_key, filing_key_contents)
# #############################################################################


# #############################################################################
def clt_aws_dynamic_reconfiguration_client(common_config_file=None):
    try:
        if not common_config_file: common_config_file = 'clt_aws_common.py'
        get_from_s3(common_config_file)
    except:
        RJE_DEBUG("WARNING", "S3 ERROR, FILE NOT IMPORTED, NOT FOUND", common_config_file) 
        return False
    return True
# #############################################################################


# #############################################################################
def clt_aws_dynamic_reconfiguration_setup(config_file=None):
    try:
        if not config_file: config_file = 'clt_aws_common.py'
        copy_to_s3(config_file)
    except:
        RJE_DEBUG("WARNING", "S3 ERROR, FILE NOT EXPORTED, NOT FOUND", common_config_file) 
        return False
    return True
# #############################################################################
