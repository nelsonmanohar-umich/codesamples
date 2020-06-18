#!/usr/bin/python

import time
import sys
import boto
import boto.ec2
import argparse


# #############################################################################
from clt_aws_boto_common import *
# #############################################################################


# #############################################################################
def aws_launcher(n=1, ec2=None, itype=None, ami=None, wait_for=True, dryrun=False):
    if not ec2: ec2 = boto.ec2.connect_to_region('us-east-1')
    if not ami: ami = COMPUTATION_AMI
    if not itype: itype = DEFAULT_INSTANCE_TYPE

    instance_list = ec2.run_instances(ami, instance_type=itype, dry_run = dryrun, key_name=AWS_KEYNAME, security_groups=SECURITY_GROUPNAME, security_group_ids=SECURITY_GROUPID)
    print '-' * 80

    print instance_list
    for instance in instance_list.instances:
        print '-' * 80
        print instance
        if wait_for:
            wait_for_instance(instance)
    print '-' * 80
    return instance_list
# #############################################################################


# #############################################################################
# google: amazon-ec2-deployment-with-boto.html
# #############################################################################
def create_instance(REQ=None, ec2=None, wait_for=True):
    if not REQ: REQ = AWS_EC2_CLIREQ
    if not ec2: ec2 = boto.ec2.connect_to_region('us-east-1')
    reservation = ec2.run_instances(**REQ)
    instance = reservation.instances[0]
    if wait_for:
        wait_for_instance(instance)
    return (reservation, instance)
# #############################################################################


# #############################################################################
# google: amazon-ec2-deployment-with-boto.html
# #############################################################################
def wait_for_instance(instance):
    print "Instance %s loading!" % (instance)
    while instance.state != "running":
        time.sleep(5)
        instance.update()
        print 'Instance state: %s' % instance.state
    describe_instance(instance)
    instance.add_tag("Launched", timenow())
    return instance
# #############################################################################


# #############################################################################
# google: amazon-ec2-deployment-with-boto.html
# #############################################################################
def _describe_instance(instance):
    print '-' * 80
    print "Instance id   : %s" % (instance.id)
    print 'Instance state: %s' % (instance.state)
    print "Instance type : %s" % (instance.instance_type)
    print "Instance IP is: %s" % (instance.ip_address)
    print "SSH via       : ssh -i %s.pem ubuntu@%s" % (AWS_KEYNAME, instance.ip_address)
    print '-' * 80
    print
    return instance
# #############################################################################


# #############################################################################
def get_active_instances(ec2=None):
    if not ec2: ec2 = boto.ec2.connect_to_region('us-east-1')
    reservations = ec2.get_all_instances()
    instances = [i for r in reservations for i in r.instances]
    return instances
# #############################################################################


# #############################################################################
# google: how-list-attributes-ec2-instance-python-and-boto/
# #############################################################################
def describe_instance(instance, detailed=True):
    _describe_instance(instance)
    if detailed and 'terminated' not in instance.state:
        attrs = instance.__dict__
        print '-' * 80
        for attr in attrs:
            print "%32s --> %s" % (attr, attrs[attr])
        print '-' * 80
        print
        return attrs
    return instance
# #############################################################################


# #############################################################################
# https://github.com/boto/boto/issues/1222
# #############################################################################
def itemize_spot_requests(ec2=None ):
    requests = ec2.get_all_spot_instance_requests()
    for r in requests:
        print '-' * 80
        print "Spot req id:", r.id
        print "Instance:   ", r.instance_id
        print "Code:       ", r.status.code
        print "Update time:", r.status.update_time
        print "Created at: ", r.create_time
        print "Valid from: ", r.valid_from
        print "Valid until:", r.valid_until
        print "Price bid:  ", r.price
        print "Message:    ", r.status.message
        print '-' * 80
        print
    return requests
# #############################################################################


# #############################################################################
if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog='clt_aws_boto_launcher.py',
                        description='launches and manages computenode instances at aws')

    # -------------------------------------------------------------------------
    parser.add_argument("--op",
                        type=str,
                        default="list,itemize",
                        help='op from create,spot,itemize,list,stop,terminate')
    parser.add_argument("--n",
                        type=int,
                        default=1,
                        help='number of instances')
    parser.add_argument("--price",
                        type=float,
                        default=0,
                        help='maximum price for spot instances')
    parser.add_argument("--region",
                        type=str,
                        default='us-east-1',
                        help='aws ec2 region')
    parser.add_argument("--itype",
                        type=str,
                        default="",
                        help='type of instance to request')
    # -------------------------------------------------------------------------


    # -------------------------------------------------------------------------
    args = parser.parse_args()
    try:
        print >>sys.stderr, '-' * 80
        for a in args:
            print >>sys.stderr, "%s" % a
        print >>sys.stderr, '-' * 80
    except:
        parser.print_help()
    # -------------------------------------------------------------------------


    # -------------------------------------------------------------------------
    INSTANCES = None
    HEADER("EC2 CONNECT")
    ec2 = boto.ec2.connect_to_region(args.region)
    # -------------------------------------------------------------------------


    # -------------------------------------------------------------------------
    if 'secgroup' in args.op:
        HEADER("SECURITY GROUP UPDATING")
        try:
            sg_cmd = 'ec2-authorize sg-d2cd6fb6 -P tcp -p 22 -s %s/16' % QHOST
            subprocess.check_call(sg_cmd.split(' '))
        except:
            pass
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    if 'launch' in args.op:
        HEADER("AWS LAUNCHER")
        instance_list = aws_launcher(n=args.n, ec2=ec2, wait_for=True)
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    if 'create' in args.op:
        HEADER("CREATE INSTANCE")
        for i in range(args.n):
            (reservation, instance) = create_instance(AWS_EC2_CLIREQ, ec2=ec2, wait_for=True)
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    if 'list' in args.op:
        HEADER("LIST INSTANCES")
        INSTANCES = get_active_instances(ec2=ec2)
        if INSTANCES:
            for instance in INSTANCES:
                describe_instance(instance, detailed=True)
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    if 'spot' in args.op:
        HEADER("SPOT INSTANCES")
        pricegoal = AWS_SPOT_PRICE_MAX
        if args.price > 0: pricegoal = args.price
        si_req = request_spot_instances(price=pricegoal, image_id=COMPUTATION_AMI, n=args.n)
        reservations = ec2.get_all_spot_instance_requests()
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    if 'spot' in args.op or 'item' in args.op:
        requests = itemize_spot_requests(ec2=ec2)
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    if 'spot' in args.op:
        HEADER("STOP INSTANCES")
        INSTANCES = get_active_instances(ec2=ec2)
        for instance in INSTANCES:
            try:
                if 'running' in instance.state:
                    res = ec2.stop_instances(instance_ids=instance.id)
            except:
                print 'PROBLEM STOPPING %s' % (instance.id)
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    if 'term' in args.op:
        HEADER("TERMINATE INSTANCES")
        INSTANCES = get_active_instances(ec2=ec2)
        for instance in INSTANCES:
            try:
                res = ec2.terminate_instances(instance_ids=instance.id)
            except:
                print 'PROBLEM TERMINATING %s' % (instance.id)
    # -------------------------------------------------------------------------

    print 'DONE'
