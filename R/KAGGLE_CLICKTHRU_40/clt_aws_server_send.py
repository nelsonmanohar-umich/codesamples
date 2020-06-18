#!/usr/bin/python


# #############################################################################
CONNECTION = None
IN_CHANNEL = None
OUT_CHANNEL = None
# #############################################################################


# #############################################################################
import sys
import time
import os.path
import argparse
# #############################################################################


# #############################################################################
from clt_aws_common import *
from clt_aws_s3_common import *
from clt_aws_boto_config import *
from clt_aws_dataloader import *
from clt_aws_server import *
# #############################################################################


# #############################################################################
def do_serverside_prep_for_train(xvar=None, xlevel=None, opmode=""):
    if not xlevel: return None
    if not xvar: return None

    factor_level_filename = get_factorlevel_cvsfilename(xvar, xlevel, opmode)

    s3_filing_key = get_proj_filingkey_for(factor_level_filename)
    if not s3_filing_key:
        if not os.path.exists(factor_level_filename):
            get_factorlevel_data(xvar, xlevel, opmode=opmode)
        copy_to_s3(factor_level_filename)
    return s3_filing_key
# #############################################################################


# #############################################################################
def place_remote_request_for(xvar=None, xlevel=None, opmode=OPMODE_TRAIN, rdata="", cvfold=1, until_cvfold=0, do_rje=True, delay=5.0):
    msg, uniq_id = "", int(time.time()) % 1000000
    if "predict" in opmode.lower(): opmode = OPMODE_TEST
    if "train" in opmode.lower(): opmode = OPMODE_TRAIN
    if "test" in opmode.lower(): opmode = OPMODE_TEST
    if do_rje:
        time.sleep(delay)
        msg = mqformatter(["OPMODE", "XVAR", "XLEVEL", "CVFOLD", "RDATA", "UNTIL"], [opmode, xvar, xlevel, cvfold, rdata, until_cvfold], uniq_id)
        mqpublish(msg, OUT_CHANNEL)
        time.sleep(delay)
    print uniq_id, msg
    return (uniq_id, msg)
# #############################################################################


# #############################################################################
def do_serverside_prep_for_predict(xvar=None, xlevel=None):
    if not xlevel: return
    if not xvar: return
    return
# #############################################################################


# #############################################################################
if __name__ == "__main__":
    # -------------------------------------------------------------------------
    parser = argparse.ArgumentParser(prog='clt_aws_server_send.py',
             description='pseudo-rpcs (sends rje: remote job entry points) to a computer which executes task and acks upon completion')
    # -------------------------------------------------------------------------
    parser.add_argument("--opmode", type=str,
             default="train", help='train or predict')
    parser.add_argument("--xvar", type=str,
             default="site_id", help='name of the factor variable')
    parser.add_argument("--xlevel", type=str,
             default="", help='factor level to analyze, (a level from the factor variable)')
    parser.add_argument("--dryrun", type=int,
             default=0, help='1/0 --> invoke/noinvoke remote model building via rje interface')
    parser.add_argument("--cvfold", type=int,
             default=1, help='index of the dataset fold to predict via rje interface')
    parser.add_argument("--until_cvfold", type=int,
             default=0, help='end index of the range of dataset folds to predict via rje interface, if any')
    parser.add_argument("--delay", type=int,
             default=3, help='throttling delay')
    parser.add_argument("--rdata", type=str,
             default="T4.RData", help='name of the remote RData image')
    # -------------------------------------------------------------------------
    args = parser.parse_args()
    # -------------------------------------------------------------------------

    args_d = dict(zip(['OPMODE', 'XVAR', 'XLEVEL', 'DRYRUN', 'RDATA', 'CVFOLD', 'UNTIL_CVFOLD', 'DELAY'],
                      [args.opmode, args.xvar, args.xlevel, args.dryrun, args.rdata, args.cvfold, args.until_cvfold, args.delay]))
    try:
        print >>sys.stderr, '-' * 80
        for x in args_d:
            print >>sys.stderr, "[%15s] --> %s" % (x, args_d[x])
        print >>sys.stderr, '-' * 80
        print >>sys.stderr
    except:
        parser.print_help()

    (OUT_CHANNEL, IN_CHANNEL) = mqinit()
 
    print 'START', args

    if "train" in args.opmode.lower():
        do_serverside_prep_for_train(xvar=args.xvar,
                                     xlevel=args.xlevel)

    if "predict" in args.opmode.lower() or "test" in args.opmode.lower():
        do_serverside_prep_for_predict(xvar=args.xvar,
                                       xlevel=args.xlevel)

    (uniq_id, msg) = place_remote_request_for(xvar=args.xvar,
                                              xlevel=args.xlevel,
                                              opmode=args.opmode,
                                              rdata=args.rdata,
                                              cvfold=args.cvfold,
                                              until_cvfold=args.until_cvfold,
                                              delay=args.delay)

    mqclose()

    print 'DONE', args

    do_preamble()

    (uniq_id, msg)
# #############################################################################
