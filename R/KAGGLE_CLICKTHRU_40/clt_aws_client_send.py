#!/usr/bin/env python


# #############################################################################
CONNECTION = None
IN_CHANNEL = None
OUT_CHANNEL = None
OPMODE_TRAIN = "train"
OPMODE_PREDICT = "predict"
# #############################################################################


# #############################################################################
import sys
import argparse
import pickle
# #############################################################################


# #############################################################################
from clt_aws_common import *
from clt_aws_s3_common import *
from clt_aws_boto_common import *
from clt_aws_client import *
# #############################################################################


# #############################################################################
if __name__ == "__main__":
    # -------------------------------------------------------------------------
    parser = argparse.ArgumentParser(prog='clt_aws_server_send.py',
             description='pseudo-rpcs (sends rje: remote job entry points) to a computer which executes task and acks upon completion')
    # -------------------------------------------------------------------------
    parser.add_argument("--opmode", type=str,
             default="test", help='train or predict')
    parser.add_argument("--xvar", type=str,
             default="site_id", help='name of the factor variable')
    parser.add_argument("--xlevel", type=str,
             default="", help='factor level to analyze, (a level from the factor variable)')
    parser.add_argument("--cvfold", type=int,
             default=1, help='index of the dataset fold to predict via rje interface')
    parser.add_argument("--rdata", type=str,
             default="T4.RData", help='name of the remote RData image')
    parser.add_argument("--status", type=str,
             default="CMD_SUCCEEDED", help='CMD_FAILED or CMD_SUCCEEDED')
    parser.add_argument("--filename", type=str,
             default="", help='name of the s3 file')
    parser.add_argument("--data", type=str,
             default="", help='arbitrary string data')
    parser.add_argument("--filing_key", type=str,
             default="None", help='s3 filing key')
    parser.add_argument("--dryrun", type=int,
             default=1, help='1/0 --> invoke/noinvoke remote model building via rje interface')
    # -------------------------------------------------------------------------
    args = parser.parse_args()
    args_d = dict(zip(["STATUS", "OPMODE", "XVAR", "XLEVEL", "FILING_KEY", "RDATA", "CVFOLD", "FILENAME", "DRYRUN"],
                      [args.status, args.opmode, args.xvar, args.xlevel, args.filing_key, args.rdata, args.cvfold, args.filename, args.dryrun]))

    # ---------------------------------------------------------------------
    try:
        print >>sys.stderr, '-' * 80
        for x in args_d:
            print >>sys.stderr, "[%15s] --> %s" % (x, args_d[x])
        print >>sys.stderr, '-' * 80
        print >>sys.stderr, '-' * 80
    except:
        parser.print_help()
    # ---------------------------------------------------------------------

    (OUT_CHANNEL, IN_CHANNEL) = mqinit()

    uniq_id = fake_uniq_id()

    # ---------------------------------------------------------------------
    data = args.data
    if not data:
        if (OPMODE_PREDICT in args.opmode.lower() or OPMODE_TEST in args.opmode.lower()):
            try:
                with open(args.filename, "r") as fp:
                    data = pickle.dumps(fp.read())
            except:
                data = args.filename
    # ---------------------------------------------------------------------

    # ---------------------------------------------------------------------
    filing_key = get_proj_filingkey_for(args.filename, s3=None)
    msg = mqformatter(("STATUS", "OPMODE", "XVAR", "XLEVEL", "FILING_KEY", "RDATA", "CVFOLD", "FILENAME", "DATA"),
                      (args.status, args.opmode, args.xvar, args.xlevel, filing_key, args.rdata, args.cvfold, args.filename, data), uniq_id)
    mqpublish(msg, OUT_CHANNEL, uniq_id)
    mqclose()

    (uniq_id, msg)
