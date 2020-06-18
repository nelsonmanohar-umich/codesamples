#!/usr/bin/env python


# #############################################################################
import logging
logging.getLogger('pika').setLevel(logging.INFO)
# #############################################################################


# #############################################################################
import time
import pika
import os
import subprocess
import boto
import json
import os.path
# #############################################################################


# #############################################################################
global S3
S3 = None
EC2 = None
CONNECTION = None
IN_CHANNEL = None
OUT_CHANNEL = None
CALLBACK_QUEUE = None
# #############################################################################


# #############################################################################
# DYNAMIC RECONFIGURATION CAPABILITY BETWEEN REMOTE SERVER & EC2 CLIENTS VIA S3
# IMPORTANT:   USED TO BROADCAST THE IP ASSIGNMENT OF THE REMOTE SERVER
# #############################################################################
from clt_aws_common import *
from clt_aws_s3_common import *
# #############################################################################


# #############################################################################
RCMD = "/usr/bin/Rscript %s/clt_aws_entry_point_train.R " % (get_prefix())
RCMD = RCMD + " XVAR %s XLEVEL %s OPMODE %s RDATA %s CVFOLD %s UNTIL_CVFOLD %s"
# #############################################################################


# #############################################################################
def mqinit(xname=None, inqname=None, outqname=None, qhost=None, ACK=True, outonly=False):
    global CONNECTION, IN_CHANNEL, OUT_CHANNEL, CALLBACK_QUEUE

    xname = VALIDATE(xname, XNAME)
    inqname = VALIDATE(inqname, RJE_QNAME)
    outqname = VALIDATE(outqname, SVR_QNAME)
    qhost = VALIDATE(qhost, QHOST)
    RJE_DEBUG("INFO", "INIT", (xname, inqname, outqname, qhost))

    CONNECTION = pika.BlockingConnection(pika.ConnectionParameters(host=qhost))
    IN_CHANNEL = CONNECTION.channel()
    IN_CHANNEL.queue_declare(queue=inqname)
    OUT_CHANNEL = CONNECTION.channel()
    OUT_CHANNEL.queue_declare(queue=outqname)
    CALLBACK_QUEUE = outqname
    RJE_DEBUG("INFO", "OPENED CHANNEL WITH CALLBACK QUEUE", (IN_CHANNEL, OUT_CHANNEL, CALLBACK_QUEUE))
    RJE_HEADER(n=1)
    RJE_DEBUG("INFO", '[*] Waiting for messages. To exit press CTRL+C', "...")
    RJE_HEADER(n=3)
    return (IN_CHANNEL, OUT_CHANNEL)
# #############################################################################


# #############################################################################
def issue_rje_command(RJE_COMMAND, rje_id, filename):
    RJE_CMD = [x.strip() for x in RJE_COMMAND.split(' ') if x.strip()]
    RJE_DEBUG("INFO", RJE_COMMAND, RJE_CMD)
    # -------------------------------------------------------------------------
    subprocess.check_call(RJE_CMD)
    # -------------------------------------------------------------------------
    msg = get_rje_tag("CMD_EXECUTED", rje_id)
    RJE_DEBUG("INFO", (msg, RJE_COMMAND))
    RJE_HEADER()
    return filename
# #############################################################################


# #############################################################################
def mqcallback(ch, method, properties, body):
    RJE_HEADER()
    RJE_HEADER()
    # -------------------------------------------------------------------------
    rje_id = get_id()
    msg = get_rje_tag("Received [%s B]" % len(body), rje_id)
    RJE_DEBUG("INFO", msg, "%r" % (body,))
    # -------------------------------------------------------------------------
    response = mqdispatcher(body, rje_id)
    # -------------------------------------------------------------------------
    RJE_HEADER()
    try:
        mqresponder(ch, method, properties, body, response, rje_id)
    except:
        RJE_DEBUG("ERROR", "PROBLEM SENDING COMPUTATION RESPONSE BACK TO SERVER", response, -3)
    # -------------------------------------------------------------------------
    return (body,)
# #############################################################################


# #############################################################################
def mq_process_rje_request(msgbody, rje_id):
    # -------------------------------------------------------------------------
    # it should clean and check parameters received, problem with msgq security
    # -------------------------------------------------------------------------
    print msgbody
    tstamp, msg = remove_tag(msgbody, sep=TAG_SEPARATOR_1)
    tag, msgbody = remove_tag(msg, sep=TAG_SEPARATOR_2)
    tag = tag[2:]
    params = json.loads(msgbody[:-1])
    # -------------------------------------------------------------------------
    print "%s %s" % (tag, rje_id)
    for x in params:
        print "[%12s] --> %s" % (x, params[x])
    # -------------------------------------------------------------------------
    if "DO_TEST" in params and params["DO_TEST"]:
        time.sleep(len(msgbody) % 3)
    # -------------------------------------------------------------------------
    return (params['OPMODE'], params['XVAR'], params['XLEVEL'], params['CVFOLD'], params['RDATA'], params['UNTIL'], tstamp, tag)
# #############################################################################


# #############################################################################
def cmd_prep_ack(opmode, rcmd, filename):
    print '-' * 80
    print "%12s --> [%s]" % ("OPMODE", opmode)
    print "%12s --> [%s]" % ("R CMD",  rcmd)
    print "%12s --> [%s]" % ("MODEL",  filename)
    print '-' * 80
    print
    return
# #############################################################################


# #############################################################################
def handle_preconditions(filename, opmode, delay=3):
    global S3
    retval = True
    try:
        if "train" in opmode:
            if not os.path.exists(filename):
                get_from_s3(filename, s3=S3)
                RJE_DEBUG("INFO", "TRAINING MODEL/CSV IMPORTED", filename)
        if "predict" in opmode:
            if not os.path.exists(filename):
                get_from_s3(filename, s3=S3)
                RJE_DEBUG("INFO", "TRAINING MODEL IMPORTED", filename)
        if "postprocessing" in opmode:
            (bucket_key, filing_key) = copy_to_s3(filename, s3=S3)
            RJE_DEBUG("INFO", "MODEL/YP EXPORTED TO S3", filename)
            retval = filing_key
            print '-' * 80
            time.sleep(delay)
    except:
        if "train" in opmode:
            RJE_DEBUG("WARNING", "S3 ERROR, TRAINING CSV NOT IMPORTED", filename)
        if "predict" in opmode:
            RJE_DEBUG("WARNING", "S3 ERROR, MODEL NOT IMPORTED", filename)
        if "postprocessing" in opmode:
            RJE_DEBUG("WARNING", "S3 ERROR, MODEL/YP NOT EXPORTED", filename)
        print '-' * 80
        return False

    if 0:
        if filing_key:
            filing_key.delete()
            RJE_DEBUG("INFO", get_rje_tag("S3_BUCKET_DELETED", rje_id), "S3_FILE: %s" % (filing_key))

    return retval
# #############################################################################


# #############################################################################
def fscleanup_remove_file(filename, rje_id):
    try:
        rcmd = "rm %s" % filename
        filename = issue_rje_command(rcmd, rje_id, filename)
    except:
        RJE_DEBUG("WARNING", "EC2 FS ERROR, MODEL/YP NOT REMOVED", filename)
    return filename
# #############################################################################


# #############################################################################
def mqdispatcher(msgbody, rje_id):
    # -------------------------------------------------------------------------
    do_preamble()
    # -------------------------------------------------------------------------
    (opmode, xvar, xlevel, cvfold, rdata, until_cvfold, t, tag) = mq_process_rje_request(msgbody, rje_id)
    msg = get_rje_tag("CMD_RECEIVED", "%s %s" % (tag, rje_id))
    RJE_DEBUG("INFO", msg, (t, opmode, xvar, xlevel))
    filing_key = None
    in_filename = ""
    out_filename = ""
    try:
        # ---------------------------------------------------------------------
        if "train" in opmode:
            if xlevel:
                in_filename = "%sP%s_%s_xy_.csv" % (get_prefix(), xvar, xlevel)
                handle_preconditions(in_filename, opmode)
                out_filename = "%sP%s-%s.RData" % (get_prefix(), xvar, xlevel)
                if not REBUILD_MODELS:
                    handle_preconditions(out_filename, opmode)
            else:
                xlevel = "None"
            rcmd = RCMD % (xvar, xlevel, OPMODE_TRAIN, rdata, cvfold, until_cvfold)
            cmd_prep_ack(opmode, rcmd, out_filename)
            if not os.path.exists(out_filename) or REBUILD_MODELS:
                out_filename = issue_rje_command(rcmd, rje_id, out_filename)
                filing_key = handle_preconditions(out_filename, "postprocessing")
        # ---------------------------------------------------------------------
        if "predict" in opmode or "test" in opmode:
            if xlevel:
                in_filename = "%sP%s-%s.RData" % (get_prefix(), xvar, xlevel)
                handle_preconditions(in_filename, opmode)
                out_filename = "P%s-%s.csv" % (xvar, xlevel)
            else:
                out_filename = "P%s_cvchunk_%s.probs" % (xvar, 1000+int(cvfold))
                xlevel = "None"
            rcmd = RCMD % (xvar, xlevel, OPMODE_TEST, rdata, cvfold, until_cvfold)
            cmd_prep_ack(opmode, rcmd, out_filename)
            out_filename = issue_rje_command(rcmd, rje_id, out_filename)
            filing_key = handle_preconditions(out_filename, "no_op")
        # ---------------------------------------------------------------------
        resp = mqformatter(["STATUS", "OPMODE", "XVAR", "XLEVEL", "FILING_KEY"],
                           ["CMD_SUCCEEDED", opmode, xvar, xlevel, filing_key],
                           rje_id)
        RJE_DEBUG("INFO", resp)
        # ---------------------------------------------------------------------
    except:
        resp = mqformatter(["STATUS", "OPMODE", "XVAR", "XLEVEL"],
                           ["CMD_FAILED", opmode, xvar, xlevel],
                           rje_id)
        RJE_DEBUG("WARNING", resp)
    finally:
        msg = get_rje_tag("CMD_PROCESSING_DONE: %s: %s: %s" % (opmode, xvar, xlevel), rje_id)
        RJE_DEBUG("INFO", msg)
    return resp
# #############################################################################


# #############################################################################
def mqresponder(ch, method, properties, body, response, rje_id):
    global CONNECTION, IN_CHANNEL, OUT_CHANNEL, CALLBACK_QUEUE
    msg = mqformatter(["MSGBODY", "TIMESTAMP", "WHOAMI"],
                       [response, "%.2f" % time.time(), whoami()],
                       rje_id)
    mqpublish(msg, OUT_CHANNEL, rje_id)
    return (body,)
# #############################################################################


# #############################################################################
def mqpublish(psmsg, channel, rje_id, xname=None, qname=None):
    msg = get_rje_tag("Responding [%s B]" % len(psmsg), rje_id)
    RJE_DEBUG("INFO", msg, "%r" % (psmsg,))
    # -------------------------------------------------------------------------
    xname = VALIDATE(xname, XNAME)
    outqname = VALIDATE(qname, SVR_QNAME)
    # -------------------------------------------------------------------------
    channel.basic_publish(exchange=xname, routing_key=outqname, body=psmsg)
    msg = get_rje_tag("SENT [%s B] %s" % (len(psmsg), psmsg), rje_id)
    RJE_DEBUG("INFO", msg, "ON CHANNEL %s" % outqname)
    RJE_HEADER()
    RJE_HEADER(n=3)
    return msg
# #############################################################################


# #############################################################################
def mqopen(mqcallback, inqname=None, ACK=True):
    global CONNECTION, IN_CHANNEL, OUT_CHANNEL, CALLBACK_QUEUE
    inqname = VALIDATE(inqname, RJE_QNAME)
    IN_CHANNEL.basic_qos(prefetch_count=1)
    IN_CHANNEL.basic_consume(mqcallback, queue=inqname, no_ack=ACK)
    return
# #############################################################################


# #############################################################################
def mqloop():
    global CONNECTION, IN_CHANNEL, OUT_CHANNEL, CALLBACK_QUEUE
    IN_CHANNEL.start_consuming()
    return
# #############################################################################


# #############################################################################
def mqclose():
    global CONNECTION, IN_CHANNEL, OUT_CHANNEL, CALLBACK_QUEUE
    IN_CHANNEL.stop_consuming()
    CONNECTION.close()
    return
# #############################################################################


# #############################################################################
if __name__ == "__main__":
    # -------------------------------------------------------------------------
    S3 = boto.connect_s3()
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    (IN_CHANNEL, OUT_CHANNEL) = mqinit()
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    mqopen(mqcallback)
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    try:
        mqloop()
    except KeyboardInterrupt:
        mqclose()
    # -------------------------------------------------------------------------
# #############################################################################
