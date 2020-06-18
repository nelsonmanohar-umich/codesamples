#!/usr/bin/env python


# #############################################################################
import time
import pika
import json
# #############################################################################


# #############################################################################
IN_CHANNEL = None
OUT_CHANNEL = None
CONNECTION = None
# #############################################################################


# #############################################################################
global S3
S3 = None
# #############################################################################


# #############################################################################
from clt_aws_common import *
from clt_aws_s3_common import *
# #############################################################################


# #############################################################################
def mqinit(inqname=None, outqname=None, xname=None, qhost=None):
    global CONNECTION, OUT_CHANNEL, IN_CHANNEL, CALLBACK_QUEUE

    xname = VALIDATE(xname, XNAME)
    inqname = VALIDATE(inqname, RJE_QNAME)
    outqname = VALIDATE(outqname, SVR_QNAME)
    qhost = VALIDATE(qhost, QHOST)
    RJE_DEBUG("INFO", "INIT", (xname, inqname, outqname, qhost))

    CONNECTION = pika.BlockingConnection(pika.ConnectionParameters(host=qhost))

    OUT_CHANNEL = CONNECTION.channel()
    OUT_CHANNEL.queue_declare(queue=inqname)

    IN_CHANNEL = CONNECTION.channel()
    IN_CHANNEL.queue_declare(queue=outqname)

    RJE_DEBUG("INFO", "OPENED CHANNELS", OUT_CHANNEL, IN_CHANNEL)

    return (OUT_CHANNEL, IN_CHANNEL)
# #############################################################################


# #############################################################################
def mqpublish(psmsg, channel, xname=None, qname=None):
    xname = VALIDATE(xname, XNAME)
    qname = VALIDATE(qname, RJE_QNAME)
    psmsg = VALIDATE(psmsg, "ACK%s OK" % TSTAMP_SEPARATOR)
    psmsg = "[%.2f]%s %r" % (time.time(), TSTAMP_SEPARATOR, psmsg,)

    rval = channel.basic_publish(exchange=xname, routing_key=qname, body=psmsg)
    RJE_HEADER()
    RJE_DEBUG("INFO", "SENT [%s B]" % len(psmsg), psmsg, "ON CHANNEL %s" % channel)
    RJE_HEADER()

    return rval
# #############################################################################


# #############################################################################
def mqclose():
    global CONNECTION
    rval = CONNECTION.close()
    RJE_DEBUG("INFO", "CLOSED CONNECTION", CONNECTION)
    return rval
# #############################################################################


# #############################################################################
def rje_response_hander(tag, msgdict, msgbody, delay=1):
    global S3

    print '-' * 80
    print tag
    for x in msgdict:
        print "\t[%10s]-->\t[%s]" % (x, msgdict[x])

    if "MSGBODY" in msgdict:
        for x in msgbody:
            print "\t[%10s]-->\t[%s]" % (x, msgbody[x])
        print '-' * 80
        xvar = msgbody['XVAR']
        xlevel = msgbody['XLEVEL']
        opmode = msgbody['OPMODE']
        try:
            filing_key = msgbody['FILING_KEY']
            filename = filing_key.split(',')[1][0:-1]
        except:
            filing_key = None
            filename = ""

    else:
        status = msgdict['STATUS']
        filing_key = msgdict['FILING_KEY']
        xvar = msgdict['XVAR']
        xlevel = msgdict['XLEVEL']
        filename = msgdict['FILENAME']
        opmode = msgdict['OPMODE']
        cvfold = msgdict['CVFOLD']
        rdata = msgdict['RDATA']
        data = msgdict['DATA']

    try:
        time.sleep(delay)
        s3_filing_key = None
        if filename:
            (s3_filing_key, contents) = aws_file_downloader(filename, s3=S3)
            RJE_DEBUG("INFO", "SUCCESSFUL S3 DOWNLOAD, MODEL/CSV", (xvar, xlevel, opmode, filing_key, filename))
    except:
        if s3_filing_key == 'False':
            RJE_DEBUG("WARNING", "PROBLEM REACHING S3, NO FEASIBLE MODEL/CSV COULD BE GENERATED", (xvar, xlevel, opmode, filing_key, filename))
            s3_filing_key = None
        else:
            RJE_DEBUG("WARNING", "PROBLEM REACHING S3, MODEL/CSV NOT DOWNLOADED", (xvar, xlevel, opmode, filing_key, filename))

    timestamp()

    return filing_key
# #############################################################################


# #############################################################################
def mqconsume(ch, method, props, body):
    # -------------------------------------------------------------------------
    do_preamble()
    # -------------------------------------------------------------------------
    print '-' * 80
    print
    print " [.] RECEIVED RESPONSE(%s)" % (body,)

    # -------------------------------------------------------------------------
    try:
        tag, msg = remove_tag(body, sep=TAG_SEPARATOR_2)
        msgdict = json.loads(msg)
        # -------------------------------------------------------------------------
        if "MSGBODY" in msgdict:
            tag, msgbody = remove_tag(msgdict["MSGBODY"], sep=TAG_SEPARATOR_2)
            msgbody = json.loads(msgbody)
            rje_response_hander(tag, msgdict, msgbody)
        else:
            rje_response_hander(tag, msgdict, msgdict)
        # -------------------------------------------------------------------------
    except:
        RJE_DEBUG("WARNING", "PROBLEM REACHING PROCESSING REMOTE RESPONSE", str(repr(body)))

    return (body,)
# #############################################################################


# #############################################################################
RJE_COUNTER = 2000
# #############################################################################


# #############################################################################
def next_id():
    global RJE_COUNTER
    RJE_COUNTER += 1
    return RJE_COUNTER
# #############################################################################


# #############################################################################
if __name__ == "__main__":
    S3 = boto.connect_s3()

    (OUT_CHANNEL, IN_CHANNEL) = mqinit()

    RJE_HEADER(n=1)
    print " [x] Awaiting RPC requests"
    RJE_HEADER(n=3)
    try:
        IN_CHANNEL.basic_consume(mqconsume, queue=SVR_QNAME, no_ack=True)
        IN_CHANNEL.start_consuming()
    except KeyboardInterrupt:
        IN_CHANNEL.stop_consuming()
        mqclose()
# #############################################################################
