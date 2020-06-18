import pandas as pd
import psycopg2
import time
# ##########################################################################


# ##########################################################################
NUMRECORDS = 10000
PGUSERNAME = "nrm"
PGPASSWORD = "nrm"
DFFILENAME = "P%s_%s_xy_%s.csv"
# ##########################################################################


# ##########################################################################
QRY_LOAD_DATA_SAMPLE = '''
-- =========================================================================
-- BASIC SELECT
-- =========================================================================
    SELECT *
        FROM (SELECT *
                FROM KAGGLE_CLT
                WHERE YYMMDDHH = '%s'
                  AND RANDOM()<0.75
                -- ORDER BY RIDX
                LIMIT %s) AS SUBSAMPLE
        -- WHERE RIDX %% 2 = 0
        ORDER BY ID;
-- =========================================================================
'''
# ##########################################################################


# ##########################################################################
QRY_LOAD_FACTOR_LEVEL_SAMPLE = '''
-- =========================================================================
-- FACTOR LEVEL BASIC SELECT
-- =========================================================================
   SELECT *
        FROM (SELECT *
                FROM KAGGLE_CLT
                WHERE YYMMDDHH = '%s'
                  AND RANDOM()<0.75
                -- ORDER BY RANDOM()
                LIMIT %s) as SUBSAMPLE
        WHERE %s='%s'
        ORDER BY ID;
-- =========================================================================
'''
# ##########################################################################


# ##########################################################################
def get_conn():
    conn = psycopg2.connect(user=PGUSERNAME, password=PGPASSWORD)
    return conn
# ##########################################################################


# ##########################################################################
def get_name_for_partition(partitions):
    return str(partitions).replace(',', '_')[1:-1]
# ##########################################################################


# ##########################################################################
def get_DDHH(YYMM="1410"):
    items = []
    for dd in ("21","22","23","24","25","26","27","28","29","30"):
        for hh in ("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23"):
            items.append(YYMM + dd + hh)
    return items
# ##########################################################################


# ##########################################################################
def get_dataset_sample(nmax=None, conn=None, write=True, uniqname="", opmode=""):
    global QRYNUMBER
    QRYNUMBER = 0

    t1 = time.time()
    if not nmax: nmax = NUMRECORDS
    DDHH = get_DDHH()
    frames050 = [pandas_query(QRY_LOAD_DATA_SAMPLE % (f, nmax), conn=conn) for f in DDHH[0:30]]
    frames100 = [pandas_query(QRY_LOAD_DATA_SAMPLE % (f, nmax), conn=conn) for f in DDHH[50:100]]
    frames150 = [pandas_query(QRY_LOAD_DATA_SAMPLE % (f, nmax), conn=conn) for f in DDHH[100:140]]
    frames200 = [pandas_query(QRY_LOAD_DATA_SAMPLE % (f, nmax), conn=conn) for f in DDHH[160:200]]
    frames250 = [pandas_query(QRY_LOAD_DATA_SAMPLE % (f, nmax), conn=conn) for f in DDHH[200:250]]
    df = pd.concat(frames050 + frames100 + frames150 + frames200 + frames250)
    print
    query_status(time.time() - t1, threshold=0)
    if write:
        summary(df)
        filename = get_factorlevel_cvsfilename("", "", opmode)
        write_csv(df, filename=filename, sep=",")
    return df
# ##########################################################################


# ##########################################################################
def query_status(delta, threshold=1.):
    global QRYNUMBER

    if delta > threshold:
        print '-' * 80
        print QRYNUMBER, "Query processed in %s seconds" % delta
        print '-' * 80
    return delta > threshold
# ##########################################################################


# ##########################################################################
def get_factorlevel_cvsfilename(xvar="", xlevel="", opmode=""):
    return DFFILENAME % (xvar, xlevel, opmode)
# ##########################################################################


# ##########################################################################
def get_factorlevel_data(xvar, xlevel, conn=None, nmax=None, write=True, opmode=""):
    global QRYNUMBER
    QRYNUMBER = 0

    t1 = time.time()
    if not nmax: nmax = NUMRECORDS
    DDHH = get_DDHH()
    frames050 = [pandas_query(QRY_LOAD_FACTOR_LEVEL_SAMPLE % (f, nmax, xvar, xlevel), conn=conn) for f in DDHH[0:30]]
    frames100 = [pandas_query(QRY_LOAD_FACTOR_LEVEL_SAMPLE % (f, nmax, xvar, xlevel), conn=conn) for f in DDHH[50:100]]
    frames150 = [pandas_query(QRY_LOAD_FACTOR_LEVEL_SAMPLE % (f, nmax, xvar, xlevel), conn=conn) for f in DDHH[100:140]]
    frames200 = [pandas_query(QRY_LOAD_FACTOR_LEVEL_SAMPLE % (f, nmax, xvar, xlevel), conn=conn) for f in DDHH[160:200]]
    frames250 = [pandas_query(QRY_LOAD_FACTOR_LEVEL_SAMPLE % (f, nmax, xvar, xlevel), conn=conn) for f in DDHH[200:250]]
    df = pd.concat(frames050 + frames100 + frames150 + frames200 + frames250)
    print
    query_status(time.time() - t1, threshold=0)
    if write:
        summary(df)
        filename = get_factorlevel_cvsfilename(xvar, xlevel, opmode)
        write_csv(df, filename=filename, sep=",")
    return df
# ##########################################################################


# ##########################################################################
QRYNUMBER = 0
def pandas_query(query, details=False, conn=None):
    global QRYNUMBER
    QRYNUMBER += 1

    t1 = time.time()
    if details:
        print query
    if not conn:
        conn = get_conn()
        df = pd.read_sql(query, conn)
        conn.close()
    else:
        df = pd.read_sql(query, conn)
    if query_status(time.time() - t1, threshold=1):
        print QRYNUMBER, df.shape, time.time() - t1, "secs"
        details = True
    else:
        print QRYNUMBER,
    if details:
        summary(df)
    return df
# ##########################################################################


# ##########################################################################
def summary(df):
    print '-' * 80
    print df.describe()
    print '-' * 80
    print df.shape
    print '-' * 80
    return df
# ##########################################################################


# ##########################################################################
def write_csv(df, filename="", sep=","):
    if filename:
        df.to_csv(filename, sep=sep)
    return
# ##########################################################################


# ##########################################################################
if __name__ == "__main__":
    NMAX = 50
    df0 = get_dataset_sample(nmax=NMAX)
    df1 = get_factorlevel_data("site_category", "50e219e0", nmax=NMAX)
