psql -u machlearn -d classifiers -e -h gateway -x auto 






-- =========================================================================
-- MODEL PARAMATERS INTO STRING
-- =========================================================================
-- 17370673079623956563,0,14102101,1005,0,85f751fd,c4e18dd6,50e219e0,5e3f096f,2347f47a,0f2161f8,a99f214a,a02a27e7,8a6b386b,1,0,21611,320,50,2480,3,297,100111,61
-- =========================================================================
-- 'ad_id', 'click', 'YYMMDDHH', 
-- 'C01', 'banner_pos', 
-- 'site_id', 'site_domain', 'site_category', 
-- 'app_id', 'app_domain', 'app_category', 
-- 'device_id', 'device_ip', 'device_model', 
-- 'device_type', 'device_conn_type', 
-- 'C14', 'C15', 'C16', 'C17', 'C18', 'C19', 'C20', 'C21'
-- =========================================================================
CREATE TABLE KAGGLE_CLT(
        AD_ID               VARCHAR(24) NOT NULL,
        CLICK               SMALLINT,
        YYMMDDHH            CHARACTER(8),
        C01                 SMALLINT,
        BANNER_POS          SMALLINT,
        SITE_ID             CHARACTER(8),
        SITE_DOMAIN         CHARACTER(8),
        SITE_CATEGORY       CHARACTER(8),
        app_id              CHARACTER(8),
        app_domain          CHARACTER(8),
        app_category        CHARACTER(8),
        device_id           CHARACTER(8),
        device_ip           CHARACTER(8),
        device_model        CHARACTER(8),
        device_type         SMALLINT,
        device_conn_type    SMALLINT,
        C14                 SMALLINT,
        C15                 SMALLINT,
        C16                 SMALLINT,
        C17                 SMALLINT,
        C18                 SMALLINT,
        C19                 SMALLINT,
        C20                 INTEGER,
        C21                 SMALLINT
    );
-- =========================================================================


    -- =========================================================================
    -- LOAD DATA STRAIGHT FROM CSV
    -- =========================================================================
    COPY KAGGLE_CLT
         FROM '/home/nrm/WORKSPACE/src/R/KAGGLE/CLICKTHRU/train'
         DELIMITER ',' 
         CSV HEADER;
    -- =========================================================================
    
    
    -- =========================================================================
    -- BASIC DEFAULT PARTITIONING MODULO HASH
    -- =========================================================================
    CREATE OR REPLACE 
        FUNCTION DEFAULT_MINHASH ( BIGINT )
            RETURNS INTEGER AS 
            '
            DECLARE
                MH  INTEGER;
            BEGIN
                MH := (($1 * 13 + 1117) % 101);
                RETURN ( MH );
            END;
            ' 
        LANGUAGE 'plpgsql';
    -- =========================================================================
    
    
    -- =========================================================================
    -- BASIC INDEXING CAPABILITY
    -- =========================================================================
    ALTER TABLE KAGGLE_CLT ADD COLUMN ID BIGSERIAL NOT NULL;
    -- =========================================================================
    
    
    -- =========================================================================
    -- BASIC HASH/CLUSTERING/SIMILARITY CAPABILITY, REQUIRES ABOVE PRECONDITION
    -- =========================================================================
    ALTER TABLE KAGGLE_CLT ADD COLUMN MINHASH INTEGER;
    UPDATE KAGGLE_CLT SET MINHASH = DEFAULT_MINHASH(ID);
    -- =========================================================================
    
    
    -- =========================================================================
    -- BASIC INDEXES
    -- =========================================================================
    CREATE INDEX INDEX_CLT             ON KAGGLE_CLT(ID);
    CREATE INDEX INDEX_CLT_BY_YYMMDDHH ON KAGGLE_CLT(YYMMDDHH);
    CREATE INDEX INDEX_CLT_BY_MINHASH  ON KAGGLE_CLT(MINHASH);
    -- =========================================================================


-- =========================================================================
-- BASIC MAINTAINANCE
-- =========================================================================
ANALYZE VERBOSE KAGGLE_CLT;
VACUUM KAGGLE_CLT;
-- =========================================================================






-- =========================================================================
-- BASIC SELECT
-- =========================================================================
SELECT      * 
    FROM (
          SELECT * 
             FROM KAGGLE_CLT
             WHERE MINHASH=2
             LIMIT 100
         ) as SUBSAMPLE
    ORDER BY YYMMDDHH;
-- =========================================================================


-- =========================================================================
-- BASIC ITEMIZATION
-- =========================================================================
SELECT      YYMMDDHH, CLICK, COUNT(CLICK) AS NUM_CLKS,
            COUNT(DISTINCT(banner_pos)) AS TBP, 
            COUNT(DISTINCT(site_id))   AS T_SID,   COUNT(DISTINCT(site_domain)) AS T_SDOM, COUNT(DISTINCT(site_category)) AS T_SCAT, 
            COUNT(DISTINCT(app_id))    AS T_AID,   COUNT(DISTINCT(app_domain))  AS T_ADOM, COUNT(DISTINCT(app_category))  AS T_ACAT, 
            COUNT(DISTINCT(device_id)) AS T_DID,   COUNT(DISTINCT(device_ip))   AS T_DIP,  COUNT(DISTINCT(device_model))  AS T_DMO,
            COUNT(DISTINCT(device_type))      AS T_DTYP, 
            COUNT(DISTINCT(device_conn_type)) AS T_DCONN
    FROM (
          SELECT * 
             FROM KAGGLE_CLT
             WHERE MINHASH=2
             LIMIT 100
         ) as KAGGLE_SUBSAMPLE
    GROUP BY YYMMDDHH, CLICK;
-- =========================================================================






-- =========================================================================
-- JUST A WAY TO RETURN TABLE ROWS THAT CAN BE PUT INTO A SUMMARY TABLE:
-- HARDCODED COLUMN NAME AT THIS TIME AS IT IS UNCLEAR HOW TO PASS ARBITRARY ONE
-- =========================================================================
-- EXPLAIN VERBOSE 
    CREATE OR REPLACE 
        FUNCTION BASIC_STATS ( )
            RETURNS 
                TABLE(YYMMDDHH CHARACTER(8), 
                      CLICK    SMALLINT, 
                      S1       BIGINT, 
                      S2       FLOAT, 
                      S3       FLOAT, 
                      S4       FLOAT) 
            AS
            '
                SELECT YYMMDDHH, 
                       CLICK,
                       COUNT(DISTINCT(C14))            AS CNT_X,
                       CAST(AVG(C14) AS FLOAT)         AS AVG_X,
                       CAST(MIN(C14) AS FLOAT)         AS MIN_X,             
                       CAST(MAX(C14) AS FLOAT)         AS MAX_X            
                FROM ( SELECT YYMMDDHH, CLICK, 
                       C14 
                       FROM KAGGLE_CLT 
                       WHERE MINHASH=2 LIMIT 100 
                     ) AS SUBSAMPLE
                GROUP BY YYMMDDHH, CLICK
                ORDER BY YYMMDDHH, CLICK;
            '
        LANGUAGE SQL;
-- =========================================================================


-- =========================================================================
SELECT YYMMDDHH, CLICK, 
    COUNT(DISTINCT(C14)) AS CNT_C14, MAX(C14) AS MAX_C14, MIN(C14) AS MIN_C14, ROUND(AVG(C14),2) as AVG_C14, 
    COUNT(DISTINCT(C15)) AS CNT_C15, MAX(C15) AS MAX_C15, MIN(C15) AS MIN_C15, ROUND(AVG(C15),2) as AVG_C15, 
    COUNT(DISTINCT(C16)) AS CNT_C16, MAX(C16) AS MAX_C16, MIN(C16) AS MIN_C16, ROUND(AVG(C16),2) as AVG_C16, 
    COUNT(DISTINCT(C17)) AS CNT_C17, MAX(C17) AS MAX_C17, MIN(C17) AS MIN_C17, ROUND(AVG(C17),2) as AVG_C17, 
    COUNT(DISTINCT(C18)) AS CNT_C18, MAX(C18) AS MAX_C18, MIN(C18) AS MIN_C18, ROUND(AVG(C18),2) as AVG_C18, 
    COUNT(DISTINCT(C19)) AS CNT_C19, MAX(C19) AS MAX_C19, MIN(C19) AS MIN_C19, ROUND(AVG(C19),2) as AVG_C19, 
    COUNT(DISTINCT(C20)) AS CNT_C20, MAX(C20) AS MAX_C20, MIN(C20) AS MIN_C20, ROUND(AVG(C20),2) as AVG_C20, 
    COUNT(DISTINCT(C21)) AS CNT_C21, MAX(C21) AS MAX_C21, MIN(C21) AS MIN_C21, ROUND(AVG(C21),2) as AVG_C21
    FROM ( SELECT *
            FROM KAGGLE_CLT 
            WHERE MINHASH=2
            LIMIT 100
         ) KAGGLE_SUBSAMPLE
    GROUP BY YYMMDDHH, CLICK
    ORDER BY YYMMDDHH, CLICK;
-- =========================================================================






-- =========================================================================
\x auto;
SELECT * 
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_NAME ILIKE '%CLT%'
       OR TABLE_NAME ILIKE '%CLF%';
\x off;
-- =========================================================================







-- =========================================================================
-- MODEL PARAMATERS INTO STRING
-- =========================================================================
CREATE OR REPLACE 
    FUNCTION GET_MODEL_FROM_REQNUM( INTEGER, CLASSIFIER_TYPE, OP_MODE_TYPE, CHARACTER, CHARACTER )
        RETURNS CHARACTER AS 
        '
        DECLARE
            MODELNAME   CHARACTER(64);
        BEGIN
            MODELNAME := CONCAT( 
                "M=",    CAST($1 AS CHARACTER),
                ",CLF=", CAST($2 AS CHARACTER),
                ",OP=",  CAST($3 AS CHARACTER),
                ",F=",   CAST($4 as CHARACTER),
                ",fl=",  CAST($5 as CHARACTER));
            RETURN ( MODELNAME );
        END;
        ' 
    LANGUAGE 'plpgsql' WITH ( ISCACHABLE );
-- =========================================================================
    





-- =========================================================================
-- GIVEN REQNUM RETRIEVE HUMAN READABLE MODEL SPECIFICATION NAME
-- =========================================================================
CREATE OR REPLACE 
    FUNCTION GET_MODEL_FROM_REQNUM( INTEGER )
        RETURNS CHARACTER AS 
        '
        DECLARE
            MODELNAME   CHARACTER(64);
        BEGIN
            SELECT 
                GET_MODEL_FROM_REQ( REQNUM, CLASSIFIER, OP_MODE, FACTOR_NAME, FACTOR_LEVEL )
                INTO MODELNAME
                FROM CLF_ARGUMENTS 
                WHERE REQNUM = $1;
            RETURN ( MODELNAME );
        END;
        ' 
    LANGUAGE 'plpgsql' WITH ( ISCACHABLE );
-- =========================================================================
    






-- =========================================================================
-- logloss <- -mean(actual*log(yhat) + (1.0-actual)*log(1.0 - yhat))
-- =========================================================================
CREATE OR REPLACE 
    FUNCTION LOG_LOSS ( INTEGER )
        RETURNS FLOAT AS 
        '
        DECLARE
            LOGLOSS FLOAT;
        BEGIN
            SELECT MEAN(P.YTRUE*LOG(2, P.PROBS) + (1-P.YTRUE)*LOG(2, 1.0-P.PROBS)) 
                INTO LOGLOSS
                FROM CLF_PROBS P
                WHERE P.IDX = ANY
                    (SELECT SUBSET 
                        FROM SUBSET_SPECS S
                        WHERE S.SUBSET_NUM = $1);
            RETURN ( LOGLOSS );
        END;
        ' 
    LANGUAGE 'plpgsql' WITH ( ISCACHABLE );
-- =========================================================================






    
-- =========================================================================
-- NEW DATA TYPES
-- =========================================================================
CREATE TYPE STATUS_TYPE     
         AS ENUM ('SETUP_STARTED', 'SETUP_COMPLETED', 'COMPUTE_STARTED', 'COMPUTE_COMPLETED', 'FLAGGED');
CREATE TYPE CLASSIFIER_TYPE 
         AS ENUM ('DT', 'KNN', 'RF', 'SVM', 'NB', 'LOGIT', 'LDA', 'QDA', 'NN', 'GLM', 'ENSEMBLE', 'OTHER');
CREATE TYPE OP_MODE_TYPE    
         AS ENUM ('TRAIN', 'CV-PREDICT', 'CV-TRAIN', 'TEST-PREDICT' );
CREATE TYPE MODEL_RATING
         AS ENUM ('FIT', 'UNDERFIT', 'OVERFIT', 'OTHER', 'UNRATED');
-- =========================================================================







-- =========================================================================
-- AUTO INCREMENT UNIQUE IDS FOR EACH REQUEST
-- =========================================================================
CREATE SEQUENCE SEQ_REQNUM;
CREATE TABLE REQUEST_NUMBERS (
    REQNUM SMALLINT NOT NULL DEFAULT NEXTVAL('SEQ_REQNUM')
);
ALTER SEQUENCE SEQ_REQNUM OWNED BY REQUEST_NUMBERS.REQNUM;
-- =========================================================================







-- =========================================================================
-- STORES THE REQUESTS FOR TRAINING OR PREDICTING MODELS FOR AN ENSEMBLE
-- =========================================================================
CREATE TABLE CLF_REQUESTS (
    REQNUM       INTEGER 
                 PRIMARY KEY,
    STATUS       STATUS_TYPE,
    TSTAMP       TIMESTAMP
);
CREATE INDEX INDEX_CLF_REQUESTS
          ON       CLF_REQUESTS (REQNUM);
-- =========================================================================






-- =========================================================================
-- STORES THE REQUESTS FOR TRAINING OR PREDICTING MODELS FOR AN ENSEMBLE
-- =========================================================================
CREATE TABLE SUBSET_SPECS (
    SUBSET_NUM   INTEGER 
                 PRIMARY KEY,
    CVFOLD_NUM   INTEGER NOT NULL,
    N            INTEGER,
    SUBSET       BIGINT[]
);
CREATE INDEX INDEX_SUBSET_SPECS_BY_CVFOLD
          ON       SUBSET_SPECS_BY_CVFOL(CVFOLD_NUM);
-- =========================================================================






-- =========================================================================
-- STORES THE ENSEMBLE MODELING ARGUMENTS SPECIFYING HOW TO MODEL IS CONSTRUCTED/USED
-- =========================================================================
CREATE TABLE CLF_ARGUMENTS (
    REQNUM       INTEGER 
                 REFERENCES CLF_REQUESTS(REQNUM),
    MODEL_NUM    INTEGER
                 DEFAULT 0 
                 CHECK ( model_num >= -1 AND model_num < 10 ),
    CLASSIFIER   CLASSIFIER_TYPE NOT NULL,
    OP_MODE      OP_MODE_TYPE NOT NULL,
    FACTOR_NAME  VARCHAR(32) 
                 DEFAULT NULL,
    FACTOR_LEVEL VARCHAR(32) 
                 DEFAULT NULL,
    Q_THRESHOLD  FLOAT DEFAULT 0.40,
    SUBSET_SPEC  INTEGER
                 SUBSET_SPECS(SUBSET_NUM)
                 NOT NULL
);
CREATE INDEX INDEX_CLF_ARGUMENTS
          ON       CLF_ARGUMENTS (REQNUM);
-- =========================================================================







-- =========================================================================
-- MAPS REQUESTS TO AWS NODES
-- =========================================================================
CREATE TABLE REQ2IP_MAPPER (
    REQNUM       INTEGER 
                 REFERENCES CLF_REQUESTS(REQNUM),
    AWS_IP       INET,     
    EXT_IP       INET,     
    HOSTNAME     TEXT,
    TSTAMP       TIMESTAMP
);
CREATE INDEX INDEX_REQ2IP_MAPPER 
          ON       REQ2IP_MAPPER (REQNUM);
-- =========================================================================






-- =========================================================================
-- PROVIDES PERSISTENT STORAGE OF COMPUTED MODELS IN PICKLED FORMAT
-- =========================================================================
CREATE TABLE CLF_MODEL (
    REQNUM        INTEGER 
                  REFERENCES CLF_REQUESTS(REQNUM),
    TRAIN_SIZE    BIGINT,
    TRAIN_SCORE   FLOAT,
    VARNAMES      TEXT[],
    VARIMPORTANCE FLOAT[],
    PARAM_NAMES   TEXT[],
    PARAM_VALUES  TEXT[],
    RATING        MODEL_RATING DEFAULT 'UNRATED',
    NOTES         TEXT DEFAULT '',
    MODEL_BLOB    TEXT DEFAULT NULL
);
CREATE INDEX INDEX_CLF_MODEL
          ON       CLF_MODEL(REQNUM);
-- =========================================================================






-- =========================================================================
-- TRACKS PERFORMANCE OF CLASSIFIERS
-- =========================================================================
CREATE TABLE METRICS (
    REQNUM       INTEGER 
                 REFERENCES CLF_REQUESTS(REQNUM),
    N            BIGINT NOT NULL
                 CHECK ( N > 0 ),
    CVFOLD_NUM   INTEGER
                 REFERENCES SUBSET_SPECS(CVFOLD_NUM)
                 NOT NULL,
    ACCURACY     FLOAT NOT NULL
                 CHECK ( ACCURACY >= 0.0 AND ACCURACY <= 1.0 ),
    BER          FLOAT NOT NULL,
    LOGLOSS      FLOAT NOT NULL,
    F1           FLOAT NOT NULL,
    PRECISION    FLOAT NOT NULL,
    RECALL       FLOAT NOT NULL,
    TPR          FLOAT NOT NULL,
    TNR          FLOAT NOT NULL,
    FPR          FLOAT NOT NULL,
    FNR          FLOAT NOT NULL,
    TSTAMP       TIMESTAMP
);
CREATE INDEX INDEX_METRICS
          ON       METRICS(REQNUM);
-- =========================================================================






-- =========================================================================
-- CONFUSION TABLE OF EACH MODEL
-- =========================================================================
CREATE TABLE CONFUSION_TABLES (
    REQNUM       INTEGER 
                 REFERENCES CLF_REQUESTS(REQNUM),
    CMAT         BIGINT[][],
    TSTAMP       TIMESTAMP
)
CREATE INDEX INDEX_CONFUSION_TABLES
          ON       CONFUSION_TABLES(REQNUM);  
-- =========================================================================






-- =========================================================================
-- STORES THE COMPUTED PROBABILITIES FOR EACH MODEL
-- =========================================================================
CREATE TABLE CLF_PROBS (
    REQNUM       INTEGER 
                 REFERENCES CLF_REQUESTS(REQNUM),
    WHICH_CLASS  SMALLINT 
                 DEFAULT 1,
    IDX          BIGINT
                 NOT NULL,
    YTRUE        SMALLINT,
    PROB         FLOAT
                 DEFAULT 0.175
                 CHECK ( PROB >= 0.0 AND PROB <= 1.0 )
);
CREATE INDEX INDEX_CLF_PROBS_BY_IDX 
          ON       CLF_PROBS(IDX);
-- =========================================================================






-- =========================================================================
-- PROVIDES DISPLAY OF CURRENT PENDING REQUESTS
-- =========================================================================
CREATE OR REPLACE
    VIEW VIEW_PENDING_REQUESTS AS 
        SELECT R.REQNUM, MAX(R.STATUS), MAX(R.TSTAMP)-MIN(R.TSTAMP),
               A.OP_MODE, A.MODEL_NUM, A.CLASSIFIER, A.FACTOR_NAME, A.FACTOR_LEVEL
            FROM CLF_REQUESTS  R, 
                 CLF_ARGUMENTS A 
            WHERE R.REQNUM = A.REQNUM 
            GROUP BY R.REQNUM, A.OP_MODE, A.MODEL_NUM, A.CLASSIFIER, A.FACTOR_NAME, A.FACTOR_LEVEL
            HAVING MAX(STATUS) <= 'COMPUTE_STARTED'
            ORDER BY R.REQNUM;
-- =========================================================================






-- =========================================================================
-- PROVIDES DISPLAY OF CURRENT COMPLETED REQUESTS
-- =========================================================================
CREATE OR REPLACE
    VIEW VIEW_COMPLETED_REQUESTS AS 
        SELECT R.REQNUM, MAX(R.STATUS), MAX(R.TSTAMP)-MIN(R.TSTAMP),
               A.OP_MODE, A.MODEL_NUM, A.CLASSIFIER, A.FACTOR_NAME, A.FACTOR_LEVEL
            FROM CLF_REQUESTS  R, 
                 CLF_ARGUMENTS A 
            WHERE R.REQNUM = A.REQNUM 
            GROUP BY R.REQNUM, A.OP_MODE, A.MODEL_NUM, A.CLASSIFIER, A.FACTOR_NAME, A.FACTOR_LEVEL
            HAVING MAX(STATUS) = 'COMPUTE_COMPLETED'
            ORDER BY R.REQNUM;
-- =========================================================================






-- =========================================================================
INSERT INTO CLF_REQUESTS VALUES ( NEXTVAL('SEQ_REQNUM'), 'SETUP_STARTED', CURRENT_TIMESTAMP ); 
INSERT INTO CLF_REQUESTS VALUES ( NEXTVAL('SEQ_REQNUM'), 'SETUP_STARTED', CURRENT_TIMESTAMP ); 
-- =========================================================================


