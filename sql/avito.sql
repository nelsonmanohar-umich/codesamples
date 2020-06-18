-- ===============================================================================================
-- ===============================================================================================
-- THE INPUT TABLES
-- ===============================================================================================
-- ===============================================================================================


BEGIN;
-- =========================================================================
-- UserID	UserAgentID	UserAgentOSID	UserDeviceID	UserAgentFamilyID
-- =========================================================================
DROP TABLE IF EXISTS AVITO_USERS;
CREATE TABLE AVITO_USERS (
    UserID	            INTEGER,
    UserAgentID         INTEGER,
    UserAgentOSID	    INTEGER,
    UserDeviceID	    INTEGER,
    UserAgentFamilyID   INTEGER
);
-- =========================================================================

   -- =========================================================================
   -- LOAD DATA STRAIGHT FROM CSV
   -- =========================================================================
   COPY AVITO_USERS
   FROM '/LOCAL/KAGGLE/AVITO/UserInfo.tsv'
   USING DELIMITERS E'\t'
   CSV HEADER;
   -- =========================================================================
COMMIT;
   

   
BEGIN;
-- =========================================================================
-- UserID	IPID	AdID	ViewDate
-- =========================================================================
DROP TABLE IF EXISTS AVITO_IMPRESSIONS;
CREATE TABLE AVITO_IMPRESSIONS (
    UserID	            INTEGER,
    IPID	            INTEGER,
    AdID	            INTEGER,
    ViewDate            TIMESTAMP
);
-- =========================================================================

   -- =========================================================================
   -- LOAD DATA STRAIGHT FROM CSV
   -- =========================================================================
   COPY AVITO_IMPRESSIONS
   FROM '/LOCAL/KAGGLE/AVITO/VisitsStream.tsv'
   USING DELIMITERS E'\t'
   CSV HEADER;
   -- =========================================================================
COMMIT;
   
   

BEGIN;
-- =========================================================================
-- table created using python avito_dataclearner.py wrt SearchItems.tsv
-- =========================================================================
DROP TABLE IF EXISTS AVITO_SEARCH_HISTORY;
CREATE TABLE AVITO_SEARCH_HISTORY (
    SearchID            INTEGER,
    SearchDate          TIMESTAMP,
    IPID                INTEGER,
    UserID              INTEGER,
    IsUserLoggedOn      SMALLINT,
    SearchQuery         VARCHAR(32),
    LocationID          VARCHAR(8),
    CategoryID          VARCHAR(8),
 -- SearchParams        VARCHAR(64),    -- ommited on this version at this time, for space
    PARAM_NGRAM	        VARCHAR(48),    -- derived categorical extract from SearchParams
    PARAM_SIZE          VARCHAR(4),     -- derived categorical extract from SearchParams
    PARAM_NUM           VARCHAR(4),     -- derived categorical extract from SearchParams
    MONTHNUM            SMALLINT,       -- derived convenience field from SearchDate
    DAYNUM              SMALLINT,       -- derived convenience field from SearchDate
    QRY_SIZE            VARCHAR(4),     -- derived categorical field from SearchQuery
    QRY_WLEN            VARCHAR(4),     -- derived categorical field from SearchQuery
    QRY_ENGL            VARCHAR(4)      -- derived categorical field from SearchQuery
);
-- =========================================================================

   -- =========================================================================
   -- LOAD DATA STRAIGHT FROM CSV
   -- =========================================================================
   COPY AVITO_SEARCH_HISTORY
   FROM '/LOCAL/KAGGLE/AVITO/SearchInfo_Extended.tsv'
   USING DELIMITERS E'\t'
   CSV HEADER;
   -- =========================================================================
   CREATE INDEX IDX_AVITO_SEARCH_HISTORY ON AVITO_SEARCH_HISTORY(SEARCHID);
   CREATE INDEX IDX_AVITO_SEARCH_HISTORY_BY_DAY ON AVITO_SEARCH_HISTORY(DAYNUM);
   CREATE INDEX IDX_AVITO_SEARCH_HISTORY_BY_MONTH ON AVITO_SEARCH_HISTORY(MONTHNUM);
   -- =========================================================================
COMMIT;



BEGIN;
-- =========================================================================
-- UserID	IPID	AdID	PhoneRequestDate
-- =========================================================================
DROP TABLE IF EXISTS AVITO_PHONE_REQUESTS;
CREATE TABLE AVITO_PHONE_REQUESTS (
    UserID              INTEGER,
    IPID                INTEGER,
    AdID                INTEGER,
    PhoneRequestDate    TIMESTAMP,
    MONTHNUM            INTEGER,
    DAYNUM              INTEGER);
-- =========================================================================

   -- =========================================================================
   -- LOAD DATA STRAIGHT FROM CSV
   -- =========================================================================
   COPY AVITO_PHONE_REQUESTS
   FROM '/LOCAL/KAGGLE/AVITO/PhoneRequestsStream_Extended.tsv'
   USING DELIMITERS E'\t'
   CSV HEADER;
   -- =========================================================================
   CREATE INDEX IDX_AVITO_PHONE_REQUESTS ON AVITO_PHONE_REQUESTS(UserID);
   CREATE INDEX IDX_AVITO_PHONE_REQUESTS_BY_ADID ON AVITO_PHONE_REQUESTS(ADID);
   -- =========================================================================
COMMIT;



BEGIN;
-- =========================================================================
-- AdID	LocationID	CategoryID	Price	IsContext	ADPARAMS_NUM	ADPARAMS_SUM	ADPARAMS_LEN	ADPARAMS_NGRAM	AD_SIZE	AD_WLEN	AD_TOPK
-- =========================================================================
DROP TABLE IF EXISTS AVITO_ADS_INFO;
CREATE TABLE AVITO_ADS_INFO (
    AdID	            INTEGER,
    LocationID	        VARCHAR(8),
    CategoryID          VARCHAR(8),
    Price               VARCHAR(16),
    IsContext           SMALLINT,
    ADPARAMS_NUM        VARCHAR(4),
    ADPARAMS_SUM        VARCHAR(4),
    ADPARAMS_LEN        VARCHAR(4),
    ADPARAMS_NGRAM      VARCHAR(16),
    AD_SIZE             VARCHAR(4),
    AD_WLEN             VARCHAR(4),
    AD_TOPK             VARCHAR(4)
);
-- =========================================================================

   -- =========================================================================
   -- LOAD DATA STRAIGHT FROM CSV
   -- =========================================================================
   COPY AVITO_ADS_INFO
   FROM '/LOCAL/KAGGLE/AVITO/AdsInfo_Extended.tsv'
   USING DELIMITERS E'\t'
   CSV HEADER;
   -- =========================================================================
   CREATE INDEX IDX_AVITO_ADS_INFO ON AVITO_ADS_INFO(AdID);
   CREATE INDEX IDX_AVITO_ADS_INFO_BY_CATID ON AVITO_ADS_INFO(CategoryID);
   CREATE INDEX IDX_AVITO_ADS_INFO_BY_LOCID ON AVITO_ADS_INFO(LocationID);
    -- =========================================================================
COMMIT;
   


BEGIN;
-- =========================================================================
-- LocationID	Level	RegionID	CityID
-- =========================================================================
DROP TABLE IF EXISTS AVITO_LOCATIONS;
CREATE TABLE AVITO_LOCATIONS (
    LocationID	        VARCHAR(8),
    LocLevelID          SMALLINT,
    RegionID            SMALLINT,
    CityID              SMALLINT
);
-- =========================================================================

   -- =========================================================================
   -- LOAD DATA STRAIGHT FROM CSV
   -- =========================================================================
   COPY AVITO_LOCATIONS
   FROM '/LOCAL/KAGGLE/AVITO/Location.tsv'
   USING DELIMITERS E'\t'
   CSV HEADER;
   -- =========================================================================
   CREATE INDEX IDX_AVITO_LOCATIONS ON AVITO_LOCATIONS(LocationID);
   -- =========================================================================
COMMIT;



BEGIN;
-- =========================================================================
-- CategoryID	Level	ParentCategoryID	SubcategoryID
-- =========================================================================
DROP TABLE IF EXISTS AVITO_CATEGORIES;
CREATE TABLE AVITO_CATEGORIES (
    CategoryID          VARCHAR(8),
    CatLevelID          SMALLINT,
    ParentCategoryID    SMALLINT,
    SubcategoryID       SMALLINT
);
-- =========================================================================

   -- =========================================================================
   -- LOAD DATA STRAIGHT FROM CSV
   -- =========================================================================
   COPY AVITO_CATEGORIES
   FROM '/LOCAL/KAGGLE/AVITO/Category.tsv'
   USING DELIMITERS E'\t'
   CSV HEADER;
   -- =========================================================================
   CREATE INDEX IDX_AVITO_CATEGORIES ON AVITO_CATEGORIES(CategoryID);
   -- =========================================================================
COMMIT;



BEGIN;
    -- =========================================================================
    -- SearchID	AdID	Position	ObjectType	HistCTR	IsClick
    -- =========================================================================
    DROP TABLE IF EXISTS AVITO_TRAIN_SEARCHCLICKSTREAM;
    CREATE TABLE AVITO_TRAIN_SEARCHCLICKSTREAM (
        SearchID            INTEGER,
        AdID                INTEGER,
        BannerPosition      SMALLINT,   -- Position
        AdType              SMALLINT,   -- ObjectType
        HistCTR             VARCHAR(12) DEFAULT '0.0',
        IsClick             VARCHAR(8) DEFAULT '0');
    -- =========================================================================

        -- =========================================================================
    
        -- LOAD DATA STRAIGHT FROM CSV
        -- =========================================================================
        COPY AVITO_TRAIN_SEARCHCLICKSTREAM
        FROM '/LOCAL/KAGGLE/AVITO/trainSearchStream.tsv'
        USING DELIMITERS E'\t'
        CSV HEADER;
        -- =========================================================================
        CREATE INDEX IDX_AVITO_TRAIN_SEARCHCLICKSTREAM ON AVITO_TRAIN_SEARCHCLICKSTREAM(SEARCHID);
        CREATE INDEX IDX_AVITO_TRAIN_SEARCHCLICKSTREAM_BY_ADID ON AVITO_TRAIN_SEARCHCLICKSTREAM(ADID);
        -- =========================================================================
COMMIT;



BEGIN;
-- =========================================================================
-- SearchID	AdID	Position	ObjectType	HistCTR	IsClick
-- =========================================================================
DROP TABLE IF EXISTS AVITO_TEST_SEARCHCLICKSTREAM;
CREATE TABLE AVITO_TEST_SEARCHCLICKSTREAM (
    ID                  INTEGER,
    SearchID            INTEGER,
    AdID                INTEGER,
    BannerPosition      SMALLINT,   -- Position
    AdType              SMALLINT,   -- ObjectType
    HistCTR             VARCHAR(12) DEFAULT '0.0'
);
-- =========================================================================

   -- =========================================================================
   -- LOAD DATA STRAIGHT FROM CSV
   -- =========================================================================
   COPY AVITO_TEST_SEARCHCLICKSTREAM
   FROM '/LOCAL/KAGGLE/AVITO/testSearchStream.tsv'
   USING DELIMITERS E'\t'
   CSV HEADER;
   -- =========================================================================
   CREATE INDEX IDX_AVITO_TEST_SEARCHCLICKSTREAM ON AVITO_TEST_SEARCHCLICKSTREAM(SEARCHID);
   CREATE INDEX IDX_AVITO_TEST_SEARCHCLICKSTREAM_BY_ADID ON AVITO_TEST_SEARCHCLICKSTREAM(ADID);
   -- =========================================================================
COMMIT;



-- ===============================================================================================
-- DONE WITH INPUTS
-- ===============================================================================================




-- ===============================================================================================
-- ===============================================================================================
-- BUILD OF USER PROFILE
-- BASIC INFORMATION ABOUT PER-USER AD CLICKING HISTORY
-- ===============================================================================================
-- ===============================================================================================

   
-- =========================================================================
-- =========================================================================
-- =========================================================================
 

BEGIN;
-- =========================================================================
-- FOR EACH USER, FIND THINGS SUCH AS:
--          CLICK VOLUME, IMPRESSION VOLUME, NUMBER OF SEARCHES, ... 
-- THAT TOOK PLACE DURING TRAINING TIMESPAN
-- =========================================================================
DROP TABLE IF EXISTS AVITO_USER_CLICK_HISTORY;
-- =========================================================================
CREATE TABLE AVITO_USER_CLICK_HISTORY AS (
   SELECT 
       U.UserID                                                               AS CLICK_HISTORY_USERID, 
       COUNT(DISTINCT(H.SearchID))                                            AS USER_SEARCH_VOLUME,
       COUNT(C.AdID)                                                          AS USER_IMPRESSION_VOLUME,
       SUM(CAST(C.IsCLick as SMALLINT))                                       AS USER_CLICK_VOLUME,
       COUNT(DISTINCT(C.AdID))                                                AS USER_NUM_DIFFERENT_ADS,
       COUNT(C.AdID)/COUNT(DISTINCT(C.AdID))                                  AS USER_AVG_AD_TARGETING_EMPHASIS,
       -- =================================================================== == =======================
       SUM(CAST(C.IsCLick AS SMALLINT))/CAST(COUNT(DISTINCT(C.SearchID)) 
        AS FLOAT)                                                             AS USER_OVERALL_SEARCH_CLICKPROB,
       SUM(CAST(C.IsCLick AS SMALLINT))/CAST(COUNT(C.AdID) AS FLOAT)          AS USER_OVERALL_AD_CLICKPROB,
       -- =================================================================== == =======================
       SUM(CASE WHEN CAST(C.HistCTR AS FLOAT) < 0.02
                  THEN 1 ELSE 0 END)/COUNT(C.IsClick)                         AS USER_LOCTR_CLICKDENSITY,
       SUM(CASE WHEN CAST(C.HistCTR AS FLOAT) > 0.02
                  THEN 1 ELSE 0 END)/COUNT(C.IsClick)                         AS USER_HICTR_CLICKDENSITY,
       -- =================================================================== == =======================
       MAX(H.SearchDate)-MIN(H.SearchDate)                                    AS USER_TOTAL_TIMELAPSE,
       CAST('2015-05-08 00:00:00' AS TIMESTAMP) - MAX(H.SearchDate)           AS USER_LAST_SEARCHED,
       -- =================================================================== == =======================
       CAST('2015-05-08 00:00:00' AS TIMESTAMP) 
            - MIN(CASE WHEN CAST(C.IsClick AS INTEGER) > 0 
                       THEN H.SearchDate 
                       ELSE CAST('2015-04-08 00:00:00' as TIMESTAMP) END)     AS USER_LAST_CLICKED
       -- =================================================================== == =======================
   FROM AVITO_USERS U
   LEFT JOIN AVITO_SEARCH_HISTORY H
        ON U.USERID = H.USERID
   LEFT JOIN AVITO_TRAIN_SEARCHCLICKSTREAM C
        ON H.SearchID = C.SearchID
   WHERE H.MONTHNUM = 5 AND H.DAYNUM BETWEEN 1 AND 7
   GROUP BY U.UserID);
-- =========================================================================
CREATE INDEX IDX_AVITO_USER_CLICK_HISTORY ON AVITO_USER_CLICK_HISTORY(CLICK_HISTORY_USERID);
-- =========================================================================
COMMIT;


BEGIN;
-- =========================================================================
-- BASIC STATS ABOUT PHONE REQUESTS FOR EACH USER DURING PROFILING TIMESPAN
-- =========================================================================
DROP TABLE IF EXISTS AVITO_USER_PHONE_REQUESTS_STATS;
CREATE TABLE AVITO_USER_PHONE_REQUESTS_STATS AS (
    -- ================================================== == ===============
    SELECT P.UserID                                       AS PHREQ_USERID,
    -- ================================================== == ===============
    COUNT(P.UserId)                                       AS PHREQ_TOTALREQ,
    COUNT(DISTINCT(P.IPID))                               AS PHREQ_DEVICE_MOBILITY,
    COUNT(DISTINCT(P.AdID))                               AS PHREQ_DIFFERENT_ADS,
    COUNT(PhoneRequestDate)/COUNT(DISTINCT(P.AdID))       AS PHREQ_PER_AD_VOLUME,
    -- ================================================== == ===============
    MAX(PhoneRequestDate)
                  - MIN(PhoneRequestDate)                 AS PHREQ_TIMESPAN,
    -- ================================================== == ===============
    (CAST('2015-05-08 00:00:00' AS TIMESTAMP) 
                  - MAX(PhoneRequestDate))                AS PHREQ_HRS_SINCE_LAST
    -- ================================================== == ===============
    FROM AVITO_PHONE_REQUESTS P
    WHERE MONTHNUM=5 AND DAYNUM BETWEEN 1 and 7
    GROUP BY UserID);
-- =========================================================================
CREATE INDEX IDX_AVITO_USER_PHONE_REQUESTS_STATS ON AVITO_USER_PHONE_REQUESTS_STATS(PHREQ_USERID);
-- =========================================================================
COMMIT;


BEGIN;
-- =========================================================================
-- THE FINAL USER PROFILE: FOR EACH USER, GATHERED FROM VARIOUS TABLES
-- =========================================================================
DROP TABLE IF EXISTS AVITO_USER_STATS_PROFILE;
CREATE TABLE AVITO_USER_STATS_PROFILE AS (
    SELECT U.*, H.*, PHREQ.*
      FROM AVITO_USERS U
      LEFT JOIN AVITO_USER_CLICK_HISTORY H
        ON U.USERID=H.CLICK_HISTORY_USERID
      LEFT JOIN AVITO_USER_PHONE_REQUESTS_STATS PHREQ
        ON PHREQ_USERID=H.CLICK_HISTORY_USERID
      ORDER BY U.USERID); 
-- =========================================================================
CREATE INDEX IDX_AVITO_USER_STATS_PROFILE ON AVITO_USER_STATS_PROFILE(CLICK_HISTORY_USERID);
-- =========================================================================
COMMIT;








-- ===============================================================================================
-- ===============================================================================================
-- BUILD OF AD PROFILE
-- ===============================================================================================
-- ===============================================================================================


-- =========================================================================
-- A CONVENIENCE, NUMERICAL COMPARISON FASTER THAN DATE CONVERSION BUT STUPID 
-- PEOPLE LIKE TO CRITIQUE TO LOOK SHARP AS LONG AS NO CONFRONTATIONAL CHECKS EXISTS
-- =========================================================================
DROP TABLE IF EXISTS AVITO_IMPRESSIONS_WITH_DAYS;
CREATE TABLE AVITO_IMPRESSIONS_WITH_DAYS AS (
     SELECT *,
            EXTRACT(DAY FROM CAST(ViewDate AS TIMESTAMP)) AS DAYNUM,
            EXTRACT(MONTH FROM CAST(ViewDate AS TIMESTAMP)) AS MONTHNUM
     FROM AVITO_IMPRESSIONS);
-- =========================================================================
CREATE INDEX IDX_AVITO_IMPRESSIONS_WITH_DAYS ON AVITO_IMPRESSIONS_WITH_DAYS(AdID);
-- =========================================================================


-- =========================================================================
-- HOW MANY USERS HAVE ADS BEEN PRESENTED TO, HOW MANY DIFFERENT ADS, AD TARGETING DIVERSITY
-- =========================================================================
DROP TABLE IF EXISTS AVITO_AD_IMPRESSION_STATS;
CREATE TABLE AVITO_AD_IMPRESSION_STATS AS (
    SELECT I.AdID                                               AS IMPRESSION_ADID, 
       COUNT(DISTINCT(I.UserID))                                AS IMPRESSION_NUM_DIFF_USERS, 
       COUNT(I.UserId)                                          AS IMPRESSION_NUM_TOTAL_USERS,
       CAST(COUNT(I.UserId) AS FLOAT)/COUNT(DISTINCT(I.UserID)) AS IMPRESSION_AD2USR_DIVERSITY
    FROM AVITO_IMPRESSIONS_WITH_DAYS I
    WHERE I.MONTHNUM=5 AND I.DAYNUM BETWEEN 1 and 7
    GROUP BY I.AdID);
-- =========================================================================
CREATE INDEX IDX_AVITO_AD_IMPRESSION_STATS ON AVITO_AD_IMPRESSION_STATS(IMPRESSION_AdID);
-- =========================================================================


-- =========================================================================
-- AdID	LocationID	CategoryID	Price	IsContext	ADPARAMS_NUM	ADPARAMS_SUM	ADPARAMS_LEN	ADPARAMS_NGRAM	AD_SIZE	AD_WLEN	AD_TOPK
-- CategoryID	Level	ParentCategoryID	SubcategoryID
-- LocationID	Level	RegionID	CityID
-- =========================================================================
DROP TABLE IF EXISTS AVITO_ADS_PROFILE;
CREATE TABLE AVITO_ADS_PROFILE AS (
     SELECT A.*, C.CatLevelID, C.ParentCategoryID, C.SubcategoryID, L.LocLevelID, L.RegionID, L.CityID 
     FROM AVITO_ADS_INFO A
     LEFT JOIN AVITO_CATEGORIES C
     ON A.CategoryID = C.CategoryID
     LEFT JOIN AVITO_LOCATIONS L 
     ON A.LocationID = L.LocationID
     LEFT JOIN AVITO_AD_IMPRESSION_STATS I
     ON A.AdID = I.IMPRESSION_ADID);
-- =========================================================================
CREATE INDEX IDX_AVITO_ADS_PROFILE ON AVITO_ADS_PROFILE(AdID);
-- =========================================================================


-- =========================================================================
-- BASIC INFORMATION ABOUT PER-AD USER CLICKING HISTORY
-- =========================================================================
DROP TABLE IF EXISTS AVITO_AD_CLICK_HISTORY;
CREATE TABLE AVITO_AD_CLICK_HISTORY AS (
   SELECT C.AdID                                                              AS CLICK_HISTORY_ADID, 
       -- =================================================================== == =======================
       COUNT(DISTINCT(H.SearchID))                                            AS AD_SEARCH_VOLUME,
       COUNT(H.UserID)                                                        AS AD_USERVIEWS,
       SUM(CAST(C.IsCLick AS INTEGER))                                        AS AD_CLICK_VOLUME,
       COUNT(DISTINCT(H.UserID))                                              AS AD_USER_REACH,
       COUNT(H.UserID)/CAST(COUNT(DISTINCT(H.UserID)) AS FLOAT)               AS AD_AVG_USR_TARGETING_EMPHASIS,
       -- =================================================================== == =======================
       SUM(CAST(C.IsCLick AS INTEGER))/CAST(COUNT(C.SearchID) AS FLOAT)       AS AD_OVERALL_SEARCH_CLICKPROB,
       SUM(CAST(C.IsCLick AS INTEGER))/CAST(COUNT(H.UserID) AS FLOAT)         AS AD_OVERALL_USR_CLICKPROB
       -- =================================================================== == =======================
   FROM AVITO_SEARCH_HISTORY H,
        AVITO_TRAIN_SEARCHCLICKSTREAM C
   WHERE H.SearchID = C.SearchID
     AND H.MONTHNUM=5 AND H.DAYNUM BETWEEN 1 and 7
   GROUP BY C.AdID);
-- =========================================================================
CREATE INDEX IDX_AVITO_AD_CLICK_HISTORY ON AVITO_AD_CLICK_HISTORY (CLICK_HISTORY_ADID);
-- =========================================================================


-- =========================================================================
-- THE FINAL AD PROFILE: WITH STATS WHETHER CLICKED OR NOT
-- =========================================================================
DROP TABLE IF EXISTS AVITO_AD_STATS_PROFILE;
CREATE TABLE AVITO_AD_STATS_PROFILE AS (
    SELECT A.*, H.*, S.*
    FROM AVITO_ADS_PROFILE A
    LEFT JOIN AVITO_AD_CLICK_HISTORY H
        ON A.AdID = H.CLICK_HISTORY_ADID
    LEFT JOIN AVITO_AD_IMPRESSION_STATS S 
        ON A.AdID = S.IMPRESSION_ADID);
-- =========================================================================
CREATE INDEX IDX_AVITO_AD_STATS_PROFILE ON AVITO_AD_STATS_PROFILE(AdID);
-- =========================================================================



BEGIN;
-- =========================================================================
-- THE TIME PIVOT FOR THE INITIAL BUILDING BLOCK OF THE DESIGN MATRIX OUTLOOK:
--          THAT IS, HOW MANY RECORDS AVAILABLE FOR TRAINING
-- =========================================================================
DROP TABLE IF EXISTS AVITO_SEARCH_CLICKS_0;
CREATE TABLE AVITO_SEARCH_CLICKS_0 AS (
        SELECT H.*,
        C.SEARCHID                                                         AS LANDING_SEARCHID,
        C.ADID                                                             AS LANDING_ADID,
        C.BANNERPOSITION                                                   AS LANDING_BANNERPOSITION,
        C.ADTYPE                                                           AS LANDING_ADTYPE,
        C.HISTCTR                                                          AS LANDING_HISTCTR,
        C.ISCLICK                                                          AS CLICK
        FROM
        (SELECT * FROM AVITO_TRAIN_SEARCHCLICKSTREAM WHERE ISCLICK = '0' OR ISCLICK IS NULL LIMIT 10000000) C
        INNER JOIN (SELECT * FROM AVITO_SEARCH_HISTORY WHERE MONTHNUM=5 AND DAYNUM BETWEEN 1 AND 7) H
        ON C.SEARCHID = H.SEARCHID
        ORDER BY SEARCHID, LANDING_ADID);
-- =========================================================================

-- =========================================================================
DROP TABLE IF EXISTS AVITO_SEARCH_CLICKS_1;
CREATE TABLE AVITO_SEARCH_CLICKS_1 AS (
        SELECT H.*,
        C.SEARCHID                                                         AS LANDING_SEARCHID,
        C.ADID                                                             AS LANDING_ADID,
        C.BANNERPOSITION                                                   AS LANDING_BANNERPOSITION,
        C.ADTYPE                                                           AS LANDING_ADTYPE,
        C.HISTCTR                                                          AS LANDING_HISTCTR,
        C.ISCLICK                                                          AS CLICK
        FROM
        (SELECT * FROM AVITO_TRAIN_SEARCHCLICKSTREAM WHERE ISCLICK = '1' LIMIT 1000000) C
        INNER JOIN (SELECT * FROM AVITO_SEARCH_HISTORY WHERE MONTHNUM=5 AND DAYNUM BETWEEN 1 AND 7) H
        ON C.SEARCHID = H.SEARCHID
        ORDER BY SEARCHID, LANDING_ADID);
-- =========================================================================

-- =========================================================================
CREATE INDEX IDX_AVITO_SEARCH_CLICKS_0 ON AVITO_SEARCH_CLICKS(DAYNUM);
-- =========================================================================

-- =========================================================================
DROP TABLE IF EXISTS AVITO_SEARCH_CLICKS;
CREATE TABLE AVITO_SEARCH_CLICKS AS (
    (SELECT * FROM AVITO_SEARCH_CLICKS_1)
    UNION (SELECT * FROM AVITO_SEARCH_CLICKS_0 WHERE DAYNUM=1 ORDER BY RANDOM() LIMIT 100000)
    UNION (SELECT * FROM AVITO_SEARCH_CLICKS_0 WHERE DAYNUM=2 ORDER BY RANDOM() LIMIT 100000)
    UNION (SELECT * FROM AVITO_SEARCH_CLICKS_0 WHERE DAYNUM=3 ORDER BY RANDOM() LIMIT 100000)
    UNION (SELECT * FROM AVITO_SEARCH_CLICKS_0 WHERE DAYNUM=4 ORDER BY RANDOM() LIMIT 100000)
    UNION (SELECT * FROM AVITO_SEARCH_CLICKS_0 WHERE DAYNUM=5 ORDER BY RANDOM() LIMIT 100000)
    UNION (SELECT * FROM AVITO_SEARCH_CLICKS_0 WHERE DAYNUM=6 ORDER BY RANDOM() LIMIT 100000)
    UNION (SELECT * FROM AVITO_SEARCH_CLICKS_0 WHERE DAYNUM=7 ORDER BY RANDOM() LIMIT 100000));
-- =========================================================================

-- =========================================================================
CREATE INDEX IDX_AVITO_SEARCH_CLICKS ON AVITO_SEARCH_CLICKS(SEARCHID);
CREATE INDEX IDX_AVITO_SEARCH_CLICKS_BY_ADID ON AVITO_SEARCH_CLICKS(LANDING_ADID);
-- =========================================================================

-- =========================================================================
-- DROP TABLE IF EXISTS AVITO_SEARCH_CLICKS_1;
-- DROP TABLE IF EXISTS AVITO_SEARCH_CLICKS_0;
-- =========================================================================
COMMIT;




-- ===============================================================================================================
-- THE DESIGN MATRIX
-- ===============================================================================================================
DROP TABLE IF EXISTS AVITO_TRAINSTREAM;
CREATE TABLE AVITO_TRAINSTREAM AS (
  SELECT 
    H.SEARCHID                                                         AS SEARCH_SEARCHID,
    H.USERID                                                           AS SEARCH_USERID,
    H.LOCATIONID                                                       AS SEARCH_LOCATIONID,
    H.CATEGORYID                                                       AS SEARCH_CATEGORYID,
    H.IPID                                                             AS SEARCH_IPID,
    H.SEARCHDATE                                                       AS SEARCH_DATE,
    -- =============================================================== == =====================
    EXTRACT(DOW FROM H.SEARCHDATE)                                     AS SEARCH_DOW,
    EXTRACT(HOUR FROM H.SEARCHDATE)                                    AS SEARCH_HOUR,
    EXTRACT(EPOCH FROM 
           ('2015-05-08 00:00:00'::timestamp -H.SEARCHDATE))/(3600*24) AS SEARCH_DAYS_SINCE,
    -- =============================================================== == =====================
    H.ISUSERLOGGEDON                                                   AS SEARCH_ISUSERLOGGEDON,
    H.PARAM_NGRAM                                                      AS SEARCH_PARAM_NGRAM,
    H.PARAM_SIZE                                                       AS SEARCH_PARAM_SIZE,
    H.PARAM_NUM                                                        AS SEARCH_PARAM_NUM,
    H.MONTHNUM                                                         AS SEARCH_MONTHNUM,
    H.DAYNUM                                                           AS SEARCH_DAYNUM,
    H.QRY_SIZE                                                         AS SEARCH_QRY_SIZE,
    H.QRY_WLEN                                                         AS SEARCH_QRY_LEN,
    H.QRY_ENGL                                                         AS SEARCH_QRY_ENGL,
    -- =============================================================== == =====================
    -- C.SEARCHID                                                         AS 
                                                                          LANDING_SEARCHID,
    -- C.ADID                                                             AS 
                                                                          LANDING_ADID,
    -- C.BANNERPOSITION                                                   AS 
                                                                          LANDING_BANNERPOSITION,
    -- C.ADTYPE                                                           AS 
                                                                          LANDING_ADTYPE,
    -- C.HISTCTR                                                          AS 
                                                                          LANDING_HISTCTR,
    -- =============================================================== == =====================
    -- C.ISCLICK                                                          AS 
                                                                          CLICK,
    -- =============================================================== == =====================
    CASE WHEN A.LOCATIONID!=H.LOCATIONID THEN 1 ELSE 0 END             AS TRANSGEOGRAPHIC_TARGETTING_AD,
    CASE WHEN A.CATEGORYID!=H.CATEGORYID THEN 1 ELSE 0 END             AS TRANSCATEGORICAL_TARGETTING_AD,
    -- =============================================================== == =====================
    A.ADID                                                             AS AD_ADID,
    A.LOCATIONID                                                       AS AD_LOCATIONIDAD,
    A.CATEGORYID                                                       AS AD_CATEGORYID,
    A.PRICE                                                            AS AD_PRICE,      
    A.ISCONTEXT                                                        AS AD_ISCONTEXT,
    -- =============================================================== == =====================
    A.ADPARAMS_NUM                                                     AS AD_ADPARAMS_NUM,
    A.ADPARAMS_SUM                                                     AS AD_ADPARAMS_SUM, 
    A.ADPARAMS_LEN                                                     AS AD_ADPARAMS_LEN,  
    A.ADPARAMS_NGRAM                                                   AS ADPARAMS_NGRAM,
    A.AD_SIZE                                                          AS AD_SIZE,            
    A.AD_WLEN                                                          AS AD_WLEN,             
    A.AD_TOPK                                                          AS AD_TOPKEYPHRASE,
    -- =============================================================== == =====================
    A.CATLEVELID                                                       AS ADCAT_LEVELID,
    A.PARENTCATEGORYID                                                 AS ADCAT_PARENTCATID,
    A.SUBCATEGORYID                                                    AS ADCAT_SUBCATID,
    A.LOCLEVELID                                                       AS ADLOC_LEVELID,
    A.REGIONID                                                         AS ADLOC_REGIONID,
    A.CITYID                                                           AS ADLOC_CITYID,
    A.CLICK_HISTORY_ADID                                               AS ADSTATS_CLICK_HISTORY,
    -- =============================================================== == =====================
    A.AD_SEARCH_VOLUME                                                 AS ADSTATS_SEARCH_VOLUME,
    A.AD_USERVIEWS                                                     AS ADSTATS_USERVIEWS,     
    A.AD_CLICK_VOLUME                                                  AS ADSTATS_CLICK_VOLUME,   
    A.AD_USER_REACH                                                    AS ADSTATS_USER_REACH,      
    A.AD_AVG_USR_TARGETING_EMPHASIS                                    AS ADSTATS_TARGERTING_EMPHASIS,
    A.AD_OVERALL_SEARCH_CLICKPROB                                      AS ADSTATS_SEARCH_CLICKPROB, 
    A.AD_OVERALL_USR_CLICKPROB                                         AS ADSTATS_USR_CLICKPROB,     
    -- =============================================================== == =====================
    A.IMPRESSION_ADID                                                  AS IMPRESSION_ADID,
    A.IMPRESSION_NUM_DIFF_USERS                                        AS IMPRESSION_NUM_TARGETED_USERS,
    A.IMPRESSION_NUM_TOTAL_USERS                                       AS IMPRESSION_NUM_VIEWS,         
    A.IMPRESSION_AD2USR_DIVERSITY                                      AS IMPRESSION_VIEWS_TO_USER_RATIO,
    -- =============================================================== == =====================
    U.USERID                                                           AS USER_USERID,
    U.USERAGENTID                                                      AS USER_AGENTID,
    U.USERAGENTOSID                                                    AS USER_AGENTOSID,
    U.USERAGENTFAMILYID                                                AS USER_AGENTFAMILYID,
    U.USERDEVICEID                                                     AS USER_DEVICEID,
    -- =============================================================== == =====================
    U.CLICK_HISTORY_USERID                                             AS USTATS_USERID,
    U.USER_SEARCH_VOLUME                                               AS USTATS_SEARCH_VOLUME,     
    U.USER_IMPRESSION_VOLUME                                           AS USTATS_IMPRESSION_VOLUME,  
    U.USER_CLICK_VOLUME                                                AS USTATS_CLICK_VOLUME,        
    -- =============================================================== == =====================
    U.USER_NUM_DIFFERENT_ADS                                           AS USTATS_NUM_DIFFERENT_ADS,    
    U.USER_AVG_AD_TARGETING_EMPHASIS                                   AS USTATS_IMPRESSION_TO_AD_RATIO,
    U.USER_OVERALL_SEARCH_CLICKPROB                                    AS USTATS_SEARCH_CLICKPROB,     
    U.USER_OVERALL_AD_CLICKPROB                                        AS USTATS_AD_CLICKPROB,          
    U.USER_LOCTR_CLICKDENSITY                                          AS USTATS_TENDENCY_TO_LOWCTR_ADS, 
    U.USER_HICTR_CLICKDENSITY                                          AS USTATS_TENDENCY_TO_HIGHCTR_ADS, 
    -- =============================================================== == =====================
    EXTRACT(EPOCH FROM U.USER_TOTAL_TIMELAPSE)/(3600*24)               AS USTATS_SEARCH_DAYSPAN,         
    EXTRACT(EPOCH FROM U.USER_LAST_SEARCHED)/(3600*24)                 AS USTATS_DAYS_SINCE_LAST_SEARCHED,
    EXTRACT(EPOCH FROM U.USER_LAST_CLICKED)/(3600*24)                  AS USTATS_DAYS_SINCE_LAST_CLICKED,  
    -- =============================================================== == =====================
    CASE WHEN U.PHREQ_USERID!=U.USERID THEN 0 ELSE 1 END               AS PHONEREQ_PSEUDOBIT,
    U.PHREQ_USERID                                                     AS PHREQ_USERID,
    U.PHREQ_TOTALREQ                                                   AS PHREQ_TOTALREQS,            
    U.PHREQ_DEVICE_MOBILITY                                            AS PHREQ_NUM_DEVICES_USED,      
    U.PHREQ_DIFFERENT_ADS                                              AS PHREQ_NUM_DIFFERENT_ADS,      
    U.PHREQ_PER_AD_VOLUME                                              AS PHREQ_REQ_TO_AD_RATIO,         
    -- =============================================================== == =====================
    EXTRACT(EPOCH FROM U.PHREQ_TIMESPAN)/(3600*24)                     AS PHREQ_DAYSPAN,                  
    EXTRACT(EPOCH FROM U.PHREQ_HRS_SINCE_LAST)/(3600*24)               AS PHREQ_DAYS_SINCE_LAST,           
    -- =============================================================== == =====================
    CASE WHEN A.AD_CLICK_VOLUME > 1 AND U.USER_CLICK_VOLUME > 1
            THEN 1 ELSE 0 END                                          AS H_CLICK_HTEST0,
    CASE WHEN A.IMPRESSION_AD2USR_DIVERSITY = 1 AND U.PHREQ_TOTALREQ > 1
            THEN 1 ELSE 0 END                                          AS H_CLICK_HTEST1,
    CASE WHEN A.IMPRESSION_AD2USR_DIVERSITY = 1 AND
              A.AD_CLICK_VOLUME < 1 AND U.USER_CLICK_VOLUME > 1
            THEN 1 ELSE 0 END                                          AS H_CLICK_HTEST2,
    -- =============================================================== == =====================
    R.PHONEREQUESTDATE                                                 AS PHREQ_DATE,
    CASE WHEN R.PHONEREQUESTDATE IS NOT NULL THEN 1 ELSE 0 END         AS PHREQ_TRUEBIT
    -- =============================================================== == =====================
    FROM
        -- (SELECT * FROM AVITO_TRAIN_SEARCHCLICKSTREAM LIMIT 1000000) C
        -- LEFT JOIN (SELECT * FROM AVITO_SEARCH_HISTORY WHERE MONTHNUM=5 AND DAYNUM BETWEEN 1 AND 7) H ON H.SEARCHID = C.SearchID
        (SELECT * FROM AVITO_SEARCH_CLICKS LIMIT 10000000) H
        -- =========================================================== == =====================
        LEFT JOIN AVITO_AD_STATS_PROFILE A
          -- ON C.ADID = A.ADID
          ON H.LANDING_ADID = A.ADID
        -- =========================================================== == =====================
        LEFT JOIN AVITO_USER_STATS_PROFILE U
          ON H.USERID=U.CLICK_HISTORY_USERID
        -- =========================================================== == =====================
        LEFT JOIN AVITO_PHONE_REQUESTS R
          ON H.USERID=R.USERID AND A.ADID = R.ADID
        -- =========================================================== == =====================
        ORDER BY H.SEARCHID, H.LANDING_ADID);
-- ===============================================================================================================

-- ===============================================================================================================
-- CREATE INDEX IDX_AVITO_TRAINSTREAM ON AVITO_TRAINSTREAM(SEARCH_USERID);
-- CREATE INDEX IDX_AVITO_TRAINSTREAM_BY_DAYNUM ON AVITO_TRAINSTREAM(SEARCH_DAYNUM);
-- CREATE INDEX IDX_AVITO_TRAINSTREAM_BY_ADID ON AVITO_TRAINSTREAM(LANDING_ADID);
-- ===============================================================================================================

-- ===============================================================================================================
COPY (SELECT * FROM AVITO_TRAINSTREAM WHERE CLICK = '1' ) TO '/tmp/avito_clicks1.csv' DELIMITER '|'  CSV HEADER;
COPY (SELECT * FROM AVITO_TRAINSTREAM WHERE CLICK = '0' OR CLICK IS NULL) TO '/tmp/avito_clicks0.csv' DELIMITER '|'  CSV HEADER;
COPY (SELECT * FROM AVITO_TRAINSTREAM) TO '/tmp/avito_train_clicks.csv' DELIMITER '|'  CSV HEADER;
-- ===============================================================================================================








-- ===============================================================================================================
-- THE DESIGN MATRIX -- time date pivot is PLAIN WRONG EVERYTHING needs to be recomputed
-- ===============================================================================================================
DROP TABLE IF EXISTS AVITO_TESTSTREAM;
CREATE TABLE AVITO_TESTSTREAM AS (
  SELECT 
    H.SEARCHID                                                         AS SEARCH_SEARCHID,
    H.USERID                                                           AS SEARCH_USERID,
    H.LOCATIONID                                                       AS SEARCH_LOCATIONID,
    H.CATEGORYID                                                       AS SEARCH_CATEGORYID,
    H.IPID                                                             AS SEARCH_IPID,
    H.SEARCHDATE                                                       AS SEARCH_DATE,
    -- =============================================================== == =====================
    EXTRACT(DOW FROM H.SEARCHDATE)                                     AS SEARCH_DOW,
    EXTRACT(HOUR FROM H.SEARCHDATE)                                    AS SEARCH_HOUR,
    EXTRACT(EPOCH FROM 
           ('2015-05-08 00:00:00'::timestamp -H.SEARCHDATE))/(3600*24) AS SEARCH_DAYS_SINCE,
    -- =============================================================== == =====================
    H.ISUSERLOGGEDON                                                   AS SEARCH_ISUSERLOGGEDON,
    H.PARAM_NGRAM                                                      AS SEARCH_PARAM_NGRAM,
    H.PARAM_SIZE                                                       AS SEARCH_PARAM_SIZE,
    H.PARAM_NUM                                                        AS SEARCH_PARAM_NUM,
    H.MONTHNUM                                                         AS SEARCH_MONTHNUM,
    H.DAYNUM                                                           AS SEARCH_DAYNUM,
    H.QRY_SIZE                                                         AS SEARCH_QRY_SIZE,
    H.QRY_WLEN                                                         AS SEARCH_QRY_LEN,
    H.QRY_ENGL                                                         AS SEARCH_QRY_ENGL,
    -- =============================================================== == =====================
    C.SEARCHID                                                         AS LANDING_SEARCHID,
    C.ADID                                                             AS LANDING_ADID,
    C.BANNERPOSITION                                                   AS LANDING_BANNERPOSITION,
    C.ADTYPE                                                           AS LANDING_ADTYPE,
    C.HISTCTR                                                          AS LANDING_HISTCTR,
    -- =============================================================== == =====================
    -- C.ISCLICK                                                       AS CLICK,
    C.ID                                                               AS ID,
    -- =============================================================== == =====================
    CASE WHEN A.LOCATIONID!=H.LOCATIONID THEN 1 ELSE 0 END             AS TRANSGEOGRAPHIC_TARGETTING_AD,
    CASE WHEN A.CATEGORYID!=H.CATEGORYID THEN 1 ELSE 0 END             AS TRANSCATEGORICAL_TARGETTING_AD,
    -- =============================================================== == =====================
    A.ADID                                                             AS AD_ADID,
    A.LOCATIONID                                                       AS AD_LOCATIONIDAD,
    A.CATEGORYID                                                       AS AD_CATEGORYID,
    A.PRICE                                                            AS AD_PRICE,      
    A.ISCONTEXT                                                        AS AD_ISCONTEXT,
    -- =============================================================== == =====================
    A.ADPARAMS_NUM                                                     AS AD_ADPARAMS_NUM,
    A.ADPARAMS_SUM                                                     AS AD_ADPARAMS_SUM, 
    A.ADPARAMS_LEN                                                     AS AD_ADPARAMS_LEN,  
    A.ADPARAMS_NGRAM                                                   AS ADPARAMS_NGRAM,
    A.AD_SIZE                                                          AS AD_SIZE,            
    A.AD_WLEN                                                          AS AD_WLEN,             
    A.AD_TOPK                                                          AS AD_TOPKEYPHRASE,
    -- =============================================================== == =====================
    A.CATLEVELID                                                       AS ADCAT_LEVELID,
    A.PARENTCATEGORYID                                                 AS ADCAT_PARENTCATID,
    A.SUBCATEGORYID                                                    AS ADCAT_SUBCATID,
    A.LOCLEVELID                                                       AS ADLOC_LEVELID,
    A.REGIONID                                                         AS ADLOC_REGIONID,
    A.CITYID                                                           AS ADLOC_CITYID,
    A.CLICK_HISTORY_ADID                                               AS ADSTATS_CLICK_HISTORY,
    -- =============================================================== == =====================
    A.AD_SEARCH_VOLUME                                                 AS ADSTATS_SEARCH_VOLUME,
    A.AD_USERVIEWS                                                     AS ADSTATS_USERVIEWS,     
    A.AD_CLICK_VOLUME                                                  AS ADSTATS_CLICK_VOLUME,   
    A.AD_USER_REACH                                                    AS ADSTATS_USER_REACH,      
    A.AD_AVG_USR_TARGETING_EMPHASIS                                    AS ADSTATS_TARGERTING_EMPHASIS,
    A.AD_OVERALL_SEARCH_CLICKPROB                                      AS ADSTATS_SEARCH_CLICKPROB, 
    A.AD_OVERALL_USR_CLICKPROB                                         AS ADSTATS_USR_CLICKPROB,     
    -- =============================================================== == =====================
    A.IMPRESSION_ADID                                                  AS IMPRESSION_ADID,
    A.IMPRESSION_NUM_DIFF_USERS                                        AS IMPRESSION_NUM_TARGETED_USERS,
    A.IMPRESSION_NUM_TOTAL_USERS                                       AS IMPRESSION_NUM_VIEWS,         
    A.IMPRESSION_AD2USR_DIVERSITY                                      AS IMPRESSION_VIEWS_TO_USER_RATIO,
    -- =============================================================== == =====================
    U.USERID                                                           AS USER_USERID,
    U.USERAGENTID                                                      AS USER_AGENTID,
    U.USERAGENTOSID                                                    AS USER_AGENTOSID,
    U.USERAGENTFAMILYID                                                AS USER_AGENTFAMILYID,
    U.USERDEVICEID                                                     AS USER_DEVICEID,
    -- =============================================================== == =====================
    U.CLICK_HISTORY_USERID                                             AS USTATS_USERID,
    U.USER_SEARCH_VOLUME                                               AS USTATS_SEARCH_VOLUME,     
    U.USER_IMPRESSION_VOLUME                                           AS USTATS_IMPRESSION_VOLUME,  
    U.USER_CLICK_VOLUME                                                AS USTATS_CLICK_VOLUME,        
    -- =============================================================== == =====================
    U.USER_NUM_DIFFERENT_ADS                                           AS USTATS_NUM_DIFFERENT_ADS,    
    U.USER_AVG_AD_TARGETING_EMPHASIS                                   AS USTATS_IMPRESSION_TO_AD_RATIO,
    U.USER_OVERALL_SEARCH_CLICKPROB                                    AS USTATS_SEARCH_CLICKPROB,     
    U.USER_OVERALL_AD_CLICKPROB                                        AS USTATS_AD_CLICKPROB,          
    U.USER_LOCTR_CLICKDENSITY                                          AS USTATS_TENDENCY_TO_LOWCTR_ADS, 
    U.USER_HICTR_CLICKDENSITY                                          AS USTATS_TENDENCY_TO_HIGHCTR_ADS, 
    -- =============================================================== == =====================
    EXTRACT(EPOCH FROM U.USER_TOTAL_TIMELAPSE)/(3600*24)               AS USTATS_SEARCH_DAYSPAN,         
    EXTRACT(EPOCH FROM U.USER_LAST_SEARCHED)/(3600*24)                 AS USTATS_DAYS_SINCE_LAST_SEARCHED,
    EXTRACT(EPOCH FROM U.USER_LAST_CLICKED)/(3600*24)                  AS USTATS_DAYS_SINCE_LAST_CLICKED,  
    -- =============================================================== == =====================
    CASE WHEN U.PHREQ_USERID!=U.USERID THEN 0 ELSE 1 END               AS PHONEREQ_PSEUDOBIT,
    U.PHREQ_USERID                                                     AS PHREQ_USERID,
    U.PHREQ_TOTALREQ                                                   AS PHREQ_TOTALREQS,            
    U.PHREQ_DEVICE_MOBILITY                                            AS PHREQ_NUM_DEVICES_USED,      
    U.PHREQ_DIFFERENT_ADS                                              AS PHREQ_NUM_DIFFERENT_ADS,      
    U.PHREQ_PER_AD_VOLUME                                              AS PHREQ_REQ_TO_AD_RATIO,         
    -- =============================================================== == =====================
    EXTRACT(EPOCH FROM U.PHREQ_TIMESPAN)/(3600*24)                     AS PHREQ_DAYSPAN,                  
    EXTRACT(EPOCH FROM U.PHREQ_HRS_SINCE_LAST)/(3600*24)               AS PHREQ_DAYS_SINCE_LAST,           
    -- =============================================================== == =====================
    CASE WHEN A.AD_CLICK_VOLUME > 1 AND U.USER_CLICK_VOLUME > 1
            THEN 1 ELSE 0 END                                          AS H_CLICK_HTEST0,
    CASE WHEN A.IMPRESSION_AD2USR_DIVERSITY = 1 AND U.PHREQ_TOTALREQ > 1
            THEN 1 ELSE 0 END                                          AS H_CLICK_HTEST1,
    CASE WHEN A.IMPRESSION_AD2USR_DIVERSITY = 1 AND
              A.AD_CLICK_VOLUME < 1 AND U.USER_CLICK_VOLUME > 1
            THEN 1 ELSE 0 END                                          AS H_CLICK_HTEST2,
    -- =============================================================== == =====================
    R.PHONEREQUESTDATE                                                 AS PHREQ_DATE,
    CASE WHEN R.PHONEREQUESTDATE IS NOT NULL THEN 1 ELSE 0 END         AS PHREQ_TRUEBIT
    -- =============================================================== == =====================
    FROM AVITO_TEST_SEARCHCLICKSTREAM C
        LEFT JOIN AVITO_SEARCH_HISTORY H
          ON H.SEARCHID = C.SearchID
        -- =========================================================== == =====================
        LEFT JOIN AVITO_AD_STATS_PROFILE A
          ON C.ADID = A.ADID
        -- =========================================================== == =====================
        LEFT JOIN AVITO_USER_STATS_PROFILE U
          ON H.USERID=U.CLICK_HISTORY_USERID
        -- =========================================================== == =====================
        LEFT JOIN AVITO_PHONE_REQUESTS R
          ON H.USERID=R.USERID AND A.ADID = R.ADID
        -- =========================================================== == =====================
        ORDER BY C.ID, H.SEARCHID, C.ADID);
-- ===============================================================================================================

-- ===============================================================================================================
CREATE INDEX IDX_AVITO_TESTSTREAM_BY_ID ON AVITO_TESTSTREAM(ID);
CREATE INDEX IDX_AVITO_TESTSTREAM_BY_ADID ON AVITO_TESTSTREAM(LANDING_ADID);
CREATE INDEX IDX_AVITO_TESTSTREAM_BY_SEARCHID ON AVITO_TESTSTREAM(SEARCH_USERID);
-- ===============================================================================================================


-- ===============================================================================================================
COPY (SELECT * FROM AVITO_TESTSTREAM ORDER BY ID) TO '/tmp/avito_test_clicks.csv' DELIMITER '|'  CSV HEADER;
-- ===============================================================================================================
