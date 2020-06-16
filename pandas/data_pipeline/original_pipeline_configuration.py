import time

# #########################################################################
# CLASSIFIERS
# #########################################################################
NAMES = ['BasicKNN',                      # 0
         'BasicSVC',                      # 1
         'BasicGaussianNB',               # 2
         'BasicMultinomialNB',            # 3
         'BasicLinearSVC',                # 4
         'BasicRadial',                   # 5
         'BasicLDA',                      # 6
         'BasicExtraTrees',               # 7
         'BasicQDA',                      # 8
         'BasicSGD',                      # 9
         'BasicAdaBoost',                 # 10
         'BasicXtremeSGDBoost',           # 11
         'BasicDecisionTree',             # 12
         'BasicGradientBoost',            # 13
         'BasicDecisionTree_01',          # 14
         'BasicRandomForest',             # 15
         'BasicForest_01',                # 16
         'BasicForest_02',                # 17
         'BasicForest_03',                # 18
         'BasicForest_04',                # 19
         'BasicForest_05',                # 20
         'BasicForest_06',                # 21
         'BasicForest_07',                # 22
         'BasicForest_08',                # 23
         'BasicForest_09',                # 24
         'BasicGradientBoost_01',         # 25
         'BasicGradientBoost_02',         # 26
         'BasicGradientBoost_03',         # 27
         'BasicGradientBoost_04',         # 28
         'BasicGradientBoost_05',         # 29
         'BasicGradientBoost_06',         # 30
         'BasicGradientBoost_07',         # 31
         'BasicGradientBoost_08',         # 32
         'BasicGradientBoost_09',         # 33
         'BasicAdaBoost_01'               # 34
         ]
# #########################################################################
APPLY_THESE = [10, 13, 14, 15] + [25, ]
# #########################################################################


# #########################################################################
# PARAMETERS
# #########################################################################
DATAFILES = {'MERGED': 'DATA/merged.csv',
             'TRAIN': 'DATA/train.csv',
             'TEST': 'DATA/test.csv'}
# ---------------------------------------------------------------------------
TEST_RUN = False
TEST_RUN = True
# ---------------------------------------------------------------------------
DATASET, SEPARATOR = ['MERGED', 'TRAIN', 'TEST'][0], '|'
# ---------------------------------------------------------------------------
TARGET_VAR, LABEL_VAR, ID_VAR = 'label', 'xxx', 'Id'
TARGET_COVAR = 'xxx'
# ---------------------------------------------------------------------------
FIRST_NROWS = 80000
if TEST_RUN:
    FIRST_NROWS = 43000
# ---------------------------------------------------------------------------
MULTICLASS = True
MULTICLASS_AUGMENT_BY = 0.10
# ---------------------------------------------------------------------------
CLASS0_TRAINWITH, CLASS1_TRAINWITH = -2, 2
CLASS0, CLASS1, TESTING_SET = 0, 1, -1
# ---------------------------------------------------------------------------
if MULTICLASS:
    TESTING_SET = 0
# ---------------------------------------------------------------------------
DESIGN_MATRIX_FILENAME = 'numerical_data.dat'
SAVE_COMPUTED_DESIGN_MATRIX = False
LOAD_COMPUTED_DESIGN_MATRIX = False
# ---------------------------------------------------------------------------
WRITE_CSV_TO = "yp_%s.csv" % time.ctime()[0:13].replace(' ', '_')
WRITE_CSV_TO = "yp_Mon_09pm.csv"
print("Will write predictive output to: %s" % WRITE_CSV_TO)
# ---------------------------------------------------------------------------
WRITE_MODEL = False
# ---------------------------------------------------------------------------
RESAMPLE_XY = True         # cv avgs WILL be meaningless but test avgs be ok
RESAMPLE_XY = False
# ---------------------------------------------------------------------------
RETRAIN_PCLF = False
# ---------------------------------------------------------------------------
USE_INCREMENTAL_READ = False
RANDOM_COLSET_AUGMENTATION = 512
USE_PRESELECTED_FEATURES = False
# ---------------------------------------------------------------------------
ABS_MIN_CHUNK_STEPSIZE = 10
INCREMENTAL_READ_MINSCORE = 10
INCREMENTAL_READ_BLOCKSIZE = 90
INCREMENTAL_READ_FSELPERC = 0.75
CHUNKING_FEATGEN_STEPSIZE = INCREMENTAL_READ_BLOCKSIZE/3
# ---------------------------------------------------------------------------
TRANSFORMS = ['', 'sqrt', '1/x', 'rank', 'log', '1/rank', 'x2', 'exp', '-exp',
              '1/sqrt', '1/log', 'x/log', 'x*log', 's/rank', 'x*rank', 'power',
              's/log']
# ---------------------------------------------------------------------------
L_STEP = 1
LEVELS = sorted(set([float(x)/200. for x in range(-600, 0, L_STEP)] +
                    [float(x)/200. for x in range(0, 600, L_STEP)] +
                    [-20, -10, -7, -5, -4, 4, 5, 7, 10, 20]))
# ---------------------------------------------------------------------------
NA_LARGE_VALS = [9999999999, ]            # range(999999990, 999999999+1)
NA_LARGE_VALS = dict(list(zip(NA_LARGE_VALS, NA_LARGE_VALS)))
# ---------------------------------------------------------------------------
NALIST = ["-", "N/A", "NA", " ", "?", "", -99999]
NALIST = NALIST + [x for x in NA_LARGE_VALS]
# ---------------------------------------------------------------------------
NA_RECODE = -1.0
# ---------------------------------------------------------------------------
VARIABLE_TRAIN_EFFORT = False
# ---------------------------------------------------------------------------
MAX_RANK_AUGMENTATION = 100
BYPASS_RANK_AUGMENTATION = True
# ---------------------------------------------------------------------------
NORMALTEST_MINLEVELS = 5
BYPASS_NORMAL_CHECKS = True
# ---------------------------------------------------------------------------
PCA_NCOMPONENTS = {'method': 'svc', 'n': 400}
PCA_NCOMPONENTS = {'method': '', 'n': 400}
DEFAULT_REGULARIZATION = 1./96.
# ---------------------------------------------------------------------------
NUM_ICA_DIMS = 4
# ---------------------------------------------------------------------------
STEPPER_MAX_COMPUTE_QUANTUM = 900
STEPPER_SAMPLING_EFFORT = 0.0
STEPPER_DEFAULT_DIRECTION = "forward"
STEPPER_NUM_ESTIMATORS = 16
STEPPER_EPSILON = 1e-5
STEPPER_DEPTH = 12
# ---------------------------------------------------------------------------
FACTORSELECTOR_VMAX = 256
FACTORSELECTOR_FMAX = 0.95
FACTORSELECTOR_MAXTIME = 900
FACTORSELECTOR_CATEGORICAL_LIMIT = 128
FACTORSELECTOR_IMPT_THRESHOLD = 1e-5
# ---------------------------------------------------------------------------
BYPASS_DATA_AUGMENTATION = False
BYPASS_INITIAL_NUMERICAL_CUTS = False
BYPASS_FEATURE_IMPORTANCES_REDUCTION = False
BYPASS_INCREMENTAL_STEP_FITTER_DATAREADY = False
BYPASS_INCREMENTAL_STEP_FITTER_FACTORSELECTOR = False
# ---------------------------------------------------------------------------
BYPASS_ICA_PROJECTIONS = True
BYPASS_LOCAL_PROJECTIONS = False
BYPASS_GLOBAL_PROJECTIONS = True
# ---------------------------------------------------------------------------
BYPASS_APPLY_PCA = True
BYPASS_PCA_SCALER = True
BYPASS_VECTOR_NORMALIZATION = False
BYPASS_PROBABILITY_CORRECTION = False
# ---------------------------------------------------------------------------
BYPASS_SPECIAL_FGENS = False
BYPASS_HSVAR_FEATURES = True
BYPASS_PAIRED_FEATURES = True
# ---------------------------------------------------------------------------
BYPASS_XTERM_SQUARING = True
BYPASS_INTERACTION_TERMS = False
BYPASS_HIGHER_INTERACTION_TERMS = True
# ---------------------------------------------------------------------------
BYPASS_SQUARING_AFTERTOP = 40
BYPASS_INTERACTIONS_AFTERTOP = 40
EXAMINE_HIGHER_TERMS_UPTO = 5
# ---------------------------------------------------------------------------
BYPASS_ADD_CENTROIDS = False
BYPASS_ADD_MARGINALS = False
BYPASS_ADD_IMPT_FACTOR_LEVELS = False
BYPASS_ADD_CATEGORICAL_FREQUENCIES = False
BYPASS_ADD_TIME_DIFFERENTIALS = True
# ---------------------------------------------------------------------------
BYPASS_APPLY_FACTOR_CONDITIONING = True
BYPASS_APPLY_BASIC_FEATURE_ENCODING = True
BYPASS_APPLY_BASIC_FEATURE_GENERATION = True
BYPASS_EARLY_REDUCE_LARGE_FACTORS = False
USE_RECIPROCAL_CODING_FOR_CATEGORIES = False
# ---------------------------------------------------------------------------
ENCODE_CATEGORICALS = True
ENCODE_NUMERICALS = False
NUMERICAL_DATATYPE = 'float32'
CODE_CATEGORIES_VIA = "counts"
CODE_MARGINALS_VIA = "counts"
# ---------------------------------------------------------------------------
NUMERICAL_AS_CATEGORY_L_LIMIT = 0
DROP_VARS_WITH_LESS_THAN_NLEVELS = 2
# ---------------------------------------------------------------------------
MAX_NUM_FACTOR_LEVELS = int(1024*2.0000)
MAX_NUM_FACTOR_LEVELS *= (2 - BYPASS_HIGHER_INTERACTION_TERMS)
UPPER_FACTOR_LEVEL_LIMIT = int(MAX_NUM_FACTOR_LEVELS * 1.25)
# ---------------------------------------------------------------------------
DROP_XTERMS_WITH_NLEVELS = min(MAX_NUM_FACTOR_LEVELS, 8192)
# ---------------------------------------------------------------------------
ZSCORE_Q = 8192
ZSCORE_COL_STEP_F = 4
ZSCORE_COL_STEP_G = ZSCORE_COL_STEP_F + 3
ADD_ZSCORES_F = False
ADD_ZSCORES_G = False
# ---------------------------------------------------------------------------
DISPLAY_NLEVELS = 3
# ---------------------------------------------------------------------------
NORM_RECODING_DIGITS = 3
ZVALS_RECODING_DIGITS = 1
ZSCORE_RECODING_DIGITS = 3
# ---------------------------------------------------------------------------
DEV_SPLIT = 0.15
PERCENT_OF_DATA_TO_USE = 100./100.
PERCENT_OF_SAMPLES_TO_TRAIN_WITH = 65./100.
# ---------------------------------------------------------------------------
CENTROID_MAX_COMPUTE_TIME = 600
CENTROID_SAMPLING_FRACTION = 0.15
CENTROID_SAMPLING_EFFORT = PERCENT_OF_SAMPLES_TO_TRAIN_WITH/64.0
NC1_CLUSTERS = 8
NC0_CLUSTERS = 8
MAX_SAMPLES_TO_CLUSTER = 128
PERCLASS_RANDOM_CENTROIDS = 2*MAX_SAMPLES_TO_CLUSTER/(NC0_CLUSTERS+NC1_CLUSTERS)
MIN_VALID_CLUSTER_SIZE = 12
# ---------------------------------------------------------------------------
DO_CLASS_PREBALANCING = False
# ---------------------------------------------------------------------------
C1_RATIO = 100./100.
C0_RATIO = 100./100.
C1_OVERSAMPLING = 1.00
C0_OVERSAMPLING = 1.00
C1_WITH_REPLACEMENT = False
C0_WITH_REPLACEMENT = False
# ---------------------------------------------------------------------------
MIDPOINT_BIAS = 0.50
# ---------------------------------------------------------------------------
GLOBAL_NOT_LOCAL_DECORRELATION = True
# ---------------------------------------------------------------------------
BYPASS_DECORRELATION_ON_NTOP = 5
LOCAL_DECORRELATION_EFFORT = 0
# ---------------------------------------------------------------------------
LOCAL_RSQRD_THRESHOLD = 0.995
GLOBAL_RSQRD_THRESHOLD = 0.95
GLOBAL_RSQRD_THRESHOLD = 0.99
# ---------------------------------------------------------------------------
MINIMUM_DECORRELATION_LEVEL = 30
# ---------------------------------------------------------------------------
BYPASS_STARTUP_DECORRELATION = True
BYPASS_EARLY_DECORRELATION = True
BYPASS_INTERMEDIARY_DECORRELATION = False
BYPASS_GLOBAL_DECORRELATION = False
# ---------------------------------------------------------------------------
PARTIAL_AND_INCREMENTAL = True
# ---------------------------------------------------------------------------
BYPASS_ORIGVAR_FSELECT = False
BYPASS_XFREQS_FSELECT = False
BYPASS_XTERM_FSELECT = False
BYPASS_BOOTSTRAP_FSELECT = False
BYPASS_INITIAL_FSELECT = False
BYPASS_CENTROID_FSELECT = False
# ---------------------------------------------------------------------------
FS_PERCENTAGES = {'origvars':        100./100.,
                  'xterms':          100./100.,
                  'xfreqs':          100./100.,
                  'marginals':       100./100.,
                  'bootstrap':       100./100.,
                  'incremental':     100./100.,
                  'initial':         100./100.,
                  'coarse':          100./100.,
                  'nongauss':        100./100.,
                  'rank':            100./100.,
                  'centroids':       100./100.,
                  'final':           100./100.}
# ---------------------------------------------------------------------------
FS_STEPS = {'origvars':              8192,
            'xterms':                8192,
            'xfreqs':                8192,
            'marginals':             8192,
            'bootstrap':             8192,
            'incremental':           8192,
            'initial':               8192,
            'coarse':                8192,
            'nongauss':              8192,
            'rank':                  8192,
            'centroids':             8192,
            'final':                 8192}
# ---------------------------------------------------------------------------
FSELECT_MINSCORE = 100
# ---------------------------------------------------------------------------
FS_MINSCORES = {'origvars':           0.0,
                'xterms':             max(FSELECT_MINSCORE, 500),
                'xfreqs':             max(FSELECT_MINSCORE, 500),
                'bootstrap':          min(FSELECT_MINSCORE, 100),
                'marginals':          max(FSELECT_MINSCORE, 500),
                'incremental':        min(FSELECT_MINSCORE, 1000),
                'initial':            min(FSELECT_MINSCORE, 1000),
                'coarse':             min(FSELECT_MINSCORE, 1000),
                'nongauss':           min(FSELECT_MINSCORE, 1000),
                'rank':               min(FSELECT_MINSCORE, 1000),
                'centroids':          min(FSELECT_MINSCORE, 1000),
                'final':              min(FSELECT_MINSCORE, 1000)}
# #########################################################################


# #########################################################################
CHANGES = """ apr-07: matching apr-06;
              apr-08: matching apr-07: 0.4501
              apr-20: major corrections to sampling
              apr-21: refined independent from bnp, with 0.835
              apr-28: added centroids, corrected sampling
              may-05: retrofit wrt springleaf classifier
              may-08: refactored
              may-11: sync'ed
              """
# #########################################################################
