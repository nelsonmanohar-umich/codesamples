#!/usr/bin/python
# -*- coding: utf-8 -*-


# #########################################################################
from definitions import DATASET_NAME
__doc__ = "builds classifier comparison/ensemble for %s dataset" % DATASET_NAME
__license__ = "Education use only. Absolutely no warranty nor commercial use"
__copyright__ = "Nelson R. Manohar (c) 2015, 2016"
__author__ = "Nelson R. Manohar"
__date__ = "Mar2016"

print(__doc__)
print(__license__)
print(__copyright__)
print(__author__)
print(__date__)
# #########################################################################


# #########################################################################
import numpy
numpy.set_printoptions(precision=3)
# -------------------------------------------------------------------------
import pandas as pd
pd.set_option('display.expand_frame_repr', True)
pd.set_option('display.max_columns', 50)
pd.set_option('display.max_rows', 50)
pd.set_option('display.precision', 1)
pd.set_option('display.width', 190)
# #########################################################################


# #########################################################################
INTERNAL_LOOP_MAX_COMPUTE_QUANTUM = 1800
USE_DEPRECATED_CODE = False
# #########################################################################


# #########################################################################
import sys
import time
import pickle
import traceback
# ---------------------------------------------------------------------------
from collections import defaultdict
import pipeline_configuration
import definitions
from definitions import augment_data
# -------------------------------------------------------------------------
from feature_selected_vars import SELECTED
from feature_selected_vars import DROPPED
# -------------------------------------------------------------------------
from factor_selector import derive_impt_factorlevel_features_from
from step_fitter import step_fitter
# -------------------------------------------------------------------------
from pipeline_functions import train_cv_split_idx
from pipeline_functions import configure_classifier
from pipeline_functions import decision_ll
from pipeline_functions import decision_sum
from pipeline_functions import clf_predict_wrt
from pipeline_functions import clf_predict
from pipeline_functions import compute_metrics_for
from pipeline_functions import build_probability_correction_model
from pipeline_functions import validate_vars
from pipeline_functions import augment_with_per_class_random_centroids
from pipeline_functions import split_dataset_by_classes_into_sets
from pipeline_functions import describe_datasets
from pipeline_functions import preprocess_multiclass_class_labels
from pipeline_functions import add_marginal_variables
from pipeline_functions import read_data
from pipeline_functions import generate_categorical_frequencies
from pipeline_functions import identify_large_factors
from pipeline_functions import process_large_factors
from pipeline_functions import generate_interaction_terms
from pipeline_functions import generate_higher_order_interactions
from pipeline_functions import generate_square_terms
from pipeline_functions import basic_feature_generation_reductionism
from pipeline_functions import basic_feature_encoding_reductionism
from pipeline_functions import generate_time_differentials_from
from pipeline_functions import incremental_feature_selection
from pipeline_functions import add_zscores_columnspan_features
from pipeline_functions import apply_factor_conditioning
from pipeline_functions import numerical_recoding
from pipeline_functions import augment_with_rank_features_for
# -------------------------------------------------------------------------
from pipeline_configuration import APPLY_THESE, NAMES
from pipeline_configuration import ADD_ZSCORES_F
from pipeline_configuration import ADD_ZSCORES_G
from pipeline_configuration import BYPASS_ADD_CATEGORICAL_FREQUENCIES
from pipeline_configuration import BYPASS_ADD_CLASSLABEL_DIFFERENTIATORS
from pipeline_configuration import BYPASS_ADD_CENTROIDS
from pipeline_configuration import BYPASS_ADD_MARGINALS
from pipeline_configuration import BYPASS_ADD_IMPT_FACTOR_LEVELS
from pipeline_configuration import BYPASS_ADD_TIME_DIFFERENTIALS
from pipeline_configuration import BYPASS_APPLY_PCA
from pipeline_configuration import BYPASS_APPLY_FACTOR_CONDITIONING
from pipeline_configuration import BYPASS_APPLY_BASIC_FEATURE_ENCODING
from pipeline_configuration import BYPASS_APPLY_BASIC_FEATURE_GENERATION
from pipeline_configuration import BYPASS_BOOTSTRAP_FSELECT
from pipeline_configuration import BYPASS_INITIAL_FSELECT
from pipeline_configuration import BYPASS_INTERACTION_TERMS
from pipeline_configuration import BYPASS_GLOBAL_PROJECTIONS
from pipeline_configuration import BYPASS_NORMAL_CHECKS
from pipeline_configuration import BYPASS_ORIGVAR_FSELECT
from pipeline_configuration import BYPASS_PCA_SCALER
from pipeline_configuration import BYPASS_RANK_AUGMENTATION
from pipeline_configuration import BYPASS_XFREQS_FSELECT
from pipeline_configuration import BYPASS_XTERM_FSELECT
from pipeline_configuration import BYPASS_XTERM_SQUARING
from pipeline_configuration import BYPASS_STARTUP_DECORRELATION
from pipeline_configuration import BYPASS_DECORRELATION_ON_NTOP
from pipeline_configuration import BYPASS_EARLY_REDUCE_LARGE_FACTORS
from pipeline_configuration import BYPASS_EARLY_DECORRELATION
from pipeline_configuration import BYPASS_INTERMEDIARY_DECORRELATION
from pipeline_configuration import BYPASS_CENTROID_FSELECT
from pipeline_configuration import BYPASS_GLOBAL_DECORRELATION
from pipeline_configuration import BYPASS_DATA_AUGMENTATION
from pipeline_configuration import BYPASS_INITIAL_NUMERICAL_CUTS
from pipeline_configuration import BYPASS_FEATURE_IMPORTANCES_REDUCTION
from pipeline_configuration import BYPASS_INCREMENTAL_STEP_FITTER_DATAREADY
from pipeline_configuration import BYPASS_TRIM_LARGE_CLASSES
from pipeline_configuration import CLASS0
from pipeline_configuration import CLASS1
from pipeline_configuration import DESIGN_MATRIX_FILENAME
from pipeline_configuration import DEV_SPLIT
from pipeline_configuration import DISPLAY_NLEVELS
from pipeline_configuration import DO_CLASS_PREBALANCING
from pipeline_configuration import DROP_VARS_WITH_LESS_THAN_NLEVELS
from pipeline_configuration import ENCODE_CATEGORICALS
from pipeline_configuration import FS_MINSCORES
from pipeline_configuration import FS_PERCENTAGES
from pipeline_configuration import FS_STEPS
from pipeline_configuration import GLOBAL_NOT_LOCAL_DECORRELATION
from pipeline_configuration import ID_VAR
from pipeline_configuration import LABEL_VAR
from pipeline_configuration import LOAD_COMPUTED_DESIGN_MATRIX
from pipeline_configuration import LARGE_CLASSSIZE_THRESHOLD
from pipeline_configuration import LARGE_CLASS_REDUCTION_GOAL
from pipeline_configuration import MULTICLASS_AUGMENT_BY
from pipeline_configuration import MULTICLASS
from pipeline_configuration import NA_RECODE
from pipeline_configuration import NORMALTEST_MINLEVELS
from pipeline_configuration import PARTIAL_AND_INCREMENTAL
from pipeline_configuration import PERCENT_OF_SAMPLES_TO_TRAIN_WITH
from pipeline_configuration import RESAMPLE_XY
from pipeline_configuration import SAVE_COMPUTED_DESIGN_MATRIX
from pipeline_configuration import TARGET_VAR, TARGET_COVAR
from pipeline_configuration import TESTING_SET
from pipeline_configuration import TEST_RUN
from pipeline_configuration import UPPER_FACTOR_LEVEL_LIMIT
from pipeline_configuration import WRITE_CSV_TO
from pipeline_configuration import WRITE_MODEL
from pipeline_configuration import ZSCORE_COL_STEP_F
from pipeline_configuration import ZSCORE_COL_STEP_G
# -------------------------------------------------------------------------
from feature_generators import apply_na_recoder
from feature_generators import apply_PCA
from feature_generators import apply_scaling
from feature_generators import decorrelate_and_project
from feature_generators import feature_select
from feature_generators import feature_select_and_project
from feature_generators import get_codes_from_categorical
from feature_generators import get_projection_features_for
from feature_generators import is_categorical
from feature_generators import is_special_col
from feature_generators import normalize_col
from feature_generators import optimize_factor_cuts
from feature_generators import print_data_shape
from feature_generators import set_original_variables
# -------------------------------------------------------------------------
from dataset_reduction import trim_class_from
from differentiator import add_classlabel_differentiators
# ---------------------------------------------------------------------------
from classifier_configuration import print_classifier_attributes
from classifier_configuration import get_feature_impt_dropset
# -------------------------------------------------------------------------
import multiclass as mcls
# #########################################################################


# #########################################################################
# GLOBALS
# #########################################################################
SELECTED = dict(list(zip(SELECTED, SELECTED)))
DROPPED = dict(list(zip(DROPPED, DROPPED)))

global TIMENOW
TIMENOW = time.time()
STARTUP_TIME = time.time()

global transform_performance
transform_performance = defaultdict(int)

global COLSET, VARSET, DROPSET
COLSET, VARSET, DROPSET = [], [], [TARGET_VAR, ID_VAR, LABEL_VAR]
# #########################################################################


# #########################################################################
PRIMES = [47411, 29771, 11291, 1571, 12851, 38291, 27971, 8051, 27131, 7571,
          37451, 26411, 4331, 16331, 19931, 14171, 9491, 35051, 38771, 44051,
          23291, 731, 7331, 26051, 34331, 36731, 48731, 21251, 42851, 42491,
          27251, 18491, 15611, 40571, 31811, 42251, 11, 39611, 49211, 30971,
          32171, 27731, 48131, 45971, 43211, 13331, 19571, 49571, 12731, 38651,
          46571, 15971, 47771, 23891, 12131, 8891, 19331, 18611, 46931, 44291,
          22931, 27371, 33971, 28331, 31211, 42971, 22091, 35651, 16811, 22811,
          30011, 3251, 11171, 36611, 34571, 18731, 37211, 43811, 9251, 8651,
          33611, 3371, 27851, 44651, 9611, 2171, 19451, 37571, 21611, 31331,
          33011, 27011, 15731, 18011, 6491, 36491, 14291, 46811, 16691, 41771,
          6131, 13211, 19211, 30371, 34811, 26891, 43091, 10211, 15251, 48251,
          47291, 30491, 30611, 20051, 46091, 48611, 20171, 39131, 7451, 33851,
          11891, 2771, 34451, 34691, 12971, 49331, 48491, 10331, 38411, 5891,
          37091, 35411, 491, 41531, 1691, 49091, 31691, 4091, 25811, 37691,
          6611, 41891, 6851, 26291, 35171, 43331, 12491, 9731, 3011, 9371,
          16091, 23411, 5771, 13571, 22211, 35891, 10931, 24731, 10091, 2051,
          6731, 47051, 28811, 17531, 41171, 30731, 6251, 31091, 17891, 14651,
          14411, 4811, 36971, 21971, 32651, 38171, 21131, 6011, 25331, 24611,
          29891, 7091, 21011, 44771, 33131, 44171, 18851, 43691, 26171, 4451,
          8411, 33491, 9011, 21731, 36851, 44891, 23651, 32411, 29651, 5651,
          131, 28571, 24491, 33251, 3131, 10691, 46331, 32051, 28451, 47891,
          16931, 44411, 39371, 39731, 16451, 10811, 26531, 45011, 37811, 17771,
          34931, 36251, 45131, 24371, 35771, 25691, 13091, 8771, 21851, 45731,
          19811, 6371, 25451, 3851, 39251, 24971, 18371, 45371, 7931, 15131,
          44531, 49691, 23171, 971, 851, 28691, 39491, 20651, 16211, 29411,
          3491, 36131, 3611, 13691, 23531, 13451, 7811, 30851, 43931, 22451,
          32531, 611, 26771, 38891, 36371, 40931, 49451, 41411, 25571, 20291,
          45251, 7691, 9971, 46691, 4211, 42131, 46451, 25931, 28211, 10451,
          1331, 27611, 47651, 48371, 12611, 17171, 19091, 24851, 22691, 27491,
          39851, 2891, 9131, 34091, 12011, 12251, 21491, 17411, 48011, 2411,
          38051, 43451, 40691, 18251, 23771, 15371, 29171, 3971, 11651, 15851,
          251, 22571, 4931, 13931, 23051, 43571, 20531, 33731, 17051, 45611,
          46211, 20771, 16571, 14051, 28931, 19691, 20411, 40331, 1091, 49931,
          24251, 17651, 31451, 1811, 15491, 6971, 45491, 371, 11771, 5171,
          9851, 14531, 5531, 41291, 15011, 32291, 31571, 7211, 48971, 11051,
          8291, 48851, 40211, 42611, 22331, 39011, 2651, 1931, 37931, 45851,
          37331, 40451, 30131, 14771, 4691, 35291, 18971, 42011, 42371, 2291,
          5051, 25211, 18131, 11411, 29291, 28091, 1211, 33371, 47531, 14891,
          35531, 36011, 20891, 8531, 32891, 13811, 8171, 24011, 34211, 26651,
          11531, 39971, 41051, 29051, 4571, 3731, 47171, 24131, 2531, 12371,
          1451, 40811, 17291, 10571, 32771, 29531, 31931, 41651, 30251, 42731,
          49811, 5411, 40091, 25091, 21371, 38531, 5291]
# ---------------------------------------------------------------------------
RANDOM_STATE = [x[0]*x[1] for x in zip(PRIMES[12:], reversed(PRIMES))]
RANDOM_STATE = RANDOM_STATE * 100
# #########################################################################


# #########################################################################
print('-' * 80)
print(time.ctime())
print('-' * 80)
if True:
    MODULE = definitions
    FIELDS = [x for x in MODULE.__dict__ if not x.startswith('_')]
    for field in sorted(FIELDS):
        print('-' * 80)
        print("%-40s: %s" % (field, MODULE.__dict__[field]))
    print('-' * 80)

if True:
    MODULE = pipeline_configuration
    FIELDS = [x for x in MODULE.__dict__ if not x.startswith('_')]
    for field in sorted(FIELDS):
        print('-' * 80)
        print("%-40s: %s" % (field, MODULE.__dict__[field]))
    print('-' * 80)
print('-' * 80)
# #########################################################################


# #########################################################################
def get_random_seed(debug=False):
    global RANDOM_STATE
    try:
        q = RANDOM_STATE.pop()
        if q < 0:
            q = None
        if q >= 4294967295:
            q = None
    except:
        q = None
    if debug:
        print("**** NEW RANDOMIZER SEED IN USE: %s" % q)
    return q
# #########################################################################


# #########################################################################
def banner(msg, numlines=3):
    ''' displays a standard banner messsage to console output '''
    global TIMENOW
    print('-' * 80)
    print("\n" * numlines)
    print('-' * 80)
    runtime = (time.time() - STARTUP_TIME)/60.
    delta = time.time() - TIMENOW
    runtime = "%7.1fm" % runtime
    delta = "%8.2fs" % delta
    print("%80s %s %30s %s" % (msg.upper(), delta, time.ctime(), runtime))
    print('-' * 80)
    if numlines > 4:
        print()
    TIMENOW = time.time()
    return msg
# #########################################################################


# #########################################################################
def print_exception(msg, err="", with_traceback=True):
    print('-' * 80)
    if not with_traceback:
        print(msg.upper()[:80] + "...")
    else:
        print(msg.upper())

    exc_type, exc_value, exc_tb = sys.exc_info()

    if with_traceback:
        print(traceback.format_exc())
        try:
            print(str(exc_type)[:80] + '...')
        except:
            print(exc_type)
    print(err)

    if not with_traceback:
        print('-' * 80)
# #########################################################################


# #########################################################################
def update_shared_state(DROPSET, data, with_target=True, reoder=True):
    if reoder:
        xvars = feature_select(data, data[TARGET_VAR], data.columns,
                               p=99./100., min_score=1,
                               sorted_by_importance=True)
        xvars = [col for col in xvars if col not in DROPSET]
        dropvars = [col for col in data if col not in xvars]
        if with_target:
            xvars = xvars + [TARGET_VAR, ]
        data = data[xvars]
        DROPSET.extend(dropvars)

    DROPSET = [x for x in set(DROPSET) if x in data.columns] + \
        [TARGET_VAR, TARGET_COVAR, ID_VAR]
    VARSET = validate_vars(data, not_in_set=DROPSET, with_target=with_target)
    data = data[VARSET]
    M, N = print_data_shape(data)
    COLSET = [c for c in data.columns]
    NCOLS = len(COLSET)

    if not MULTICLASS:
        data[TARGET_VAR] = data[TARGET_VAR].astype('int')
        print("CLASS1: ", CLASS1, len(data[data[TARGET_VAR] == CLASS1]))
        print("CLASS0: ", CLASS0, len(data[data[TARGET_VAR] == CLASS0]))

    if MULTICLASS:
        for val in sorted(set(data[TARGET_VAR])):
            m = len(data[data[TARGET_VAR] == val])
            try:
                print("CLASS[%s, %s]: %s" % (val, LABELS[val], m))
            except:
                print("CLASS[%s]: %s" % (val, m))

    m = len(data[data[TARGET_VAR] == TESTING_SET])
    print("|TESTSET[Y=%s]|=%s" % (TESTING_SET, m))

    return data, DROPSET, VARSET, M, N, COLSET, NCOLS
# #########################################################################


# #########################################################################
# MAIN
# #########################################################################
if __name__ == "__main__":
    # #####################################################################
    banner('reading dataframe')
    data, dropset = read_data()
    if len(dropset):
        DROPSET.extend(dropset)
    ORIGINAL_VARIABLES = set_original_variables(data)

    # #####################################################################
    if not BYPASS_DATA_AUGMENTATION:
        banner('augmentation of data')
        data = augment_data(data)
        ORIGINAL_VARIABLES = set_original_variables(data)

    # #####################################################################
    if not BYPASS_ADD_CLASSLABEL_DIFFERENTIATORS:
        data = add_classlabel_differentiators(data, encode=True)

    # #####################################################################
    if ENCODE_CATEGORICALS:
        banner('categorical encoding')
        for col in data:
            banner('encoding %s' % col)
            if TARGET_VAR in col:
                continue
            data[col] = apply_na_recoder(data[col], data[TARGET_VAR], col=col)
            data[col] = get_codes_from_categorical(data[col], col=col)

    # #####################################################################
    if not BYPASS_INITIAL_NUMERICAL_CUTS:
        banner('optimization of numerical cuts')
        start = time.time()
        for col in [x for x in data if x not in [TARGET_VAR, ID_VAR]]:
            colname = "%s_opticuts" % col
            if not is_categorical(data[col], col=col):
                L = len(set(data[col]))
                print(colname, L)
                if L > min(UPPER_FACTOR_LEVEL_LIMIT, 2048):
                    data[col] = optimize_factor_cuts(data, col=col).copy()
                    # DROPSET.add(col)

            if (time.time() - start) > INTERNAL_LOOP_MAX_COMPUTE_QUANTUM:
                print('compute time exceeded', time.time() - start)
                break

    # #####################################################################
    if not BYPASS_ADD_IMPT_FACTOR_LEVELS:
        banner('auto discovery of important factor levels')
        FSELECTED_INDICATOR_VARIABLES = \
            derive_impt_factorlevel_features_from(data)
        for col in FSELECTED_INDICATOR_VARIABLES:
            data[col] = FSELECTED_INDICATOR_VARIABLES[col].copy()
            print('added fselected factor-level indicator to data', col)

    # #####################################################################
    banner('var description')
    for col in data:
        banner(col)
        print(data[col].describe(include='all'))
        # print(pd.Categorical.from_array(data[col]).describe())
        print(pd.Categorical(data[col]).describe())

    # #####################################################################
    data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
        update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_ORIGVAR_FSELECT:
        banner('origvar feature selection')
        colset = [x for x in data]
        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt="origvars", stem="A")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_STARTUP_DECORRELATION:
        banner('origvar decorrelation processing')
        colset = [x for x in data][BYPASS_DECORRELATION_ON_NTOP:]
        data, DROPSET = \
            decorrelate_and_project(data, DROPSET,
                                    do_projections=True,
                                    wrt="origvars", stem="B",
                                    colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if MULTICLASS:
        banner("preprocessing MULTI-CLASS LABELS/TARGET_VAR: %s" % TARGET_VAR)
        data, (LABELS, CLASS_LABELS, CLASS_LABEL_ENC, CLASSES,
               MAPPING, INVERSE_MAPPING) = \
            preprocess_multiclass_class_labels(data)

    # #####################################################################
    if not BYPASS_ADD_MARGINALS:
        banner('augmentation with selected marginals')
        data = add_marginal_variables(data)

    # #####################################################################
    if ENCODE_CATEGORICALS and DROP_VARS_WITH_LESS_THAN_NLEVELS >= 2:
        banner('enforcing limited numericals and categoricals')
        for col in data:
            vals = [x if pd.notnull(x) else NA_RECODE for x in data[col]]
            L = len(set(vals))
            if L < DROP_VARS_WITH_LESS_THAN_NLEVELS:
                del data[col]
                print(col, 'deleted, not enough levels', L)
                print('-' * 80)
                continue

    # #####################################################################
    if not BYPASS_ORIGVAR_FSELECT:
        banner('marginals feature selection')
        colset = [x for x in data if 'MARGINAL' in x]
        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt="marginals", stem="C")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_STARTUP_DECORRELATION:
        banner('origvar decorrelation processing')
        colset = [x for x in data]
        colset = colset[BYPASS_DECORRELATION_ON_NTOP:]
        data, DROPSET = \
            decorrelate_and_project(data, DROPSET,
                                    do_projections=True,
                                    wrt="marginals", stem="D",
                                    colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if True:
        banner('writing the input test id sequence as read')
        try:
            ct = data[data[TARGET_VAR] == TESTING_SET]
            if ID_VAR in ct and len(ct[ID_VAR]):
                with open('ids.csv', 'w') as fp:
                    header = "%s,%s\n" % ("ROWNUM", "ID")
                    fp.write(header)
                    for i, x in enumerate(ct[ID_VAR]):
                        out = "%s,%s\n" % (i, x)
                        fp.write(out)
        except Exception as err:
            print_exception('problem writing id seq', err=err)

    # #####################################################################
    if not BYPASS_ADD_CATEGORICAL_FREQUENCIES:
        banner('categorical frequencies processing')
        data, dropset = generate_categorical_frequencies(data)
        if len(dropset):
            DROPSET.extend(dropset)
            data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
                update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_EARLY_REDUCE_LARGE_FACTORS:
        banner('large factor projection')
        large_factor_colset = identify_large_factors(data)
        if len(large_factor_colset):
            data, _ = feature_select_and_project(data, DROPSET,
                                                 colset=large_factor_colset,
                                                 wrt="bootstrap", stem="E")
            data = process_large_factors(data, large_factor_colset)

    # #####################################################################
    if not BYPASS_EARLY_DECORRELATION:
        banner('xfreqs decorrelation processing')
        colset = [x for x in data if 'freqs' in x]
        data, DROPSET = decorrelate_and_project(data, DROPSET,
                                                do_projections=True,
                                                wrt="xfreqs", stem="F",
                                                colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_XFREQS_FSELECT:
        banner('xfreqs feature selection')
        colset = [x for x in data.columns if 'freqs' in x]
        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt="xfreqs", stem="G")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_EARLY_DECORRELATION:
        banner('xterm decorrelation processing')
        colset = [x for x in data if 'XTERM' in x]
        data, DROPSET = decorrelate_and_project(data, DROPSET,
                                                do_projections=True,
                                                wrt="xterms", stem="H",
                                                colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_INTERACTION_TERMS:
        banner('adding interaction terms')
        data = generate_interaction_terms(data)

    # #####################################################################
    if not BYPASS_XTERM_FSELECT:
        banner('xterm feature selection')
        colset = [x for x in data.columns if 'XTERM' in x]
        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt="xterms", stem="I")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_INTERACTION_TERMS:
        banner('adding third degree interaction terms')
        data = generate_higher_order_interactions(data)

    # #####################################################################
    if not BYPASS_XTERM_FSELECT:
        banner('xterm feature selection')
        colset = [x for x in data.columns if 'XTERM' in x]
        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt="xterms", stem="K")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_EARLY_DECORRELATION:
        banner('xterm decorrelation processing')
        colset = [x for x in data if 'XTERM' in x]
        data, DROPSET = decorrelate_and_project(data, DROPSET,
                                                do_projections=True,
                                                wrt="xterms", stem="J",
                                                colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_XTERM_SQUARING:
        banner('adding squaring terms')
        data = generate_square_terms(data)

    # #####################################################################
    if not BYPASS_XTERM_FSELECT:
        banner('xterm feature selection')
        colset = [x for x in data.columns if 'XTERM' in x]
        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt="xterms", stem="M")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_EARLY_DECORRELATION:
        banner('xterm decorrelation processing')
        colset = [x for x in data if 'XTERM' in x]
        data, DROPSET = decorrelate_and_project(data, DROPSET,
                                                do_projections=True,
                                                wrt="xterms", stem="L",
                                                colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_BOOTSTRAP_FSELECT:
        banner('bootstrap feature selection')
        colset = [x for x in data.columns]
        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt="bootstrap", stem="N")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_EARLY_DECORRELATION:
        banner('bootstrap decorrelation processing')
        colset = [x for x in data]
        colset = colset[BYPASS_DECORRELATION_ON_NTOP:]
        data, DROPSET = decorrelate_and_project(data, DROPSET,
                                                do_projections=True,
                                                wrt="bootstrap", stem="O",
                                                colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_FEATURE_IMPORTANCES_REDUCTION:
        banner('feature importances, I')
        X = data[data[TARGET_VAR] != TESTING_SET]
        xvars = [x for x in X if x not in [TARGET_VAR, ID_VAR]]
        X, Y = X[xvars], X[TARGET_VAR]
        dropset, _ = get_feature_impt_dropset(X, Y)
        if len(dropset):
            DROPSET.extend(dropset)
            data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
                update_shared_state(DROPSET, data, with_target=True)
        del X, Y

    # #####################################################################
    if not BYPASS_APPLY_BASIC_FEATURE_GENERATION:
        banner('correlation reductionism and feature generation')
        for idx, col in enumerate(COLSET):
            data, dropset = \
                basic_feature_generation_reductionism(data, col,
                                                      i=idx, COLSET=COLSET)
            if len(dropset):
                DROPSET.extend(dropset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_BOOTSTRAP_FSELECT:
        banner('bootstrap feature selection')
        colset = [x for x in data.columns]
        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt="bootstrap", stem="P")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_EARLY_DECORRELATION:
        banner('bootstrap decorrelation processing')
        colset = [x for x in data][BYPASS_DECORRELATION_ON_NTOP:]
        data, DROPSET = decorrelate_and_project(data, DROPSET,
                                                do_projections=True,
                                                wrt="bootstrap", stem="Q",
                                                colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_APPLY_BASIC_FEATURE_ENCODING:
        banner('data reductionism: factor encoding')
        i, prev_col, step = 0, "", min(len(COLSET), FS_STEPS['incremental'])
        for col in COLSET:
            i += 1
            vals = [x if pd.notnull(x) else NA_RECODE for x in data[col]]
            data, dropset = \
                basic_feature_encoding_reductionism(data, col, vals,
                                                    prev_col=prev_col)
            if len(dropset):
                DROPSET.extend(dropset)

            if i % step == 0 or i == N or TARGET_VAR in col:
                banner('incremental feature selection over factor-encoded fgen')
                print(data[TARGET_VAR].describe(include='all'))

                if PARTIAL_AND_INCREMENTAL:
                    colset = [x for x in COLSET[0: i]
                              if x not in [ID_VAR, TARGET_VAR]
                              and x in data.columns]
                else:
                    colset = [x for x in COLSET[i-step: i]
                              if x not in [ID_VAR, TARGET_VAR]
                              and x in data.columns]
                try:
                    fselect_vars = \
                        feature_select(data, data[TARGET_VAR], colset,
                                       min_score=FS_MINSCORES['incremental'],
                                       p=FS_PERCENTAGES['incremental'])
                    fselect_drop = [x for x in colset if x not in fselect_vars
                                    and x in data.columns
                                    and TARGET_VAR not in x]
                    banner('G: data encoding feature selection dropped factors')
                    P = get_projection_features_for(data[fselect_drop],
                                                    y=data[TARGET_VAR])
                    for pcol in P.columns:
                        pcolname = "GVAR_%s_%s_%s" % (i, pcol, int(time.time()))
                        print('proj. col for incr. fsel. drop', pcol, pcolname)
                        data[pcolname] = P[pcol].copy()
                    DROPSET.extend(fselect_drop)
                except Exception as err:
                    print_exception('exc w/ incr fselect wrt get-proj', err=err)

            if TARGET_VAR not in col and TARGET_VAR not in prev_col:
                prev_col = col[:]

        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_BOOTSTRAP_FSELECT:
        banner('bootstrap feature selection')
        colset = [x for x in data.columns]
        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt="bootstrap", stem="R")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_EARLY_DECORRELATION:
        banner('bootstrap decorrelation processing')
        colset = [x for x in data][BYPASS_DECORRELATION_ON_NTOP:]
        data, DROPSET = decorrelate_and_project(data, DROPSET,
                                                do_projections=True,
                                                wrt="bootstrap", stem="S",
                                                colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_ADD_TIME_DIFFERENTIALS:
        banner('time differences')
        year_cols = [x for x in data.columns if 'year' in x]
        mon_cols = [x for x in data.columns if 'mon' in x]
        day_cols = [x for x in data.columns if 'day' in x]
        data = generate_time_differentials_from(data,
                                                year_cols, mon_cols, day_cols)

    # #####################################################################
    if not BYPASS_INITIAL_FSELECT:
        banner('subselecting among initial factors')
        data, dropset = incremental_feature_selection(data, stage="initial")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    # STRATIFICATION/CLASS BALANCING
    # #####################################################################
    if DO_CLASS_PREBALANCING:
        banner('class balancing')
        try:
            train_set, dev_set, cv_set, test_set = \
                split_dataset_by_classes_into_sets(data)

            TSLEN, DSLEN, XVLEN, PSLEN = \
                len(train_set), len(dev_set), len(cv_set), len(test_set)

            if PSLEN:
                data = pd.concat([train_set, dev_set, cv_set, test_set])
            else:
                data = pd.concat([train_set, dev_set, cv_set])

            DROPSET.append(LABEL_VAR)
            DROPSET.append(ID_VAR)
            try:
                labels = [x for x in data if LABEL_VAR in x]
                print(labels)
                if len(labels):
                    del data[labels]
            except Exception as err:
                print_exception('w/ deleting label var in class bal', err=err)

            data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
                update_shared_state(DROPSET, data, with_target=True)
        except Exception as err:
            print_exception('problem during class balancing', err=err)
    else:
        print('dataset kept as is, will be split later on')

    # #####################################################################
    if ADD_ZSCORES_F:
        banner('ADD_ZSCORES_F: extra features indicators')
        data = add_zscores_columnspan_features(data, STEP=ZSCORE_COL_STEP_F)

    # #####################################################################
    if not BYPASS_BOOTSTRAP_FSELECT:
        banner('bootstrap feature selection')
        colset = [x for x in data.columns]
        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt="bootstrap", stem="U")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_EARLY_DECORRELATION:
        banner('bootstrap decorrelation processing')
        colset = [x for x in data]
        colset = colset[BYPASS_DECORRELATION_ON_NTOP:]
        data, DROPSET = \
            decorrelate_and_project(data, DROPSET,
                                    do_projections=True,
                                    wrt="bootstrap", stem="V",
                                    colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    # NORMALITY CHECK
    # #####################################################################
    coarse_xvars, nongauss_xvars = [], []
    if not BYPASS_NORMAL_CHECKS:
        banner('normality check')
        for col in data.columns:
            print('-' * 80)
            if is_special_col(col) or TARGET_VAR == col:
                print(col, "SKIPPED SPECIAL COLUMNS")
                transform_performance['specialcols'] += 1
                continue
            try:
                vals = [x for x in data[col]]
                levels = sorted(set(vals))
                L = len(levels)
                if L < NORMALTEST_MINLEVELS:
                    print(col, "SKIPPED COARSE FACTOR", levels[:DISPLAY_NLEVELS])
                    transform_performance['coarse'] += 1
                    coarse_xvars.append(col)
                    continue
            except Exception as err:
                print_exception('problem cats:normalization', err=err)

            data[col], ok = normalize_col(data[col], col)
            if ok:
                transform_performance['gaussian'] += 1
                continue
            else:
                transform_performance['nongauss'] += 1
                nongauss_xvars.append(col)

        banner('normalization performance')
        for f in transform_performance:
            print("%16s: %s" % (f, transform_performance[f]))
        print('-' * 80)

    # #####################################################################
    coarse_xvars = [x for x in coarse_xvars if TARGET_VAR not in x]
    if len(coarse_xvars):
        banner('subselecting among coarse factors')
        data, dropset = incremental_feature_selection(data,
                                                      wrt_colset=coarse_xvars,
                                                      stage="coarse")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    nongauss_xvars = [x for x in nongauss_xvars if TARGET_VAR not in x]
    if len(nongauss_xvars):
        banner('subselecting among non-gaussian variables')
        data, dropset = incremental_feature_selection(data,
                                                      wrt_colset=nongauss_xvars,
                                                      stage="nongauss")
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_INTERMEDIARY_DECORRELATION:
        banner('intermediary decorrelation processing')
        colset = [x for x in data][BYPASS_DECORRELATION_ON_NTOP:]
        data, DROPSET = decorrelate_and_project(data, DROPSET,
                                                do_projections=True,
                                                wrt="intermediary", stem="W",
                                                colset=colset)
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if not BYPASS_APPLY_FACTOR_CONDITIONING:
        banner('factor conditioning')
        data = apply_factor_conditioning(data)

    # #####################################################################
    if not BYPASS_INCREMENTAL_STEP_FITTER_DATAREADY:
        banner('incremental step fitter')
        incrementally_selected_xvars = step_fitter(data)
        incrementally_selected_xvars = dict(list(zip(incrementally_selected_xvars,
                                                incrementally_selected_xvars)))
        DROPSET.extend([x for x in data.columns
                        if x not in incrementally_selected_xvars])
        data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    if USE_DEPRECATED_CODE and not BYPASS_FEATURE_IMPORTANCES_REDUCTION:
        banner('feature importances, II')
        X = data[data[TARGET_VAR] != TESTING_SET]
        xvars = [x for x in X if x not in [TARGET_VAR, ID_VAR]]
        X, Y = X[xvars], X[TARGET_VAR]
        dropset, _ = get_feature_impt_dropset(X, Y)
        if len(dropset):
            DROPSET.extend(dropset)
            data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
                update_shared_state(DROPSET, data, with_target=True)
        del X, Y

    # #####################################################################
    # subselect over w/o the centroids added in, for distance meas. purposes
    # #####################################################################
    if USE_DEPRECATED_CODE and not BYPASS_ADD_CENTROIDS:
        if not BYPASS_CENTROID_FSELECT:
            banner('feature selection before centroids')
            colset = [x for x in data]
            data, DROPSET = feature_select_and_project(data, DROPSET,
                                                       colset=colset,
                                                       wrt="centroids",
                                                       stem="X")
            data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
                update_shared_state(DROPSET, data, with_target=True)

        banner('augmentation with centroids')
        data = augment_with_per_class_random_centroids(data)

        # #################################################################
        # subselect over with the centroids added in, in case shift took place
        # #################################################################
        if not BYPASS_CENTROID_FSELECT:
            banner('feature selection after centroids')
            colset = [x for x in data]
            data, DROPSET = feature_select_and_project(data, DROPSET,
                                                       colset=colset,
                                                       wrt="centroids",
                                                       stem="Y")
            data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
                update_shared_state(DROPSET, data, with_target=True)

    # #####################################################################
    # DATA CONDITIONING: NUMERICAL
    # #####################################################################
    banner('numerical representation')
    numerical_data = numerical_recoding(data, VARSET=VARSET)
    numerical_data[TARGET_VAR] = [x for x in data[TARGET_VAR]]
    if not MULTICLASS:
        numerical_data[TARGET_VAR] = \
            pd.Categorical(data[TARGET_VAR], categories=[TESTING_SET, CLASS0, CLASS1])
            # pd.Categorical.from_array(data[TARGET_VAR], categories=[TESTING_SET, CLASS0, CLASS1])
        numerical_data[TARGET_VAR] = \
            numerical_data[TARGET_VAR].fillna(TESTING_SET)

    # #####################################################################
    # DATA CONDITIONING: RANK NUMERICAL
    # #####################################################################
    banner('rank-based numerical representation')
    if not BYPASS_RANK_AUGMENTATION and FS_PERCENTAGES['rank']:
        numerical_data = augment_with_rank_features_for(numerical_data)
    del data

    # #####################################################################
    if ADD_ZSCORES_G:
        banner('ADD_ZSCORES_G: extra features indicators')
        numerical_data = add_zscores_columnspan_features(numerical_data,
                                                         STEP=ZSCORE_COL_STEP_G)

    # #####################################################################
    if USE_DEPRECATED_CODE:
        def drop_term(term, x, endsw=False):
            if not endsw:
                return term in x
            return term in x or x.endswith(term[:-1])

        banner('experimenting with forced term dropping')
        dropcols = [x for x in numerical_data
                    if drop_term(TARGET_VAR, x) or drop_term(ID_VAR, x)]
        # DROPSET.extend(dropcols)
        numerical_data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, numerical_data, with_target=True)

    # #####################################################################
    banner('subselecting among rank and numerical variables')
    colset = [col for col in numerical_data.columns
              if col not in [ID_VAR, TARGET_VAR]]
    numerical_data, dropset = \
        incremental_feature_selection(numerical_data, wrt_colset=colset,
                                      stage="final")
    numerical_data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
        update_shared_state(DROPSET, numerical_data, with_target=True)

    # #####################################################################
    if GLOBAL_NOT_LOCAL_DECORRELATION and not BYPASS_GLOBAL_DECORRELATION:
        banner('global decorrelation processing')
        colset = VARSET[BYPASS_DECORRELATION_ON_NTOP:]
        numerical_data, DROPSET = \
            decorrelate_and_project(numerical_data, DROPSET,
                                    do_projections=True,
                                    wrt="global", stem="Z",
                                    colset=colset)
        numerical_data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
            update_shared_state(DROPSET, numerical_data, with_target=True)

    # #####################################################################
    # FULL PROJECTION
    # #####################################################################
    if USE_DEPRECATED_CODE and not BYPASS_GLOBAL_PROJECTIONS:
        try:
            banner('O: full projection')
            P = get_projection_features_for(numerical_data,
                                            y=numerical_data[TARGET_VAR],
                                            ncols=2)
            for pcol in P.columns:
                pcolname = "OVAR_%s_%s_%s" % (i, pcol, int(time.time()))
                print('projection col for full projection', pcol, pcolname)
                numerical_data[pcolname] = P[pcol].copy()
        except Exception as err:
            print_exception('problem with projection', err=err)

    # #####################################################################
    try:
        fselect_vars = feature_select(numerical_data,
                                      numerical_data[TARGET_VAR],
                                      VARSET,
                                      p=FS_PERCENTAGES['final'],
                                      min_score=FS_MINSCORES['final'],
                                      sorted_by_importance=True)

        fselect_drop = [x for x in colset
                        if x not in fselect_vars
                        and x in numerical_data.columns
                        and TARGET_VAR not in x]

        DROPSET.extend(fselect_drop)
    except Exception as err:
        print_exception('problem with summary fselection', err=err)

    # #####################################################################
    banner('final variable sets')
    numerical_data, DROPSET, VARSET, M, N, COLSET, NCOLS = \
        update_shared_state(DROPSET, numerical_data, with_target=True)

    # #####################################################################
    if not BYPASS_PCA_SCALER:
        numerical_data = apply_scaling(numerical_data,
                                       zvals=True,
                                       minmax=False,
                                       maxabs=False,
                                       robust=False)

    # #####################################################################
    # DATASET READY
    # #####################################################################
    banner('dataset ready')
    XVARS = [x for x in VARSET if x in numerical_data and TARGET_VAR not in x]
    X, Y = numerical_data[XVARS], numerical_data[TARGET_VAR]
    M, N = print_data_shape(numerical_data)
    DATASETS = [(DATASET_NAME, X.copy(), Y.copy()), ]

    # #####################################################################
    if USE_DEPRECATED_CODE:
        if SAVE_COMPUTED_DESIGN_MATRIX and not LOAD_COMPUTED_DESIGN_MATRIX:
            banner('storing transformed design matrix data')
            try:
                pickle.dump(numerical_data, open(DESIGN_MATRIX_FILENAME, "wb"))
            except Exception as err:
                print_exception('problem with saving data', err=err)

        if LOAD_COMPUTED_DESIGN_MATRIX:
            banner('loading transformed design matrix data')
            try:
                numerical_data = pickle.load(open(DESIGN_MATRIX_FILENAME))
            except Exception as err:
                print_exception('problem with loading data', err=err)

    del numerical_data


# #########################################################################
# training and predicting
# #########################################################################
banner('training and predicting')
dataset_number, debug = 0, False
for ds in DATASETS:
    dataset_name, X, Y = ds
    banner(dataset_name)

    YP, TRAINING_YP, TEST_YP, thresholds, cv_thresholds, METRICS = \
        pd.DataFrame(), pd.DataFrame(), pd.DataFrame(), {}, {}, {}

    PCA_X = X
    if not BYPASS_APPLY_PCA:
        try:
            PCA_X = apply_PCA(X, y=Y)
            print(PCA_X[list(range(0, 3, 1))+list(range(-3, 0, -1))].describe(include='all'))
        except Exception as err:
            print_exception('problem with apply_PCA', err=err)
            PCA_X = X

    PCA_X[TARGET_VAR] = Y
    VARSET = validate_vars(PCA_X, not_in_set=DROPSET, with_target=True)
    PCA_X = PCA_X[VARSET]

    if not MULTICLASS:
        train_set, dev_set, cv_set, test_set = \
            split_dataset_by_classes_into_sets(PCA_X)

    if MULTICLASS:
        # PCA_X[TARGET_VAR] = pd.Categorical.from_array(Y)
        PCA_X[TARGET_VAR] = pd.Categorical(Y)
        print(PCA_X[TARGET_VAR].describe(include='all'))
        train_set, dev_set, cv_set, test_set = \
            train_cv_split_idx(PCA_X,
                               percentage=PERCENT_OF_SAMPLES_TO_TRAIN_WITH,
                               devtst_ratio=DEV_SPLIT,
                               augment_by=MULTICLASS_AUGMENT_BY,
                               randomize=True)

    WITHOUT_Y = sorted(set([x for x in PCA_X if TARGET_VAR not in x]))
    JUST_Y = TARGET_VAR
    banner("VARS IN USE BY CLASSIFIER: %s" % ", ".join(WITHOUT_Y))

    if not RESAMPLE_XY:
        X_train, y_train = train_set[WITHOUT_Y].values, train_set[JUST_Y]
        X_dev, y_dev = dev_set[WITHOUT_Y].values, dev_set[JUST_Y]
        X_cv, y_cv = cv_set[WITHOUT_Y].values, cv_set[JUST_Y]
        X_test, y_test = test_set[WITHOUT_Y].values, test_set[JUST_Y]

    if not BYPASS_ADD_CENTROIDS:
        banner('augmentation with centroids')
        train_set = augment_with_per_class_random_centroids(train_set)

    if not BYPASS_TRIM_LARGE_CLASSES:
        banner('trimming large-size classes')
        for yclass in sorted(set(train_set[TARGET_VAR])):
            if yclass == -1:
                continue
            class_size = len(train_set[train_set[TARGET_VAR] == yclass])
            if class_size > LARGE_CLASSSIZE_THRESHOLD:
                train_set = trim_class_from(train_set,
                                            which_class=yclass,
                                            upto=LARGE_CLASS_REDUCTION_GOAL,
                                            maxiter=5)

        if not BYPASS_ADD_CENTROIDS:
            banner('augmentation with centroids')
            train_set = augment_with_per_class_random_centroids(train_set)

    describe_datasets(train_set, dev_set, cv_set, test_set)

    # #####################################################################
    # iterate over classifiers
    # #####################################################################

    def get_clname(clfname, class_ix):
        clf_classname = "%s_%s" % (clfname, class_ix)
        return clf_classname

    p_clf = None
    for cnum, name in enumerate(NAMES):
        if cnum not in APPLY_THESE:
            continue

        banner("training samples")
        if RESAMPLE_XY:
            train_set, dev_set, cv_set, test_set = \
                split_dataset_by_classes_into_sets(PCA_X)
            X_train, y_train = train_set[WITHOUT_Y].values, train_set[JUST_Y]
            X_dev, y_dev = dev_set[WITHOUT_Y].values, dev_set[JUST_Y]
            X_cv, y_cv = cv_set[WITHOUT_Y].values, cv_set[JUST_Y]
            X_test, y_test = test_set[WITHOUT_Y].values, test_set[JUST_Y]

        if True:
            print('-' * 80)
            print("X(TRAIN,DEV,CV):", X_train.shape, X_dev.shape, X_cv.shape)
            print("train", X_train[0:1, 0:10])
            print("dev  ", X_dev[0:1, 0:10])
            print("cv   ", X_cv[0:1, 0:10])
            if len(X_test):
                print("test ", X_test[0:1, 0:10])
            print('-' * 80)
            print("Y(TRAIN,DEV,CV)")
            print('train', len(y_train), [x for x in y_train][0:31])
            print("dev  ", len(y_dev), [x for x in y_dev][0:31])
            print("cv   ", len(y_cv), [x for x in y_cv][0:31])
            if len(X_test):
                print("test ", len(y_test), [x for x in y_test][0:31])
            print('-' * 80)

        banner("configuring classifier %s" % name)
        try:
            if debug:
                banner("xyvals")
                print(pd.DataFrame(X_train).describe().transpose())
                print('-' * 80)
            print(pd.DataFrame(y_train).describe())
            print("yvals: ", set(y_train))
            print('-' * 80)

            print("CONFIGURING/TRAINING CLASSIFIER: ", name)
            clf, u_clf = configure_classifier(name, X_train.shape[1])
            clf.fit(X_train, y_train)
            print(clf)

        except Exception as err:
            print_exception('problem with building model', err=err)
            print(name)
            print(clf)
            continue

        banner('classifier attributes: %s' % name)
        try:
            print_classifier_attributes(clf=clf)
            print_classifier_attributes(clf=u_clf)
        except Exception as err:
            print_exception('problem printing clf params', err=err)

        if WRITE_MODEL:
            banner('writing classifier to disk: %s' % name)
            try:
                pickle.dump(clf, open('clf_%s.dat' % name, "wb"))
            except Exception as err:
                print_exception('problem pickling model', err=err)

        try:
            banner("PREDICTING TRAIN DATA FOR: %s" % name)
            YPP, YPC, trained_threshold, perf_metrics = \
                clf_predict(clf, X_train, y_train,
                            name, "TRAIN", threshold=None)
            perf_metrics['score'] = clf.score(X_train, y_train)
            # perf_metrics['cv_score'] = cross_val_score(clf,
            #                                            X_train, y_train, cv=3)
            METRICS["train_%s" % name] = perf_metrics.copy()
        except Exception as err:
            print_exception('problem with predicting with model', err=err)
            print(name)
            print(clf)
            continue

        try:
            banner("GENERATED ENSEMBLE CLASSIFIER")
            thresholds["train_%s" % name] = trained_threshold

            if not MULTICLASS:
                TRAINING_YP["%s" % name] = YPP
                TRAINING_YP["%s_ll" % name] = decision_ll(YPP, YPC)
                TRAINING_YP["%s_ycc" % name] = (YPC - YPP) * YPP
                TRAINING_YP["%s_ypc" % name] = (1 + YPP) * YPC
                TRAINING_YP["%s_ypa" % name] = (1 - YPP) * YPC
                TRAINING_YP["%s_sum" % name] = decision_sum(TRAINING_YP)
                TRAINING_YP["%s_yp2" % name] = YPP * YPP
                TRAINING_YP["%s_ym2" % name] = (1 - YPP) * (1 + YPP)
                TRAINING_YP["%s_yps" % name] = numpy.sqrt(YPP)
                clf_cols = [x for x in NAMES if x in TRAINING_YP.columns]
                ens_cols = [x for x in TRAINING_YP.columns if 'ensemb' not in x]
                p_clf = build_probability_correction_model(
                    TRAINING_YP[ens_cols], y_train)

            if MULTICLASS:
                clf_cols = [NAMES[x] for x in APPLY_THESE]
                for col in YPP:
                    colname = get_clname(name, col)
                    TRAINING_YP[colname] = YPP[col]
                    print('TRAINING_YP stored', colname)
                perf_metrics['logloss'] = \
                    mcls.multiclass_log_loss(y_train, YPP, classes=CLASS_LABELS)
                print('-' * 80)

        except Exception as err:
            print_exception('problem with ensemble model', err=err)
            print(name)
            print(clf)
            continue

        banner("PREDICTING DEV DATA FOR: %s" % name)
        YPP, YPC, dev_threshold, perf_metrics = \
            clf_predict(clf, X_dev, y_dev,
                        name, "DEV", threshold=trained_threshold)
        perf_metrics['score'] = clf.score(X_dev, y_dev)
        METRICS["dev_%s" % name] = perf_metrics.copy()
        thresholds["dev_%s" % name] = dev_threshold

        banner("PREDICTING CV DATA FOR: %s" % name)
        use_threshold = trained_threshold
        use_threshold = dev_threshold
        YPP, YPC, cv_threshold, perf_metrics = \
            clf_predict(clf, X_cv, y_cv,
                        name, "CV", threshold=use_threshold)
        perf_metrics['score'] = clf.score(X_cv, y_cv)
        METRICS["cv_%s" % name] = perf_metrics.copy()
        thresholds["cv_%s" % name] = cv_threshold
        cv_thresholds[name] = (cv_threshold, dev_threshold, trained_threshold)
        trained_threshold = dev_threshold
        thresholds[name] = trained_threshold

        if not MULTICLASS:
            YP["%s" % name] = YPP
            YP["%s_ll" % name] = decision_ll(YPP, YPC)
            YP["%s_ycc" % name] = (YPC - YPP) * YPP
            YP["%s_ypc" % name] = (1 + YPP) * YPC
            YP["%s_ypa" % name] = (1 - YPP) * YPC
            YP["%s_sum" % name] = decision_sum(YP)
            YP["%s_yp2" % name] = YPP * YPP
            YP["%s_yps" % name] = numpy.sqrt(YPP)
            YP["%s_ym2" % name] = (1 - YPP) * (1 + YPP)

        if MULTICLASS:
            for col in YPP:
                colname = get_clname(name, col)
                YP[colname] = YPP[col]
                print('XVAL/CV YP stored', colname)
            perf_metrics['logloss'] = \
                mcls.multiclass_log_loss(y_cv, YPP, classes=CLASS_LABELS)
            print('-' * 80)

        banner("CV ENSEMBLE: %s %s" % ([col for col in YP.columns], name))
        try:
            if not MULTICLASS:
                E_THRESHOLD = 0.5
                YP["ensemble_mean"] = numpy.array(YP[clf_cols].mean(axis=1))
                YPP_, YPC_, _, perf_metrics_ = \
                    compute_metrics_for(y_cv,
                                        numpy.array(YP["ensemble_mean"]),
                                        E_THRESHOLD)
                METRICS['cv_ensemble_avg_yp'] = perf_metrics_

                E_YPP = clf_predict_wrt(p_clf, "ensemble", YP[ens_cols])
                YP["ensemble"] = numpy.array(E_YPP)

                print(YP.describe().transpose())
                print('-' * 80)

                E_THRESHOLD = 0.5
                YPP_, YPC_, _, perf_metrics_ = \
                    compute_metrics_for(y_cv, numpy.array(E_YPP), E_THRESHOLD)
                METRICS['cv_ensemble_yp'] = perf_metrics_

            if MULTICLASS:
                for c, cl in enumerate(CLASS_LABELS):
                    per_class_cols = [get_clname(cname, c)
                                      for cname in clf_cols]
                    current_per_class_cols = [cname for cname in per_class_cols
                                              if cname in YP]
                    YP[get_clname("ensemble_mean", c)] = \
                        numpy.array(YP[current_per_class_cols].mean(axis=1))
                    print('COMPUTED XVAL/CV average for', cl, \
                        current_per_class_cols)
                    print('-' * 80)

                try:
                    ypp = YP[[col for col in YP if "ensemble_mean" in col]]
                    perf_metrics['logloss'] = \
                        mcls.multiclass_log_loss(y_cv, ypp, classes=CLASS_LABELS)
                    try:
                        perf_metrics['score'] = ((ypp.idxmax(axis=1) - y_cv) == 0).sum()/len(y_cv)
                    except:
                        pass
                    METRICS['cv_ensemble_yp'] = perf_metrics
                except Exception as err:
                    print(err)

                print('-' * 80)

        except KeyboardInterrupt:
            print('Received Keyboard Interrupt, Wrapping Up')
            break
        except Exception as err:
            print_exception('problem w/ ensemble CV prediction', err=err)

        banner('SUMMARY')
        for attr in METRICS[list(METRICS.keys())[0]]:
            print(attr)
            for x in sorted(METRICS.keys()):
                if attr not in METRICS[x]:
                    print("%40s\t%s" % (x, None))
                    continue
                val = METRICS[x][attr]
                if "metrics" not in attr and "cmat" not in attr:
                    print("%40s\t%s" % (x, val))
                else:
                    print("%40s\n%s" % (x, val))
                    print('-' * 80)
            print('-' * 80)
        print('-' * 80)

        if not TEST_RUN:
            banner("PREDICTING TEST DATA FOR: %s" % name)
            # X_test = pickle.load(open(DESIGN_MATRIX_FILENAME))

            YPP, YPC, _, _ = clf_predict(clf, X_test, y_test,
                                         name, "TEST", threshold=use_threshold)
            if not MULTICLASS:
                TEST_YP["%s" % name] = YPP
                TEST_YP["%s_ll" % name] = decision_ll(YPP, YPC)
                TEST_YP["%s_ycc" % name] = (YPC - YPP) * YPP
                TEST_YP["%s_ypc" % name] = (1 + YPP) * YPC
                TEST_YP["%s_ypa" % name] = (1 - YPP) * YPC
                TEST_YP["%s_sum" % name] = decision_sum(TEST_YP)
                TEST_YP["%s_yp2" % name] = YPP * YPP
                TEST_YP["%s_ym2" % name] = (1 - YPP) * (1 + YPP)
                TEST_YP["%s_yps" % name] = numpy.sqrt(YPP)

                banner("TEST ENSEMBLE: %s %s" % ([col for col in YP.columns],
                                                 name))
                try:
                    TEST_YP["ensemble_mean"] = \
                        numpy.array(TEST_YP[clf_cols].mean(axis=1))
                    E_YPP = \
                        clf_predict_wrt(p_clf, "ensemble", TEST_YP[ens_cols])
                    TEST_YP["ensemble"] = numpy.array(E_YPP)
                    TEST_YP.to_csv(WRITE_CSV_TO, sep=',', header=True)
                except Exception as err:
                    print_exception('problem w/ ensemble TEST pred', err=err)

            if MULTICLASS:
                print("WRITING MULTICLASS OUTPUT WRT", name)
                try:
                    for col in YPP:
                        colname = get_clname(name, col)
                        TEST_YP[colname] = YPP[col]
                        print('TEST YP stored', colname)
                except Exception as err:
                    print_exception('problem reading multiclass probs', err=err)

                for c, cl in enumerate(CLASS_LABELS):
                    per_class_cols = [get_clname(cname, c)
                                      for cname in clf_cols]
                    current_class_cols = [cname for cname in per_class_cols
                                          if cname in TEST_YP]
                    TEST_YP[get_clname("ensemble_mean", c)] = \
                        numpy.array(TEST_YP[current_class_cols].mean(axis=1))
                    print('COMPUTED TEST average for', cl, current_class_cols)
                    print('-' * 80)

                print('-' * 80)

                TEST_YP.to_csv(WRITE_CSV_TO, sep=',', header=True)

            TEST_YP.to_csv(WRITE_CSV_TO, sep=',', header=True)
            # print TEST_YP.describe().transpose()
    # ------------------------------------------------------------------------
    TEST_YP.to_csv(WRITE_CSV_TO, sep=',', header=True)

print(PCA_X[TARGET_VAR].describe(include='all'))
print('yp output written to:', WRITE_CSV_TO)
print('DONE')
