#!/usr/bin/python
# -*- coding: utf-8 -*-


# #########################################################################
MAX_COMPUTE_QUANTUM = 1800
USE_DEPRECATED_CODE = False
# #########################################################################


# #########################################################################
import random
import pipeline_configuration
# ---------------------------------------------------------------------------
import definitions
from definitions import DATASET_NAME
# ---------------------------------------------------------------------------
from feature_selected_vars import SELECTED
from feature_selected_vars import DROPPED
from feature_selected_vars import INTERACTION_TERMS
from feature_selected_vars import MARGINAL_VARS
# ---------------------------------------------------------------------------
from step_fitter import step_fitter
import importlib
# #########################################################################


# #########################################################################
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
import pandas as pd
pd.set_option('display.expand_frame_repr', True)
pd.set_option('display.max_columns', 50)
pd.set_option('display.max_rows', 50)
pd.set_option('display.precision', 1)
pd.set_option('display.width', 190)
# #########################################################################


# #########################################################################
import numpy
numpy.set_printoptions(precision=3)
# #########################################################################


# #########################################################################
import sys
import time
import traceback
# ---------------------------------------------------------------------------
from collections import defaultdict
# ---------------------------------------------------------------------------
from sklearn.preprocessing import LabelEncoder
# ---------------------------------------------------------------------------
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier
# ---------------------------------------------------------------------------
from sklearn.calibration import CalibratedClassifierCV
# from sklearn.cross_validation import cross_val_score
# from sklearn.pipeline import make_pipeline
# ---------------------------------------------------------------------------
from sklearn import metrics
#from sklearn import grid_search
import sklearn.model_selection as grid_search

from sklearn.metrics import f1_score
from sklearn.metrics import roc_auc_score
from sklearn.cluster import KMeans
# ---------------------------------------------------------------------------
import classifier_configuration
from classifier_configuration import get_nsamples
from classifier_configuration import get_numnodes
from classifier_configuration import get_leafsize
from classifier_configuration import TREE_CRITERION
from classifier_configuration import get_feature_impt_dropset
# ---------------------------------------------------------------------------
import multiclass as mcls
# #########################################################################


# #########################################################################
# GLOBALS
# #########################################################################
global TIMENOW
TIMENOW = time.time()
STARTUP_TIME = time.time()

global LABELS, CLASS_LABELS
LABELS, CLASS_LABELS = [], []
# #########################################################################


# #########################################################################
global transform_performance
transform_performance = defaultdict(int)
# #########################################################################


# #########################################################################
SELECTED = dict(list(zip(SELECTED, SELECTED)))
DROPPED = dict(list(zip(DROPPED, DROPPED)))
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
from pipeline_configuration import BYPASS_ADD_CATEGORICAL_FREQUENCIES
from pipeline_configuration import BYPASS_ADD_CENTROIDS
from pipeline_configuration import BYPASS_HIGHER_INTERACTION_TERMS
from pipeline_configuration import C0_OVERSAMPLING
from pipeline_configuration import C0_RATIO
from pipeline_configuration import C0_WITH_REPLACEMENT
from pipeline_configuration import C1_OVERSAMPLING
from pipeline_configuration import C1_RATIO
from pipeline_configuration import C1_WITH_REPLACEMENT
from pipeline_configuration import CENTROID_APPROACH
from pipeline_configuration import CENTROID_MAX_COMPUTE_TIME
from pipeline_configuration import CENTROID_SAMPLING_EFFORT
from pipeline_configuration import CENTROID_SAMPLING_FRACTION
from pipeline_configuration import CHUNKING_FEATGEN_STEPSIZE
from pipeline_configuration import CLASS0
from pipeline_configuration import CLASS0_TRAINWITH
from pipeline_configuration import CLASS1
from pipeline_configuration import CLASS1_TRAINWITH
from pipeline_configuration import DATAFILES
from pipeline_configuration import DATASET
from pipeline_configuration import DEV_SPLIT
from pipeline_configuration import DROP_VARS_WITH_LESS_THAN_NLEVELS
from pipeline_configuration import ENCODE_NUMERICALS
from pipeline_configuration import ABS_MIN_CHUNK_STEPSIZE
from pipeline_configuration import FIRST_NROWS
from pipeline_configuration import FS_MINSCORES
from pipeline_configuration import FS_PERCENTAGES
from pipeline_configuration import FS_STEPS
from pipeline_configuration import GLOBAL_NOT_LOCAL_DECORRELATION
from pipeline_configuration import ID_VAR
from pipeline_configuration import INCREMENTAL_READ_BLOCKSIZE
from pipeline_configuration import INCREMENTAL_READ_FSELPERC
from pipeline_configuration import INCREMENTAL_READ_MINSCORE
from pipeline_configuration import LABEL_VAR
from pipeline_configuration import LOCAL_DECORRELATION_EFFORT
from pipeline_configuration import MAX_NUM_FACTOR_LEVELS
from pipeline_configuration import MAX_RANK_AUGMENTATION
from pipeline_configuration import MAX_SAMPLES_TO_CLUSTER
from pipeline_configuration import MIDPOINT_BIAS
from pipeline_configuration import MIN_VALID_CLUSTER_SIZE
from pipeline_configuration import MULTICLASS
from pipeline_configuration import NALIST
from pipeline_configuration import NA_RECODE
from pipeline_configuration import NC0_CLUSTERS
from pipeline_configuration import NUMERICAL_AS_CATEGORY_L_LIMIT
from pipeline_configuration import NUMERICAL_DATATYPE
from pipeline_configuration import PARTIAL_AND_INCREMENTAL
from pipeline_configuration import PERCENT_OF_DATA_TO_USE
from pipeline_configuration import PERCENT_OF_SAMPLES_TO_TRAIN_WITH
from pipeline_configuration import PERCLASS_RANDOM_CENTROIDS
from pipeline_configuration import RANDOM_COLSET_AUGMENTATION
from pipeline_configuration import SEPARATOR
from pipeline_configuration import TARGET_VAR
from pipeline_configuration import TESTING_SET
from pipeline_configuration import UPPER_FACTOR_LEVEL_LIMIT
from pipeline_configuration import USE_INCREMENTAL_READ
from pipeline_configuration import USE_PRESELECTED_FEATURES
from pipeline_configuration import ZSCORE_COL_STEP_F
from pipeline_configuration import ZVALS_RECODING_DIGITS
# #########################################################################
from pipeline_configuration import BYPASS_PROBABILITY_CORRECTION
from pipeline_configuration import BYPASS_EARLY_REDUCE_LARGE_FACTORS
from pipeline_configuration import EXAMINE_HIGHER_TERMS_UPTO
from pipeline_configuration import BYPASS_SQUARING_AFTERTOP
# #########################################################################
from pipeline_configuration import BYPASS_INCREMENTAL_STEP_FITTER_FACTORSELECTOR
# #########################################################################
from feature_generators import apply_scaler_for_column
from feature_generators import apply_scaling
from feature_generators import decorrelate
from feature_generators import derived_from_highly_significant_vars
from feature_generators import derived_from_paired_columns
from feature_generators import feature_select
from feature_generators import feature_select_and_project
from feature_generators import fselect
from feature_generators import generate_substitute_for_decorrelated_vars_from
from feature_generators import get_codes_from_categorical
from feature_generators import get_freqs
from feature_generators import get_interaction_terms
from feature_generators import get_levels
from feature_generators import get_marginal_wrt
from feature_generators import get_projection_features_for
from feature_generators import get_zvals
from feature_generators import highly_correlated
from feature_generators import is_highly_significant_var
from feature_generators import is_special_col
from feature_generators import is_valid_xterm
from feature_generators import nleveling
from feature_generators import normalize_col
from feature_generators import print_data_shape
from feature_generators import process_date
from feature_generators import process_numeric
from feature_generators import process_ssn
from feature_generators import process_title
from feature_generators import quantize_vector
from feature_generators import set_in_order_given
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
def set_classifier_labels(labels=(), class_labels=()):
    global LABELS, CLASS_LABELS
    LABELS, CLASS_LABELS = labels[:], class_labels[:]
    return LABELS, CLASS_LABELS
# #########################################################################


# #########################################################################
def find_threshold(y_train, ypp, override=False, m=1, debug=False):
    val_max, threshold = 0.0, 0.5
    if override:
        return MIDPOINT_BIAS

    print("    THRESHOLDS :", end=' ')
    f1_sum = 0.0
    vals = []
    for t in range(1, 99, 1):
        t = float(t)/100.
        ypc = ypp > t
        cmat = metrics.confusion_matrix(y_train, ypc)
        cmat = pd.DataFrame(cmat)

        f1_val = f1_score(y_train, ypc)
        logloss = metrics.log_loss(y_train, ypp)
        try:
            auc_score = roc_auc_score(y_train, ypp)
        except:
            auc_score = 0.0

        ix = int(t*100) - 1
        vals.append(f1_val)
        if ix > m:
            f1_sum = float(sum(vals[max(ix-m, 0):ix]))
            metric = f1_sum/float(m)
        else:
            metric = f1_val
        metric = f1_val

        threshold_test = metric >= val_max
        if t > 0.5:
            threshold_test = metric > val_max
        if threshold_test:
            val_max = metric
            threshold = t

        if debug:
            print("    THRESHOLD  :", t)
            print("    TRAIN F1   :", f1_val)
            print("    TRAIN LLOSS:", logloss)
            print("    TRAIN AUC  :", auc_score)
            print("    TRAIN CMAT :\n", cmat)
            print("    " + '-' * 76)
        else:
            print("(%.3f --> %8.6f),  " % (t, metric), end=' ')

    print()
    return threshold
# #########################################################################


# #########################################################################
def train_cv_split_idx(data,
                       percentage=PERCENT_OF_SAMPLES_TO_TRAIN_WITH,
                       devtst_ratio=DEV_SPLIT,
                       augment_by=0,
                       describe_nrows=5,
                       randomize=False):
    if MULTICLASS:
        devtst_ratio = 0.05

    test_set = data[data[TARGET_VAR] == TESTING_SET]
    print(len(test_set))
    train_data = data[data[TARGET_VAR] != TESTING_SET]
    if randomize:
        train_data = train_data.sample(n=len(train_data),
                                       random_state=get_random_seed())

    if augment_by:
        ylabels = sorted(set(train_data[TARGET_VAR]))
        for clabel in ylabels:
            cslice = train_data[train_data[TARGET_VAR] == clabel]
            P = max(min(int(len(cslice) * augment_by) + 1, 100), 250)
            random_slice_01 = cslice.sample(n=P,
                                            replace=True,
                                            random_state=get_random_seed())
            train_data = pd.concat([train_data.copy(), random_slice_01.copy()])

    M = len(train_data)
    pivot = int(M * percentage)
    dev_pivot = int(M * (percentage + (1.0 - percentage) * devtst_ratio))

    train_set = train_data[:pivot]
    dev_set = train_data[pivot:dev_pivot]
    cv_set = train_data[dev_pivot:]

    print('-' * 80)
    print("LENS", train_set.shape, dev_set.shape, cv_set.shape, test_set.shape)
    print("PIVOTS", pivot, dev_pivot, percentage)
    print('-' * 80)
    print('TRAIN SET:')
    print(train_set[0:describe_nrows])
    print('-' * 80)
    print('DEV SET:')
    print(dev_set[0:describe_nrows])
    print('-' * 80)
    print('XVAL/CV SET:')
    print(cv_set[0:describe_nrows])
    print('-' * 80)
    print('TEST SET:')
    print(test_set[0:describe_nrows])
    print('-' * 80)
    return (train_set, dev_set, cv_set, test_set)
# #########################################################################


# #########################################################################
# "Nearest Neighbors", "SVC", "LinearSVC", "Radial", "Decision Tree",
# "Random Forest", "AdaBoost", "GaussianNB", "MultinomialNB", "LDA", "QDA"
# #########################################################################
def configure_classifier(name, N, K=2):
    print("Branching: classifier_configuration.configure_classifier()")
    importlib.reload(classifier_configuration)
    clf, under_clf = classifier_configuration.configure_classifier(name, N, K=K)
    return clf, under_clf
# #########################################################################


# #########################################################################
def apply_probability_correction(YPP, threshold, bias_midpoint=MIDPOINT_BIAS):
    YPC = get_YPC(YPP, threshold)
    if MULTICLASS:
        if not BYPASS_PROBABILITY_CORRECTION:
            for col in YPP:
                y_minval = min(YPP[col].min(), 0.)
                y_maxval = max(YPP[col].max(), 1.)
                YPP[col] = (YPP[col] - y_minval) / (y_maxval - y_minval)
        return (YPP, YPC)

    if BYPASS_PROBABILITY_CORRECTION:
        YPC = YPP >= 0.5
        return (YPP, YPC)

    # bias = 0.5 - threshold
    bias = bias_midpoint - threshold
    YPP = YPP + bias * 2./3.0
    # YPP[YPP > 1] = 1.0
    # YPP[YPP < 0] = 0.0
    y_minval = min(YPP.min(), 0.)
    y_maxval = max(YPP.max(), 1.)

    YPP = (YPP - y_minval) / (y_maxval - y_minval)
    YPC = YPP >= 0.5

    return (YPP, YPC)
# #########################################################################


# #########################################################################
def decision_ll(YPP, YPC):
    ypp = numpy.maximum(1e-15, YPP)
    ypp = numpy.minimum(1 - 1e-15, ypp)

    if not MULTICLASS:
        vals = ((YPC * numpy.log(ypp)) + ((1-YPC) * numpy.log(1-ypp)))/32

    if MULTICLASS:
        vals = ((numpy.log(ypp[:, 1])) + numpy.log(1-ypp[:, 1]))/2
    return vals
# #########################################################################


# #########################################################################
def decision_sum(YP, what="sum"):
    vals = YP.sum(axis=1).mean()
    if "sum" in what:
        vals = YP.sum(axis=1).mean()
    return vals
# #########################################################################


# #########################################################################
def is_match(p, q, threshold=0.5):
    qval = q >= threshold
    if p == qval:
        return "+"
    return "X"
# #########################################################################


# #########################################################################
def clf_predict_wrt(clf, name, Xt,
                    do_kaggle_numerical_correction=False,
                    do_prob_range_correction=True,
                    nmax=32, debug=True):

    out_format = "[%3s]: %8s --> %10s %1s, \t...\t [%4s]: %8s --> %12s %1s"

    YPC0 = clf.predict(Xt)

    if not MULTICLASS:
        if hasattr(clf, "predict_proba"):
            YPP = clf.predict_proba(Xt)
            YPP = YPP[:, 1]
        elif hasattr(clf, "decision_function"):
            YPP = clf.decision_function(Xt)

        YPP = numpy.nan_to_num(YPP)
        if do_kaggle_numerical_correction:
            for ii in range(len(YPP)):
                YPP[ii] = numpy.max(numpy.min(YPP[ii], 1.0 - 10**-15), 10**-15)
            YPP = numpy.nan_to_num(YPP)

        if do_prob_range_correction and "ensemble" not in name:
            print("CORRECTING PROBABILITY WRT", name)
            YPP = (YPP - YPP.min()) / (YPP.max() - YPP.min())
            YPP = numpy.nan_to_num(YPP)

        nmin = min(nmax, len(Xt))
        if debug:
            for ii in range(nmin):
                print(out_format % (ii, YPC0[ii], numpy.round(YPP[ii], 6),
                                    is_match(YPC0[ii], YPP[ii]),
                                    -ii, YPC0[-ii], numpy.round(YPP[-ii], 6),
                                    is_match(YPC0[-ii], YPP[-ii])))
            print('-' * 80)
        return YPP

    if MULTICLASS:
        if hasattr(clf, "predict_proba"):
            YPP = clf.predict_proba(Xt)
        elif hasattr(clf, "decision_function"):
            YPP = clf.decision_function(Xt)

        YPP = pd.DataFrame(YPP)
        try:
            try:
                YPP.columns = CLASS_LABELS
            except:
                YPP.columns = CLASS_LABELS[1:]
        except Exception as err:
            print_exception('incomplete prediction categorical labels', err=err)

        for col in YPP:
            YPP[col] = numpy.nan_to_num(YPP[col])

            if do_prob_range_correction and "ensemble" not in name:
                print("CORRECTING PROBABILITY WRT", name)
                try:
                    YPP[col] = (YPP[col] - YPP[col].min()) / \
                        (YPP[col].max() - YPP[col].min())
                    YPP[col] = numpy.nan_to_num(YPP[col])
                except Exception as err:
                    print_exception('problem applying prob correction', err=err)

        print(name, 'YP')
        print(YPP.describe())
        print('-' * 80)

        return YPP
# #########################################################################


# #########################################################################
def get_YPC(YPP, threshold):
    if not threshold:
        threshold = 0.5

    if not MULTICLASS:
        YPC = YPP >= threshold
    else:
        YPC = YPP

    return YPC
# #########################################################################


# #########################################################################
def clf_predict(clf, Xt, yt,
                name, mode, threshold=None, correct=True, debug=False):

    banner("%s: %s" % (name, mode))

    YPP = clf_predict_wrt(clf, name, Xt)

    if not MULTICLASS:
        YPC = YPP >= threshold
        if "TEST" in mode.upper():
            if correct and not is_generative_classifier(name):
                (YPP, YPC) = apply_probability_correction(YPP, threshold)
            perf_metrics = None
            return YPP, YPC, threshold, perf_metrics

        if not threshold:
            threshold = find_threshold(yt, YPP)
        else:
            new_thresh = find_threshold(yt, YPP)
            print("threshold:", threshold, new_thresh, threshold == new_thresh)

            if "CV" not in mode.upper():
                threshold = new_thresh  # WARNING: PARAMETER TEST

        if correct and not is_generative_classifier(name):
            _, _, _, perf_metrics = compute_metrics_for(yt, YPP, threshold)
            (YPP, YPC) = apply_probability_correction(YPP, threshold)

            threshold = 0.5
            _, _, _, perf_metrics = compute_metrics_for(yt, YPP, threshold)
            YPC = YPP >= 0.5
        else:
            YPC = get_YPC(YPP, threshold)
            _, _, _, perf_metrics = compute_metrics_for(yt, YPP,
                                                        threshold, YPC=YPC)

        return YPP, YPC, threshold, perf_metrics

    if MULTICLASS:
        YPC = clf.predict(Xt)
        if "TEST" in mode.upper():
            if correct and not is_generative_classifier(name):
                (YPP, YPC) = apply_probability_correction(YPP, threshold)
            perf_metrics = {}
            return YPP, YPC, threshold, perf_metrics

        _, _, _, perf_metrics = compute_metrics_for(yt, YPP, threshold, YPC=YPC)

        return YPP, YPC, threshold, perf_metrics
# #########################################################################


# #########################################################################
def compute_metrics_for(yt, YPP, threshold, YPC=()):
    if not len(YPC):
        YPC = get_YPC(YPP, threshold)

    f1_val, logloss, auc_score = 0.0, 0.0, 0.0

    cmat = metrics.confusion_matrix(yt, YPC)
    cmat = pd.DataFrame(cmat)
    perf_metrics = metrics.classification_report(yt, YPC)

    if MULTICLASS:
        ll = mcls.multiclass_log_loss(yt, YPP, classes=CLASS_LABELS)
        print('-' * 80)
        print("FINAL THRESHOLD:", threshold)
        perf_metrics = {'metrics': perf_metrics,
                        'logloss': ll,
                        'cmat': cmat}

    if not MULTICLASS:
        f1_val = f1_score(yt, YPC)
        logloss = metrics.log_loss(yt, YPP)
        auc_score = roc_auc_score(yt, YPP)
        print('-' * 80)
        print("FINAL THRESHOLD:", threshold)
        print("FINAL F1_SCORE :", f1_val)
        print("FINAL LOGLOSS  :", logloss)
        print("FINAL ROC/AUC  :", auc_score)
        perf_metrics = {'metrics': perf_metrics,
                        'f1_score': f1_val,
                        'logloss': logloss,
                        'auc_score': auc_score,
                        'cmat': cmat}

    print("FINAL CMAT     :")
    print(cmat)
    print('-' * 80)

    return YPP, YPC, threshold, perf_metrics
# #########################################################################


# #########################################################################
def build_probability_correction_model(yp_matrix, y_t, K=2, DT=True):
    p_clf = GradientBoostingClassifier()
    try:
        if DT:
            params = {'n_estimators': (3, ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_depth': (32, ),
                      'verbose': (2, ),
                      'max_features': (1.00, ),
                      'max_leaf_nodes': (get_numnodes(8), ),
                      'min_samples_leaf': (get_leafsize(8), ),
                      'learning_rate': (1./8., )}
            p_clf = GradientBoostingClassifier()
        else:
            params = {'n_estimators': (128, ),
                      'criterion': (TREE_CRITERION, ),
                      'max_depth': (12, ),
                      'min_samples_leaf': (5, 11, 23),
                      'max_leaf_nodes': (4096, ),
                      'class_weight': ('subsample', None)}
            p_clf = RandomForestClassifier()

        p_clf = grid_search.GridSearchCV(p_clf, params, cv=K, error_score=0)
    except Exception as err:
        print_exception('problem building ensemble predictor', err=err)

    if 0:
        try:
            p_clf = CalibratedClassifierCV(p_clf, cv=K, method="sigmoid")
        except Exception as err:
            print_exception('problem calibrating ensemble predictor', err=err)

    try:
        p_clf.fit(yp_matrix, y_t)
    except Exception as err:
        print_exception('problem fitting ensemble predictor', err=err)

    print(p_clf)
    print('-' * 80)
    try:
        if hasattr(p_clf, "best_params_"):
            print('-' * 80)
            print("ENSEMBLE PARAMS", p_clf.best_params_)
            print('-' * 80)
    except Exception as err:
        print_exception('problem printing ensemble best parameters', err=err)
    return p_clf
# #########################################################################


# #########################################################################
def is_generative_classifier(name):
    if name in ['Gaussian', 'NB', 'LDA', 'QDA', 'Multinomial']:
        return True
    return False
# #########################################################################


# #########################################################################
def validate_vars(data,
                  which_vars=(),
                  not_in_set=(),
                  remove_duplicates=True,
                  with_target=True,
                  debug=False):

    if not len(which_vars):
        which_vars = [x for x in data.columns]

    if len(not_in_set):
        tvars = set(not_in_set)
        not_in_set = dict(list(zip(tvars, tvars)))

    which_vars = set_in_order_given(which_vars)

    dvars = [x for x in which_vars
             if x in data.columns and x not in not_in_set
             and TARGET_VAR not in x]

    if with_target:
        dvars = dvars + [TARGET_VAR, ]
        # print "TARGET_VAR", TARGET_VAR, "WAS ADDED"

    if debug:
        print('-' * 80)
        print("CURRENT VARSET", len(dvars), dvars)
        print('-' * 80)
    return dvars
# #########################################################################


# #########################################################################
def reduce_data(data,
                which_vars=(),
                not_in_set=(),
                with_target=True,
                debug=True):

    if debug:
        print('-' * 80)
        print("VARS IN DATA", [x for x in data.columns])
        print('-' * 80)
        try:
            print(data[TARGET_VAR].describe(include='all'))
        except:
            print('-' * 80)

    if not len(which_vars):
        which_vars = data.columns
        dvars = validate_vars(data,
                              not_in_set=not_in_set,
                              with_target=True)
    else:
        dvars = validate_vars(data,
                              which_vars=which_vars,
                              not_in_set=not_in_set,
                              with_target=with_target)
    data = data[[x for x in dvars if x in data.columns]]
    M, N = print_data_shape(data)
    return data, M, N
# #########################################################################


# #########################################################################
def incremental_reader(filename, debug=True, nlevels=32):
    DROPSET = []

    xxx = pd.read_csv(DATAFILES[DATASET],
                      sep=SEPARATOR,
                      na_values=NALIST,
                      nrows=10,
                      engine='c')
    data = pd.DataFrame()
    print(xxx.columns)
    print_data_shape(xxx)
    target = [i for i, x in enumerate(xxx.columns) if TARGET_VAR in x]

    for i in range(0, len(xxx.columns), INCREMENTAL_READ_BLOCKSIZE):
        # -----------------------------------------------------------------
        banner('READING CHUNK')
        colset = xxx.columns[i:i+INCREMENTAL_READ_BLOCKSIZE]
        colnums = list(range(i, i+len(colset)))
        print(i, i+min(INCREMENTAL_READ_BLOCKSIZE, len(colset)), colset, colnums)
        chunk = pd.read_csv(DATAFILES[DATASET],
                            sep=SEPARATOR,
                            na_values=NALIST,
                            nrows=FIRST_NROWS,
                            usecols=colnums+target,
                            engine='c')
        # -----------------------------------------------------------------
        # for c in chunk.columns:
        #   # colname = "%s_cvals" % c
        #   # chunk[c] = get_vals_from_codes(chunk[c],
        #                                    use_reciprocals=True, col=colname)
        # -----------------------------------------------------------------
        orig_chunk = pd.DataFrame()
        for c in chunk.columns:
            orig_chunk[c] = chunk[c]
        # -----------------------------------------------------------------
        step = max(len(chunk.columns)/CHUNKING_FEATGEN_STEPSIZE,
                   ABS_MIN_CHUNK_STEPSIZE)
        for j in range(0, len(chunk.columns), step):
            wrt = [x for x in chunk.columns if TARGET_VAR not in x][j:j+step]
            if len(wrt) < 2:
                continue
            try:
                banner('A: chunk reader as-is colset summary feature gen')
                print(j, j+step, wrt)
                P = get_projection_features_for(chunk[wrt],
                                                y=chunk[TARGET_VAR],
                                                ncols=1)
                for pcol in P.columns:
                    pcolname = "AVAR_%s_%s_%s" % (i+j, pcol, int(time.time()))
                    print('projection col for as-in chunk cols', pcol, pcolname)
                    data[pcolname] = P[pcol].copy()
            except Exception as err:
                print_exception('problem w/ as-in projection on chunk', err=err)
        # -----------------------------------------------------------------
        banner('ENCODING CHUNK COLUMNS')
        for c in chunk.columns:
            print('-' * 80)
            print(i, c)
            if TARGET_VAR in c:
                data[c] = orig_chunk[c].astype('int')
                print(data[c].describe(include='all'))
                continue

            vals = [x if pd.notnull(x) else NA_RECODE for x in chunk[c]]
            vals = pd.Series(vals)
            chunk[c] = vals.copy()
            orig_chunk[c] = vals.copy()
            levels = sorted(set(vals))
            L = len(levels)
            print(c, 'na_recoded representation done', get_levels(levels))

            try:
                if L < NUMERICAL_AS_CATEGORY_L_LIMIT:
                    raise Exception

                colname = "%s_zvals_%s" % (c, ZVALS_RECODING_DIGITS)
                zvals = get_zvals(vals)
                chunk[colname] = zvals.copy()
                orig_chunk[colname] = zvals.copy()
                levels = sorted(set(vals))
                print(c, 'zscore repr. created via', colname, get_levels(levels))

                colname = "%s_cat" % c
                vals = get_codes_from_categorical(vals, col=colname)
                chunk[colname] = vals.copy()
                orig_chunk[colname] = vals.copy()
                levels = sorted(set(vals))
                print(c, 'category repr. created:', colname, get_levels(levels))
                print(c, 'was numerical; processing successfully completed')

            except:
                vals = get_codes_from_categorical(vals, col=colname)
                chunk[c] = vals.copy()
                orig_chunk[colname] = vals.copy()
                levels = sorted(set(vals))
                print(c, 'category repr. created:', colname, get_levels(levels))
                print(c, 'was categorical; processing successfully completed')

            chunk[c] = chunk[c].astype(NUMERICAL_DATATYPE)
            levels = sorted(set(vals))
            L = len(levels)
            # print c, "COL has:", get_levels(levels)
        # -----------------------------------------------------------------
            if L < DROP_VARS_WITH_LESS_THAN_NLEVELS:
                del chunk[c]
                del orig_chunk[c]
                print(c, 'deleted col with less than 2 levels ...')
                print('-' * 80)

        # -----------------------------------------------------------------
        try:
            fselect_vars = feature_select(chunk,
                                          chunk[TARGET_VAR],
                                          chunk.columns,
                                          p=INCREMENTAL_READ_FSELPERC,
                                          min_score=INCREMENTAL_READ_MINSCORE,
                                          sorted_by_importance=True)
            fselect_drop = [x for x in chunk.columns
                            if x not in fselect_vars
                            and TARGET_VAR not in x]
            # -------------------------------------------------------------
            banner('B: chunk reader fselect selected vars fgen')
            P = get_projection_features_for(chunk[fselect_vars],
                                            y=chunk[TARGET_VAR],
                                            ncols=1)
            for pcol in P.columns:
                pcolname = "BVAR_%s_%s_%s" % (i, pcol, int(time.time()))
                print('projection col for fselect keep', pcol, pcolname)
                data[pcolname] = P[pcol].copy()
            # -------------------------------------------------------------
            banner('C: chunk reader fselect non-selected/dropped vars fgen')
            P = get_projection_features_for(chunk[fselect_drop],
                                            y=chunk[TARGET_VAR],
                                            ncols=1)
            for pcol in P.columns:
                pcolname = "CVAR_%s_%s_%s" % (i, pcol, int(time.time()))
                print('projection col for fselect drop', pcol, pcolname)
                data[pcolname] = P[pcol].copy()

            DROPSET.extend(fselect_drop)
            # -------------------------------------------------------------
            for v in chunk.columns:
                if v in fselect_vars:
                    continue
                try:
                    if v not in TARGET_VAR:
                        print('dropping not fselected column', v)
                        del chunk[v]
                except Exception as err:
                    print_exception('problem correlation fselection', err=err)
            # -------------------------------------------------------------
        except Exception as err:
            print_exception('problem correlation fselection', err=err)
        # -----------------------------------------------------------------
        banner('LOCAL DECORRELATION')
        try:
            Q, keep, drop = decorrelate(chunk)  # , colset=fselect_vars)
            keepcols = [x for x in chunk.columns if x not in drop]
            # -------------------------------------------------------------
            wrt = [x for x in chunk.columns
                   if x in list(drop.keys()) and TARGET_VAR not in x]
            banner('D: chunk reader decorrelation dropped vars fgen')
            P = get_projection_features_for(chunk[wrt], y=chunk[TARGET_VAR])
            for pcol in P.columns:
                pcolname = "DVAR_%s_%s_%s" % (i, pcol, int(time.time()))
                print('projection col for decorrelate drop', pcol, pcolname)
                data[pcolname] = P[pcol].copy()
            # -------------------------------------------------------------
            chunk, M, N = reduce_data(chunk, which_vars=keepcols)
        except Exception as err:
            print_exception('problem with decorrelation processing', err=err)
        # -----------------------------------------------------------------
        banner('CURRENT DATA FRAME')
        for col in chunk.columns:
            data[col] = orig_chunk[col]
        if debug:
            print('-' * 80)
            print("COLUMNS", [x for x in data.columns])
            print('-' * 80)
        print_data_shape(data)
        # -----------------------------------------------------------------

    banner('FINAL DATA FRAME')
    cols = validate_vars(data,
                         which_vars=[x for x in data.columns])
    print("INCREMENTAL READER, COLUMNS", cols)
    print('-' * 80)
    data = data[cols]
    print_data_shape(data)
    return data, DROPSET
# #########################################################################


# #########################################################################
def preselected_columns_read(datafile):
    xxx = pd.read_csv(datafile,
                      sep=SEPARATOR,
                      na_values=NALIST,
                      nrows=10,
                      engine='c')
    M, N = print_data_shape(xxx)
    # ---------------------------------------------------------------------
    PROBLEM_COLS = [10, 11, 12, 157, 158, 167, 177, 214, 231]
    RANDOM_COLSET = numpy.random.randint(1, 1934, RANDOM_COLSET_AUGMENTATION)
    SELECTED_COLNUMS = [i for i, c in enumerate(xxx.columns)
                        if (c in SELECTED or i in RANDOM_COLSET)
                        and i not in PROBLEM_COLS]
    del xxx
    # ---------------------------------------------------------------------
    data = pd.read_csv(DATAFILES[DATASET],
                       sep=SEPARATOR,
                       na_values=NALIST,
                       nrows=FIRST_NROWS,
                       usecols=SELECTED_COLNUMS,
                       engine='c')
    M, N = print_data_shape(data)

    data = data[[c for c in data.columns if c in SELECTED]]
    return data
# #########################################################################


# #########################################################################
def evaluate_training_dataset(c1, c0, cv1, cv0,
                              verify_wrt_full_dataset=True,
                              f1_min=0.96, ll_max=0.30, auc_min=0.88):
    ok = False

    def get_xvars(X):
        xvars = [x for x in X if TARGET_VAR not in x
                 and ID_VAR not in x and LABEL_VAR not in x]
        return xvars

    try:
        def terminate_sampling(f1, auc, ll):
            if f1 > f1_min:
                print('f1 value  goal was met')
                return True
            if ll < ll_max:
                print('log-loss  goal was met')
                return True
            if auc > auc_min:
                print('auc score goal was met')
                return True
            return False

        X = pd.concat([c1, c0])
        X = X[get_xvars(X)]
        y = pd.concat([c1[TARGET_VAR], c0[TARGET_VAR]])

        banner('verifying train set feature_importances')
        _, t_clf = get_feature_impt_dropset(X, y)

        if verify_wrt_full_dataset:
            Xt = pd.concat([c1, c0, cv1, cv0])
            Xt = Xt[get_xvars(Xt)]
            yt = pd.concat([c1[TARGET_VAR], c0[TARGET_VAR],
                            cv1[TARGET_VAR], cv0[TARGET_VAR]])
        else:
            Xt = pd.concat([cv1, cv0])
            Xt = Xt[get_xvars(Xt)]
            yt = pd.concat([cv1[TARGET_VAR], cv0[TARGET_VAR]])

        ypc = t_clf.predict(Xt)
        if hasattr(t_clf, "predict_proba"):
            ypp = t_clf.predict_proba(Xt)
            ypp = ypp[:, 1]
        elif hasattr(t_clf, "decision_function"):
            ypp = t_clf.decision_function(Xt)
        print()

        cmat = metrics.confusion_matrix(yt, ypc)
        cmat = pd.DataFrame(cmat)
        f1_val = f1_score(yt, ypc)
        logloss = metrics.log_loss(yt, ypp)
        aucval = roc_auc_score(yt, ypp)
        print()

        print("f1val for training sample: %.3f goal: %.3f" % (f1_val, f1_min))
        print("lloss for training sample: %.3f goal: %.3f" % (logloss, ll_max))
        print("auc   for training sample: %.3f goal: %.3f" % (aucval, auc_min))
        print('-' * 80)
        print(cmat)
        print('-' * 80)
        ok = terminate_sampling(f1_val, aucval, logloss)
    except Exception as err:
        print_exception('problem with testing sample set preds', err=err)
    return ok, aucval
# #########################################################################


# #########################################################################
# delete the label var column for training classifier purposes
# #########################################################################
def delete_var_from(dataset, labeled_var=LABEL_VAR):
    try:
        lvars = [x for x in dataset if labeled_var in x]
        print('deleting', lvars)
        if len(lvars):
            for lvar in lvars:
                if lvar in dataset:
                    del dataset[lvar]
    except Exception as err:
        print_exception('during labeled var %s delete' % labeled_var,
                        err=err, with_traceback=True)
    return dataset
# #########################################################################


# #########################################################################
def in_limit(n, nmin=2, nmax=256):
    n = max(n, nmin)
    n = min(n, nmax)
    return n
# #########################################################################


# #########################################################################
def add_random_centroid_for(data, cx=None,
                            f=CENTROID_SAMPLING_FRACTION,
                            min_cluster_size=MIN_VALID_CLUSTER_SIZE,
                            max_sampling_size=MAX_SAMPLES_TO_CLUSTER,
                            n=1,
                            nc=32,
                            approach=CENTROID_APPROACH,
                            fast_mode=False, do_replace=False, debug=False):

    cx_sample = data[data[TARGET_VAR] == cx].sample(frac=f, replace=do_replace)
    cx_size = len(cx_sample)
    if cx_size > max_sampling_size:
        cx_sample = cx_sample.sample(n=max_sampling_size, replace=do_replace)
        cx_size = len(cx_sample)
    num_clusters = in_limit(nc, nmin=2, nmax=len(cx_sample)-1)
    min_cluster_size = in_limit(min_cluster_size,
                                nmin=cx_size/nc-1,
                                nmax=cx_size/nc*2-1)

    if not fast_mode:
        try:
            clusterer = KMeans(n_clusters=num_clusters, max_iter=500, n_init=40)
            cmappings = clusterer.fit(cx_sample)
            clabeling = cmappings.labels_
            p = len(set(clabeling))

            sizes = [(len(cx_sample[clabeling == j]), j) for j in range(p)]
            sizes = sorted(sizes, reverse=True)
            print("clustering found[min_size=%s]:" % min_cluster_size, p, sizes, end=' ')

            cx_random_centroids = []
            for j in range(p):
                if debug:
                    print("%s:" % j, end=' ')
                cluster_idx = sizes[j][1]
                cx_sample_j = cx_sample[clabeling == cluster_idx]
                cx_size_j = len(cx_sample_j)
                if cx_size_j < min_cluster_size:
                    print('...', end=' ')
                    continue
                if "medoid" in approach:
                    cx_centroid_j = \
                        cx_sample_j.median(skipna=False).round(decimals=n)
                else:
                    cx_centroid_j = \
                        cx_sample_j.mean(skipna=False).round(decimals=n)
                cx_centroid_j = numpy.nan_to_num(cx_centroid_j)
                cx_random_centroids.append(cx_centroid_j.copy())
                if debug:
                    print('|c_sample(j=%s)|= %s' % (cluster_idx, cx_size_j), end=' ')
            print('...done [%s]' % len(cx_random_centroids))
            return cx_random_centroids

        except Exception as err:
            print_exception('during class %s clustering' % cx,
                            err=err, with_traceback=True)

    if "medoid" in approach:
        cx_centroid = cx_sample.median(skipna=False).round(decimals=n)
    else:
        cx_centroid = cx_sample.mean(skipna=False).round(decimals=n)
    cx_centroid = numpy.nan_to_num(cx_centroid)
    return [cx_centroid.copy(), ]
# #########################################################################


# #########################################################################
def time_exceeded(t_start, quota):
    if time.time() - t_start > quota:
        return True
    return False
# #########################################################################


# #########################################################################
def augment_with_per_class_random_centroids(data,
                                            nc=NC0_CLUSTERS,
                                            preprocess=False,
                                            add_medoids=True,
                                            kmax=PERCLASS_RANDOM_CENTROIDS,
                                            debug=False):

    if BYPASS_ADD_CENTROIDS:
        print('BYPASS_ADD_CENTROIDS is enabled')
        return data

    if preprocess:
        data = apply_scaling(data)
        for col in data:
            try:
                data[col] = [x if pd.notnull(x) else NA_RECODE
                             for x in data[col]]
            except:
                # data[col] = pd.Categorical.from_array(data[col])
                data[col] = pd.Categorical(data[col])
                data[col] = data[col].codes
        print('original size (before centroids)', data.shape)

    c = data.transpose()

    classes = [int(x) for x in set(data[TARGET_VAR])]
    print("CLASSES PROCESSED BY CENTROID AUGMENTATION: ", classes)

    class_sets, centroid_set, t_start = {}, {}, time.time()
    for ci in classes:
        banner("centroid augmentation for class: %s" % ci)
        if ci == TESTING_SET:
            continue

        class_sets[ci] = data[data[TARGET_VAR] == ci]
        # kmax = in_limit(kmax, nmin=8, nmax=len(class_sets[ci])/nc)
        print(ci, len(class_sets[ci]))
        for k in range(kmax):
            c_random_centroids = add_random_centroid_for(data, cx=ci, nc=nc)
            for j in range(len(c_random_centroids)):
                cix = k*kmax+j
                c["c%s_random_centroid_%s" % (ci, cix)] = \
                    c_random_centroids[j].copy()
            if time_exceeded(t_start, CENTROID_MAX_COMPUTE_TIME):
                break

        if add_medoids:
            centroid_set[ci] = class_sets[ci].median(skipna=False)
            c['c%s_overall_medoid' % ci] = centroid_set[ci].copy()

        centroid_set[ci] = class_sets[ci].mean(skipna=False)
        c['c%s_overall_centroid' % ci] = centroid_set[ci].copy()

        for k in range(int(kmax/3)+1):
            c['c%s_sample_medoid_%s' % (ci, k)] = class_sets[ci].sample(
                frac=CENTROID_SAMPLING_EFFORT).median(skipna=False).copy()
            centroid_set[ci] = class_sets[ci].sample(
                frac=CENTROID_SAMPLING_EFFORT).mean(skipna=False)
            c['c%s_sample_centroid_%s' % (ci, k)] = centroid_set[ci].copy()
            if time_exceeded(t_start, CENTROID_MAX_COMPUTE_TIME):
                break

        centroid_added_cols = [col for col in c.columns
                               if 'c%s_' % ci in str(col)
                               and 'centroid' in str(col)]

        if debug:
            print()
            t = c[centroid_added_cols].copy()
            t.index = data.columns
            print(t.describe())
            print('-' * 80)

    data = c.transpose().copy()
    print('augmented size (after centroids)', data.shape)
    return data
# #########################################################################


# #########################################################################
def split_dataset_by_classes_into_sets(data,
                                       kmax=10,
                                       randomize=False):
    global C1, C0
    data = data[sorted([x for x in set(data.columns)
                        if TARGET_VAR not in x and ID_VAR not in x
                        and LABEL_VAR not in x]) + [TARGET_VAR, ]]

    # P: what fraction of the whole data will be used, default all
    M, N = data.shape
    P = int(M * PERCENT_OF_DATA_TO_USE)

    # initialization of the label_var
    data[LABEL_VAR] = TESTING_SET

    # extract the test chunk and label it testing_set (not class0 nor class1)
    ct = data[data[TARGET_VAR] == TESTING_SET]

    # extract class1 chunk and label it class c1
    c1 = data[data[TARGET_VAR] == CLASS1]   # .copy()
    c1_initial = len(c1)
    c1[LABEL_VAR] = CLASS1

    # extract class0 chunk and label it class c0
    c0 = data[data[TARGET_VAR] == CLASS0]   # .copy()
    c0_initial = len(c0)
    c0[LABEL_VAR] = CLASS0

    # initial randomization, shuffle the c1 and c0 samples
    if randomize:
        c1 = c1.sample(frac=1.0, replace=False, random_state=get_random_seed())
        c0 = c0.sample(frac=1.0, replace=False, random_state=get_random_seed())

    # how many unique samples are available on each class given dataset in use
    c1_data_cap = min(int(P * C1_RATIO), len(c1))
    c0_data_cap = min(int(P * C0_RATIO), len(c0))

    # a training ratio between class1 and class0 was requested, compute it
    c1_training_cap = int(PERCENT_OF_SAMPLES_TO_TRAIN_WITH * c1_data_cap)
    c0_training_cap = int(PERCENT_OF_SAMPLES_TO_TRAIN_WITH * c0_data_cap)

    # label a portion of each class for training
    c1_valid_idx = dict(list(zip(list(range(c1_training_cap)), list(range(c1_training_cap)))))
    c0_valid_idx = dict(list(zip(list(range(c0_training_cap)), list(range(c0_training_cap)))))

    # training and xval labeling for each class
    c1[LABEL_VAR] = [CLASS1_TRAINWITH
                     if i in c1_valid_idx
                     else CLASS1 for i in range(len(c1))]
    c0[LABEL_VAR] = [CLASS0_TRAINWITH
                     if i in c0_valid_idx
                     else CLASS0 for i in range(len(c0))]

    # shuffle the c1 and c0 samples
    c1 = c1.sample(frac=1.0, replace=False, random_state=get_random_seed())
    c0 = c0.sample(frac=1.0, replace=False, random_state=get_random_seed())

    # extract training chunks for each class
    c1_training = c1[c1[LABEL_VAR] == CLASS1_TRAINWITH]
    c0_training = c0[c0[LABEL_VAR] == CLASS0_TRAINWITH]

    # extract cross validation chunks for each class
    c1_cv = c1[c1[LABEL_VAR] == CLASS1]
    c0_cv = c0[c0[LABEL_VAR] == CLASS0]

    # delete the labeling var used to project out the sets
    c1 = delete_var_from(c1, labeled_var=LABEL_VAR)
    c0 = delete_var_from(c0, labeled_var=LABEL_VAR)
    c1_training = delete_var_from(c1_training, labeled_var=LABEL_VAR)
    c0_training = delete_var_from(c0_training, labeled_var=LABEL_VAR)
    c1_cv = delete_var_from(c1_cv, labeled_var=LABEL_VAR)
    c0_cv = delete_var_from(c0_cv, labeled_var=LABEL_VAR)
    ct = delete_var_from(ct, labeled_var=LABEL_VAR)
    data = delete_var_from(data, labeled_var=LABEL_VAR)

    # if oversampling is in use, augment the corresponding training caps
    # and then augment the training chunks accordingly
    NC = max(c1_training_cap, c0_training_cap)
    c0_is_c1_times_greater = \
        int(len(c0_training)/len(c1_training) * C1_OVERSAMPLING/C0_OVERSAMPLING)
    k1max = max(min(c0_is_c1_times_greater, kmax), 1)
    k0max = int(1/k1max) + 1
    print("kmax = %s, %s" % (k1max, k0max))

    d1_training = c1_training.copy()
    if C1_WITH_REPLACEMENT:
        c1_training_cap = int(max(1.-C1_OVERSAMPLING, 0.05) * NC)
        if C1_OVERSAMPLING < 1:
            c1_training_cap = int(max(C1_OVERSAMPLING, 0.05) * NC)
        d1_training = c1_training.sample(c1_training_cap, replace=True,
                                         random_state=get_random_seed())
        for k in range(k1max):
            d1_training = pd.concat([c1_training, d1_training])

    d0_training = c0_training.copy()
    if C0_WITH_REPLACEMENT:
        c0_training_cap = int(max(1.-C0_OVERSAMPLING, 0.05) * NC)
        if C0_OVERSAMPLING < 1:
            c0_training_cap = int(max(C0_OVERSAMPLING, 0.05) * NC)
        d0_training = c0_training.sample(c0_training_cap, replace=True,
                                         random_state=get_random_seed())
        for k in range(k0max):
            d0_training = pd.concat([c0_training, d0_training])

    banner('testing sample set wrt predictive power')
    aucvals, ok, j, jmax = [0., ], False, 0, 0
    while not ok:
        print('-' * 80)
        print("%3s C1:%8s %8s C0:%8s %8s" % (j,
                                             c1_training_cap, len(d1_training),
                                             c0_training_cap, len(d0_training)))

        ok, aucval = evaluate_training_dataset(d1_training, d0_training,
                                               c1_cv, c0_cv)
        aucvals.append(aucval)
        print("AUCs: %s" % numpy.array(aucvals).round(decimals=3))

        if ok:
            print('condition met')
            break

        if j >= jmax:
            print('iter exceeded')
            break

        k1max = int(k1max * 0.95)
        k0max = int(k0max * 0.95)
        k1max = max(min(k1max, kmax), 1)
        k0max = max(min(k0max, kmax), 1)
        print("kmax = %s, %s" % (k1max, k0max))

        d1_training = c1_training.copy()
        if C1_WITH_REPLACEMENT:
            c1_training_cap = int(max(c1_training_cap * (1. - 0.02), 0.05) * NC)
            d1_training = c1_training.sample(c1_training_cap, replace=True,
                                             random_state=get_random_seed())
            for k in range(k1max):
                d1_training = pd.concat([c1_training, d1_training])

        d0_training = c0_training.copy()
        if C0_WITH_REPLACEMENT:
            c0_training_cap = int(max(c0_training_cap * (1. + 0.01), 0.05) * NC)
            d0_training = c0_training.sample(c0_training_cap, replace=True,
                                             random_state=get_random_seed())
            for k in range(k0max):
                d0_training = pd.concat([c0_training, d0_training])

        j = j + 1
    print('-' * 80)

    if C1_WITH_REPLACEMENT:
        c1_training = d1_training.copy()

    if C0_WITH_REPLACEMENT:
        c0_training = d0_training.copy()

    banner("c1: class balancing stats")
    print("C1 INITIAL         : ", c1_initial)
    print("C1 ORIGINAL        : ", c1.shape)
    print("C1 TRAINING WITH   : ", c1_training.shape)
    print("C1 XVALIDATING WITH: ", c1_cv.shape)
    print('-' * 80)
    print("C1 DATA CAP:       : ", c1_data_cap)
    print("C1 TRAINING CAP    : ", c1_training_cap)
    print("C1 OVERSAMPLING?   : ", C1_WITH_REPLACEMENT, C1_OVERSAMPLING)
    print('-' * 80)

    banner("c0: class balancing stats")
    print("C0 INITIAL         : ", c0_initial)
    print("C0 ORIGINAL        : ", c0.shape)
    print("C0 TRAINING WITH   : ", c0_training.shape)
    print("C0 XVALIDATING WITH: ", c0_cv.shape)
    print('-' * 80)
    print("C0 DATA CAP:       : ", c0_data_cap)
    print("C0 TRAINING CAP    : ", c0_training_cap)
    print("C0 OVERSAMPLING?   : ", C0_WITH_REPLACEMENT, C0_OVERSAMPLING)
    print('-' * 80)

    if len(ct):
        banner("ct: class balancing stats")
        print("CT ORIGINAL        : ", ct.shape)
        print('-' * 80)

    # remember the size of the classes used for training
    C1, C0 = len(c1_training), len(c0_training)

    # put together the class0 and class1 training samples and shuffle them
    training_set = pd.concat([c1_training.copy(), c0_training.copy()])
    training_set = training_set.sample(frac=1.0, replace=False,
                                       random_state=get_random_seed()).copy()

    # remember the name of the columns as they were drop by numpy
    data_columns = [x for x in training_set.columns]

    # enforce the specified development set take aside samples
    DEV_PIVOT = int(len(training_set) * (1.0 - DEV_SPLIT)) - 1

    # split original training set into a train set and a dev set
    # and put together the class0 and class1 cv samples and shuffle them
    train_set = pd.DataFrame(training_set.values[:DEV_PIVOT, :])
    dev_set = pd.DataFrame(training_set.values[DEV_PIVOT:, :])
    cv_set = pd.concat([c1_cv, c0_cv]).sample(frac=1.0, replace=False,
                                              random_state=get_random_seed())

    # the test set is ready, already
    test_set = ct

    # name the columns back again
    train_set.columns = data_columns
    dev_set.columns = data_columns
    cv_set.columns = data_columns
    test_set.columns = data_columns

    return train_set.copy(), dev_set.copy(), cv_set.copy(), test_set.copy()
# #########################################################################


# #########################################################################
def describe_datasets(train_set, dev_set, xval_set, test_set):
    banner('datasets')
    return
    # colset = list(range(0, 2, 1)) + list(range(-2, 0, 1))
    colset = None #list(range(0, 2, 1)) + list(range(-2, 0, 1))
    print('-' * 80)
    print("TRAIN", train_set.shape)
    print(train_set[colset].describe(include='all'))
    print('-' * 80)
    print("DEV  ", dev_set.shape)
    print(dev_set[colset].describe(include='all'))
    print('-' * 80)
    print("XVAL ", xval_set.shape)
    print(xval_set[colset].describe(include='all'))
    print('-' * 80)
    if len(test_set):
        print("TEST ", test_set.shape)
        print(test_set[colset].describe(include='all'))
        print('-' * 80)
    return
# #########################################################################


# #########################################################################
def preprocess_multiclass_class_labels(data, debug=False):
    global LABELS, CLASS_LABELS

    # data[TARGET_VAR] = pd.Categorical.from_array(data[TARGET_VAR])
    data[TARGET_VAR] = pd.Categorical(data[TARGET_VAR])
    data[TARGET_VAR] = [x if pd.notnull(x) and x else TESTING_SET
                        for x in data[TARGET_VAR]]
    print(data[TARGET_VAR].describe(include='all'))
    print('-' * 80)

    # LABELS = pd.Categorical.from_array(data[TARGET_VAR]).categories
    LABELS = pd.Categorical(data[TARGET_VAR]).categories
    CLASS_LABELS = [x for x in LABELS if str(x) != '-1']  # + [-1, ]
    set_classifier_labels(labels=LABELS, class_labels=CLASS_LABELS)

    print("CLASS LABELS", CLASS_LABELS)
    print(data[TARGET_VAR].describe(include='all'))
    print('-' * 80)

    CLASS_LABEL_ENC = LabelEncoder()
    CLASS_LABEL_ENC.fit(LABELS)
    CLASSES = list(CLASS_LABEL_ENC.classes_)

    A = CLASS_LABEL_ENC.transform(CLASSES)
    B = CLASS_LABEL_ENC.inverse_transform(A)
    MAPPING, INVERSE_MAPPING = dict(list(zip(A, B))), dict(list(zip(B, A)))
    print("ENCODER_MAPPING:", end=' ')
    print('-' * 80)
    print('%16s     %16s' % ("-" * 12, "-" * 12))
    print('%16s     %16s' % ("PREDICTION_CODE", "ORIGINAL_VAL"))
    print('%16s     %16s' % ("-" * 12, "-" * 12))
    for k in MAPPING:
        print('%16s --> %16s' % (k, MAPPING[k]))
    print('%16s     %16s' % ("-" * 12, "-" * 12))
    print('-' * 80)

    print('-' * 80)
    print('%32s     %16s' % ("-" * 32, "-" * 12))
    print('%32s     %16s' % ("ORIGINAL_VAL", "PREDICTION_CODE"))
    print('%32s     %16s' % ("-" * 32, "-" * 12))
    for k in INVERSE_MAPPING:
        print('%16s --> %16s' % (k, INVERSE_MAPPING[k]))
    print('%32s     %16s' % ("-" * 32, "-" * 12))
    print('-' * 80)

    try:
        for tag in ['DOMINANT_CATLABEL_TOP', 'DOMINANT_CATLABEL_2ND',
                    'DOMINANT_CATLABEL_3RD']:
            data[tag] = [INVERSE_MAPPING[x]
                         if x in INVERSE_MAPPING else NA_RECODE
                         for x in data[tag]]
    except Exception as err:
        print(err)
        print('-' * 80)

    ORIGINAL_TARGET_VAR = data[TARGET_VAR].copy()
    try:
        data[TARGET_VAR] = CLASS_LABEL_ENC.\
            transform(data[TARGET_VAR])
    except Exception as err:
        print(err)
        data[TARGET_VAR] = CLASS_LABEL_ENC.\
            transform([str(x) for x in data[TARGET_VAR]])

    DECODED_Y = CLASS_LABEL_ENC.inverse_transform(data[TARGET_VAR])
    assert list(DECODED_Y) == [x for x in ORIGINAL_TARGET_VAR]
    print('-' * 80)

    return data, (LABELS, CLASS_LABELS, CLASS_LABEL_ENC, CLASSES,
                  MAPPING, INVERSE_MAPPING)
# #########################################################################


# #########################################################################
# augmentation with marginals
# #########################################################################
def add_marginal_variables(data, pmax=16, nmax=400):

    def is_valid_marginal(data, c1, c2):
        if c1 in data and c2 in data:
            L1, L2 = len(set(data[c1])), len(set(data[c2]))
            if L1 > 1 and L2 > 1:
                return True
        print(c1, c2, 'is not suitable marginal combination')
        return False

    FACTOR_LEVEL_VARIABLES = [x for x in data if 'FSEL_' in x]

    if not BYPASS_INCREMENTAL_STEP_FITTER_FACTORSELECTOR:
        if len(FACTOR_LEVEL_VARIABLES):
            MARGINAL_VARS_UPDATE = \
                step_fitter(data, colset=FACTOR_LEVEL_VARIABLES)

            if len(MARGINAL_VARS_UPDATE):
                MARGINAL_VARS.extend(MARGINAL_VARS_UPDATE)

    TOPVARS = []
    try:
        colset = [x for x in data if TARGET_VAR not in x and 'MARGIN' not in x]
        if len(colset):
            TOPVARS = feature_select(data, data[TARGET_VAR], colset,
                                     p=25./100., min_score=100,
                                     sorted_by_importance=True)
    except Exception as err:
        print_exception('problem while identifying top features', err=err)

    AUGMENTED_MARGINAL_VARS = set_in_order_given(MARGINAL_VARS+TOPVARS)

    banner('GENERATING MARGINALS WRT: %s' % ','.join(AUGMENTED_MARGINAL_VARS))
    for c1 in AUGMENTED_MARGINAL_VARS:
        SHUFFLED_MARGINAL_VARS = [x for x in AUGMENTED_MARGINAL_VARS]
        random.shuffle(SHUFFLED_MARGINAL_VARS)
        for c2 in SHUFFLED_MARGINAL_VARS[:pmax]:
            if c2 == c1:
                continue
            cols = sorted([c1, c2])
            if is_valid_marginal(data, c1, c2):
                colname = "MARGINAL_%s_BY_%s" % (cols[0], cols[1])
                if colname not in data:
                    banner(colname)
                    data[colname] = get_marginal_wrt(data, cols[0], cols[1])
                    print('-' * 80)

        if len([x for x in data if "MARGINAL" in x]) > nmax:
            break

    return data
# #########################################################################


# #########################################################################
# DATA LOADING
# #########################################################################
def read_data():
    DROPSET = []
    try:
        if USE_INCREMENTAL_READ:
            data, DROPSET = incremental_reader(DATAFILES[DATASET])
        elif USE_PRESELECTED_FEATURES:
            data = preselected_columns_read(DATAFILES[DATASET])
        else:
            raise Exception
    except Exception as err:
        print_exception('defaulting to standard read', err=err)

        banner('default reader: standard reader')
        data = pd.read_csv(DATAFILES[DATASET],
                           sep=SEPARATOR,
                           na_values=NALIST,
                           nrows=FIRST_NROWS,
                           engine='c')

    banner('original columns read')
    print(data.describe(include='all'))
    print('-' * 80)
    print(data.columns)
    print('-' * 80)
    return data, DROPSET
# #########################################################################


# #########################################################################
def generate_categorical_frequencies(data, debug=False):
    dropset = []
    for j, col in enumerate(data.columns):
        banner('ORIGINAL VARIABLE: data[%s-%s]' % (j, col))
        if ID_VAR in col:
            continue

        try:
            if debug:
                print(j, col)
                print(data[col].describe(include='all'))
                print("    " + '-' * 76)

            vals = [x if pd.notnull(x) else NA_RECODE for x in data[col]]
            # vals = pd.Categorical.from_array(vals)
            vals = pd.Categorical(vals)
            if TARGET_VAR in col:
                print(vals.describe())
                continue

            levels = set(vals)
            L = len(levels)
            if L < 2:
                print(col, 'deleting col', levels)
                dropset.append(col)
                continue

            if debug:
                freqs = vals.describe()
                print(freqs)
                print("    " + '-' * 76)

            if not BYPASS_ADD_CATEGORICAL_FREQUENCIES:
                if L > MAX_NUM_FACTOR_LEVELS:
                    vals = nleveling(vals, col=col)
                    levels = sorted(set(vals))
                    L = len(levels)
                    if BYPASS_EARLY_REDUCE_LARGE_FACTORS:
                        print(col, 'reduced in levels to', L)
                        data[col] = vals.copy()

                if L <= MAX_NUM_FACTOR_LEVELS:
                    P = get_freqs(vals, col=col)
                    for colname in P:
                        data[colname] = P[colname].copy()
                else:
                    print(col, 'not freq-analyzed because:', get_levels(levels))

        except Exception as err:
            print_exception('problem with column', err=err)

    return data, dropset
# #########################################################################


# #########################################################################
def identify_large_factors(data):
    large_factor_colset = []
    for col in [col for col in data.columns if col not in [ID_VAR, TARGET_VAR]]:
        vals = sorted([x if pd.notnull(x) else NA_RECODE for x in data[col]])
        levels = sorted(set([NA_RECODE, ] + vals))
        L = len(levels)
        if L > UPPER_FACTOR_LEVEL_LIMIT:
            try:
                vals.mean() - vals.min()
                large_factor_colset.append(col)
                print(col, 'is large factor col, levels=', L)
            except:
                print(col, 'skipped')
    return large_factor_colset
# #########################################################################


# #########################################################################
def process_large_factors(data, large_factor_colset):
    for col in [col for col in large_factor_colset
                if col in data and col not in [ID_VAR, TARGET_VAR]]:
        print('-' * 80)
        vals = sorted([x if pd.notnull(x) else NA_RECODE for x in data[col]])
        levels = sorted(set([NA_RECODE, ] + vals))
        L = len(levels)
        if L > UPPER_FACTOR_LEVEL_LIMIT:
            try:
                vals = nleveling(vals, col=col)
                data[col] = vals.copy()
            except:
                print(col, 'skipped')
    return data
# #########################################################################


# #########################################################################
def generate_interaction_terms(data):
    interactions = get_interaction_terms(data, fast_mode=False)

    banner('GENERATING INTERACTION_TERMS WRT: %s' % str(interactions))

    for interaction in interactions:
        try:
            v1, v2 = sorted(interaction)
            colname = "XTERM_%s_BY_%s" % (v1, v2)
            if colname not in data:
                print('interaction term:', colname)
                vals = data[v1] * data[v2]
                vals = nleveling(vals, col=colname)
                L = len(set(vals))
                if is_valid_xterm(L, colname):
                    data[colname] = apply_scaler_for_column(xvals=vals,
                                                            y=data[TARGET_VAR],
                                                            colname=colname)
                print('    ' + '-' * 76)
        except Exception as err:
            print_exception('problem w/ column', err=err, with_traceback=True)
            print(interaction, 'interaction term(s) skipped', err)
    return data
# #########################################################################


# #########################################################################
def generate_higher_order_interactions(data):
    if not BYPASS_HIGHER_INTERACTION_TERMS:
        first_order_terms = [x for x in INTERACTION_TERMS
                             if x in data and x not in [TARGET_VAR, ID_VAR]]

        interactions = get_interaction_terms(data, fast_mode=False)
        for interaction in interactions:
            for v0 in first_order_terms[:EXAMINE_HIGHER_TERMS_UPTO]:
                if v0 not in data:
                    continue
                elif v0 in [TARGET_VAR, ID_VAR]:
                    continue
                elif v0 in interaction:
                    continue

                try:
                    v1, v2 = sorted(interaction)
                    colname = "XTERM_%s_BY_%s" % (v1, v2)
                    if colname not in data:
                        continue

                    v0, v1, v2 = sorted([v0, v1, v2])
                    colname = "XTERM_%s_BY_%s_BY_%s" % (v0, v1, v2)
                    if colname not in data:
                        print('interaction term:', colname)
                        vals = data[v0] * data[v1] * data[v2]
                        vals = nleveling(vals, col=colname)
                        L = len(set(vals))
                        if is_valid_xterm(L, colname):
                            data[colname] = \
                                apply_scaler_for_column(xvals=vals,
                                                        y=data[TARGET_VAR],
                                                        colname=colname)
                        print('    ' + '-' * 76)
                except Exception as err:
                    print_exception('problem w/ column', err=err,
                                    with_traceback=False)
                    print(interaction, 'interaction term(s) skipped')
    return data
# #########################################################################


# #########################################################################
def generate_square_terms(data):
    interactions = get_interaction_terms(data, ntop=BYPASS_SQUARING_AFTERTOP)
    for interaction in interactions:
        try:
            v1, v2 = sorted(interaction)

            colname = "XTERM_SQRD_%s" % (v1)
            if colname not in data:
                print('square interaction term', colname)
                vals = data[v1] * data[v1]
                vals = nleveling(vals, col=colname)
                L = len(set(vals))
                if is_valid_xterm(L, colname):
                    data[colname] = apply_scaler_for_column(xvals=vals,
                                                            y=data[TARGET_VAR],
                                                            colname=colname)
                print('    ' + '-' * 76)

            colname = "XTERM_SQRD_%s" % (v2)
            if colname not in data:
                print('square interaction term', colname)
                vals = data[v2] * data[v2]
                vals = nleveling(vals, col=colname)
                L = len(set(vals))
                if is_valid_xterm(L, colname):
                    data[colname] = apply_scaler_for_column(xvals=vals,
                                                            y=data[TARGET_VAR],
                                                            colname=colname)
                print('-' * 80)
        except Exception as err:
            print_exception('problem w/ column', err=err, with_traceback=False)
            print(interaction, 'square interaction term(s) skipped')
    return data
# #########################################################################


# #########################################################################
# DATA REDUCTION: correlation and feature generation
# #########################################################################
def basic_feature_generation_reductionism(data, col,
                                          i=None, COLSET=(), debug=False):
    DROPSET = []

    if col not in data:
        print(col, 'col already removed')
        return data, DROPSET

    vals = [x if pd.notnull(x) else NA_RECODE for x in data[col]]
    levels = sorted(set(vals))  # + [NA_RECODE, ])
    L = len(levels)
    banner("FG: %s had %s" % (col, get_levels(levels)))

    if L < DROP_VARS_WITH_LESS_THAN_NLEVELS:
        print(col, "SKIPPED, AS IT HAS ", get_levels(levels))
        del data[col]
        return data, DROPSET

    if TARGET_VAR in col:
        print(data[TARGET_VAR].describe(include='all'))
        return data, DROPSET

    if ID_VAR in col and ID_VAR in data:
        print(data[ID_VAR].describe(include='all'))
        return data, DROPSET

    if L > UPPER_FACTOR_LEVEL_LIMIT:
        try:
            # vals = decrease_nlevels_for(data[col], col=col)
            print("FG2:", col, "unmodified but may/will be deleted", \
                get_levels(levels))
            # DROPSET.append(col)

            if debug:
                colname = "%s_spread" % col
                vals = nleveling(data[col], col=col, n=3)
                data[colname] = vals.copy()
                levels = sorted(set(data[colname]))
                print("FG2:", colname, "transformed into spread, now w/:", \
                    get_levels(levels))
                # print data[colname].describe(include='all')

            if debug:
                d = ZVALS_RECODING_DIGITS + 1
                colname = "%s_zvals_%s" % (col, d)
                data[colname] = get_zvals(data[col], do_rounding_off=d).copy()
                levels = sorted(set(data[colname]))
                print("FG2:", colname, "transformed into zvals, now w/:", \
                    get_levels(levels))
                # print data[colname].describe(include='all')
        except Exception as err:
            print_exception('problem with nleveling column', err=err)
            print("FG2:", col, "remains unmodified", get_levels(levels))

    try:
        correlated, dropcol = highly_correlated(data, COLSET[i-1], COLSET[i])
        if correlated and TARGET_VAR not in dropcol:
            print(dropcol, 'scheduled to be dropped due to high correlation')
            DROPSET.append(dropcol)

            if GLOBAL_NOT_LOCAL_DECORRELATION:
                raise Exception

            try:
                P = generate_substitute_for_decorrelated_vars_from(data,
                                                                   COLSET[i-1],
                                                                   COLSET[i])
                for pcol in P.columns:
                    pcolname = "EVAR_%s_%s_%s" % (i, pcol, int(time.time()))
                    data[pcolname] = P[pcol].copy()
            except Exception as err:
                if not GLOBAL_NOT_LOCAL_DECORRELATION:
                    print_exception('problem w/ decorrelation dropvar', err=err)
                else:
                    print('skipped pairwise computation', COLSET[i-1], COLSET[i])

        DROPSET = [x for x in set(DROPSET) if x in data] + [TARGET_VAR, ]
        if len(DROPSET) % 32 == 0:
            # ---------------------------------------------------------
            banner('E: incremental non-fselected; get_proj for DROPSET')
            P = get_projection_features_for(data[DROPSET], y=data[TARGET_VAR])
            for pcol in P.columns:
                pcolname = "EVAR_%s_%s_%s" % (i, pcol, int(time.time()))
                print('projection col for decorrelate drop', pcol, pcolname)
                data[pcolname] = P[pcol].copy()
            print('-' * 80)

            try:
                banner("DROPPING: %s" % ",".join(DROPSET))
                VARSET = validate_vars(data, not_in_set=DROPSET,
                                       with_target=True)
                data = data[VARSET]
            except Exception as err:
                print_exception('problem dropvar', err=err)
                print('-' * 80)

                for dropcol in DROPSET:
                    if dropcol in data.columns:
                        print("--- DROPPING:", dropcol)
                        try:
                            data = data[[x for x in data.columns
                                         if not x == dropcol]]
                            time.sleep(2)
                        except Exception as err:
                            print_exception('problem dropvar', err=err)

            print('-' * 80)
            DROPSET = [x for x in set(DROPSET) if x in data] + [TARGET_VAR, ]
            M, N = print_data_shape(data)

    except Exception as err:
        # print_exception('problem with decorrelation analysis', err=err)
        print('-' * 80)

    try:
        for k in range(2, LOCAL_DECORRELATION_EFFORT):
            correlated, dropcol = highly_correlated(data,
                                                    COLSET[i-k],
                                                    COLSET[i])
            if correlated and TARGET_VAR not in dropcol:
                DROPSET.append(dropcol)
                break
    except Exception as err:
        print('-' * 80)

    return data, DROPSET
# #########################################################################


# #########################################################################
def basic_feature_encoding_reductionism(data, col, vals, prev_col=""):
    DROPSET = []
    levels = sorted(set(vals))  # + [NA_RECODE, ])
    L = len(levels)
    banner("FE: %s has %s" % (col, get_levels(levels)))

    if ID_VAR in col and ID_VAR in data:
        print(data[ID_VAR].describe(include='all'))
        return data, DROPSET

    if TARGET_VAR in col:
        print(data[TARGET_VAR].describe(include='all'))
        return data, DROPSET

    if col not in data:
        print(col, 'col already removed')
        return data, DROPSET

    if L < DROP_VARS_WITH_LESS_THAN_NLEVELS:
        print("FE:", col, "COL SKIPPED, IT HAS ", get_levels(levels))
        del data[col]
        return data, DROPSET

    if is_highly_significant_var(col):
        P = derived_from_highly_significant_vars(data, col, vals)
        for pcol in P.columns:
            pcolname = "HSV_%s_%s" % (pcol, int(time.time()))
            print('COL is HSVAR DERIVED', pcol, pcolname)
            data[pcolname] = P[pcol].copy()

    # if not GLOBAL_NOT_LOCAL_DECORRELATION:
    #     try:
    #         correlated, dropcol = \
    #             highly_correlated(data, COLSET[i-1], COLSET[i])
    #         if correlated and TARGET_VAR not in col:
    #             DROPSET.append(dropcol)
    #             print "--- DROPPING:", dropcol
    #             return data, DROPSET
    #     except Exception, err:
    #         print_exception('problem with decorrelation analysis', err=err)

    if prev_col:
        P = derived_from_paired_columns(data, col, prev_col)
        for pcol in P.columns:
            pcolname = "PAIR_%s_%s" % (pcol, int(time.time()))
            print('COL is pair_derived', pcol, pcolname)
            data[pcolname] = P[pcol].copy()

    if L <= MAX_NUM_FACTOR_LEVELS and not is_special_col(col):
        if ENCODE_NUMERICALS:
            data[col] = get_codes_from_categorical(vals, col=col)
        # print data[col].describe(include='all')
        print(col, "retained as-is", get_levels(levels))
    else:
        print(col, "to be cut into factors", get_levels(levels))

        try:
            try:
                if "SSN" in col:
                    print(col, 'attempt to process special ssn field')
                    P = process_ssn(data, vals, col)
                    for colname in P.columns:
                        data[colname] = P[colname].copy()
                        print('generated column', colname)

                    data[col] = get_codes_from_categorical(vals, col=col)
                    print(data[col].describe(include='all'))
                    return data, DROPSET
            except:
                print("FE:", col, "problem with %s SSN levels, skipped col" % L)

            try:
                P = process_numeric(data, vals, col)
                for colname in P.columns:
                    data[colname] = P[colname].copy()
                    print('processed numeric column', colname)
                # return data, DROPSET
            except Exception as err:
                print_exception('problem with numeric processing', err=err,
                                with_traceback=False)

            try:
                P = process_date(data, vals, col)
                for colname in P:
                    data[colname] = P[colname].copy()
                    print('processed date column', colname)
                # return data, DROPSET
            except Exception as err:
                print_exception('problem with timedate processing', err=err,
                                with_traceback=False)

            try:
                P = process_title(data, vals, col)
                for colname in P:
                    data[colname] = P[colname].copy()
                    print('processed title column', colname)
                # return data, DROPSET
            except Exception as err:
                print_exception('problem with namefield processing', err=err,
                                with_traceback=False)

        except Exception as err:
            print_exception('problem with column processing', err=err)
            print("FE:", col, "col skipped", get_levels(levels))
            # DROPSET.append(col)

    return data, DROPSET
# #########################################################################


# #########################################################################
def generate_time_differentials_from(data, year_cols, mon_cols, day_cols):
    try:
        P = get_projection_features_for(data[year_cols], y=data[TARGET_VAR])
        for pcol in P.columns:
            pcolname = "DATE_%s_%s_%s" % ('y', pcol, int(time.time()))
            print('projection col for year diff cols', pcol, pcolname)
            data[pcolname] = P[pcol].copy()
    except Exception as err:
        print_exception('problem w/ as-in projection on dates', err=err)

    try:
        P = get_projection_features_for(data[mon_cols], y=data[TARGET_VAR])
        for pcol in P.columns:
            pcolname = "DATE_%s_%s_%s" % ('m', pcol, int(time.time()))
            print('projection col for mon diff cols', pcol, pcolname)
            data[pcolname] = P[pcol].copy()
    except Exception as err:
        print_exception('problem w/ as-in projection on dates', err=err)

    try:
        P = get_projection_features_for(data[day_cols], y=data[TARGET_VAR])
        for pcol in P.columns:
            pcolname = "DATE_%s_%s_%s" % ('d', pcol, int(time.time()))
            print('projection col for day diff cols', pcol, pcolname)
            data[pcolname] = P[pcol].copy()
    except Exception as err:
        print_exception('problem w/ as-in projection on dates', err=err)

    try:
        print(year_cols)
        for y, c in enumerate(year_cols):
            for y0 in range(y):
                year_end, year_start = year_cols[y], year_cols[y0]
                year_col = "DATE_%s_%s_diff" % (year_end, year_start)
                data[year_col] = data[year_end] - data[year_start]
                data[year_col] = data[year_col].fillna(NA_RECODE)  # NA_RECODE
    except Exception as err:
        print_exception('problem with iterated time_diff_col', err=err)

    try:
        print(mon_cols)
        for y, c in enumerate(mon_cols):
            for y0 in range(y):
                mon_end, mon_start = mon_cols[y], mon_cols[y0]
                mon_col = "DATE_%s_%s_diff" % (mon_end, mon_start)
                data[mon_col] = data[mon_end]/4 - data[mon_start]/4 \
                    + data[year_col] * 4
                data[mon_col] = data[mon_col].fillna(NA_RECODE)  # NA_RECODE
    except Exception as err:
        print_exception('problem with iterated time_diff_col', err=err)

    try:
        print(day_cols)
        for y, c in enumerate(day_cols):
            for y0 in range(y):
                day_end, day_start = day_cols[y], day_cols[y0]
                day_col = "DATE_%s_%s_diff" % (day_end, day_start)
                data[day_col] = data[day_end] - data[day_start]
                data[day_col] = data[day_col].fillna(NA_RECODE)  # NA_RECODE
    except Exception as err:
        print_exception('problem with iterated time_diff_col', err=err)

    return data
# #########################################################################


# #########################################################################
def incremental_feature_selection(data, wrt_colset=(), stage="", debug=False):
    DROPSET = []

    if not wrt_colset:
        wrt_colset = data.columns
    wrt_colset = [col for col in wrt_colset if col not in [ID_VAR, TARGET_VAR]]

    step = min(len(wrt_colset), FS_STEPS[stage])
    for i in range(0, len(wrt_colset), step):
        banner("STAGE: %16s, [%s, %s]" % (stage.upper(), i, i+step))

        if PARTIAL_AND_INCREMENTAL:
            colset = wrt_colset[:i+step]
        else:
            colset = wrt_colset[i:i+step]

        data, DROPSET = feature_select_and_project(data, DROPSET,
                                                   colset=colset,
                                                   wrt=stage, stem="T")

        VARSET = validate_vars(data, not_in_set=DROPSET, with_target=True)
        data = data[VARSET]
        M, N = print_data_shape(data)
        print('-' * 80)
        print("iteration:      ", i-step, i)
        print("data.shape:     ", data.shape)
        if debug:
            print("# FSELECTED VARS", len(VARSET), VARSET)
        print('-' * 80)
    return data, DROPSET
# #########################################################################


# #########################################################################
def add_zscores_columnspan_features(data, STEP=ZSCORE_COL_STEP_F):
    # XVARS = [col for col in data.columns
    # if col in ORIGINAL_VARIABLES and col not in [ID_VAR, TARGET_VAR]]
    XVARS = [col for col in data.columns if col not in [ID_VAR, TARGET_VAR]]

    stopping_column_number = len(XVARS) - STEP - 1
    col_start, col_end = STEP*2+1, stopping_column_number

    P = pd.DataFrame()
    for i in range(col_start, col_end, STEP):
        colset = XVARS[i-STEP:i]
        if TARGET_VAR in colset:
            colset = [x for x in colset if TARGET_VAR not in x]

        try:
            c_mean = data[colset].mean(axis=0).mean()
            c_stdv = data[colset].mean(axis=0).std()

            featurename = "fsum_t_%s" % i
            P["tmp"] = (data[colset].sum(axis=1) - c_mean)/c_stdv
            d_mean = (P["tmp"] > 0).mean()
            print("%4s %10s   %10.6f" % (i, featurename, d_mean), end=' ')

            if fselect(d_mean, colset=colset):
                data[featurename] = P["tmp"].copy()
                data[featurename] = data[featurename].round(decimals=2)
                cvals = (data[colset].sum(axis=1) - c_mean)/c_stdv
                cvals, ok = normalize_col(cvals, colset[0])
                if ok:
                    featurename = "fsum_z_%s" % i
                    cvals = pd.Series(cvals)
                    cvals = cvals.round(decimals=1)
                    cvals = get_zvals(cvals)
                    data[featurename] = cvals.copy()
            else:
                print()

            # print featurename, d["tmp"].describe()
        except Exception as err:
            print_exception('problem fsum indicators', err=err)
            print('problem building fsum feature', i, colset)

    return data
# #########################################################################


# #########################################################################
def apply_factor_conditioning(data):
    i = 0
    for col in [col for col in data.columns if col not in [ID_VAR, TARGET_VAR]]:
        i += 1
        vals = sorted([x if pd.notnull(x) else NA_RECODE for x in data[col]])
        levels = sorted(set([NA_RECODE, ] + vals))
        L = len(levels)
        banner("FC: %s has %s" % (col, get_levels(levels)))

        if L <= MAX_NUM_FACTOR_LEVELS:
            if ENCODE_NUMERICALS:
                data[col] = data[col].astype('category',
                                             categories=levels, ordered=True)
            # if not BYPASS_PCA_SCALER and data[col].abs().max() > 1024:
            #    data[col] = apply_scaler_for_column(xvals=data[col],
            #                                        y=data[TARGET_VAR],
            #                                        colname=col)
        else:
            if is_special_col(col):
                try:
                    vals = [x if pd.notnull(x) else NA_RECODE
                            for x in data[col]]
                    data[col] = vals[:]
                except Exception as err:
                    print_exception('problem with nafill', err=err)

            if L <= MAX_NUM_FACTOR_LEVELS:
                print(col, "***** unmodified, left as-is w/", get_levels(levels))

            if L > MAX_NUM_FACTOR_LEVELS:
                try:
                    print("%82s" % col, "may overfit w/", get_levels(levels))
                    data[col] = quantize_vector(vals,
                                                how="zscore", col=col).copy()
                except Exception as err:
                    print_exception('problem w/ col wrt zscore', err=err)
                    print("%82s" % col, "...was skipped...", get_levels(levels))
    return data
# #########################################################################


# #########################################################################
def numerical_recoding(data, VARSET=()):
    i = 0
    numerical_data = pd.DataFrame()

    for j, colname in enumerate(VARSET):
        print('-' * 80)

        if TARGET_VAR in colname:
            continue

        if colname not in data:
            print(colname, 'not in data')
            continue

        i += 1
        vals = [x if pd.notnull(x) else NA_RECODE for x in data[colname]]
        vals = pd.Series(vals)
        numerical_data[colname] = vals.copy()
        levels = sorted(set(vals))
        print("NUM", i, colname, get_levels(levels))

        try:
            del data[colname]
        except Exception as err:
            print(j, colname, data.shape)
            print_exception('problem with delete of column', err=err)

    return numerical_data
# #########################################################################


# #########################################################################
def augment_with_rank_features_for(numerical_data):
    rank_data = pd.DataFrame()
    rank_data[TARGET_VAR] = numerical_data[TARGET_VAR]
    i = 0
    for col in [x for x in numerical_data if TARGET_VAR not in x]:
        i += 1

        # if col not in ORIGINAL_VARIABLES:
        #     continue

        if i >= MAX_RANK_AUGMENTATION:
            break

        vals = [x if pd.notnull(x) else NA_RECODE for x in numerical_data[col]]
        levels = sorted(set(vals))  # + [NA_RECODE, ])
        banner("rank processing: #%s %s %s" % (i, col, get_levels(levels)))

        try:
            colname = "%s_RANK" % col
            vals = get_codes_from_categorical(vals, col=colname)
            rank_data[colname] = vals.copy()
            levels = sorted(set(rank_data[colname]))  # + [NA_RECODE, ])
            print("RANK", i, colname, get_levels(levels))
        except Exception as err:
            print_exception('problem with rank data column', err=err)
            print("RANK", i, colname, "processing skipped")
        print('-' * 80)

    try:
        # rank_data[TARGET_VAR] = numerical_data[TARGET_VAR]
        rank_xvars = \
            feature_select(rank_data,
                           rank_data[TARGET_VAR],
                           [x for x in rank_data if TARGET_VAR not in x],
                           p=FS_PERCENTAGES['rank'],
                           min_score=FS_MINSCORES['rank'],
                           sorted_by_importance=True)
    except Exception as err:
        print_exception('problem with rank data feature selection', err=err)
        rank_xvars = rank_data.columns

    for colname in rank_xvars:
        numerical_data[colname] = rank_data[colname].copy()

    return numerical_data
# #########################################################################


if __name__ == "__main__":
    print('DONE:%s' % __name__)
