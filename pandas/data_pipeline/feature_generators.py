#!/usr/bin/python
# -*- coding: utf-8 -*-


# #########################################################################
__doc__ = "provides feature encoders, generators for classification purposes"
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
import copy
import time
import random
import traceback
# ---------------------------------------------------------------------------
from feature_selected_vars import SELECTED
from feature_selected_vars import DROPPED
from feature_selected_vars import HIGHLY_SIGNIFICANT_VARS
from feature_selected_vars import INTERACTION_TERMS
# ---------------------------------------------------------------------------
from scipy.stats import rankdata
from scipy.stats import normaltest
from collections import defaultdict
# ---------------------------------------------------------------------------
from sklearn.decomposition import PCA
from sklearn.decomposition import FastICA
from sklearn.decomposition import KernelPCA
# from sklearn.decomposition import RandomizedPCA
from sklearn.decomposition import PCA as RandomizedPCA
# ---------------------------------------------------------------------------
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import MaxAbsScaler
from sklearn.preprocessing import RobustScaler
#from sklearn.preprocessing import Imputer
from sklearn.impute import SimpleImputer as Imputer
# imputer = SimpleImputer(missing_values=np.nan, strategy='mean')
# ---------------------------------------------------------------------------
from sklearn.feature_selection import chi2
from sklearn.feature_selection import SelectKBest
# from sklearn import grid_search
from sklearn.model_selection import learning_curve
import sklearn.model_selection as grid_search
# ---------------------------------------------------------------------------
try:
    from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
    LDA = LinearDiscriminantAnalysis
except:
    from sklearn.lda import LDA
from sklearn.svm import LinearSVC
# ---------------------------------------------------------------------------
# #########################################################################


# #########################################################################
# GLOBALS
# #########################################################################
global TIMENOW
TIMENOW = time.time()
STARTUP_TIME = time.time()
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
APPLY_THESE = [2, 7, 10] + [6, 13, 14, 15] + list(range(16, 25, 3)) + list(range(25, 34, 3))
APPLY_THESE = [7, 10, 14, 15, 34]
# #########################################################################


# #########################################################################
global ORIGINAL_VARIABLES
ORIGINAL_VARIABLES = {}
# #########################################################################


# #########################################################################
from definitions import BRANCHES
from definitions import TITLES
from definitions import CITIES
from definitions import ROLES
from definitions import DATE_COLS
from definitions import SPECIAL_COLS
# #########################################################################


# #########################################################################
from pipeline_configuration import BYPASS_GLOBAL_PROJECTIONS
from pipeline_configuration import BYPASS_HSVAR_FEATURES
from pipeline_configuration import BYPASS_ICA_PROJECTIONS
from pipeline_configuration import BYPASS_INTERACTIONS_AFTERTOP
from pipeline_configuration import BYPASS_LOCAL_PROJECTIONS
from pipeline_configuration import BYPASS_NORMAL_CHECKS
from pipeline_configuration import BYPASS_PAIRED_FEATURES
from pipeline_configuration import BYPASS_SPECIAL_FGENS
from pipeline_configuration import BYPASS_VECTOR_NORMALIZATION
from pipeline_configuration import CODE_CATEGORIES_VIA
from pipeline_configuration import CODE_MARGINALS_VIA
from pipeline_configuration import DEFAULT_REGULARIZATION
from pipeline_configuration import DISPLAY_NLEVELS
from pipeline_configuration import DROP_VARS_WITH_LESS_THAN_NLEVELS
from pipeline_configuration import DROP_XTERMS_WITH_NLEVELS
from pipeline_configuration import FSELECT_MINSCORE
from pipeline_configuration import FS_MINSCORES
from pipeline_configuration import FS_PERCENTAGES
from pipeline_configuration import GLOBAL_RSQRD_THRESHOLD
from pipeline_configuration import ID_VAR
from pipeline_configuration import LABEL_VAR
from pipeline_configuration import LOCAL_RSQRD_THRESHOLD
from pipeline_configuration import L_STEP
from pipeline_configuration import MAX_NUM_FACTOR_LEVELS
from pipeline_configuration import MINIMUM_DECORRELATION_LEVEL
from pipeline_configuration import NA_RECODE
from pipeline_configuration import NORM_RECODING_DIGITS
from pipeline_configuration import NUMERICAL_DATATYPE
from pipeline_configuration import NUM_ICA_DIMS
from pipeline_configuration import PCA_NCOMPONENTS
from pipeline_configuration import TARGET_VAR
from pipeline_configuration import TESTING_SET
from pipeline_configuration import TRANSFORMS
from pipeline_configuration import USE_RECIPROCAL_CODING_FOR_CATEGORIES
from pipeline_configuration import NA_LARGE_VALS
from pipeline_configuration import ZSCORE_Q
from pipeline_configuration import ZSCORE_RECODING_DIGITS
from pipeline_configuration import ZVALS_RECODING_DIGITS
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
def set_original_variables(data, colset=()):
    banner('original variables:')
    global ORIGINAL_VARIABLES
    ORIGINAL_VARIABLES = sorted(set([x for x in data.columns]))
    ORIGINAL_VARIABLES = dict(list(zip(ORIGINAL_VARIABLES, ORIGINAL_VARIABLES)))
    print(ORIGINAL_VARIABLES)
    print('-' * 80)
    return ORIGINAL_VARIABLES
# #########################################################################


# #########################################################################
def print_data_shape(data, min_ncols=256):
    M, N = data.shape
    print("data.shape", data.shape)
    print('-' * 80)

    if len(data.columns) < min_ncols:
        return M, N

    def report_wrt_var(data, lookup_term, term_name="", override=False):
        nlen = len([x for x in data.columns if lookup_term in x])
        if nlen:
            if not term_name:
                term_name = lookup_term.upper() + "S"
            print("%6s: [%s] " % (term_name, nlen))
        return

    print('-' * 80)
    try:
        print('ORIG: [%s] ' % len([x for x in data if x in ORIGINAL_VARIABLES]))
        print('-' * 80)
    except:
        print('-' * 80)

    for term in ["AVAR", "BVAR", "CVAR", "DVAR", "EVAR", "FVAR", "GVAR",
                 "HVAR", "IVAR", "JVAR", "KVAR", "LVAR", "MVAR", "NVAR",
                 "OVAR", "UVAR", "PAIR", "RANK", "ival", "oval", "qval",
                 "zval", "cat", "zscore", "norm", "orig", "cuts",
                 "fsum", "gsum", "HSV", "LDA", "ICA", "SIG",
                 'DATE', "MARGINAL", "XTERM", "_BY_", "SQRD",
                 "_freqs", "xfreq",
                 "scores", "chars", "codes",
                 "zvals", "cuts", "norm",
                 "diff", "sqr", "sqr", "mxmn", "ssn"]:
        report_wrt_var(data, term)
    print('-' * 80)
    return M, N
# #########################################################################


# #########################################################################
def handle_title(x):
    for S in [TITLES, BRANCHES, ROLES, CITIES]:
        if x in S:
            return S[x]
        if str(x) in S:
            return S[x]
    return NA_RECODE
# #########################################################################


# #########################################################################
def is_topfreq_titled(x, na=False, n=4):
    try:
        ret = handle_title(x)
        return ret
    except:
        return NA_RECODE
# #########################################################################


# #########################################################################
def get_encoded_token(x, na=False, n=6):
    x = str(x)
    x = x.upper()
    x = x.replace(";", " ")
    x = x.replace("&", " ")
    x = x.replace("?", " ")
    x = x.replace(",", " ")
    x = x.replace("#", " ")
    x = x.replace("!", " ")
    x = x.replace("(", " ")
    x = x.replace(")", " ")
    x = x.replace(".", " ")
    x = x.replace("'", " ")
    x = x.replace(" FOR ", " ")
    x = x.replace(" IN ", " ")
    x = x.replace("  ", " ")
    items = x.split()
    if len(items) > 1:
        x = " ".join([item[0:3] for item in items[0:3]
                      if item.strip() and len(item) > 2])
    else:
        x = x[:n/2] + "-" + x[-n/2:]
    return x
# #########################################################################


# #########################################################################
def is_special_col(col):
    if col in SPECIAL_COLS:
        print('-' * 80)
        print(col, SPECIAL_COLS[col], "special handling")
        return True
    return False
# #########################################################################


# #########################################################################
def is_highly_significant_var(col, override=False):
    if override and col in ORIGINAL_VARIABLES:
        return True

    if sum(["ICA" in col, "LDA" in col, "HSV" in col, "PCA" in col]):
        return False

    if col in HIGHLY_SIGNIFICANT_VARS:
        return True

    return False
# #########################################################################


# ############################################################################
def is_categorical(vals, col=""):
    vals = pd.Series(vals)
    try:
        vals.mean()
        is_categorical = False
        print(col, 'is numerical')
    except:
        is_categorical = True
        print(col, 'is categorical')
    return is_categorical
# ############################################################################


# #########################################################################
def is_normally_distributed(x, col, f, p_threshold=0.2, n=5, nmax=5000):
    if BYPASS_NORMAL_CHECKS:
        return False

    m = len(x)
    p_vals = [normaltest(random.sample(x, min(nmax, m)))[1] for i in range(n)]
    p_vals = [p if numpy.isfinite(p) else 0.0 for p in p_vals]
    p_val = numpy.mean(p_vals)

    if p_val < p_threshold:
        if f == '':
            print("%s(%s): " % (f.upper()[0:3], col[4:]), end=' ')
        print("%s()," % (f.upper()[0:3]), end=' ')
        return False
    else:
        transform_performance[f] += 1
        p_txt = ",".join(["%.3f" % p for p in p_vals if p > p_threshold])
        print("%s(%s)" % (f.upper(), col), p_txt, "APPRX.NORM.DISTR")
        return True
# #########################################################################


# #########################################################################
def is_original_variable(colname):
    if len(colname) != 8:
        return False
    return True
# #########################################################################


# #########################################################################
def highly_correlated(data, col1, col2, corr_method='pearson',
                      debug=False, fselect_ordered=True,
                      origvars_only=True, corr_threshold=LOCAL_RSQRD_THRESHOLD):
    cols, correlated, idx = [col1, col2], False, 1
    if TARGET_VAR in cols:
        return correlated, cols[idx]

    if origvars_only:
        if not is_original_variable(col1) and not is_original_variable(col2):
            return correlated, cols[idx]

    c = data[cols].corr(method=corr_method, min_periods=36)
    # c = numpy.nan_to_num(c)
    rval = c[col1][col2]
    print("correlation[%s, %s] = [%s, %s]" % (col1, col2, rval, rval*rval))
    if debug:
        print(c)
    if rval*rval >= corr_threshold:
        correlated = True
        print("*** HIGHLY CORRELATED: ", cols, rval*rval)
        if not fselect_ordered:
            try:
                fselector = SelectKBest(chi2, 1)
                fselector.fit_transform(data[cols], data[TARGET_VAR])
                scores = numpy.nan_to_num(fselector.scores_)
                print("scores", scores)
                indices = [x for x in fselector.get_support(indices=True)]
                print("indices", indices)
                for x in sorted(zip(scores, list(range(len(scores)))), reversed=True):
                    print(x)
                idx = x[1]
            except:
                pass
    return correlated, cols[idx]
# #########################################################################


# #########################################################################
# heuristic filter for z-score binary features aligned wrt class-priors
# #########################################################################
def fselect(d_mean, cl_min=0.50, cl_max=0.50, override=False, colset=()):
    d_mean = numpy.abs(d_mean)
    override = override and d_mean != 1.0
    if override or 1./ZSCORE_Q < d_mean and d_mean < (ZSCORE_Q-1.)/ZSCORE_Q:
        if d_mean < cl_min or d_mean > cl_max:
            print("SELECTED: ", colset, end=' ')
            return True
    return False
# #########################################################################


# ############################################################################
def apply_na_recoder(vals, y, col="", strategy="", nmax=None):
    null_vals = [x for x in vals if not pd.notnull(x)]
    number_of_null_values = len(null_vals)
    if not number_of_null_values:
        print('no na_recoding needed/applied for', col)
        return vals

    lots_of_null_values = 0.10 * len(vals)
    if number_of_null_values > lots_of_null_values:
        print('number of nulls is high within series, keeping and recoding')
        vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
        vals = pd.Series(vals)
        return vals.copy()

    if not nmax:
        nmax = len(y) - 1

    if not strategy:
        if is_categorical(vals, col=col):
            strategy = "most_frequent"
            # vals = pd.Categorical.from_array(vals)
            # vals = vals.codes
            vals = pd.Categorical(vals).codes
        else:
            strategy = "mean"
            vals = numpy.array(vals)

    df = pd.DataFrame()
    df[col] = vals
    df[TARGET_VAR] = y

    try:
        numerical_na_recoder = Imputer(missing_values=numpy.NaN,
                                       strategy=strategy, axis=0)
        xtrain = df[df[TARGET_VAR] != TESTING_SET]
        nrows = min(len(xtrain)-1, nmax)
        del xtrain[TARGET_VAR], df[TARGET_VAR]
        numerical_na_recoder.fit(
            numpy.array(xtrain.sample(n=nrows, random_state=get_random_seed())))
        xx = numerical_na_recoder.transform(numpy.array(df))
        df = pd.DataFrame()
        df[col] = xx[:, 0]
        vals = [x if pd.notnull(x) else NA_RECODE for x in df[col]]
        print('recoded col %s using %s' % (col, strategy))
    except Exception as err:
        print_exception('w/ na-recoding %s' % col, err=err, with_traceback=True)

    vals = numpy.nan_to_num(vals)
    return vals
# ############################################################################


# ############################################################################
def get_fit_fselector(data, y, wrt_vars,
                      get_obj=False,
                      p=100./100.,
                      min_score=1.0,
                      recode_data=False,
                      nmax=1000000):
    print()
    print(data.describe(include='all'))

    n = data[(y != TESTING_SET)].shape[0]
    just_xvars = [x for x in wrt_vars if x not in [TARGET_VAR, ]]
    if recode_data:
        for c in just_xvars:
            print(c, end=' ')
            data[c] = apply_scaler_for_column(xvals=data[c],
                                              y=data[TARGET_VAR], colname=c)
            data[c] = data[c].fillna(NA_RECODE)
            print(data[c].describe(include='all'))
    print()
    print('-' * 80)

    subsample = data[(y != TESTING_SET)].sample(min(nmax, n),
                                                random_state=get_random_seed())

    k = int(len(wrt_vars) * p)
    if not k:
        k = 'all'
    ok, fselector = False, SelectKBest(chi2, k=k)

    if not ok:
        try:
            print('using subsample[wrt_vars] as is')
            for c in just_xvars:
                subsample[c] = numpy.nan_to_num(subsample[c])
                subsample[c] = subsample[c] - subsample[c].min() + 1
            fselector.fit_transform(subsample[just_xvars],
                                    subsample[TARGET_VAR])
            ok = True
        except Exception as err:
            print_exception('fselect-1', err=err, with_traceback=True)
            print(c, data[c].describe().transpose())
            print('-' * 80)

    if not ok:
        try:
            for c in just_xvars:
                data[c] = get_codes_from_categorical(data[c], col=c)
                data[c] = data[c] - data[c].min() + 1
            subsample = data[(y != TESTING_SET)].\
                sample(min(nmax, n), random_state=get_random_seed())
            print('using subsample[wrt_vars] + min + 1')
            fselector.fit_transform(subsample[just_xvars],
                                    subsample[TARGET_VAR])
            ok = True
        except Exception as err:
            print_exception('fselect-2', err=err, with_traceback=False)
            print(c, data[c].describe().transpose())
            print('-' * 80)

    if not ok:
        try:
            print('using subsample[wrt_vars].abs().fillna(NA_RECODE) cats')
            for c in just_xvars:
                data[c] = get_codes_from_categorical(data[c], col=c)
                data[c] = data[c] - data[c].min() + 1
            xxx = subsample[just_xvars].abs().fillna(NA_RECODE)
            fselector.fit_transform(xxx, subsample[TARGET_VAR])
            ok = True
        except Exception as err:
            print_exception('fselect-3', err=err, with_traceback=False)
            print(c, data[c].describe().transpose())
            print('-' * 80)

    if not ok:
        try:
            print('using subsample[wrt_vars].abs().fillna(NA_RECODE)')
            fselector.fit_transform(
                subsample[just_xvars].abs().fillna(NA_RECODE),
                subsample[TARGET_VAR])
            ok = True
        except Exception as err:
            print_exception('fselect-4', err=err, with_traceback=False)

    ok = hasattr(fselector, "scores_")
    if ok:
        if get_obj:
            # problem with shallow copy?
            return copy.deepcopy(fselector)

        indices = [x for x in fselector.get_support(indices=True)
                   if fselector.scores_[x] > min_score]
        scores = [x for x in fselector.scores_]
        pvals = [x for x in fselector.pvalues_]
        return ok, indices, scores, pvals

    return ok, [], [], []
# ############################################################################


# ############################################################################
def feature_select(data, y, wrt_vars,
                   min_score=FSELECT_MINSCORE,
                   sorted_by_importance=True,
                   p=0.001,
                   nmax=1000000,
                   debug=False):

    wrt_vars = [x for x in wrt_vars
                if x in data and x not in [TARGET_VAR, ID_VAR]]

    y = get_codes_from_categorical(y, col=TARGET_VAR)

    m = 24 # max(min(24, len(wrt_vars)/2), 1)

    banner("FSELECT: ([%s] VARS) " % len(wrt_vars) +
           ", ".join(wrt_vars[:m]) + " ... " + ", ".join(wrt_vars[-m:]))

    if not len([x for x in wrt_vars if TARGET_VAR not in x]):
        return wrt_vars

    ok, indices, scores, pvals = get_fit_fselector(data, y, wrt_vars,
                                                   min_score=min_score,
                                                   p=p,
                                                   nmax=nmax)
    print()

    fnames = [wrt_vars[x] for x in indices]  # if x in data.columns]

    fnames = validate_fselect_colset(wrt_vars, fnames)
    if debug:
        print("SELECTED", len(fnames), fnames)

    fnames, most_important_features = [], []
    ii = 0
    if ok:
        for x in sorted(zip(numpy.nan_to_num(scores),
                            pvals,
                            list(range(len(scores)))), reverse=True):
            try:
                i, p, s = x[2], numpy.round(x[1], 2), numpy.round(x[0], 2)
            except Exception as err:
                i, p, s = x[2], x[1], x[0]
                print(err)

            col = wrt_vars[i]
            fnames.append(col)
            levels = sorted(set(data[col]))
            L = len(levels)
            if i in indices:
                print("FS:** %5s %5s %80s %8s %12s %12s" % (i, ii, col, L, s, p))
                most_important_features.append(col)
            else:
                print("FS:   %5s %5s %80s %8s %12s %12s" % (i, ii, col, L, s, p))
            ii += 1

    if sorted_by_importance:
        if len(most_important_features):
            fnames = most_important_features
        else:
            print('no features were selected; returning ordered fnames')
            print(fnames)

    return fnames
# #########################################################################


# #########################################################################
def feature_select_and_project(data, DROPSET, colset=(), wrt="", stem="W"):
    try:
        if not len(colset):
            return data, DROPSET
            # colset = [x for x in data]

        fselect_vars = feature_select(data,
                                      data[TARGET_VAR],
                                      colset,
                                      min_score=FS_MINSCORES[wrt],
                                      p=FS_PERCENTAGES[wrt])
        # -------------------------------------------------------------
        fselect_drop = [x for x in colset
                        if x not in fselect_vars
                        and x in data.columns
                        and TARGET_VAR not in x]
        # -------------------------------------------------------------
        banner('%s: %s getproj fgen for dropped vars' % (stem, wrt))
        P = get_projection_features_for(data[fselect_drop],
                                        y=data[TARGET_VAR])
        for pcol in P.columns:
            pcolname = "%sVAR_%s_%s_%s" % (stem, 0, pcol, int(time.time()))
            print('projection col for fselect drop', pcol, pcolname)
            data[pcolname] = P[pcol].copy()

        DROPSET.extend(fselect_drop)
        # -------------------------------------------------------------
    except Exception as err:
        print_exception('problem with %s feature selection' % wrt, err=err)
    return data.copy(), DROPSET[:]
# #########################################################################


# #########################################################################
def decorrelate_and_project(num_data, DROPSET, do_projections=True,
                            colset=(), wrt="", stem="", ncols=3):
    try:
        Q, keep, drop = decorrelate(num_data, colset=colset)
        # corr_keepcols = [x for x in num_data.columns if x not in drop]
        corr_dropcols = [x for x in num_data.columns if x in drop]

        DROPSET.extend(corr_dropcols)

        if do_projections:
            banner('%s: %s decorrelation dropped vars fgen' % (stem, wrt))
            P = get_projection_features_for(num_data[corr_dropcols],
                                            y=num_data[TARGET_VAR],
                                            ncols=ncols)
            for pcol in P.columns:
                pcolname = "%sVAR_%s_%s_%s" % (stem, 0, pcol, int(time.time()))
                print('projection col for decorrelate drop', pcol, pcolname)
                num_data[pcolname] = P[pcol].copy()

    except Exception as err:
        print_exception('problem with %s decorrelation processing' % wrt,
                        err=err)

    return num_data, DROPSET
# #########################################################################


# #########################################################################
# augment/validate an a feature_selected colset subset with kept variables
# #########################################################################
def validate_fselect_colset(original_colset, final_dvars):
    # unique and sort
    final_dvars = [x for x in sorted(set(final_dvars))]

    # return only independent vars
    final_dvars = [x for x in final_dvars if TARGET_VAR not in x]

    return final_dvars
# #########################################################################


# #########################################################################
# normalization transforms
# #########################################################################
def normalize_col(vals, col, do_nlevel=False):
    datacol = [x if pd.notnull(x) else NA_RECODE for x in vals]
    datacol = pd.Series(datacol)

    try:
        for transform in TRANSFORMS:
            xvals = datacol
            if transform == 'log':
                xvals = numpy.log(datacol - datacol.min() + 1)
            elif transform == 'sqrt':
                xvals = numpy.sqrt(datacol - datacol.min())
            elif transform == '1/x':
                xvals = 1.0/datacol
            elif transform == 'rank':
                xvals = rankdata(datacol)
            elif transform == '1/rank':
                xvals = 1.0/rankdata(datacol)
            elif transform == 'x2':
                xvals = numpy.array([x*x for x in rankdata(datacol)])
            elif transform == 'exp':
                xvals = numpy.exp(datacol)
            elif transform == '-exp':
                xvals = numpy.exp(-datacol)
            elif transform == '1/sqrt':
                xvals = 1.0/numpy.sqrt(datacol - datacol.min())
            elif transform == '1/log':
                xvals = 1.0/numpy.log(datacol - datacol.min() + 1)
            elif transform == 'x/log':
                xvals = datacol/numpy.log(datacol - datacol.min() + 1)
            elif transform == 'x*log':
                xvals = datacol * numpy.log(datacol - datacol.min() + 1)
            elif transform == 's/rank':
                xvals = numpy.sqrt(datacol - datacol.min())/rankdata(datacol)
            elif transform == 'x*rank':
                xvals = datacol*rankdata(datacol)
            elif transform == 'power':
                xvals = numpy.power(datacol, 1./3.)
            elif transform == 's/log':
                xvals = numpy.sqrt(datacol - datacol.min())
                xvals = xvals/numpy.log(datacol - datacol.min() + 1)

            xvals = numpy.nan_to_num(xvals)
            if do_nlevel:
                xvals = nleveling(xvals, col=col)

            if is_normally_distributed(xvals, col, transform):
                datacol = xvals
                print(col, transform, '---- (APPROX.) OK -----')
                return datacol, True
        print()
    except Exception as err:
        print_exception('normalization', err=err)
    return datacol, False
# #########################################################################


# #########################################################################
def get_levels(levels, precision=3):
    L = len(levels)
    try:
        vals = ["%.4f" % x for x in levels[:DISPLAY_NLEVELS]]
    except:
        vals = ["%s" % x for x in levels[:DISPLAY_NLEVELS]]
    out = ",".join(vals)
    out = "%s levels: %s ..." % (L, out)
    return out
# #########################################################################


# #########################################################################
def get_pnames(basename, n):
    pnames = ["%s_%i" % (x, i) for i, x in enumerate([basename, ] * n)]
    return pnames
# #########################################################################


# #########################################################################
def get_marginal_wrt(data, col1, col2, how=CODE_MARGINALS_VIA):
    vals1 = nleveling(data[col1], col=col1)
    vals2 = nleveling(data[col2], col=col2)

    vals = ["%s:%s" % (x, y) for x, y in zip(vals1, vals2)]
    # vals = pd.Categorical.from_array(vals)
    vals = pd.Categorical(vals) #.codes

    freqs = vals.describe()
    if "codes" == how:
        vals = vals.codes
    elif "freqs" == how:
        vals = [freqs['freqs'][x] for x in vals.codes]
    elif "counts" == how:
        vals = [freqs['counts'][x] for x in vals.codes]
    elif how == "1/counts":
        freqs = vals.describe()
        vals = [1./freqs['counts'][x] if freqs['counts'][x] else 1.0
                for x in vals.codes]
    elif how == "1/freqs":
        freqs = vals.describe()
        fvals = [freqs['freqs'][x] for x in vals.codes]
        maxval = 999999
        if numpy.min(fvals):
            maxval = 2.0/numpy.min(fvals)
        vals = [1./freqs['freqs'][x] if freqs['freqs'][x] else maxval
                for x in vals.codes]
    vals = pd.Series(vals)

    return vals.copy()
# #########################################################################


# #########################################################################
def get_codes_from_categorical(vals, col="",
                               how=CODE_CATEGORIES_VIA,
                               na_recode=True,
                               debug=False):
    vals = pd.Series(vals)
    if TARGET_VAR in col:
        print(pd.Categorical(vals).describe())
        return vals.copy()

    if is_categorical(vals, col=col):
        if na_recode:
            vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
            vals = pd.Series(vals)

        # vals = pd.Categorical.from_array(vals)
        vals = pd.Categorical(vals).codes
        if how == "counts":
            freqs = vals.describe()
            vals = [freqs['counts'][x] for x in vals.codes]
        elif how == "freqs":
            freqs = vals.describe()
            vals = [freqs['freqs'][x] for x in vals.codes]
        elif how == "1/counts":
            freqs = vals.describe()
            vals = [1./freqs['counts'][x] if freqs['counts'][x] else 1.0
                    for x in vals.codes]
        elif how == "1/freqs":
            freqs = vals.describe()
            fvals = [freqs['freqs'][x] for x in vals.codes]
            maxval = 999999
            if numpy.min(fvals):
                maxval = 2.0/numpy.min(fvals)
            vals = [1./freqs['freqs'][x] if freqs['freqs'][x] else maxval
                    for x in vals.codes]
        elif how == "codes":
            vals = vals.codes
        else:
            vals = vals.codes

        vals = pd.Series(vals)
        # vals = vals.astype(NUMERICAL_DATATYPE)
        print(col, '... encoded from category into numerical using %s...' % how)
        if debug:
            print(col, 'is categorical')
            print(vals.describe())
            print('-' * 80)
    else:
        if na_recode:
            vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
            vals = pd.Series(vals)

        print(col, 'is numerical')
        # vals = vals.astype(NUMERICAL_DATATYPE)
        if debug:
            vals = pd.Series(vals)
            print(vals.describe())
            print('-' * 80)
    return vals.copy()
# #########################################################################


# #########################################################################
def get_freqs(ovals, col="", optimize=True):
    P = pd.DataFrame()

    if TARGET_VAR in col:
        return P

    try:
        # codes = pd.Categorical.from_array(ovals)
        codes = pd.Categorical(ovals)
        freqs = codes.describe().copy()
        # if len(freqs['counts']) == 2 and freqs['counts'].min() == 1:
        #   # print col, 'skipping, just one sample on data level 2 out of 2.'
        #   # return P
        freqs = freqs['freqs'].copy()

        try:
            colname = "%s_xfreqs" % col
            vals = pd.Series([freqs[x] * x for x in ovals])
            levels = set(vals)
            L = len(levels)
            if L >= DROP_VARS_WITH_LESS_THAN_NLEVELS:
                P[colname] = vals.copy()
                print("%s ...added xfreqs[%s]: %s" % (colname, col,
                                                      get_levels(levels)))
        except:
            pass

        if not optimize:
            try:
                colname = "%s_freqs" % col
                vals = pd.Series([freqs[x] for x in ovals])
                levels = set(vals)
                L = len(levels)
                if L >= DROP_VARS_WITH_LESS_THAN_NLEVELS:
                    P[colname] = vals.copy()
                    print("%s ...added  freqs[%s]: %s" % (colname, col,
                                                          get_levels(levels)))
            except:
                pass
    except Exception as err:
        print_exception('problem with nleveling coding',
                        err=err, with_traceback=False)

    if len(P):
        print(P.describe(include='all'))
    return P.copy()
# #########################################################################


# #########################################################################
def nleveling(vals, n=4, nmin=-4, col="", do_log=False):
    terminate = False
    try:
        vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
        vals = pd.Series(vals)
        if do_log:
            print('nleveling via log: ...', end=' ')
            try:
                vals = [1./x if x else NA_RECODE for x in vals]
                vals = pd.Series(vals)
            except:
                print(col, 'could not apply log transform on nleveling')
        else:
            print('nleveling: ...', end=' ')

        while not terminate:
            if n >= 0:
                vals = vals.round(decimals=n)
            else:
                vals = vals/10.
                vals = vals.round(decimals=0)

            levels = set(vals)
            L = len(levels)
            print("n=%s (%s), " % (n, L), end=' ')

            if L <= MAX_NUM_FACTOR_LEVELS:
                terminate = True

            n = n - 1
            if n < nmin:
                terminate = True

        print()
        levels = sorted(set(vals))
        L = len(levels)

        if L <= MAX_NUM_FACTOR_LEVELS:
            print("%82s" % col + "... and now it has", get_levels(levels))
            return vals.copy()

        if L > MAX_NUM_FACTOR_LEVELS:
            xvals = quantize_vector(vals, how="unit", col=col)
            xvals = pd.Series(xvals)
            levels = sorted(set(xvals))
            L = len(levels)
            # if L == 2 and not do_log:
            #    # xvals = nleveling(vals, col=col, do_log=True)
            #    # levels = sorted(set(xvals))
            #    # L = len(levels)
            print("%82s" % col + " ... and now has", get_levels(levels))
            return xvals.copy()

    except Exception as err:
        print_exception('problem with nleveling descaler', err=err,
                        with_traceback=False)

    vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
    vals = pd.Series(vals)
    levels = sorted(set(vals))
    L = len(levels)
    print("%82s" % col + " ... finally has", get_levels(levels))

    return vals.copy()
# #########################################################################


# #########################################################################
def decrease_nlevels_for(vals, nstart=4, col=""):
    vals = pd.Series(vals)
    vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
    vals = pd.Series(vals)
    levels = sorted(set(vals))
    L = len(levels)

    if L <= MAX_NUM_FACTOR_LEVELS:
        print("%82s" % col + " ... already has", get_levels(levels))
        return vals.copy()

    if L > MAX_NUM_FACTOR_LEVELS:
        print(col, 'QUANTIZING...')
        try:
            vals = quantize_vector(vals, col=col)
            print(col, '... retained as numerical but quantized...')
        except:
            vals = get_codes_from_categorical(vals, col=col)

        vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
        vals = pd.Series(vals)
        levels = sorted(set(vals))
        L = len(levels)
        print(col, '... currently has', get_levels(levels))

    if L <= MAX_NUM_FACTOR_LEVELS:
        print("%82s" % col + " ... and now has", get_levels(levels))
        return vals.copy()

    try:
        vals = nleveling(vals, n=nstart, col=col)

        if True:
            vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
            vals = pd.Series(vals)
            levels = sorted(set(vals))
            L = len(levels)
            print("... and now it has", get_levels(levels))

            if L > MAX_NUM_FACTOR_LEVELS:
                vals = quantize_vector(vals, how="zscore", col=col)
                vals = pd.Series(vals)
                levels = sorted(set(vals))
                L = len(levels)
                print(" ... and now has", get_levels(levels))
    except Exception as err:
        print_exception('problem with nleveling descaler', err=err)
    print('-' * 80)
    vals = pd.Series(vals)
    return vals.copy()
# #########################################################################


# #########################################################################
def is_projection_needed(X):
    if not len(X.columns):
        return False
    if len(X.columns) <= 2:
        for col in X.columns:
            if len(set(X[col])) < 2:
                return False
        return False
    return True
# #########################################################################


# #########################################################################
def get_projection_features_for(X, y=(),
                                ncols=1, max_num_iter=50000,
                                describe_ncols=5, debug=False):
    banner('computing projection features for %s' % X.columns)
    if BYPASS_LOCAL_PROJECTIONS:
        return pd.DataFrame()

    if not is_projection_needed(X):
        return pd.DataFrame()

    print('-' * 80)
    for i in range(describe_ncols, len(X.columns), describe_ncols):
        colset = [x for x in X.columns][i-describe_ncols:i]
        print(X[colset].describe(include='all').transpose())
        print('-' * 80)
    print('-' * 80)

    if not len(y):
        y = X[TARGET_VAR]

    X = X[[x for x in X.columns if TARGET_VAR not in x]]
    ncols = max(min((len(X.columns)-1)/2 - 1, ncols), 1)

    xtrain_rows = [i for i, yy in enumerate(y) if yy != TESTING_SET]
    if debug:
        print(len(xtrain_rows), xtrain_rows[0:12], xtrain_rows[-12:])

    for col in [x for x in X.columns if TARGET_VAR not in x]:
        vals = pd.Series(X[col])
        vals = get_codes_from_categorical(vals, col=col)
        vals = apply_scaler_for_column(xvals=vals, y=y, colname=col)
        X[col] = nleveling(vals, col=col)

    X = numpy.array(X).reshape(X.shape[0], X.shape[1])

    pnames = []
    try:
        XPCA = LDA(n_components=ncols, solver='svd')
        XPCA.fit(X[xtrain_rows, :], y[xtrain_rows])
        XX = XPCA.transform(X)
        print("LDA data.shape", XX.shape)
        pnames += get_pnames("LDA", XX.shape[1])
        lda_ok = True
    except Exception as err:
        print_exception('problem with LDA', err=err)
        lda_ok = False

    if not BYPASS_ICA_PROJECTIONS:
        XPCA = FastICA(n_components=ncols, max_iter=max_num_iter)
        XPCA.fit(X[xtrain_rows, :])
        X5 = XPCA.transform(X)
        if not lda_ok:
            XX = X5
        else:
            XX = numpy.hstack((XX, X5.reshape(X5.shape[0], X5.shape[1])))
        print("ICA data.shape", X5.shape)
        pnames += get_pnames("ICA", X5.shape[1])

    SCALER = StandardScaler()
    SCALER.fit(XX[(y != TESTING_SET)])
    XX = SCALER.transform(XX)

    XX = pd.DataFrame(XX)
    XX.columns = pnames
    print('-' * 80)
    for col in XX.columns:
        print(col)
        vals = pd.Series(XX[col])
        vals = get_codes_from_categorical(vals, col=col)
        vals = apply_scaler_for_column(xvals=vals, y=y, colname=col)
        XX[col] = nleveling(vals, col=col)
        print('    ' + '-' * 76)
    print('-' * 80)
    return XX
# #########################################################################


# #########################################################################
def apply_scaler_for_column(xvals=pd.Series(), y=pd.Series(),
                            colname="", robust=False):
    d = pd.DataFrame()
    d[colname] = xvals.copy()
    d[TARGET_VAR] = y.copy()
    try:
        SCALER = StandardScaler()
        if robust:
            SCALER = RobustScaler()

        SCALER.fit(d[d[TARGET_VAR] != TESTING_SET])

        d = pd.DataFrame(SCALER.transform(d))
        d.columns = [colname, TARGET_VAR]
        xvals = pd.Series([x for x in d[colname]])
        print(colname, 'was scaled with robust=', robust)
    except Exception as err:
        print_exception('problem with vector scaling',
                        err=err, with_traceback=False)
    xvals = pd.Series([x if pd.notnull(x) else NA_RECODE for x in xvals])
    return xvals.copy()
# #########################################################################


# #########################################################################
def apply_scaling(data, zvals=True, minmax=False, maxabs=False, robust=False):
    COLUMNS = [x for x in data.columns]
    y = data[TARGET_VAR].copy()

    if zvals:
        SCALER = StandardScaler()
        if robust:
            SCALER = RobustScaler()
        SCALER.fit(data[data[TARGET_VAR] != TESTING_SET])
        data = pd.DataFrame(SCALER.transform(data))
        data.columns = COLUMNS
        data[TARGET_VAR] = y.copy()

    if minmax:
        MINMAXSCALER = MinMaxScaler()
        MINMAXSCALER.fit(data[data[TARGET_VAR] != TESTING_SET])
        data = pd.DataFrame(MINMAXSCALER.transform(data))
        data.columns = COLUMNS
        data[TARGET_VAR] = y.copy()

    if maxabs:
        MAXABSSCALER = MaxAbsScaler()
        MAXABSSCALER.fit(data[data[TARGET_VAR] != TESTING_SET])
        data = pd.DataFrame(MAXABSSCALER.transform(data))
        data.columns = COLUMNS
        data[TARGET_VAR] = y.copy()

    return data
# #########################################################################


# #########################################################################
def apply_PCA(X, y=(), nmax=1000000):
    banner('apply pca')
    XPCA = None
    if not len(y):
        y = X[TARGET_VAR].copy()

    X = X[sorted([x for x in set(X.columns)
                  if TARGET_VAR not in x and LABEL_VAR not in x])]
    augmented_colnames = [x for x in X.columns if
                          TARGET_VAR not in x and LABEL_VAR not in x]
    X = X[augmented_colnames]
    M, N = X.shape

    train_rows = [i for i, yy in enumerate(y) if yy != TESTING_SET]
    n = len(train_rows)

    X = numpy.array(X)

    if n > nmax:
        train_rows = pd.Series(train_rows).\
            sample(min(nmax, n), random_state=get_random_seed())

    if 'kernel' == PCA_NCOMPONENTS['method']:
        XPCA = KernelPCA(n_components=PCA_NCOMPONENTS['n'], kernel='rbf')
        XPCA.fit(X[train_rows, :])
        X = XPCA.transform(X)

    elif 'random' == PCA_NCOMPONENTS['method']:
        XPCA = RandomizedPCA(n_components=PCA_NCOMPONENTS['n'])
        XPCA.fit(X)
        X = XPCA.transform(X)

    elif 'basic' == PCA_NCOMPONENTS['method']:
        XPCA = PCA(n_components=PCA_NCOMPONENTS['n'])
        XPCA.fit(X)
        X = XPCA.transform(X)

    elif 'lda' == PCA_NCOMPONENTS['method']:
        eta = numpy.random.random(M*N).reshape(M, N)/1E6
        X = X + eta
        XPCA = LDA(n_components=PCA_NCOMPONENTS['n'])
        XPCA.fit(X[train_rows, :], y[train_rows])
        X = XPCA.transform(X)

    elif 'svc' == PCA_NCOMPONENTS['method']:
        banner('computing regularization towards feature selection')
        try:
            # raise Exception, "grid skipped"
            params = {'penalty': ('l1', ),
                      'C': (1./16., 1./32., 1./64., 1./96., 1./128., 1./144.),
                      'dual': (False, ),
                      'fit_intercept': (False, ),
                      'class_weight': (None, )}
            XPCA = LinearSVC()
            XPCA = grid_search.GridSearchCV(XPCA, params, cv=5, error_score=0)
            xtrain_rows = pd.Series(train_rows).\
                sample(n/2, random_state=get_random_seed())
            XPCA.fit(X[xtrain_rows, :], y[xtrain_rows])
            print('-' * 80)
            print(params)
            print(XPCA)
            print('-' * 80)
            if hasattr(XPCA, "best_params_"):
                best_params = XPCA.best_params_
                print("XPCA PARAMS", best_params)
                print('-' * 80)
                REGULARIZATION = best_params['C']
                CLASS_WEIGHTS = best_params['class_weight']
        except Exception as err:
            print_exception('problem with PCA', err=err)
            CLASS_WEIGHTS, REGULARIZATION = None, DEFAULT_REGULARIZATION

        banner('applying regularization feature selection')
        try:
            XPCA = LinearSVC(penalty='l1',
                             C=REGULARIZATION,
                             dual=False,
                             fit_intercept=False,
                             class_weight=CLASS_WEIGHTS)
            xtrain_rows = pd.Series(train_rows).\
                sample(n/2, random_state=get_random_seed())
            print("fitting", X.shape)
            PCA.fit(X[xtrain_rows, :], y[xtrain_rows])
            svc_coeffs = XPCA.coef_[0]
            X00 = numpy.array(numpy.ma.average(X, axis=1, weights=svc_coeffs))
            print("transforming", X.shape)
            X = XPCA.transform(X)
            print("data.shape", X.shape)

            X01 = X.max(axis=1)
            X02 = X00/X01
            X = numpy.hstack((X, X00.reshape(X.shape[0], 1)))
            augmented_colnames += get_pnames("SVC_WEIGHTS", 1)
            X = numpy.hstack((X, X02.reshape(X.shape[0], 1)))
            augmented_colnames += get_pnames("SVC_MAX", 1)
            print("data.shape", X.shape)
        except Exception as err:
            print_exception('problem with pca:svc coeffs', err=err)

    if not BYPASS_GLOBAL_PROJECTIONS:
        try:
            banner('creating pca/lda projection features')
            print("data.shape", X.shape)

            XPCA = LDA(n_components=PCA_NCOMPONENTS['n'])
            XPCA.fit(X[train_rows, :], y[train_rows])
            X1 = XPCA.transform(X)
            print("data.shape", X1.shape)

            XPCA = LDA(n_components=1)
            XPCA.fit(numpy.abs(X[train_rows, :]), y[train_rows])
            X2 = XPCA.transform(X)
            print("data.shape", X2.shape)

            banner('creating ica projection features')
            if NUM_ICA_DIMS and not BYPASS_ICA_PROJECTIONS:
                XPCA = FastICA(n_components=NUM_ICA_DIMS, max_iter=50000)
                XPCA.fit(X[train_rows, :])
                X5 = XPCA.transform(X)
                print("data.shape", X5.shape)
                X = numpy.hstack((X, X5.reshape(X5.shape[0], X5.shape[1])))
                augmented_colnames += get_pnames("ICA_SIGNAL", X5.shape[1])

            X = numpy.hstack((X, X1.reshape(X1.shape[0], X1.shape[1])))
            augmented_colnames += get_pnames("FULL_LDA_SIGNAL", X1.shape[1])
            # --------------------------------------------------------------
            X = numpy.hstack((X, X2.reshape(X2.shape[0], X2.shape[1])))
            augmented_colnames += get_pnames("FULL_LDA_abs_SIGNAL", X2.shape[1])
            # --------------------------------------------------------------
            print("data.shape", X.shape)
        except Exception as err:
            print_exception('problem with lda:ica', err=err)

        banner('creating lda interaction features')
        try:
            X1 = X1[:, 0]
            X1 = numpy.nan_to_num(X1)
            X1 = quantize_vector(X1, col="SIGNAL").copy()
            # --------------------------------------------------------------
            XX0 = (X1 + 1) * (X1 - 1)
            X = numpy.hstack((X, XX0.reshape(X.shape[0], 1)))
            augmented_colnames += get_pnames("FULL_LDA_SSQR_SIGNAL", 1)
            # --------------------------------------------------------------
            XX0 = (X1 - 1) * (X1 - 1)
            X = numpy.hstack((X, XX0.reshape(X.shape[0], 1)))
            augmented_colnames += get_pnames("FULL_LDA_DSQR_SIGNAL", 1)
            # --------------------------------------------------------------
            XX0 = (X1 + 1) * (X1 + 1) * (X1 - 1)
            X = numpy.hstack((X, XX0.reshape(X.shape[0], 1)))
            augmented_colnames += get_pnames("FULL_LDA_THRD_SIGNAL", 1)
            # --------------------------------------------------------------
            XX0 = (numpy.abs(X1) - 1) * (X1 - 1)
            X = numpy.hstack((X, XX0.reshape(X.shape[0], 1)))
            augmented_colnames += get_pnames("FULL_LDA_RSQR_SIGNAL", 1)
            # --------------------------------------------------------------
            XX0 = numpy.sqrt(X1 + 1)
            X = numpy.hstack((X, XX0.reshape(X.shape[0], 1)))
            augmented_colnames += get_pnames("FULL_LDA_SROT_SIGNAL", 1)
            # --------------------------------------------------------------
        except Exception as err:
            print_exception('problem with lda:matrix', err=err)
        print("data.shape", X.shape)

    # -----------------------------------------------------------------
    X = pd.DataFrame(X)
    X.columns = augmented_colnames
    # -----------------------------------------------------------------
    try:
        print('-' * 80)
        if True:
            for col in X.columns:
                if "SIGNAL" in col:
                    print(col)
                    vals = pd.Series(X[col])
                    vals = get_codes_from_categorical(vals, col=col)
                    vals = apply_scaler_for_column(xvals=vals, y=y, colname=col)
                    X[col] = nleveling(vals, col=col)
                    print('    ' + '-' * 76)

        try:
            X[TARGET_VAR] = y.copy()
            fvars = feature_select(X,
                                   X[TARGET_VAR],
                                   X.columns,
                                   p=FS_PERCENTAGES['final'],
                                   min_score=FS_MINSCORES['final'],
                                   sorted_by_importance=True)
            del X[TARGET_VAR]
        except Exception as err:
            print_exception('problem with final fselect', err=err)
            fvars = X.columns

        for v in X.columns:
            if v in fvars and TARGET_VAR not in v and LABEL_VAR not in v:
                continue
            print('dropping not fselected column', v)
            del X[v]

        M, N = print_data_shape(X)
        print("data.shape", X.shape)

        try:
            final_columnset = X.columns
            SCALER = StandardScaler()
            SCALER.fit(X[y != TESTING_SET])
            SCALER.fit(X)
            X = SCALER.transform(X)
            X = pd.DataFrame(X)
            X.columns = final_columnset
        except Exception as err:
            print_exception('problem with final scaling', err=err)

    except Exception as err:
        print_exception('problem with final fselect', err=err)

    return X
# #########################################################################


# #########################################################################
def get_nlevels_for(vals, round_off=0, na_recode=True, col=""):
    vals = pd.Series(vals)
    if na_recode:
        vals = [x if pd.notnull(x) else NA_RECODE for x in vals]

    vals = pd.Series(vals)
    if round_off:
        vals = vals.round(decimals=round_off)

    levels = sorted(set(vals))
    L = len(levels)
    # print "*", col, ": COL has:", get_levels(levels)
    return vals, L
# #########################################################################


# #########################################################################
def apply_vector_normalization(vals,
                               centered=False,
                               round_off=NORM_RECODING_DIGITS):
    vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
    vals = pd.Series(vals)

    if BYPASS_VECTOR_NORMALIZATION:
        return vals

    try:
        y_minval = min(vals.min(), 0.)
        y_maxval = max(vals.max(), 1.)
    except:
        y_minval = min(numpy.min(vals), 0.)
        y_maxval = max(numpy.max(vals), 1.)

    vals = (vals - y_minval) / (y_maxval - y_minval)

    if centered:
        vals = vals * 2.0 - 1.0

    if round_off:
        vals = vals.round(decimals=round_off)

    vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
    vals = pd.Series(vals)

    return vals
# #########################################################################


# #########################################################################
def round_off_vector(vals, round_off=NORM_RECODING_DIGITS):
    vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
    vals = pd.Series(vals)
    if round_off:
        vals = vals.round(decimals=round_off)
    return vals
# #########################################################################


# #########################################################################
def get_zvals(vals, do_rounding_off=ZVALS_RECODING_DIGITS):
    vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
    vals = pd.Series(vals)

    vals = vals.astype(NUMERICAL_DATATYPE)
    vals = (vals - vals.mean())/vals.std()

    vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
    vals = pd.Series(vals)

    if do_rounding_off:
        vals = vals.round(decimals=do_rounding_off)
        vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
        vals = pd.Series(vals)
    return vals.copy()
# #########################################################################


# #########################################################################
def zscore_vector(vals, round_off=ZSCORE_RECODING_DIGITS):
    vals = get_zvals(vals)
    if round_off:
        vals = vals.round(decimals=round_off)
    return vals
# #########################################################################


# #########################################################################
def get_ivals(vals, round_off=NORM_RECODING_DIGITS, col=""):
    vals = pd.Series(vals)
    try:
        minval = vals.min()
    except:
        minval = 0.0

    vals = vals.fillna(NA_RECODE)
    validation_check = vals - minval + 1.0
    ivals = 1.0/validation_check
    ivals = ivals.fillna(NA_RECODE)

    levels = sorted(set(ivals))
    print(col, "transformed via reciprocals into:", get_levels(levels))
    return ivals
# #########################################################################


# #########################################################################
def get_vals_from_codes(codes,
                        round_off=NORM_RECODING_DIGITS,
                        use_reciprocals=USE_RECIPROCAL_CODING_FOR_CATEGORIES,
                        col=""):
    try:
        if USE_RECIPROCAL_CODING_FOR_CATEGORIES:
            codes = get_ivals(codes, round_off=round_off, col=col)
    except Exception as err:
        print_exception('problem with reciprocal coding',
                        err=err, with_traceback=False)
        codes = get_codes_from_categorical(codes, col=col)
        return codes

    try:
        codes = decrease_nlevels_for(codes, col=col)
    except Exception as err:
        print_exception('problem with nleveling in val-codes',
                        err=err, with_traceback=False)

    return codes
# #########################################################################


# #########################################################################
def quantize_vector(vals, round_off=NORM_RECODING_DIGITS, how="unit", col=""):
    print("QUANTIZING COLUMN: %s..." % col)

    vals = nleveling(vals, col=col)
    levels = sorted(set(vals))
    L = len(levels)

    if L <= MAX_NUM_FACTOR_LEVELS:
        print("%82s" % col, "seems ok, after na_recode/rounding w/", L, "levels")
        return vals

    if L > MAX_NUM_FACTOR_LEVELS:
        print("%82s" % col, "may overfit w/", L, "levels")

    print("%82s" % col, "generating quantized replacement:")
    vals = get_zvals(vals)
    vals = nleveling(vals, col=col)
    levels = sorted(set(vals))
    L = len(levels)
    if L <= MAX_NUM_FACTOR_LEVELS:
        return vals

    vals = apply_vector_normalization(vals, round_off=NORM_RECODING_DIGITS)
    vals = nleveling(vals, col=col)
    levels = sorted(set(vals))
    L = len(levels)
    if L > MAX_NUM_FACTOR_LEVELS:
        print("%82s" % col, "could not find suitable quantization")
    return vals
# #########################################################################


# #########################################################################
def is_decorrelation_criteria_met(Q, keep, drop,
                                  min_num_corr_tuples,
                                  maxcorr=GLOBAL_RSQRD_THRESHOLD):
    print(Q, len(list(keep.keys())), len(list(drop.keys())), int(min_num_corr_tuples))
    print('-' * 80)
    if len(list(keep.keys())) > min_num_corr_tuples:
        return True
    if Q <= maxcorr and len(list(keep.keys())):
        return True
    return False
# #########################################################################


# #########################################################################
# assumes sorting by importance when replacing by most significant, otherwise
# replaces by "equivalent" ones (in terms of extremely high correlation)
# #########################################################################
def decorrelate(A, colset=(),
                start=1, end=MINIMUM_DECORRELATION_LEVEL,
                step=1, nmax=0,
                decorrelation_effort=0.20,
                debug=0):
    if not colset:
        colset = [x for x in A.columns if TARGET_VAR not in x]
    else:
        colset = [x for x in colset if TARGET_VAR not in x and x in A.columns]

    minlen = len(colset)*decorrelation_effort
    if nmax:
        nmax = min(nmax, len(A))
        C = A[colset].sample(nmax, random_state=get_random_seed()).corr()
    else:
        C = A[colset].corr()
    D = C.transpose() * C
    for q in range(start, end, step):
        print('-' * 80)
        keep, drop, Q = {}, {}, (100.-q)/100.
        print(Q)

        E = (D > Q).astype('int')
        F = E.sum(axis=0)
        G = [(i, [j for j, k in enumerate(E[colset[i]] > 0) if k])
             for i, x in enumerate(F > 1) if x]

        for i, x in enumerate(G):
            selected = x[0]
            correlated_to_selected = x[1]
            if not len([k for k in correlated_to_selected if k in keep]):
                keep[selected] = [x for x in correlated_to_selected
                                  if x != selected]
                if debug:
                    print("%s added to %s" % (selected, keep))

        if is_decorrelation_criteria_met(Q, keep, drop, minlen):
            print('-' * 80)
            print("CORRELATED COLUMNS SETS", keep)
            print('-' * 80)
            for selected in keep:
                print("K:", colset[selected], "D:[", end=' ')
                for d in keep[selected]:
                    dropcol = colset[d]
                    drop[dropcol] = (d, dropcol, selected)
                    print(dropcol, end=' ')
                print("]; ")
            print('-' * 80)
            print("QVAL", Q)
            if debug:
                print("DROP", sorted(drop.keys()))
                print(G)
                print("KEEP", sorted(keep.keys()))
            print('-' * 80)
            break
    return Q, keep, drop
# #########################################################################


# #########################################################################
def generate_substitute_for_decorrelated_vars_from(data, old_col, corr_col):
    if True:
        return pd.DataFrame()

    d = pd.DataFrame()
    vals1 = get_codes_from_categorical(data[old_col], col=old_col)
    vals0 = get_codes_from_categorical(data[corr_col], col=corr_col)
    d[old_col] = data[old_col]
    d[corr_col] = data[corr_col]
    d["SUBST_%s_%s_diff" % (old_col, corr_col)] = (vals1 - vals0).copy()
    d["SUBST_%s_%s_dsqr" % (old_col, corr_col)] = \
        (vals0.codes + vals1.codes) * (vals0.codes - vals1.codes)
    d["SUBST_%s_%s_ssqr" % (old_col, corr_col)] = \
        (vals0.codes - vals1.codes) * (vals0.codes + vals1.codes)
    d["SUBST_%s_%s_mxmn" % (old_col, corr_col)] = \
        d.max(axis=1) - d.min(axis=1)
    cols = d.columns
    print(d[cols].describe(include='all').transpose())
    print('-' * 80)

    try:
        c = d.corr().abs()
        col = c.idxmin()[0]
        cols = []
        if corr_col != col:
            cols = [col, ]
        print("keeping minimal decorrelated col", cols, c.min()[0])
    except Exception as err:
        print_exception('problem with decorrelation', err=err)
        cols = d.columns

    d = d[cols]
    return d.copy()
# #########################################################################


# #########################################################################
def process_ssn(data, vals, col):
    d = pd.DataFrame()

    print(col, 'attempt to process as ssn four digit')
    colname = "%s_ssn4" % col
    vals = [str(x)[0:4] if pd.notnull(x) else "0000" for x in data[col]]
    d[colname] = get_codes_from_categorical(vals, col=colname)
    print(d[colname].describe(include='all'))

    print(col, 'attempt to process as ssn three digit')
    colname = "%s_ssn3" % col
    vals = [str(x)[0:3] if pd.notnull(x) else "000" for x in data[col]]
    d[colname] = get_codes_from_categorical(vals, col=colname)
    print(d[colname].describe(include='all'))

    print(col, 'attempt to process as ssn two digit')
    colname = "%s_ssn2" % col
    vals = [str(x)[0:2] if pd.notnull(x) else "00" for x in data[col]]
    d[colname] = get_codes_from_categorical(vals, col=colname)
    print(d[colname].describe(include='all'))

    return d
# #########################################################################


# #########################################################################
def process_numeric(data, vals, col,
                    do_zvals=False, do_cuts=True, do_unit=False,
                    overwrite_col=True, debug=False):
    vals = pd.Series(vals)
    d = pd.DataFrame()

    get_zvals(data[col])

    if debug:
        print(col, 'recognized it as and processing as numeric')
        print(data[col].describe(include='all'))
        print('-' * 80)

    simple_maxval = data[col].max()
    maxval = max([x for x in data[col] if x not in NA_LARGE_VALS])
    if maxval < simple_maxval:
        for k, mx in enumerate(NA_LARGE_VALS.keys()):
            vals[vals == mx] = maxval + k + 1

    vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
    vals = pd.Series(vals)

    if col not in ORIGINAL_VARIABLES and 'SIGNAL' not in col:
        print(col, 'not an original variables, skipping some features')
        d[col] = vals.copy()
        levels = sorted(set(vals))
        print(col, "NOT transformed, w/:", get_levels(levels))
        return d

    if do_zvals:
        try:
            print()
            colname = "%s_zvals_%s" % (col, ZVALS_RECODING_DIGITS)
            d[colname] = get_zvals(vals).copy()
            levels = sorted(set(d[colname]))
            print(colname, "transformed into zvals, now w/:", get_levels(levels))
            if debug:
                print(d[colname].describe(include='all'))
                print('-' * 80)
        except Exception as err:
            print_exception('problem with zvals', err=err, with_traceback=False)

    if do_unit:
        try:
            colname = "%s_norm_%s" % (col, NORM_RECODING_DIGITS)
            d[colname] = quantize_vector(vals, how="unit", col=colname).copy()
            levels = sorted(set(d[colname]))
            print(colname, "transformed into unit, now w/:", get_levels(levels))
            if debug:
                print(d[colname].describe(include='all'))
                print('-' * 80)
        except Exception as err:
            print_exception('problem with norm', err=err, with_traceback=False)

    if do_cuts:
        try:
            d[colname] = optimize_factor_cuts(data, col=col)
        except Exception as err:
            print_exception('problem with cuts', err=err, with_traceback=False)
            colname = "%s_cuts_%s" % (col, L_STEP)
            dmax, dmin = data[col].max() + 1, data[col].min() - 1
            dstp = float(dmax-dmin)/MAX_NUM_FACTOR_LEVELS
            tvals = pd.cut(data[col], numpy.arange(dmin, dmax, dstp))
            tvals = get_codes_from_categorical(tvals, col=colname)
            d[colname] = tvals.copy()
            levels = sorted(set(d[colname]))
            print(colname, "transformed into cuts, now w/:", get_levels(levels))
            if debug:
                print(d[colname].describe(include='all'))
                print('-' * 80)

    try:
        if overwrite_col:
            if colname in d:
                d[col] = d[colname].copy()
                del d[colname]
    except:
        pass

    return d
# #########################################################################


# #########################################################################
def optimize_factor_cuts(data, yvals=(), col="", do_speedup=True,
                         clim=255, min_ncuts=8, q=8., nmax=100000,
                         override=False, overwrite=True, debug=False):
    d = pd.DataFrame()

    if is_categorical(data[col], col=col):
        print(col, 'is_categorical, numerical cuts processing skipped')
        return data[col]

    xvals = get_codes_from_categorical(data[col], col=col)
    L = len(set(xvals))

    if L < min(MAX_NUM_FACTOR_LEVELS, clim) and not override:
        print(col, 'is_numerical, but few levels, processing skipped', L)
        return data[col]

    if not len(yvals):
        yvals = data[TARGET_VAR]

    if debug:
        print(xvals.describe())
        print('-' * 80)

    maxval, minval = xvals.max(), xvals.min()
    if maxval == minval:
        print('max and min are the same, processing skipped', maxval, minval)
        return data[col]

    delta = min((maxval - minval)/1000., 1e-3)
    valrng = float((maxval + delta) - (minval))
    numsteps = min(L, MAX_NUM_FACTOR_LEVELS) + 1
    if min_ncuts > 1:
        numsteps = min(L, MAX_NUM_FACTOR_LEVELS, min_ncuts) + 1
    stepsize = float(valrng)/float(numsteps)
    print('-' * 80)
    print("SEARCH: min=%10s max=%10s numsteps=%10s stepsize=%10s" % \
        (round(minval, 2), round(maxval, 2),
         round(numsteps*q, 2), round(stepsize/q, 2)))

    d[TARGET_VAR] = data[TARGET_VAR]
    for alt_stepsize in numpy.arange(stepsize/q, stepsize, stepsize/q):
        colname = "%s_cuts_with_stepsize=%s" % (col, alt_stepsize)
        steps = numpy.arange(minval, maxval, alt_stepsize)
        if debug:
            print("using stepsize=%10s, steps=%s" % (alt_stepsize, steps))
            print(d[colname].describe(include='all'))
            print('-' * 80)
        d[colname] = pd.cut(xvals, steps)
        if do_speedup:
            #d[colname] = pd.Categorical.from_array(d[colname]).codes
            d[colname] = pd.Categorical(d[colname]).codes

    xvars = [x for x in d.columns if x != TARGET_VAR]
    fvars = feature_select(d, yvals, xvars,
                           sorted_by_importance=True,
                           nmax=nmax,
                           debug=False)

    if not len(fvars):
        fvars = xvars

    colname = fvars[0]
    if overwrite:
        data[col] = d[colname].copy()
    else:
        data[colname] = d[colname].copy()

    if debug:
        banner(colname)
        y_conditioned_vals = ["%s_%s" % (x, y) for x, y in
                              zip(d[colname], d[TARGET_VAR])
                              if y != TESTING_SET]
        # y_conditioned_vals = pd.Categorical.from_array(y_conditioned_vals)
        y_conditioned_vals = pd.Categorical(y_conditioned_vals)
        print(y_conditioned_vals.describe())
        print('-' * 80)
        # print(pd.Categorical.from_array(d[colname]).describe())
    print('-' * 80)
    return d[colname]
# #########################################################################


# #########################################################################
def process_date(data, vals, col):
    d = pd.DataFrame()

    print(col, 'attempt to process as date')
    if col not in DATE_COLS:
        raise Exception

    print(col, 'year encoding')
    colname = "%s_year" % col
    d[col] = [x if pd.notnull(x) else NA_RECODE for x in data[col]]
    vals = [str(x)[5:7] if x != NA_RECODE else "00" for x in d[col]]
    d[colname] = get_codes_from_categorical(vals, col=colname)
    print(d[colname].describe(include='all'))

    print(col, 'day encoding')
    colname = "%s_day" % col
    vals = [str(x)[0:2] if x != NA_RECODE else "00" for x in data[col]]
    d[colname] = get_codes_from_categorical(vals, col=colname)
    print(d[colname].describe(include='all'))

    print(col, 'month encoding')
    colname = "%s_mon" % col
    vals = [str(x)[2:5] if x != NA_RECODE else "JAN" for x in data[col]]
    d[colname] = get_codes_from_categorical(vals, col=colname)
    print(d[colname].describe(include='all'))

    print(col, 'date encoding')
    vals = [str(x)[0:7] if x != NA_RECODE else "00JAN00" for x in data[col]]
    vals = [x[5:7] + x[2:5] for x in vals]
    d[col] = get_codes_from_categorical(vals, col=col)
    print(d[col].describe(include='all'))

    return d
# #########################################################################


# #########################################################################
def process_title(data, vals, col, ncol=3, extra_effort=1, debug=False):
    if debug:
        print(col, 'attempt to process as title')
        print(data[col].describe(include='all'))

    d = pd.DataFrame()
    if BYPASS_SPECIAL_FGENS:
        return d

    colname = "%s_frst%schars" % (col, ncol)
    vals = ["".join([x for x in str(x) if x.isalpha()]) for x in data[col]]
    vals = [str(x)[:ncol] for x in vals]
    d[colname] = get_codes_from_categorical(vals, col=colname)
    if debug:
        print(d[colname].describe(include='all'))

    colname = "%s_scores" % col
    vals = [is_topfreq_titled(x) for x in data[col]]
    d[colname] = get_codes_from_categorical(vals, col=colname)
    if debug:
        print(d[colname].describe(include='all'))

    if extra_effort > 0:
        colname = "%s_codes" % col
        d[colname] = get_codes_from_categorical(vals, col=colname)
        if debug:
            print(d[colname].describe(include='all'))

        colname = "%s_last%schars" % (col, ncol)
        vals = ["".join([x for x in str(x) if x.isalpha()]) for x in data[col]]
        vals = [str(x)[-ncol:] for x in vals]
        d[colname] = get_codes_from_categorical(vals, col=colname)
        print(d[colname].describe(include='all'))
        if debug:
            print(d[colname].describe(include='all'))

        colname = "%s_chrlen" % col
        vals = [len(str(get_encoded_token(x))) for x in data[col]]
        d[colname] = get_codes_from_categorical(vals, col=colname)
        if debug:
            print(d[colname].describe(include='all'))

    return d
# #########################################################################


# #########################################################################
def derived_from_paired_columns(data, col, prev_col,
                                extra_effort=0, only_origvars=True, debug=False,
                                skip_terms=(TARGET_VAR,
                                            'ICA', 'LDA', 'HSV', 'PAIR')):
    d = pd.DataFrame()
    if BYPASS_PAIRED_FEATURES:
        return d

    if col not in data or prev_col not in data:
        return d

    if TARGET_VAR == col or TARGET_VAR == prev_col:
        return d

    if only_origvars:
        if col not in ORIGINAL_VARIABLES or prev_col not in ORIGINAL_VARIABLES:
            return d

    for term in skip_terms:
        if term in col or term in prev_col:
            return d

    vals1 = [x if pd.notnull(x) else NA_RECODE for x in data[col]]
    vals0 = [x if pd.notnull(x) else NA_RECODE for x in data[prev_col]]

    try:
        vals1 = quantize_vector(vals1, col=col)
    except:
        # vals1 = pd.Categorical.from_array(vals1)
        # vals1 = vals1.codes
        vals1 = pd.Categorical(vals1).codes

    try:
        vals0 = quantize_vector(vals0, col=col)
    except:
        # vals0 = pd.Categorical.from_array(vals0)
        # vals0 = vals0.codes
        vals0 = pd.Categorical(vals0).codes

    if extra_effort:
        vals = vals0 * vals1
        colname = "%s_%s_mult" % (col, prev_col)
        d[colname] = nleveling(get_zvals(vals), col=colname)
        if debug:
            print(d[colname].describe(include='all'))

    try:
        vals = [min(max(x, y), 1.0) for (x, y) in zip(vals0, vals1)]
        colname = "%s_%s_mxmn" % (col, prev_col)
        d[colname] = nleveling(get_zvals(vals), col=colname)
        if debug:
            print(d[colname].describe(include='all'))

        vals = [max(min(x, y), 0.0) for (x, y) in zip(vals0, vals1)]
        colname = "%s_%s_mnmx" % (col, prev_col)
        d[colname] = nleveling(get_zvals(vals), col=colname)
        if debug:
            print(d[colname].describe(include='all'))
    except:
        pass

    return d
# #########################################################################


# #########################################################################
def derived_from_highly_significant_vars(data, col, vals,
                                         extra_effort=1,
                                         only_origvars=True,
                                         skip_terms=(TARGET_VAR, )):
    d = pd.DataFrame()
    if BYPASS_HSVAR_FEATURES:
        return d

    if TARGET_VAR == col:
        return d

    if only_origvars:
        if col not in ORIGINAL_VARIABLES:
            return d

    for term in skip_terms:
        if term in col:
            return d

    if is_highly_significant_var(col):
        banner("HIGHLY SIGNIGICANT VAR PROCESSING: %s" % col)

        if extra_effort > 0:
            vals = get_codes_from_categorical(data[col], col=col, how="codes")
            qvals = get_codes_from_categorical(data[col], col=col, how="counts")
            colname = "%s_vxq" % col
            d[colname] = quantize_vector(qvals * vals, col=colname)
            print(d[colname].describe(include='all'))

        if extra_effort > 1:
            zvals = get_codes_from_categorical(data[col], col=col, how="freqs")
            colname = "%s_vxz" % col
            d[colname] = quantize_vector(zvals * vals, col=colname)
            print(d[colname].describe(include='all'))

            colname = "%s_qxz" % col
            d[colname] = quantize_vector(qvals * zvals, col=colname)
            print(d[colname].describe(include='all'))

    return d
# #########################################################################


# #########################################################################
def set_in_order_given(ordered):
    final_set = []
    for i, x in enumerate(ordered):
        if x not in final_set:
            final_set.append(x)
    return final_set
# #########################################################################


# #########################################################################
def get_interaction_terms(data, fast_mode=False,
                          ntop=BYPASS_INTERACTIONS_AFTERTOP):
    TOPVARS = []
    try:
        colset = [x for x in data if TARGET_VAR not in x and 'MARGIN' not in x]
        if len(colset):
            TOPVARS = feature_select(data, data[TARGET_VAR], colset,
                                     p=25./100., min_score=100,
                                     sorted_by_importance=True)
    except Exception as err:
        print_exception('problem with identifying topvars fselect', err=err)

    AUGMENTED_INTERACTION_TERMS = \
        set_in_order_given([x for x in INTERACTION_TERMS]+TOPVARS)

    first_order_terms = [x for x in AUGMENTED_INTERACTION_TERMS
                         if x in data and x not in [TARGET_VAR, ID_VAR]]
    first_order_terms = first_order_terms[:ntop]
    if fast_mode:
        interactions = list(zip(first_order_terms[0:], first_order_terms[1:]))
        return interactions

    interactions = []
    for i, v1 in enumerate(first_order_terms):
        for j, v2 in enumerate(first_order_terms[i+1:]):
            interactions.append((v1, v2))

    # interactions = sorted(set(interactions))

    interactions = set_in_order_given(interactions)
    return interactions
# #########################################################################


# #########################################################################
def is_valid_xterm(L, colname):
    if L < DROP_VARS_WITH_LESS_THAN_NLEVELS:
        print(colname, 'dropped due to too few L values:', L)
        return False
    if L > DROP_XTERMS_WITH_NLEVELS:
        print(colname, 'dropped due to too many L values:', L)
        return False
    return True
# #########################################################################

print('%s:DONE' % __name__)
