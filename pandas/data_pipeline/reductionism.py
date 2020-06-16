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
import traceback
# ---------------------------------------------------------------------------
from pipeline_configuration import CODE_CATEGORIES_VIA
from pipeline_configuration import FSELECT_MINSCORE
from pipeline_configuration import GLOBAL_RSQRD_THRESHOLD
from pipeline_configuration import ID_VAR
from pipeline_configuration import MINIMUM_DECORRELATION_LEVEL
from pipeline_configuration import NA_RECODE
from pipeline_configuration import TARGET_VAR
from pipeline_configuration import TESTING_SET
# ---------------------------------------------------------------------------
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import RobustScaler
# ---------------------------------------------------------------------------
from collections import defaultdict
# ---------------------------------------------------------------------------
from sklearn.preprocessing import Imputer
# ---------------------------------------------------------------------------
from sklearn.feature_selection import chi2
from sklearn.feature_selection import SelectKBest
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
PRIMES = [47411, 29771, 11291, 1571, 12851, 38291, 27971, 8051, 27131, 7571,
          37451, 26411, 4331, 16331, 19931, 14171, 9491, 35051, 38771, 44051,
          23291, 731, 7331, 26051, 34331, 36731, 48731, 21251, 42851, 42491,
          27251, 18491, 15611, 40571, 31811, 42251, 11, 39611, 49211, 30971,
          32171, 27731, 48131, 45971, 43211, 13331, 19571, 49571, 12731, 38651,
          46571, 15971, 47771, 23891, 12131, 8891, 19331, 18611, 46931, 44291,
          49811, 5411, 40091, 25091, 21371, 38531, 5291]
# ---------------------------------------------------------------------------
RANDOM_STATE = [x[0]*x[1] for x in zip(PRIMES[12:], reversed(PRIMES))]
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
            vals = pd.Categorical(vals)
            vals = vals.codes
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


# #########################################################################
def get_codes_from_categorical(vals, col="",
                               how=CODE_CATEGORIES_VIA,
                               na_recode=True,
                               debug=False):
    vals = pd.Series(vals)
    if TARGET_VAR in col:
        # print(pd.Categorical.from_array(vals).describe())
        print(pd.Categorical(vals).describe())
        return vals.copy()

    if is_categorical(vals, col=col):
        if na_recode:
            vals = [x if pd.notnull(x) else NA_RECODE for x in vals]
            vals = pd.Series(vals)

        # vals = pd.Categorical.from_array(vals)
        vals = pd.Categorical(vals)
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
        if debug:
            vals = pd.Series(vals)
            print(vals.describe())
            print('-' * 80)
    return vals.copy()
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

    m = max(min(24, len(wrt_vars)/2), 1)

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
if __name__ == "__main__":
    data = pd.read_csv('DATA/merged.csv', sep="|")

    banner('decorrelation')
    Q, _, dropvars = decorrelate(data,
                                 colset=[c for c in data if c != TARGET_VAR])
    if dropvars:
        keepvars = [c for c in data if c not in dropvars] + [TARGET_VAR, ]
        data = data[keepvars]
        print(Q, dropvars)

    banner('feature selection')
    keepvars = feature_select(data, data[TARGET_VAR], data.columns,
                              min_score=1, p=99./100, sorted_by_importance=True)
    if keepvars:
        keepvars = [c for c in data if c != TARGET_VAR] + [TARGET_VAR, ]
        data = data[keepvars]

