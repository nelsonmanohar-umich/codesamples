import time
import random
import pandas as pd
from pipeline_configuration import TARGET_VAR, TESTING_SET, NA_RECODE
from feature_generators import banner
from feature_generators import feature_select
from feature_generators import is_categorical
from feature_generators import ORIGINAL_VARIABLES
from sklearn.metrics import confusion_matrix
from classifier_configuration import get_feature_impt_dropset
from sklearn.ensemble import GradientBoostingClassifier
from pipeline_configuration import FACTORSELECTOR_VMAX
from pipeline_configuration import FACTORSELECTOR_FMAX
from pipeline_configuration import FACTORSELECTOR_MAXTIME
from pipeline_configuration import FACTORSELECTOR_CATEGORICAL_LIMIT
from pipeline_configuration import FACTORSELECTOR_IMPT_THRESHOLD


# #########################################################################
# from feature_generators import get_codes_from_categorical
# from sklearn.preprocessing import MultiLabelBinarizer
# from sklearn.feature_extraction import DictVectorizer
# from sklearn.feature_extraction.text import CountVectorizer
# from sklearn.feature_extraction import FeatureHasher
# #########################################################################


# #########################################################################
def is_small_factor(vals, col="", categorical_limit=32, debug=False):
    vals, is_factor = pd.Series(vals), False
    try:
        vals.mean()
        ftype = "is_numerical"
    except Exception:
        ftype = "is_categorical"

    L = len(set(vals))
    if L < categorical_limit:
        ftype = "is_smallfactor"

    if debug:
        print(col, ftype)

    if "is_smallfactor" in ftype:
        print('%s is_smallfactor [%s levels]' % (col, L))
        is_factor = True

    return is_factor
# #########################################################################


# #########################################################################
def terminate(d, start, maxtime, vmax):
    terminate = False

    if (time.time() - start) > maxtime:
        print('a max compute time exceeded', time.time() - maxtime)
        terminate = True

    if len(d.columns) > vmax:
        print('a max threshold number of factor levels computed', vmax)
        terminate = True

    return terminate
# #########################################################################


# #########################################################################
def derive_impt_factorlevel_features_from(data,
                                          just_topn=6,
                                          do_speedup=True,
                                          vmax=FACTORSELECTOR_VMAX,
                                          fmax=FACTORSELECTOR_FMAX,
                                          maxtime=FACTORSELECTOR_MAXTIME,
                                          categorical_limit=FACTORSELECTOR_CATEGORICAL_LIMIT,
                                          remove_duplicates=False,
                                          impt_threshold=FACTORSELECTOR_IMPT_THRESHOLD,
                                          debug=0):

    if len(data.columns) > 128:
        just_topn = min(just_topn, 3)
    if len(data.columns) < 32:
        just_topn = min(just_topn, 12)

    d = pd.DataFrame()
    yvals = data[TARGET_VAR]
    n = max(min(int(len(yvals)/1000), 8192), 128)

    start, top_factors = time.time(), []
    which_vars = data.columns
    if do_speedup:
        which_vars = ORIGINAL_VARIABLES
    for col in which_vars:
        banner(col)

        if col.startswith(TARGET_VAR[:6]):
            continue

        vals = [x if pd.notnull(x) else NA_RECODE for x in data[col]]
        L = len(set(vals))
        if L > categorical_limit:
            print(col, 'has too many levels', L)
            continue

        if is_categorical(vals, col=col):
            # vals = pd.Categorical.from_array(vals)
            vals = pd.Categorical(vals)
        else:
            vals = pd.Series(vals)

        if is_small_factor(vals, categorical_limit=categorical_limit, col=col):
            L = len(set(vals))
            if L < 3:
                print(col, 'has too few levels', L)
                continue

            # vals = pd.Categorical.from_array(vals)
            vals = pd.Categorical(vals)
            freqs = vals.describe()
            which_factors = vals.categories

            if just_topn:
                freqs = freqs.nlargest(min(just_topn, len(freqs)), 'counts')
                which_factors = \
                    [i for i, x in zip(freqs.index,
                                       freqs['counts'][freqs.index]) if x > n]
            print(freqs)
            print('-' * 80)

            if debug:
                try:
                    cmat = pd.DataFrame(confusion_matrix(yvals, vals.codes))
                    print(cmat)
                except:
                    print('could not print cmat')
                print('-' * 80)

            if len(vals.categories) == 2 and len(which_factors) == 2:
                which_factors = which_factors[0]

            for cat in which_factors:
                if debug:
                    print(col, cat)
                colname = "FSEL_%s_%s" % (col, cat)
                vals = [1 if x == cat else 0 for x in data[col]]
                if len(set(vals)) >= 2:
                    d[colname] = pd.Series(vals)
                if terminate(d, start, maxtime, vmax):
                    break

            if len(which_factors):
                colname = "FSEL_%s_%s" % (col, which_factors[0])
                if colname in d:
                    top_factors.append(colname)

        if terminate(d, start, maxtime, vmax):
            break

    for col1 in top_factors:
        randomized_top_factors = top_factors[:]
        random.shuffle(randomized_top_factors)
        for col2 in randomized_top_factors:
            if col1 != col2:
                cols = sorted([col1, col2])
                col1, col2 = cols[0], cols[1]
                colname = "PLUS_%s_%s" % (col1, col2)
                if colname not in d:
                    print(colname)
                    d[colname] = d[col1] + d[col2]
                if terminate(d, start, maxtime, vmax*2):
                    break
        if terminate(d, start, maxtime, vmax*2):
            break

    if not len(d):
        return d

    d["FSEL_ANY"] = d.any(axis=1).astype('int')
    d["FSEL_SUM"] = d.sum(axis=1).astype('int')
    for i in range(1, 9, 2):
        d["FSEL_TG%s" % i] = (d["FSEL_SUM"] > i).astype('int')
    d["FSEL_AVG"] = d.mean(axis=1)
    d["FSEL_STD"] = d.std(axis=1)
    d["FSEL_SUM_EXT"] = d.sum(axis=1).astype('int')
    if not do_speedup:
        d["FSEL_CSUM"] = d.cumsum(axis=1).sum(axis=1)
        d['FSEL_DOT'] = d.dot(d.transpose()).sum(axis=1)

    print('-' * 80)
    if remove_duplicates:
        d = d.transpose().drop_duplicates().transpose()

    for col in d:
        L = len(set(d[col]))
        if L < 2:
            del d[col]
            print(col, 'col has two few levels', L)
            print('-' * 80)

    d[TARGET_VAR] = data[TARGET_VAR]
    X, Y = [x for x in d if x != TARGET_VAR], TARGET_VAR
    d_train = d[d[TARGET_VAR] != TESTING_SET]
    fvars = feature_select(d_train, d_train[TARGET_VAR], d_train.columns,
                           sorted_by_importance=True)
    if not len(fvars):
        fvars = X
    print("fselected_variables:", fvars)
    print('-' * 80)

    X, Y = fvars, TARGET_VAR
    if not fmax:
        d_train = d[d[TARGET_VAR] != TESTING_SET]
    else:
        d_train = d[d[TARGET_VAR] != TESTING_SET].sample(frac=fmax)

    if do_speedup:
        clf = GradientBoostingClassifier(n_estimators=12, max_depth=8)
        clf.fit(d_train[X], d_train[Y])
        dropset = [x for x, y in zip(X, clf.feature_importances_)
                   if y < impt_threshold]
    else:
        dropset, clf = \
            get_feature_impt_dropset(d_train[X], d_train[Y],
                                     get_importances=True,
                                     importance_threshold=impt_threshold)

    banner('factor level selector findings')
    keepset = [x for x in X if x not in dropset]
    print("KEEPSET", keepset)
    print('-' * 80)

    if debug:
        print("DROPSET", dropset)
        print('-' * 80)
        print(clf)
        print('-' * 80)

    fi = list(zip(X, [float("%.3f" % x) for x in clf.feature_importances_]))
    fi = sorted(fi, key=lambda x: x[1], reverse=True)
    keepset = [x[0] for x in fi if x[0] in keepset]
    print("SORTED", fi)
    print('-' * 80)

    print('-' * 80)
    print(data.shape, d_train.shape, d.shape, d[keepset].shape)
    print('-' * 80)
    return d[keepset]
# #########################################################################


# #########################################################################
if __name__ == "__main__":
    data = pd.read_csv('DATA/merged.csv', sep='|')

    P = derive_impt_factorlevel_features_from(data)
    for col in P:
        print(col)

    print('DONE: factor_selector')
