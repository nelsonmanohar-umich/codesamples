import pandas as pd
import time
import numpy
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier
from pipeline_configuration import TARGET_VAR, TARGET_COVAR, TESTING_SET, ID_VAR
from feature_generators import get_codes_from_categorical
from feature_generators import feature_select
from pipeline_configuration import STEPPER_MAX_COMPUTE_QUANTUM
from pipeline_configuration import STEPPER_SAMPLING_EFFORT
from pipeline_configuration import STEPPER_DEFAULT_DIRECTION
from pipeline_configuration import STEPPER_NUM_ESTIMATORS
from pipeline_configuration import STEPPER_EPSILON, STEPPER_DEPTH


# #########################################################################
def get_model(X, Y, debug=False):
    model = "     %s ~ %s" % (Y, " + ".join(X))
    if debug:
        print('SELECTED MODEL', model)
    return model
# #########################################################################


# #########################################################################
def fit(clf, d_train, X, Y, debug=False):
    clf.fit(d_train[X], d_train[Y])
    fi = clf.feature_importances_
    fi = [(x, y) for x, y in zip(X, fi[:])]
    f_imp = sorted(fi, key=lambda x: x[1], reverse=True)

    if hasattr(clf, "train_score_"):
        oob_s = numpy.mean(clf.train_score_)
    elif hasattr(clf, "oob_score_"):
        oob_s = clf.oob_score_

    if debug:
        print('-' * 80)
        print('Given', X, Y)
        print('-' * 80)
        print("IMPORTANCES", f_imp)
        print("SCORE      ", oob_s)
        print('-' * 80)
    return clf, f_imp, oob_s
# #########################################################################


# #########################################################################
def do_step(start, col, oob, new_oob, eps,
            maxtime=STEPPER_MAX_COMPUTE_QUANTUM, msg="ADDED"):
    terminate = False
    delay = time.time() - start

    if (new_oob < oob):
        print('%8s STEP [%s added] = %.5f, %.3f secs elapsed' % \
            (msg, col, new_oob, delay))
        if (oob - new_oob) < eps:
            print('eps [%.3f] termination at: %.4f' % (eps, oob-new_oob))
            terminate = True

    if (time.time() - start) > maxtime:
        print('secs [%.3f] termination at: %.4f' % (maxtime, delay))
        terminate = True
        print('-' * 80)

    return terminate
# #########################################################################


# #########################################################################
def backward_step_fitter(data, colset=(),
                         clf=None,
                         encode=True,
                         apply_second_pass=True,
                         fmax=0,
                         maxtime=STEPPER_MAX_COMPUTE_QUANTUM,
                         warm=True,
                         increasing_depth=False,
                         n_estimators=16,
                         do_speedup=False,
                         eps=1e-6,
                         depth=12,
                         random_seed=101):

    if not fmax:
        d_train = data[data[TARGET_VAR] != TESTING_SET]
    else:
        d_train = data[data[TARGET_VAR] != TESTING_SET].sample(frac=fmax)

    X, Y = colset, TARGET_VAR
    clf, f_imp, oob_s = fit(clf, d_train, X, Y)

    XX, dropped, oob, start = X, [], numpy.nan_to_num(numpy.inf), time.time()
    examined = dict(list(zip(colset, [0, ]*len(colset))))
    terminate = False
    for col, imp in f_imp:
        examined[col] = 1
        clf = GradientBoostingClassifier(random_state=random_seed,
                                         warm_start=warm,
                                         max_depth=depth+1,
                                         n_estimators=n_estimators)
        XX_without_col = [x for x in XX if x != col]
        if not len(XX_without_col):
            break
        new_clf, fi, new_oob = fit(clf, d_train, XX_without_col, Y)
        terminate = do_step(start, col, oob, new_oob, eps,
                            maxtime=maxtime, msg="DELETED")
        if new_oob < oob:
            XX = XX_without_col[:]
            print(get_model(XX, Y))
            print('    ' + '-' * 76)
            print("     IMPORTANCES:", fi)
            print('-' * 80)
            clf, oob = new_clf, new_oob
        else:
            if apply_second_pass:
                dropped.append(col)
        if terminate:
            break

    print('-' * 80)
    terminate = False
    for col, imp in f_imp:
        if col in XX:
            continue

        clf = GradientBoostingClassifier(random_state=random_seed,
                                         warm_start=warm,
                                         max_depth=depth+1,
                                         n_estimators=n_estimators)
        new_clf, _, new_oob = fit(clf, d_train, XX + [col, ], Y)
        terminate = do_step(start, col, oob, new_oob, eps,
                            maxtime=maxtime, msg="ADDED")
        if new_oob < oob:
            XX.append(col)
            print(get_model(XX, Y))
            clf, oob = new_clf, new_oob
            depth += 1
        else:
            if apply_second_pass:
                dropped.append(col)
        if terminate:
            break

    print('-' * 80)
    terminate = False
    for col in dropped:
        if col in XX:
            continue

        clf = GradientBoostingClassifier(random_state=random_seed,
                                         warm_start=warm,
                                         max_depth=depth+1,
                                         n_estimators=n_estimators)
        new_clf, _, new_oob = fit(clf, d_train, XX + [col, ], Y)
        terminate = do_step(start, col, oob, new_oob, eps,
                            maxtime=maxtime, msg="EXTRA")
        if new_oob < oob:
            XX.append(col)
            print(get_model(XX, Y))
            clf, oob = new_clf, new_oob
        if terminate:
            break

    if len(XX):
        X = XX

    print('-' * 80)
    remainder = [col for col in examined if not examined[col]]
    if len(remainder):
        print('added remainder of unexamined columns', remainder)
        X.extend(remainder)
    print('-' * 80)

    return X
# #########################################################################


# #########################################################################
def forward_step_fitter(data, colset=(),
                        clf=None,
                        encode=True,
                        apply_second_pass=True,
                        fmax=0,
                        maxtime=STEPPER_MAX_COMPUTE_QUANTUM,
                        warm=True,
                        increasing_depth=False,
                        add_unexplored=True,
                        n_estimators=16,
                        do_speedup=True,
                        eps=1e-6,
                        depth=12,
                        random_seed=101):

    if not fmax:
        d_train = data[data[TARGET_VAR] != TESTING_SET]
    else:
        d_train = data[data[TARGET_VAR] != TESTING_SET].sample(frac=fmax)

    X, Y = colset, TARGET_VAR
    clf, f_imp, oob_s = fit(clf, d_train, X, Y)

    XX, dropped, oob, start = [], [], numpy.nan_to_num(numpy.inf), time.time()
    examined = dict(list(zip(colset, [0, ]*len(colset))))
    terminate = False
    for col, imp in f_imp:
        examined[col] = 1

        #  it need be gbc as it is robust to overfitting as cv is not done
        clf = GradientBoostingClassifier(random_state=random_seed,
                                         warm_start=warm,
                                         max_depth=depth+1,
                                         n_estimators=n_estimators)

        new_clf, fi, new_oob = fit(clf, d_train, XX + [col, ], Y)
        terminate = do_step(start, col, oob, new_oob, eps,
                            maxtime=maxtime, msg="ADDED")
        if new_oob < oob:
            XX.append(col)
            print(get_model(XX, Y))
            clf, oob = new_clf, new_oob
            print('    ' + '-' * 76)
            print("     IMPORTANCES:", fi)
            if increasing_depth:
                depth += 1
                print("     DEPTH:", depth)
            print('-' * 80)
        else:
            if apply_second_pass:
                print('col did not enhance model', col)
                dropped.append(col)
        if terminate:
            break

    print('-' * 80)
    if terminate:
        dropped = []
    terminate = False
    for col in dropped:
        if col in XX:
            continue

        if True or increasing_depth:
            depth += 1

        clf = GradientBoostingClassifier(random_state=random_seed,
                                         warm_start=warm,
                                         max_depth=depth,
                                         n_estimators=n_estimators)

        new_clf, _, new_oob = fit(clf, d_train, XX + [col, ], Y)
        terminate = do_step(start, col, oob, new_oob, eps,
                            maxtime=maxtime, msg="EXTRA")
        if new_oob < oob:
            XX.append(col)
            print(get_model(XX, Y))
            clf, oob = new_clf, new_oob
        else:
            print('col has been dropped, 2nf pass did not enhance model', col)
        if terminate:
            break

    if len(XX):
        X = XX

    print('-' * 80)
    remainder = [col for col in examined if not examined[col]]
    if add_unexplored and len(remainder):
        print('added remainder of unexamined columns', remainder)
        X.extend(remainder)

    print('-' * 80)

    return X
# #########################################################################


# #########################################################################
def step_fitter(data, colset=(),
                mode=STEPPER_DEFAULT_DIRECTION,
                encode=True,
                apply_second_pass=True,
                fmax=STEPPER_SAMPLING_EFFORT,
                maxtime=STEPPER_MAX_COMPUTE_QUANTUM,
                n_estimators=STEPPER_NUM_ESTIMATORS,
                warm=True,
                increasing_depth=False,
                eps=STEPPER_EPSILON,
                depth=STEPPER_DEPTH,
                do_speedup=True,
                debug=False,
                random_seed=101):

    if not len(colset):
        colset = data.columns

    if not fmax:
        d_train = data[data[TARGET_VAR] != TESTING_SET]
    else:
        d_train = data[data[TARGET_VAR] != TESTING_SET].sample(frac=fmax)

    if do_speedup:
        print('-' * 80)
        if debug:
            print('pre  feature_selection', colset)
        colset = feature_select(d_train, d_train[TARGET_VAR], colset,
                                sorted_by_importance=True)
        if debug:
            print('post feature_selection', colset)
        print('-' * 80)

    colset = [x for x in colset if x not in [TARGET_VAR, TARGET_COVAR, ID_VAR]]
    print("WORKING WITH", colset)
    print('-' * 80)

    #  it need be rfc as it is reliable on feature importance additive effects
    clf = RandomForestClassifier(n_estimators=n_estimators*3+1,
                                 oob_score=True, random_state=random_seed)

    if encode:
        for col in colset:
            data[col] = get_codes_from_categorical(data[col], col=col)

    X0, X1 = [], []
    if "back" in mode.lower():
        X0 = backward_step_fitter(data, colset=colset, clf=clf,
                                  encode=encode,
                                  apply_second_pass=apply_second_pass,
                                  fmax=fmax,
                                  maxtime=maxtime,
                                  n_estimators=n_estimators,
                                  warm=warm,
                                  increasing_depth=increasing_depth,
                                  do_speedup=do_speedup,
                                  eps=eps,
                                  depth=depth,
                                  random_seed=random_seed)

    if "forw" in mode.lower():
        X1 = forward_step_fitter(data, colset=colset, clf=clf,
                                 encode=encode,
                                 apply_second_pass=apply_second_pass,
                                 fmax=fmax,
                                 maxtime=maxtime,
                                 n_estimators=n_estimators,
                                 warm=warm,
                                 increasing_depth=increasing_depth,
                                 do_speedup=do_speedup,
                                 eps=eps,
                                 depth=depth,
                                 add_unexplored="back" not in mode.lower(),
                                 random_seed=random_seed)

    X = X0 + [x for x in X1 if x not in X0]
    print("MODEL: ", get_model(X, TARGET_VAR))

    return X
# #########################################################################


# #########################################################################
if __name__ == "__main__":
    data = pd.read_csv('DATA/merged.csv', sep="|")
    print(data.shape)
    print(data.columns)
    print('-' * 80)

    features = step_fitter(data)
