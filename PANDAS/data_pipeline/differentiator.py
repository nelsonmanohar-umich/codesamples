import numpy
import pandas as pd

from pipeline_configuration import TARGET_VAR, TARGET_COVAR, ID_VAR
from feature_generators import banner, print_exception
from feature_generators import feature_select
from feature_generators import get_codes_from_categorical

from sklearn.ensemble import GradientBoostingClassifier
from sklearn import metrics
#from sklearn import grid_search
import sklearn.model_selection as grid_search



# #########################################################################
def is_valid_ypp(p_score):
    if p_score > 0.999:
        return False
    if p_score < 0.699:
        return False
    return True
# #########################################################################


# #########################################################################
def add_classlabel_differentiators(data, colset=(), wrt=(),
                                   skip=(TARGET_COVAR, TARGET_VAR, ID_VAR),
                                   n=8, depth=3, leafsize=15,
                                   subsample=0.8, rate=1./3., K=8,
                                   fselect=False, encode=True, debug=False):

    colset, skip = [x for x in data.columns], [x for x in skip if x in data]
    if not len(wrt):
        wrt = sorted(set(data[TARGET_VAR]))

    p, final_wrt = pd.DataFrame(), []
    p[TARGET_VAR] = data[TARGET_VAR].copy()
    headings = []
    for i, c in enumerate(wrt):
        print(i, c)
        if c == -1 or c == "-1":
            continue
        vals = [1.0 if x else 0.0 for x in data[TARGET_VAR] == c]
        headings.append([sum(vals), c])
        if not len(vals):
            print('y0=%s not present in targetvar=%s' % (c, TARGET_VAR))
            continue
        c = str(c)
        p[c] = pd.Series(vals).copy()
        final_wrt.append(c)

    wrt = final_wrt[:]
    if not len(wrt):
        print('selected %s not present in targetvar=%s' % (wrt, TARGET_VAR))
        return data
    print(p[wrt].describe().transpose())
    print('-' * 80)

    banner('y-conditioned factor effect for: %s wrt %s' %
           (", ".join(colset), ", ".join(wrt)))

    for col in data:
        if encode:
            data[col] = get_codes_from_categorical(data[col], col=col)
    print('-' * 80)
    for col in p:
        if encode:
            p[col] = get_codes_from_categorical(p[col], col=col)

    for heading in wrt:
        clf, ypp, ypc, ypt = None, (), (), ()
        try:
            xvars = [col for col in data if TARGET_VAR not in col
                     and col not in wrt and col not in skip]
            banner("class label: [%s], been predicted wrt [%s]" %
                   (heading, ", ".join(xvars)))

            if fselect:
                xvars = feature_select(data[xvars], data[heading], xvars,
                                       sorted_by_importance=True)

            # gradient boosting via stumps to reduce overfitting
            parameters = {'n_estimators': (int(n), ),
                          'verbose': (0, ),
                          'subsample': (subsample, ),
                          'max_depth': (depth, ),
                          'max_features': (1.0, ),
                          'min_samples_leaf': (leafsize, ),
                          'learning_rate': (rate, )}
            gb_clf = GradientBoostingClassifier()
            gbc = grid_search.GridSearchCV(gb_clf,
                                           parameters, cv=K, error_score=0)

            for clf, uclf, name in [(gbc, gb_clf, 'boosting'), ]:
                try:
                    d_train = data[data[TARGET_VAR] != -1]
                    p_train = p[p[TARGET_VAR] != -1]

                    clf.fit(d_train[xvars], p_train[heading])
                    if debug:
                        print(clf)
                        print('-' * 80)

                    if hasattr(uclf, "train_score_"):
                        oob_s = numpy.mean(uclf.train_score_)
                        print("train score", oob_s)
                    if hasattr(uclf, "feature_importances_"):
                        print("feat impt", list(zip(xvars, uclf.feature_importances_)))
                        print('-' * 80)
                    if hasattr(uclf, "oob_score_"):
                        print("oob score", uclf.oob_score_)

                    if hasattr(clf, "predict_proba"):
                        ypp = clf.predict_proba(data[xvars])
                        ypp = pd.Series(ypp[:, 1])
                    if hasattr(clf, "predict"):
                        ypt = clf.predict(data[xvars])
                    if hasattr(clf, "decision_function"):
                        # ypc = clf.decision_function(data[xvars])
                        ypc = pd.Series(ypp >= 0.5).astype('int')
                        if len(set(ypc)) < 2:
                            print('skipping single value predictor', set(ypc))
                            continue

                    if hasattr(clf, "score"):
                        p_score = clf.score(d_train[xvars], p_train[heading])
                        print('score of this predictor', p_score)
                        if not is_valid_ypp(p_score):
                            print('skipping predictor b/c score')
                            continue

                    if len(ypp):
                        data['YPP_CLF(%s_%s)' % (name, heading)] = ypp.copy()
                        logloss = metrics.log_loss(numpy.array(p[heading]),
                                                   numpy.array(ypp))
                        print("LOGLOSS", logloss)
                        print('-' * 80)
                    if len(ypc):
                        data['YPC_CLF(%s_%s)' % (name, heading)] = ypc.copy()
                        ypc = pd.Series(ypc)
                    if len(ypt):
                        data['YPT_CLF(%s_%s)' % (name, heading)] = ypt.copy()
                        ypt = pd.Series(ypt)

                except Exception as err:
                    print_exception("error while fit/predict", err=err)

        except Exception as err:
            print_exception("error in main loop", err=err)
            print(err)

    banner('differentiator summary')
    created_cols = [col for col in data
                    if len([True for heading in wrt if heading in col])]
    print(data[created_cols].describe().transpose())
    return data
# #########################################################################


# #########################################################################
if __name__ == "__main__":
    data = pd.read_csv('DATA/merged.csv', sep='|')
    # from definitions import augment_data
    # data = augment_data(data)
    data = add_classlabel_differentiators(data)
