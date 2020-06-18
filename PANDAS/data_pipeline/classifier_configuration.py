#!/usr/bin/python
# -*- coding: utf-8 -*-


# #########################################################################
'''
'''
# #########################################################################


# #########################################################################
__doc__ = "builds classifier comparison plus ensemble for santander dataset"
__license__ = "Education use only. Absolutely no warranty. No commercial use"
__copyright__ = "Nelson R. Manohar (c) 2015, 2016"
__author__ = "Nelson R. Manohar"
__date__ = "Mar2016"
# #########################################################################
print('-' * 80)
print(__doc__)
print(__license__)
print(__copyright__)
print(__author__)
print(__date__)
# #########################################################################
print('-' * 80)
print("IMPORTANT NOTE:")
print("    EXECUTING CONFIGURATION FOUND IN:")
print("    " + __name__)
print('-' * 80)
# #########################################################################


# #########################################################################
from sklearn.svm import SVC
try:
    from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
    from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis
    LDA = LinearDiscriminantAnalysis
    QDA = QuadraticDiscriminantAnalysis
except:
    from sklearn.lda import LDA
    from sklearn.qda import QDA
from sklearn.svm import LinearSVC
from sklearn.naive_bayes import GaussianNB
from sklearn.naive_bayes import MultinomialNB
from sklearn.linear_model import SGDClassifier
from sklearn.ensemble import BaggingClassifier
from sklearn.ensemble import AdaBoostClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier
#from sklearn import grid_search
import sklearn.model_selection as grid_search

# #########################################################################


# #########################################################################
global RANDOM_STATE
global EFFORT
global CLF
RANDOM_STATE, EFFORT, CLF = None, None, None
# #########################################################################


# #########################################################################
RANDOM_STATE = [13*53*17*31, 13*53*17, 101*41, 31415169271,
                101*41*53, 113*23*11, 77*11*31,
                23*43*13*11, 53*37*17, 53*37*13,
                101*41*13*53*37, 101*41*13*53,
                13*53*143, 101*43*17, 101*43*101,
                23*43*31] * 10
# -------------------------------------------------------------------------
PRIMES = [47411, 29771, 11291, 1571, 12851, 38291, 27971, 8051, 27131, 7571,
          46571, 15971, 47771, 23891, 12131, 8891, 19331, 18611, 46931, 44291,
          23291, 731, 7331, 26051, 34331, 36731, 48731, 21251, 42851, 42491,
          37451, 26411, 4331, 16331, 19931, 14171, 9491, 35051, 38771, 44051,
          16091, 23411, 5771, 13571, 22211, 35891, 10931, 24731, 10091, 2051,
          22931, 27371, 33971, 28331, 31211, 42971, 22091, 35651, 16811, 22811,
          27251, 18491, 15611, 40571, 31811, 42251, 11, 39611, 49211, 30971,
          30011, 3251, 11171, 36611, 34571, 18731, 37211, 43811, 9251, 8651,
          33611, 3371, 27851, 44651, 9611, 2171, 19451, 37571, 21611, 31331,
          47291, 30491, 30611, 20051, 46091, 48611, 20171, 39131, 7451, 33851,
          6131, 13211, 19211, 30371, 34811, 26891, 43091, 10211, 15251, 48251,
          11891, 2771, 34451, 34691, 12971, 49331, 48491, 10331, 38411, 5891,
          37091, 35411, 491, 41531, 1691, 49091, 31691, 4091, 25811, 37691,
          6611, 41891, 6851, 26291, 35171, 43331, 12491, 9731, 3011, 9371,
          6731, 47051, 28811, 17531, 41171, 30731, 6251, 31091, 17891, 14651,
          32171, 27731, 48131, 45971, 43211, 13331, 19571, 49571, 12731, 38651,
          14411, 4811, 36971, 21971, 32651, 38171, 21131, 6011, 25331, 24611,
          29891, 7091, 21011, 44771, 33131, 44171, 18851, 43691, 26171, 4451,
          8411, 33491, 9011, 21731, 36851, 44891, 23651, 32411, 29651, 5651,
          33011, 27011, 15731, 18011, 6491, 36491, 14291, 46811, 16691, 41771,
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
# -------------------------------------------------------------------------
RANDOM_STATE = [x[0]*x[1] for x in zip(PRIMES[12:], reversed(PRIMES))]
RANDOM_STATE = RANDOM_STATE * 100
# #########################################################################


# #########################################################################
EFFORT = 2.0
EFFORT = 2.5
# -------------------------------------------------------------------------
TREE_CRITERION = 'entropy'
TREE_CRITERION = 'gini'
# -------------------------------------------------------------------------
BASE_TREELEAF_SIZE = 512
BASE_TREELEAF_SIZE = 8
# -------------------------------------------------------------------------
TREESIZE_SCALE = 1.00  # wrt 4096
BASE_TREENODE_MAX = 4096*TREESIZE_SCALE
# -------------------------------------------------------------------------
BUILD_FOREST_VIA = "ExtraTrees"
BUILD_FOREST_VIA = "RandomForest"
# -------------------------------------------------------------------------
PERCENTAGE_FEATURES = 0.60
PERCENTAGE_SAMPLES = 0.999
# -------------------------------------------------------------------------
FEATURE_IMPORTANCE_THRESHOLD = 1e-5
# #########################################################################


# #########################################################################
def print_args():
    print('-' * 80)
    for arg in sorted(["EFFORT", "BUILD_FOREST_VIA", "BASE_TREELEAF_SIZE",
                       'PERCENTAGE_FEATURES', 'PERCENTAGE_SAMPLES',
                       "BASE_TREENODE_MAX", "TREE_CRITERION"]):
        print("%32s : %s" % (arg, eval(arg)))
    print('-' * 80)
# #########################################################################


# #########################################################################
def get_random_seed():
    global RANDOM_STATE
    q = RANDOM_STATE.pop()
    if q >= 4294967295:
        q = None
    print()
    print('-' * 80)
    print("**** NOTE: NEW RANDOMIZER SEED IN USE: %s" % q)
    print('-' * 80)
    return q
# #########################################################################


# #########################################################################
def get_nsamples(p=PERCENTAGE_SAMPLES, nmin=0.05, nmax=1.00):
    nsamples = max(min(float(p), nmax), nmin)
    print('nsamples : %s' % nsamples)
    return nsamples
# #########################################################################


# #########################################################################
def get_nfeatures(p=PERCENTAGE_FEATURES, nmin=0.05, nmax=1.0):
    nfeatures = max(min(float(p), nmax), nmin)
    print('nfeatures: %s' % nfeatures)
    return nfeatures
# #########################################################################


# #########################################################################
def get_effort(n, nmin=3, nmax=4096):
    effort = max(min(int(n * EFFORT + 1), nmax), nmin)
    print('effort   : %s' % effort)
    return effort
# #########################################################################


# #########################################################################
def get_numnodes(n, nmin=2, nmax=4096*128):
    nleaves = max(min(int(n * BASE_TREENODE_MAX), nmax), nmin)
    print('nleaves  : %s' % nleaves)
    return nleaves
# #########################################################################


# #########################################################################
def get_leafsize(n, nmin=1, nmax=16384*8):
    leafsize = max(min(int(n * BASE_TREELEAF_SIZE), nmax), nmin)
    print('leafsize : %s' % leafsize)
    print('-' * 80)
    return leafsize
# #########################################################################


# #########################################################################
def reset_effort(effort):
    global EFFORT
    EFFORT = effort
    return EFFORT
# #########################################################################


# #########################################################################
def configure_classifier(name, N, K=2):
    global CLF

    # -----------------------------------------------------------------------
    def BasicKNN():
        clf = KNeighborsClassifier(n_neighbors=3, p=1, weights='distance',
                                   algorithm='auto')
        dlf = BaggingClassifier(clf,
                                verbose=2,
                                random_state=get_random_seed(),
                                n_estimators=get_effort(1),
                                bootstrap=True,
                                oob_score=True,
                                bootstrap_features=True,
                                max_samples=get_nsamples(p=0.05),
                                max_features=get_nfeatures(p=0.05))
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicSVC():
        parameters = {'kernel': ('poly', ), 'degree': (1, ),
                      'C': (1./2., 1.), 'class_weight': ('balanced', None)}
        clf = SVC(probability=True, kernel="poly")
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGaussianNB():
        clf = GaussianNB()
        dlf = BaggingClassifier(clf,
                                verbose=2,
                                random_state=get_random_seed(),
                                n_estimators=get_effort(16),
                                bootstrap=True,
                                oob_score=True,
                                bootstrap_features=True,
                                max_samples=get_nsamples(),
                                max_features=get_nfeatures())
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicMultinomialNB():
        # parameters = {'n_estimators': (get_effort(32), ),
        #               'bootstrap_features': (True, ),
        #               'max_samples': (get_nsamples(), ),
        #               'max_features': (get_nfeatures(), )}
        clf = MultinomialNB()
        dlf = BaggingClassifier(clf,
                                random_state=get_random_seed(),
                                n_estimators=get_effort(32),
                                verbose=2,
                                bootstrap=True,
                                oob_score=True,
                                bootstrap_features=True,
                                max_samples=get_nsamples(),
                                max_features=get_nfeatures())
        # clf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicLinearSVC():
        parameters = {'penalty': ('l1', 'l2'),
                      'fit_intercept': (True, ),
                      'dual': (False, ),
                      'class_weight': ('balanced', None),
                      'C': (1./2., 1.)}
        clf = LinearSVC(penalty='l1', C=1.0,
                        class_weight=None, dual=False, fit_intercept=True)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        gclf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        dlf = BaggingClassifier(gclf,
                                random_state=get_random_seed(),
                                n_estimators=get_effort(8),
                                verbose=2,
                                bootstrap=True,
                                oob_score=True,
                                bootstrap_features=True,
                                max_samples=get_nsamples(p=0.20),
                                max_features=get_nfeatures(p=0.90))
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicRadial():
        # params = {'C': (1., ), 'gamma': (1./128., ), 'class_weight': (None, )}
        clf = SVC(probability=True, kernel='rbf', C=1.00, gamma=1./128.)
        # clf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        dlf = BaggingClassifier(clf,
                                random_state=get_random_seed(),
                                n_estimators=get_effort(8),
                                verbose=2,
                                bootstrap=True,
                                oob_score=True,
                                bootstrap_features=True,
                                max_samples=get_nsamples(p=0.15),
                                max_features=get_nfeatures(p=0.40))
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicLDA():
        clf = LDA()
        if False:
            dlf = BaggingClassifier(clf,
                                    random_state=get_random_seed(),
                                    n_estimators=get_effort(1),
                                    verbose=2,
                                    bootstrap=True,
                                    oob_score=True,
                                    bootstrap_features=True,
                                    max_samples=get_nsamples(),
                                    max_features=get_nfeatures())
        else:
            parameters = {'n_components': list(range(3, 8))}
            dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicQDA():
        clf = QDA()
        if False:
            dlf = BaggingClassifier(clf,
                                    verbose=2,
                                    random_state=get_random_seed(),
                                    n_estimators=get_effort(64),
                                    bootstrap=True,
                                    oob_score=True,
                                    bootstrap_features=True,
                                    max_samples=get_nsamples(),
                                    max_features=get_nfeatures())
        else:
            parameters = {'reg_param': (1./512., 1./64., 0.0)}
            dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicSGD():
        parameters = {'shuffle': (True, ),
                      'loss': ('squared_hinge', ),
                      'class_weight': (None, ),
                      'fit_intercept': (True, ),
                      'shuffle': (True, False),
                      'average': (True, ),
                      'penalty': ('l1', 'l2')}
        clf = SGDClassifier()
        glf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        dlf = BaggingClassifier(glf,
                                random_state=get_random_seed(),
                                n_estimators=get_effort(64),
                                verbose=2,
                                bootstrap=True,
                                oob_score=True,
                                bootstrap_features=True,
                                max_samples=get_nsamples(),
                                max_features=get_nfeatures())
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicExtraTrees():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(32), ),
                      'verbose': (1, ),
                      'criterion': (TREE_CRITERION, ),
                      'max_depth': (32, ),
                      'bootstrap': (True, ),
                      'max_features': (get_nfeatures(), ),
                      # 'max_leaf_nodes': (get_numnodes(16), ),
                      'class_weight': (None, ),
                      'min_samples_leaf': (get_leafsize(0.5), )}
        clf = ExtraTreesClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicAdaBoost():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(10), ),
                      'learning_rate': (1./8., )}
        clf = DecisionTreeClassifier(max_depth=32,
                                     max_leaf_nodes=get_numnodes(16),
                                     max_features=get_nfeatures(),
                                     class_weight=None,
                                     min_samples_leaf=get_leafsize(1.6))
        clf = AdaBoostClassifier(clf)
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicAdaBoost_01():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(18), ),
                      'learning_rate': (1./16., )}
        clf = DecisionTreeClassifier(max_depth=32,
                                     max_leaf_nodes=get_numnodes(16),
                                     max_features=get_nfeatures(),
                                     class_weight=None,
                                     min_samples_leaf=get_leafsize(0.8))
        clf = AdaBoostClassifier(clf)
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGradientBoost():
        parameters = {'n_estimators': (get_effort(16), ),
                      'verbose': (3, ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(0.5), ),
                      'learning_rate': (1./2., )}
        clf = GradientBoostingClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGradientBoost_01():
        parameters = {'n_estimators': (get_effort(24), ),
                      'verbose': (3, ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.0), ),
                      'learning_rate': (1./3., )}
        clf = GradientBoostingClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGradientBoost_02():
        parameters = {'n_estimators': (get_effort(24), ),
                      'verbose': (3, ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.1), ),
                      'learning_rate': (1./4., )}
        clf = GradientBoostingClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGradientBoost_03():
        parameters = {'n_estimators': (get_effort(24), ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.2), ),
                      'verbose': (3, ),
                      'learning_rate': (1./5., )}
        clf = GradientBoostingClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGradientBoost_04():
        parameters = {'n_estimators': (get_effort(24), ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.3), ),
                      'verbose': (3, ),
                      'learning_rate': (1./6., )}
        clf = GradientBoostingClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGradientBoost_05():
        parameters = {'n_estimators': (get_effort(24), ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.4), ),
                      'verbose': (3, ),
                      'learning_rate': (1./8., )}
        clf = GradientBoostingClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGradientBoost_06():
        parameters = {'n_estimators': (get_effort(24), ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.5), ),
                      'verbose': (3, ),
                      'learning_rate': (1./10., )}
        clf = GradientBoostingClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGradientBoost_07():
        parameters = {'n_estimators': (get_effort(24), ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.6), ),
                      'verbose': (3, ),
                      'learning_rate': (1./12., )}
        clf = GradientBoostingClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGradientBoost_08():
        parameters = {'n_estimators': (get_effort(24), ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.7), ),
                      'verbose': (3, ),
                      'learning_rate': (1./16., )}
        clf = GradientBoostingClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicGradientBoost_09():
        parameters = {'n_estimators': (get_effort(24), ),
                      'loss': ('deviance', ),
                      'subsample': (get_nsamples(), ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.8), ),
                      'verbose': (3, ),
                      'learning_rate': (1./20., )}
        clf = GradientBoostingClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        # clf = CalibratedClassifierCV(clf, cv=K, method="sigmoid")
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicXtremeSGDBoost():
        clf = SGDClassifier(shuffle=True, penalty='l2', loss='squared_hinge')
        dlf = GradientBoostingClassifier(base_estimator=clf,
                                         n_estimators=get_effort(48),
                                         learning_rate=1./16.)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicDecisionTree():
        clf = DecisionTreeClassifier(max_features=get_nfeatures(),
                                     criterion=TREE_CRITERION,
                                     max_depth=32,
                                     max_leaf_nodes=get_numnodes(16),
                                     min_samples_leaf=get_leafsize(1),
                                     class_weight=None)
        dlf = BaggingClassifier(clf,
                                random_state=get_random_seed(),
                                n_estimators=get_effort(128),
                                verbose=3,
                                bootstrap=True,
                                oob_score=True,
                                bootstrap_features=True,
                                max_samples=get_nsamples(),
                                max_features=get_nfeatures())
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicDecisionTree_01():
        clf = DecisionTreeClassifier(max_features=get_nfeatures(),
                                     criterion=TREE_CRITERION,
                                     max_depth=32,
                                     max_leaf_nodes=get_numnodes(16),
                                     min_samples_leaf=get_leafsize(0.5),
                                     class_weight=None)
        dlf = BaggingClassifier(clf,
                                random_state=get_random_seed(),
                                n_estimators=get_effort(16),
                                verbose=2,
                                bootstrap=True,
                                oob_score=True,
                                bootstrap_features=True,
                                max_samples=get_nsamples(),
                                max_features=get_nfeatures())
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicRandomForest():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(16), ),
                      'oob_score': (True, ),
                      'verbose': (1, ),
                      'bootstrap': (True, ),
                      'criterion': (TREE_CRITERION, ),
                      'max_depth': (32, ),
                      'class_weight': (None, ),
                      'max_features': (get_nfeatures(), ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.0), )}
        clf = RandomForestClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicForest_01():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(24), ),
                      'verbose': (1, ),
                      'warm_start': (True, ),
                      'oob_score': (True, ),
                      'bootstrap': (True, ),
                      'criterion': (TREE_CRITERION, ),
                      'class_weight': (None, ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.1), ),
                      'max_features': (get_nfeatures(), )}

        if 'Trees' not in BUILD_FOREST_VIA:
            clf = RandomForestClassifier()
        else:
            clf = ExtraTreesClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicForest_02():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(24), ),
                      'verbose': (1, ),
                      'oob_score': (True, ),
                      'bootstrap': (True, ),
                      'warm_start': (True, ),
                      'criterion': (TREE_CRITERION, ),
                      'class_weight': (None, ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.2), ),
                      'max_features': (get_nfeatures(), )}
        if 'Trees' not in BUILD_FOREST_VIA:
            clf = RandomForestClassifier()
        else:
            clf = ExtraTreesClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicForest_03():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(24), ),
                      'verbose': (1, ),
                      'oob_score': (True, ),
                      'bootstrap': (True, ),
                      'criterion': (TREE_CRITERION, ),
                      'class_weight': (None, ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.3), ),
                      'max_features': (get_nfeatures(), )}
        if 'Trees' not in BUILD_FOREST_VIA:
            clf = RandomForestClassifier()
        else:
            clf = ExtraTreesClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicForest_04():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(24), ),
                      'verbose': (1, ),
                      'oob_score': (True, ),
                      'bootstrap': (True, ),
                      'criterion': (TREE_CRITERION, ),
                      'class_weight': (None, ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.4), ),
                      'max_features': (get_nfeatures(), )}
        if 'Trees' not in BUILD_FOREST_VIA:
            clf = RandomForestClassifier()
        else:
            clf = ExtraTreesClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicForest_05():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(24), ),
                      'verbose': (1, ),
                      'oob_score': (True, ),
                      'bootstrap': (True, ),
                      'criterion': (TREE_CRITERION, ),
                      'class_weight': (None, ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.5), ),
                      'max_features': (get_nfeatures(), )}
        if 'Trees' not in BUILD_FOREST_VIA:
            clf = RandomForestClassifier()
        else:
            clf = ExtraTreesClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicForest_06():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(24), ),
                      'verbose': (1, ),
                      'oob_score': (True, ),
                      'bootstrap': (True, ),
                      'criterion': (TREE_CRITERION, ),
                      'class_weight': (None, ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.6), ),
                      'max_features': (get_nfeatures(), )}
        if 'Trees' not in BUILD_FOREST_VIA:
            clf = RandomForestClassifier()
        else:
            clf = ExtraTreesClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicForest_07():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(24), ),
                      'verbose': (1, ),
                      'oob_score': (True, ),
                      'bootstrap': (True, ),
                      'criterion': (TREE_CRITERION, ),
                      'class_weight': (None, ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.7), ),
                      'max_features': (get_nfeatures(), )}
        if 'Trees' not in BUILD_FOREST_VIA:
            clf = RandomForestClassifier()
        else:
            clf = ExtraTreesClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicForest_08():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(24), ),
                      'verbose': (1, ),
                      'oob_score': (True, ),
                      'bootstrap': (True, ),
                      'criterion': (TREE_CRITERION, ),
                      'class_weight': (None, ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.8), ),
                      'max_features': (get_nfeatures(), )}
        if 'Trees' not in BUILD_FOREST_VIA:
            clf = RandomForestClassifier()
        else:
            clf = ExtraTreesClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    def BasicForest_09():
        parameters = {'random_state': (get_random_seed(), ),
                      'n_estimators': (get_effort(24), ),
                      'verbose': (1, ),
                      'bootstrap': (True, ),
                      'oob_score': (True, ),
                      'criterion': (TREE_CRITERION, ),
                      'class_weight': (None, ),
                      'max_leaf_nodes': (get_numnodes(16), ),
                      'min_samples_leaf': (get_leafsize(1.9), ),
                      'max_features': (get_nfeatures(), )}
        if 'Trees' not in BUILD_FOREST_VIA:
            clf = RandomForestClassifier()
        else:
            clf = ExtraTreesClassifier()
        dlf = grid_search.GridSearchCV(clf, parameters, cv=K, error_score=0)
        return dlf, clf
    # -----------------------------------------------------------------------

    dlf, clf = eval(name + "()")
    CLF = clf
    return dlf, clf
# #########################################################################


# #########################################################################
def get_underneath_clf():
    global CLF
    return CLF
# #########################################################################


# #########################################################################
def print_attribute(clf=None, attribute=""):
    if not clf:
        clf = CLF

    retval = None

    print('-' * 80)
    try:
        if hasattr(clf, attribute):
            print("%20s:" % attribute)
            exp = "clf.%s" % attribute
            retval = eval(exp)
            print(retval)
        else:
            print('classifier has no such attribute:', attribute)
    except:
        print('error processing', attribute)
    print('-' * 80)

    return retval
# #########################################################################


# #########################################################################
def print_classifier_attributes(clf=None, detailed=False):
    print('classifier attributes')
    retval = None
    try:
        if not clf:
            clf = CLF
        print(clf)
        print('-' * 80)

        try:
            print('PARAMETERS:')
            print(clf.get_params(deep=True))
            print('-' * 80)
        except Exception as err:
            print('problem printing get_params', err)

        print_attribute(clf=clf, attribute="best_params_")
        print_attribute(clf=clf, attribute="features_")
        retval = \
            print_attribute(clf=clf, attribute="feature_importances_")
        print_attribute(clf=clf, attribute="oob_improvement_")
        print_attribute(clf=clf, attribute="oob_score_")
        if detailed:
            print_attribute(clf=clf, attribute="estimators_")
            print_attribute(clf=clf, attribute="classes_")
            print_attribute(clf=clf, attribute="nclasses_")
            print_attribute(clf=clf, attribute="n_outputs_")
            print_attribute(clf=clf, attribute="oob_decision_function_")
            print_attribute(clf=clf, attribute="steps")

    except Exception as err:
        print('problem printing CLF params', err)

    return retval
# #########################################################################


# #########################################################################
def get_feature_impt_dropset(X, Y,
                             debug=False,
                             get_importances=False,
                             importance_threshold=FEATURE_IMPORTANCE_THRESHOLD):
    dropset = []

    FEATURE_IMPORTANCE_CLASSIFIER = \
        RandomForestClassifier(max_depth=8,
                               n_estimators=get_effort(24),
                               random_state=get_random_seed(),
                               max_features=get_nfeatures(),
                               oob_score=True,
                               verbose=2,
                               bootstrap=True,
                               criterion=TREE_CRITERION,
                               max_leaf_nodes=get_numnodes(8))

    if FEATURE_IMPORTANCE_THRESHOLD:
        FEATURE_IMPORTANCE_CLASSIFIER.fit(X, Y)
        importances = \
            print_classifier_attributes(clf=FEATURE_IMPORTANCE_CLASSIFIER)

        if len(importances):
            f2imp_pairs = list(zip(importances, X.columns))

            dropset = [(col, imp) for imp, col in f2imp_pairs
                       if imp < importance_threshold]

            keepset = [(col, imp) for imp, col in f2imp_pairs
                       if imp >= importance_threshold]

            if debug:
                print("KEEPING", keepset)
                print('-' * 80)

            if get_importances:
                dropset = dict(dropset)
            else:
                dropset = [col for col, imp in dropset]

            print("DROPPING", dropset)
            print('-' * 80)
    return dropset, FEATURE_IMPORTANCE_CLASSIFIER
# #########################################################################


# #########################################################################
print_args()
# #########################################################################
