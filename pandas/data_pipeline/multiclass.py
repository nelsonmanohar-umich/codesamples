import numpy as np
import pandas
from sklearn import metrics


def multiclass_log_loss(y_true, y_pred, classes=(), eps=1e-15):
    mcl = 0.0
    try:
        # Multi class version of Logarithmic Loss metric.
        # https://www.kaggle.com/wiki/MultiClassLogLoss
        # idea from this post:
        # http://www.kaggle.com/c/emc-data-science/forums/t/2149/is-anyone-noticing-difference-betwen-validation-and-leaderboard-error/12209#post12209
        # Parameters
        # ----------
        # y_true : array, shape = [n_samples]
        # y_pred : array, shape = [n_samples, n_classes]
        # Returns
        # -------
        # loss : float

        y_true = np.array(y_true)

        nrows = len(y_true)
        ypred_shape = tuple([nrows, len(classes)])
        actual = np.zeros(ypred_shape)

        preds = np.zeros(ypred_shape)
        for i, col in enumerate(classes):
            preds[:, i] = y_pred[col]
        y_pred = preds

        def print_df(xname, x):
            print("%16s" % xname)
            x = pandas.DataFrame(x)
            if len(x.columns) == len(classes):
                x.columns = classes
            print(x.describe())
            print('-' * 80)
            return x

        try:
            # normalize row sums to 1
            predictions = np.clip(y_pred, eps, 1 - eps)
            predictions /= predictions.sum(axis=1)[:, np.newaxis]

            y_true_idx = y_true - 1
            actual[:, y_true_idx] = 1
            vsota = np.sum(actual * np.log(predictions))
            ll = -1.0 / nrows * vsota

            print('-' * 80)
            print("Y_VALS", sorted(set(y_true)))
            print('-' * 80)
            print_df("Y_PRED", y_pred)
            print_df("Y_TRUE", y_true)
            print_df("PREDICTIONS", predictions)
            print('-' * 80)
            print("multiclass_logloss [alt.]", ll)
        except Exception as err:
            print(err)

        mcl = metrics.log_loss(y_true, y_pred)
        print("multiclass_logloss [real]", mcl)
        print('-' * 80)

    except Exception as err:
        print(err)

    return mcl


def multiclass_logloss(y_true, y_pred, classes=(), eps=1e-15, debug=True):
    y_true = np.array(y_true)
    nrows = len(y_true)
    ypred_shape = tuple([nrows, len(classes)])

    preds = np.zeros(ypred_shape)
    for i, col in enumerate(classes):
        preds[:, i] = y_pred[col]
    y_pred = np.clip(preds, eps, 1 - eps)

    mcl = metrics.log_loss(y_true, y_pred)
    if debug:
        print("multiclass_logloss [real]", mcl)
        print('-' * 80)

    return mcl


def multiclass_logloss_vals(y_true, y_pred, classes=(), eps=1e-15, debug=True):
    y_true = np.array(y_true)
    nrows = len(y_true)
    ypred_shape = tuple([nrows, len(classes)])

    preds = np.zeros(ypred_shape)
    for i, col in enumerate(classes):
        preds[:, i] = y_pred[col]
    y_pred = np.clip(preds, eps, 1 - eps)

    predictions = np.clip(y_pred, eps, 1 - eps)
    predictions /= predictions.sum(axis=1)[:, np.newaxis]
    vals = np.log(predictions)

    return vals
