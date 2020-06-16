from feature_generators import optimize_factor_cuts
# from  statsmodels.sandbox.infotheo import condentropy
import pandas as pd


if __name__ == "__main__":
    data = pd.read_csv('DATA/merged.csv', sep="|")
    print(data.shape)
    print(data.columns)
    print(data.describe())
    print('-' * 80)

    for col in data:
        d = optimize_factor_cuts(data, yvals=(), q=4, col=col)
