import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
from pandas.tools.plotting import scatter_matrix
from pipeline_configuration import TARGET_VAR, TARGET_COVAR, ID_VAR
from pipeline_configuration import TESTING_SET, NA_RECODE
from feature_generators import banner
from feature_generators import is_categorical
from feature_generators import get_codes_from_categorical


# #########################################################################
def get_valid_colset(colset, wrt=(), skip=(), n=8):
    if len(colset) > n:
        print('only first %s columns be plotted' % n, colset[:n])
        colset = colset[:n]

    skip.extend(wrt)
    skip.extend([TARGET_VAR, TARGET_COVAR, ID_VAR])
    if not len(colset):
        colset = [col for col in data if col not in skip]

    colset = [col for col in colset if col not in skip]

    return colset
# #########################################################################


# #########################################################################
def validate_format(fileformat, def_format="png", valid_formats=('png', 'pdf')):
    if fileformat not in valid_formats:
        print('invalid format given [%s], defaulting to %s' % (fileformat,
                                                               def_format))
        fileformat = def_format

    return fileformat
# #########################################################################


# #########################################################################
def factor_plots(data, colset=(), wrt=(), skip=(), what="freqs",
                 pkind="pie", fileformat="png", fname="varexpl",
                 nrows=4, ncols=3, encode=True, fmax=0, debug=True):
    wrt = [x for x in data if x in wrt]
    skip = [x for x in data if x in skip]
    colset = [x for x in data if x in colset]
    colset = get_valid_colset(colset, wrt=wrt, skip=skip, n=nrows*ncols)
    for col in colset:
        if is_categorical(data[col], col=col):
            data[col] = [x if pd.notnull(x) else "N/A" for x in data[col]]
        else:
            data[col] = [x if pd.notnull(x) else NA_RECODE for x in data[col]]

    d_train = data[data[TARGET_VAR] != TESTING_SET]

    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(16, 16))

    i, j = 0, 0
    for col in colset + [TARGET_VAR, ]:
        if is_categorical(d_train[col], col=col):
            # p = pd.Categorical.from_array(d_train[col])
            p = pd.Categorical(d_train[col])
        else:
            # p = pd.Categorical.from_array(d_train[col].astype('int'))
            p = pd.Categorical(d_train[col].astype('int'))
        freqs = p.describe()
        freqs = freqs.nlargest(min(10, len(freqs)), what)
        if debug:
            print(freqs)
            print('-' * 80)

        freqs[what].plot(ax=axes[i, j], kind=pkind, labels=freqs.index)
        axes[i, j].set_title(col)
        j = j + 1
        if j >= ncols:
            j, i = 0, i + 1
        if i*ncols + j >= nrows*ncols:
            break

    if not fname:
        fig = plt.show()
    else:
        fileformat = validate_format(fileformat)
        fname = "%s.%s" % (fname, fileformat)
        fig = plt.savefig(fname, orientation='landscape', format=fileformat)

    return fig
# #########################################################################


# #########################################################################
def factor_piecharts(data, colset=(), wrt=(), skip=(), what="freqs",
                     fileformat="png", fname="varexpl_pieplot",
                     encode=True, fmax=0, debug=True):
    matplotlib.style.use('ggplot')
    banner('factor piechart plots for: %s' % ", ".join(colset))
    return factor_plots(data, colset=colset, wrt=wrt, skip=skip, what=what,
                        pkind="pie", fileformat=fileformat, fname=fname,
                        encode=encode, fmax=fmax, debug=debug)
# #########################################################################


# #########################################################################
def get_subsample_of(d, x_plus_y_vars, fmax=0, nmax=256000):
    if not fmax or fmax < 0:
        fmax = 0.99999

    with_replacement = fmax >= 1

    subsample = d[x_plus_y_vars].sample(frac=fmax, replace=with_replacement)

    if subsample.shape[0] > nmax:
        subsample = subsample[x_plus_y_vars].sample(n=nmax)

    return subsample
# #########################################################################


# #########################################################################
def factor_pairwise_scatterplots(data, colset=(), wrt=(), skip=(), what="freqs",
                                 fileformat="png", fname="varexpl_scatterplot",
                                 encode=True, fmax=0, nmax=64000, debug=True):
    matplotlib.style.use('ggplot')
    wrt = [x for x in data if x in wrt]
    skip = [x for x in data if x in skip]
    colset = get_valid_colset(colset, wrt=wrt, skip=skip)
    x_plus_y_vars = colset + [TARGET_VAR, ]
    banner('factor pairwise scatter plots for: %s' %
           ", ".join(colset + [TARGET_VAR, ]))

    for col in colset:
        if encode:
            if is_categorical(data[col], col=col):
                data[col] = get_codes_from_categorical(data[col], col=col)
        data[col] = [x if pd.notnull(x) else NA_RECODE for x in data[col]]

    d_train = data[data[TARGET_VAR] != TESTING_SET]
    subsample = get_subsample_of(d_train, x_plus_y_vars, fmax=fmax, nmax=nmax)

    fig = scatter_matrix(subsample, figsize=(16, 16))

    if not fname:
        plt.show()
    else:
        fileformat = validate_format(fileformat)
        fname = "%s.%s" % (fname, fileformat)
        plt.savefig(fname, orientation='landscape', format=fileformat)

    return fig
# #########################################################################


# #########################################################################
def conditional_factor_effect_plots(data, colset=(), wrt=(), skip=(),
                                    what="freqs", nbins=20, nmax=128000, fmax=0,
                                    fileformat="png", fname="varexpl_ycondplot",
                                    encode=False, debug=True):

    matplotlib.style.use('ggplot')
    wrt = [x for x in data if x in wrt]
    skip = [x for x in data if x in skip]
    colset = get_valid_colset(colset, wrt=wrt, skip=skip)
    x_plus_y_vars = colset + wrt
    banner('y-conditioned factor effect plots for: %s wrt %s' %
           (",".join(colset), ", ".join(wrt)))

    for col in x_plus_y_vars:
        if is_categorical(data[col], col=col):
            data[col] = [x if pd.notnull(x) else "NA" for x in data[col]]
            data[col] = pd.Categorical(data[col])
        else:
            data[col] = [x if pd.notnull(x) else NA_RECODE for x in data[col]]
        if encode:
            data[col] = get_codes_from_categorical(data[col], col=col)

    d_train = data[data[TARGET_VAR] != TESTING_SET]
    subsample = get_subsample_of(d_train, x_plus_y_vars, fmax=fmax, nmax=nmax)
    pdata = subsample[x_plus_y_vars].groupby(wrt)

    partition_labels = [str(x[0]) for x in pdata]
    headings = ["%s=%s|%s=%s" % (wrt[0], eval(part)[0], wrt[1], eval(part)[1])
                for part in partition_labels]
    print(headings)

    if debug:
        print('-' * 80)
        for heading, partition in zip(headings, pdata):
            banner(heading)
            print(partition)

    for heading, partition in zip(headings, pdata):
        try:
            banner(heading)
            partition = pd.DataFrame(partition[1])
            if partition.empty:
                continue
            partition[colset].hist(figsize=(16, 16))
            if not fname:
                plt.show()
            else:
                fileformat = validate_format(fileformat)
                figname = "%s_%s.%s" % (fname, heading, fileformat)
                plt.savefig(figname, orientation='landscape', format=fileformat)
        except Exception as err:
            print(err)

    return pdata
# #########################################################################


# #########################################################################
if __name__ == "__main__":
    data = pd.read_csv('DATA/merged.csv', sep='|')

    COVAR, SKIP = "Sex", "Name"

    if False:
        factor_piecharts(data,
                         # fname="",
                         skip=[SKIP, ],
                         wrt=[TARGET_VAR, ])

        factor_pairwise_scatterplots(data,
                                     # fname="",
                                     skip=[SKIP, ],
                                     wrt=[TARGET_VAR, ])

    conditional_factor_effect_plots(data,
                                    # fname="",
                                    skip=[SKIP, ],
                                    wrt=[TARGET_VAR, COVAR])

    print('DONE: factor_effect plotter')
