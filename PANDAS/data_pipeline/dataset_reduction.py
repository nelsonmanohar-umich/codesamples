import pandas as pd
import numpy
import time
from pipeline_configuration import TARGET_VAR, ID_VAR, TESTING_SET, NA_RECODE
from pipeline_configuration import CLASS1, CLASS0, MULTICLASS
from pipeline_configuration import CENTROID_MAX_COMPUTE_TIME
from pipeline_configuration import BYPASS_ADD_CENTROIDS
from feature_generators import banner
# from pipeline_configuration import CENTROID_APPROACH
# from pipeline_configuration import CENTROID_SAMPLING_FRACTION
# from pipeline_configuration import MIN_VALID_CLUSTER_SIZE
# from pipeline_configuration import MAX_SAMPLES_TO_CLUSTER
from feature_generators import apply_scaling
from feature_generators import print_exception
from sklearn.cluster import KMeans


# #########################################################################
def in_limit(n, nmin=2, nmax=256):
    n = max(n, nmin)
    n = min(n, nmax)
    return n
# #########################################################################


# #########################################################################
def time_exceeded(t_start, quota):
    if time.time() - t_start > quota:
        return True
    return False
# #########################################################################


# #########################################################################
def replace_with_random_centroid_for(data,
                                     cx=None,
                                     f=0.20,
                                     min_cluster_size=256,
                                     max_sampling_size=2048,
                                     ndigits=1,
                                     nc=32,
                                     n_starts=25,
                                     max_iter=512,
                                     approach='centroid',
                                     fast_mode=False,
                                     do_replace=False,
                                     debug=False):

    cx_sample = data[data[TARGET_VAR] == cx].sample(frac=f, replace=do_replace)
    cx_size = len(cx_sample)
    if cx_size > max_sampling_size:
        cx_sample = cx_sample.sample(n=max_sampling_size, replace=do_replace)
    num_clusters = in_limit(nc, nmin=2, nmax=len(cx_sample)-1)
    row_ids, replace_row_ids = cx_sample.index, []
    if debug:
        print('examining rows', len(row_ids), row_ids)

    if not fast_mode:
        try:
            clusterer = KMeans(n_clusters=num_clusters,
                               max_iter=max_iter, n_init=n_starts)
            cmappings = clusterer.fit(cx_sample)
            clabeling = cmappings.labels_
            p = len(set(clabeling))

            sizes = [(len(cx_sample[clabeling == j]), j) for j in range(p)]
            sizes = sorted(sizes, reverse=True)
            print(p, sizes)

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
                cx_centroid_j = \
                    cx_sample_j.mean(skipna=False).round(decimals=ndigits)
                replace_row_ids.extend([rid for rid in cx_sample_j.index])
                cx_centroid_j = numpy.nan_to_num(cx_centroid_j)
                cx_random_centroids.append(cx_centroid_j.copy())
                if debug:
                    print('|c_sample(j=%s)|= %s' % (cluster_idx, cx_size_j), end=' ')
            print('...done [%s]' % len(cx_random_centroids))
            if debug:
                print('will replace row ids', replace_row_ids)
            return cx_random_centroids, replace_row_ids, row_ids

        except Exception as err:
            print_exception('during class %s clustering' % cx,
                            err=err, with_traceback=True)

    cx_centroid = cx_sample.mean(skipna=False).round(decimals=ndigits)
    cx_centroid = numpy.nan_to_num(cx_centroid)
    return [cx_centroid.copy(), ], replace_row_ids, row_ids
# #########################################################################


# #########################################################################
def data_describe(data, heading):
    if TARGET_VAR not in data:
        data = data.transpose()
    banner(heading)
    print("CURRENT DATA SHAPE", data.shape)
    for yclass in sorted(set(data[TARGET_VAR])):
        print("C[%2s] DATA SHAPE: %s" % \
            (int(yclass), data[data[TARGET_VAR] == yclass].shape))
    return
# #########################################################################


# #########################################################################
def centroid_consequence_wrt_samples(c,
                                     stem=0,
                                     cx=None,
                                     nc=32,
                                     min_cluster_size=256,
                                     max_sampling_size=2048,
                                     conseq="replace",
                                     debug=False):
    data_describe(c, 'C%s clustering search %s' % (cx, stem))

    c1_random_centroids, rids, full_rids = \
        replace_with_random_centroid_for(c.transpose(),
                                         cx=cx,
                                         min_cluster_size=min_cluster_size,
                                         max_sampling_size=max_sampling_size,
                                         nc=nc)

    for j in range(len(c1_random_centroids)):
        c["c%s_centroid_%s" % (cx, stem+j)] = c1_random_centroids[j].copy()

    if debug:
        print(cx, 'RECEIVED RIDS', rids)

    if "add" not in conseq.lower():
        delete_rids = [rid for rid in sorted(set(rids)) if rid in c]
        print('rows already subsummed', len(rids) - len(delete_rids))
        print('deleting newly subsummed rows', len(delete_rids), delete_rids)
        delete_rids = dict(list(zip(delete_rids, delete_rids)))
        c = c[[rid for rid in c if rid not in delete_rids]]
    return c  #.copy()
# #########################################################################


# #########################################################################
def compare_vectors(c1_centroid, c0_centroid, debug=False):
    a = [(z, x, y) for z, x, y in zip(data.columns, c1_centroid, c0_centroid)]
    i = 0
    for i, aa in enumerate(a):
        col, a1, a0 = aa[0], aa[1], aa[2]
        if a1 != a0:
            i += 1
            print("%4s (%68s) : %16.6f  --> %16.6f" % (i, col, a1, a0))
    return i
# #########################################################################


# #########################################################################
def add_basic_class_centroids(c, cx=None, stem="original", debug=False):
    centroid_name = 'c%s_%s_centroid' % (cx, stem)
    banner(centroid_name)

    data = c.transpose()
    centroid_sample_j = data[data[TARGET_VAR] == cx]

    centroid_j = centroid_sample_j.mean(skipna=False)
    c[centroid_name] = centroid_j.copy()

    if debug:
        print(c[centroid_name].describe())

    return c.copy()
# #########################################################################


# #########################################################################
def trim_class_using_random_centroids(data,
                                      which_class=None,
                                      nc=16,
                                      min_cluster_size=256,
                                      max_sampling_size=2048,
                                      conseq="replace",
                                      do_scaling=False,
                                      preprocess=True,
                                      ktimes=4,
                                      debug=False):

    data_describe(data, 'CURRENTLY')

    if do_scaling:
        data = apply_scaling(data)

    if preprocess:
        xvars = [col for col in data if col not in [TARGET_VAR, ID_VAR]]
        for col in xvars:
            try:
                data[col] = [x if pd.notnull(x) else NA_RECODE
                             for x in data[col]]
            except:
                # data[col] = pd.Categorical.from_array(data[col])
                data[col] = pd.Categorical(data[col])
                data[col] = data[col].codes
        print('original size (before centroids)', data.shape)

    c = data.transpose()
    c = add_basic_class_centroids(c, cx=which_class, stem="original", debug=0)
    t_start = time.time()
    for i in range(0, ktimes*nc, nc):
        c = centroid_consequence_wrt_samples(c,
                                             stem=i,
                                             cx=which_class,
                                             conseq=conseq,
                                             min_cluster_size=min_cluster_size,
                                             max_sampling_size=max_sampling_size,
                                             nc=nc,
                                             debug=0)
        if time_exceeded(t_start, CENTROID_MAX_COMPUTE_TIME):
            break

    c = add_basic_class_centroids(c, cx=which_class, stem="modified", debug=0)

    data = c.transpose().copy()
    print('augmented size (after centroids)', data.shape)

    return data
# #########################################################################


# #########################################################################
def trim_class_from(data, which_class=None, upto=0.10,
                    maxiter=3, maxtime=300, debug=False):
    def terminate(M, i, start, maxiter, maxtime):
        if M < goal or i > maxiter or (time.time() - start) > maxtime:
            return True
        return False

    banner('Trimming class %s by %s%%' % (which_class, upto * 100))
    data_describe(data, "INITIAL SIZE")
    M, N = data.shape
    goal, i, start = int(M * (1. - upto)), 0, time.time()
    while not terminate(M, i, start, maxiter, maxtime):
        data = trim_class_using_random_centroids(data,
                                                 which_class=which_class,
                                                 nc=16,
                                                 min_cluster_size=256,
                                                 max_sampling_size=2048,
                                                 conseq="replace",
                                                 do_scaling=False,
                                                 preprocess=True,
                                                 ktimes=8,
                                                 debug=0)
        M, N = data.shape
    data_describe(data, "FINAL SIZE")
    return data
# #########################################################################


# #########################################################################
if __name__ == "__main__":
    data = pd.read_csv('DATA/merged.csv', sep='|')
    if 'Name' in data:
        del data['Name']

    data = trim_class_from(data, which_class=CLASS0, upto=0.20, maxiter=5)
    print('DONE: dataset_reduction')
