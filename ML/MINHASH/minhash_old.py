#!/usr/bin/python


# #################################################################
import math
import random
from collections import defaultdict
import numpy as np
import pandas as pd
import time
import bitarray
import argparse
import sys
# #################################################################


# #################################################################
# stackoverflow.com/questions/11707586
pd.set_option('display.max_rows', 100)
pd.set_option('display.max_columns', 30)
pd.set_option('display.width', 200)
pd.set_option('display.height', 200)
pd.set_option('display.precision', 2)
# #################################################################


# #################################################################
# FEATURE_NAME_HASH = defaultdict(int)
# FEATURE_NAME_CODE = defaultdict(int)
# GLOBAL_HASHES = dict()
# LEXICON = {}
# DEBUG_OUTPUT_LEVEL = 3
# DEBUG_LEVELS = dict(zip(["ERROR", "WARNING", "INFO", "DEBUG"], range(4)))
# #################################################################


# #################################################################
class MINHASH_CLASS(object):
# #################################################################

    # #################################################################
    def __init__(self, debug_level=3):
        self.FEATURE_NAME_HASH = defaultdict(int)
        self.FEATURE_NAME_CODE = defaultdict(int)
        self.GLOBAL_HASHES = dict()
        self.LEXICON = {}
        self.DEBUG_OUTPUT_LEVEL = debug_level
        self.DEBUG_LEVELS = dict(list(zip(["ERROR", "WARNING", "INFO", "DEBUG"], list(range(4)))))
        self.LAST_TIMESTAMP = time.time()
        self.HEADER("MINHASH CLASS INITIALIZED")
        self.JCSMAT = None
        self.SignatureMatrix = None
        return
    # #################################################################

    # #################################################################
    def TIMESTAMP(self, msg):
        ct_time = time.time()
        delta = "%8.3f secs" % (ct_time - self.LAST_TIMESTAMP)
        self.INFO(msg, delta)
        self.LAST_TIMESTAMP = ct_time
        return delta
    # #################################################################

    # #################################################################
    def HEADER(self, msg="", n=1):
        if msg:
            self.TIMESTAMP(msg)
        for i in range(n):
            print('-' * 80)
        print()
        print()
        print()
        for i in range(n):
            print('-' * 80)
    # #################################################################

    # #################################################################
    def WARNING(self, msg, val=""):
        return self.PRINTER("WARNING", msg, val)
    # #################################################################

    # #################################################################
    def DEBUG(self, msg, val):
        return self.PRINTER("DEBUG", msg, val)
    # #################################################################

    # #################################################################
    def INFO(self, msg, val):
        return self.PRINTER("INFO", msg, val)
    # #################################################################

    # #################################################################
    def PRINTER(self, ptype, msg, val):
        if self.DEBUG_LEVELS[ptype] < self.DEBUG_OUTPUT_LEVEL:
            print("%10s: %32s %s" % (ptype, msg, val))
        return val
    # #################################################################

    # #################################################################
    # stackoverflow.com/generating-large-prime-numbers-in-python...
    # #################################################################
    def get_prime_number_between(self, RMIN=1E0, RMAX=1E3):
        if RMAX < RMIN:
            WARNING("INTERVAL [RMIN,RMAX] is EMPTY.", (RMIN, RMAX))
        RMIN = (int(max(RMIN, 8)/2) << 1)+1
        RMAX = (max(int(RMAX/2) << 1, RMIN))+1
        self.DEBUG("INTERVAL [RMIN,RMAX] RESET to:", (RMIN, RMAX))
        while True:
            p = random.randrange(RMIN, RMAX, 2)
            Q = int(math.sqrt(p)+1)
            if all(p % x != 0 for x in range(3, Q, 2)):
                return p
    # #################################################################

    # #################################################################
    def generate_hash_function(self, n,  a, b):
        i = 0
        while True:
            i = i + 1
            a = self.get_prime_number_between(2, a)
            b = self.get_prime_number_between(1, b)
            p = n
            if all((a-b, b-p, a-p)):
                if (a, b) not in self.GLOBAL_HASHES:
                    self.GLOBAL_HASHES[(a, b)] = (a, b, p)
                    break
                if i % 1E2 == 0:
                    self.WARNING("DEFAULTING HASH GENERATION", self.TIMESTAMP("HASH ERROR"))
                    break
        self.DEBUG("GENERATED HASH:", "%sx+%s mod %s" % (a, b, p))
        return (a, b, p)
    # #################################################################

    # #################################################################
    def generate_random_iterator(self, n, a_range=2047, b_range=255):
        (a, b, p) = self.generate_hash_function(n, a_range, b_range)
        random_iterator = ((a*x+b) % p for x in range(n))
        return random_iterator
    # #################################################################

    # #################################################################
    def do_compress(self, ngrams):
        self.tokens = defaultdict(int)
        for ngram in ngrams:
            token = ngram[:]
            self.tokens[token] = self.tokens[token]+1
        return self.tokens.copy()
    # #################################################################

    # #################################################################
    def do_ngrams(self, vector, k, compress=False):
        self.ngrams = vector
        if len(vector) > k:
            if k == 2: self.ngrams = list(zip(vector, vector[1:]))
            if k == 3: self.ngrams = list(zip(vector, vector[1:], vector[2:]))
            if k == 4: self.ngrams = list(zip(vector, vector[1:], vector[2:], vector[3:]))
            if k == 5: self.ngrams = list(zip(vector, vector[1:], vector[2:], vector[3:], vector[4:]))
            if k == 6: self.ngrams = list(zip(vector, vector[1:], vector[2:], vector[3:], vector[4:], vector[5:]))
            if k == 7: self.ngrams = list(zip(vector, vector[1:], vector[2:], vector[3:], vector[4:], vector[5:], vector[6:]))
            if k > 7:  WARNING("max ngrams level is 7, vector unchanged")
        if compress: self.ngrams = do_compress(self.ngrams)
        return self.ngrams
    # #################################################################

    # #################################################################
    def do_shingling(self, unigrams, k=2):
        self.kgram_features = self.do_ngrams(unigrams, k)
        return self.kgram_features
    # #################################################################

    # #################################################################
    def get_lexicon(self, vectors, debug=0):
        # global LEXICON
        self.lexicon = set()
        for colnum in vectors.columns:
            self.lexicon.update(set(vectors[colnum]))
        self.frozen_lexicon = tuple(sorted([x for x in self.lexicon]))
        self.LEXICON = dict(list(zip(self.frozen_lexicon, list(range(len(self.frozen_lexicon))))))
        if debug:
            with open('lexicon.dat', 'w') as fp:
                fp.write(repr(self.LEXICON))
        return (self.frozen_lexicon, len(self.lexicon), len(vectors.index))
    # #################################################################

    # #################################################################
    def get_lexicon_representation(self, vector):
        self.lexicon_based_representation = bitarray.bitarray(len(self.LEXICON))
        self.lexicon_based_representation.setall(0)
        for idx in [self.LEXICON[x] for x in vector]:
            self.lexicon_based_representation[idx] = 1
        return self.lexicon_based_representation
    # #################################################################

    # #################################################################
    def verify(self, v, n):
        self.pairing = list(zip(tuple(v), tuple([x[0] for x in n if x[1]])))
        self.score = [1 if x[0] != x[1] else 0 for x in self.pairing]
        return "OK" if sum(self.score) == 0 else "NOT_OK"
    # #################################################################

    # #################################################################
    def get_jaccard_column_similarity_matrix(self, vectors, debug=0):
        #shingled_vectors = [ do_shingling(vector, k=2) for vector in vectors]
        #lexicon = get_lexicon(shingled_vectors)
        self.lexicon, N, M = self.get_lexicon(vectors)
        self.JCSMAT = []
        for sample_num in vectors.index:
            vector = vectors.ix[sample_num]
            self.JCSMAT.append(self.get_lexicon_representation(vector))
            if debug:
                nval = self.JCSMAT[sample_num]
                if sample_num % 1000 == 0:
                    nval = list(zip(sorted(self.LEXICON.keys()), nval))
                    self.DEBUG("VECTOR[%6s]: " % sample_num, self.verify(vector, nval))
                    if debug > 1:
                        self.DEBUG("SPARSE  ", nval)
        self.JCSMAT = pd.DataFrame(self.JCSMAT).T
        self.DEBUG("COLUMN SIMILARITY MATRIX\n", self.JCSMAT)
        self.HEADER("JACCARD SIMILARITY MATRIX CONSTRUCTION DONE")
        return self.JCSMAT
    # #################################################################

    # #################################################################
    def get_jaccard_similarity(self, jcsmat, col_i, col_j, debug=1):
        '''jcsmat rows are indexed as elements of the lexicon (universal set),
           columns are the samples expressed in terms of the universal lexicon
           '''
        jaccard_sim = 0.0
        x = bitarray.bitarray(jcsmat[col_i])
        y = bitarray.bitarray(jcsmat[col_j])
        N = len(x)
        xy_intersection = [x[idx] & y[idx] for idx in range(N)]
        xy_union = [x[idx] | y[idx] for idx in range(N)]
        if debug:
            if debug > 1:
                self.DEBUG("SIMILARITY BETWEEN:", list(zip(x, y)))
            self.DEBUG("xy_intersection", (sum(xy_intersection), xy_intersection))
            self.DEBUG("xy_union", (sum(xy_union), xy_union))
        XandY = sum(xy_intersection)
        XorY = sum(xy_union)
        if XorY:
            jaccard_sim = XandY/float(XorY)
        return jaccard_sim
    # #################################################################

    # #################################################################
    def get_approx_jaccard_similarity(self, sigmat, col_i, col_j, debug=True):
        ''' computes the similarity between signatures which represents
            the jaccard similarity approximation between two columns
            in terms of equivalency of minhash values contained on their
            columns for the underlying NSIGS permutation hashes'''
        x = sigmat[col_i]
        y = sigmat[col_j]
        N = len(x)
        xy_intersection = [1 if not int(x[idx] - y[idx]) else 0 for idx in range(N)]
        if debug:
            self.DEBUG("SIMILARITY BETWEEN:", list(zip(x, y)))
            self.DEBUG("xy_intersection", (sum(xy_intersection), xy_intersection))
        jaccard_sim = float(sum(xy_intersection))/float(N)
        return float(jaccard_sim+0)
    # #################################################################

    # #################################################################
    def do_minhashing_kernel(self, jcsmat):
        ''' the minhash is defined as the row number of a given column,
            given a specified permutation (i.e., hashing random ordering)
            for which the row/column val represents the FIRST activation
            for that column. Similarly, the signature is the collection
            of such first-of-kind values across all columns of the
            jcsmat (i.e., samples of the data expressed in terms of the
            lexicon).'''
        N, M = jcsmat.shape
        MAXINT = N+1
        jSignatureMap = [MAXINT for x in range(M)]
        hash_iterator = self.generate_random_iterator(N)
        c = dict()
        for col in jcsmat.columns:
            c[col] = bitarray.bitarray(jcsmat[col])
        for ridx in hash_iterator:
            which_cols = [x for x in jcsmat.columns if c[x][ridx] > 0]
            for col in which_cols:
                if ridx < jSignatureMap[col]:
                    jSignatureMap[col] = ridx
            if any([item == MAXINT for item in jSignatureMap]):
                continue
            break
        return jSignatureMap
    # #################################################################

    # #################################################################
    def do_minhashing_core(self, jcsmat, num_signatures=10):
        N, M = jcsmat.shape
        self.SignatureMatrix = pd.DataFrame(np.zeros((num_signatures, M)))
        S = self.SignatureMatrix.T
        # signature = do_minhashing_kernel(jcsmat) # S[k] = signature[:]
        for k in range(num_signatures):
            S[k] = self.do_minhashing_kernel(jcsmat)
        self.SignatureMatrix = S.T
        self.INFO("Signature Matrix\n", self.SignatureMatrix)
        return self.SignatureMatrix
    # #################################################################

    # #################################################################
    def do_minhashing(self, jcsmat, num_signatures=10, debug=0):
        ''' returns an upper triangular matrix of the approx. js distances
            computed using a non-iterative alternative computation.'''
        self.SignatureMatrix = self.do_minhashing_core(jcsmat, num_signatures)
        N, M = self.SignatureMatrix.shape
        timemark = time.time()
        self.approxJS = np.zeros((M, M))
        c = [0 for x in range(M)]
        for j in range(M):
            c[j] = np.array(self.SignatureMatrix[j])
        out = ""
        for i in range(M):
            for j in range(i, M):
                # A: idxs = [idx for idx in xrange(N) if not int(SignatureMatrix[i][idx]) ^ int(SignatureMatrix[j][idx])]
                # A: approxJS[j][i] = float(len(idxs))/float(N)
                # B: idxs = 0 # for idx in xrange(N): if not (SignatureMatrix[i][idx] - SignatureMatrix[j][idx]): idxs += 1
                idxs = N - np.count_nonzero(c[i]-c[j])
                self.approxJS[j][i] = idxs
                self.approxJS[i][j] = self.approxJS[j][i]
            out += "(%s: %.4f), " % (i, time.time()-timemark)
            timemark = time.time()
        self.HEADER("MINHASHING APPROXIMATION DONE USING %s SIGNATURES" % num_signatures)
        self.INFO("MINHASHING STATS\n", out)
        self.HEADER("MINHASHING COMPUTATION DONE")
        self.approxJS = pd.DataFrame(self.approxJS/float(N))
        self.INFO("~Jaccard Sim. Matrix\n", self.approxJS)
        self.HEADER("APPROX JACCARD COMPUTATION DONE")
        return self.approxJS
    # #################################################################

    # #################################################################
    def do_locality_sensing_hashing(self, signatures):
        approximation_matrix = [x for x in signatures]
        return approximation_matrix
    # #################################################################

    # #################################################################
    def rename_df_byfeature(self, df, compress=True):
        '''renames each feature value to a unique value across features
           for example, value z1 in feature f1 no longer is the same token
           as value z1 for feature f2 but rather f1-z1 and f2-z1, resp.'''
        self.FEATURE_NAME_HASH = defaultdict(int)
        self.FEATURE_NAME_CODE = defaultdict(int)
        self.HEADER("FEATURE CODING/HASHING START")
        idx = 0
        for i in df.columns:
            f = "F%s" % str(i).zfill(2)
            df[i] = ["%s:%s" % (f, x) for x in df[i]]
            for x in df[i]:
                if x not in self.FEATURE_NAME_HASH:
                    idx = idx + 1
                    self.FEATURE_NAME_HASH[x] = 0
                    self.FEATURE_NAME_CODE[x] = idx
                self.FEATURE_NAME_HASH[x] = self.FEATURE_NAME_HASH[x] + 1

        self.DEBUG("A) INPUT DATA\n", df)

        if compress:
            for i in df.columns:
                df[i] = [self.FEATURE_NAME_CODE[x] for x in df[i]]
        self.INFO("D) RECODED INPUT DATA\n", df)
        self.INFO("TOTAL NUMBER TERMS", (idx, df.shape))
        self.HEADER("FEATURE CODING DONE")

        return df
    # #################################################################

    # #################################################################
    def print_feature_encoding(self):
        '''prints the mapping as well as frequency statistics for input
           values in terms of the speficied f[i]-val[j] encoding tokens'''
        self.INFO("B) FEATURE HASHING\n", [(x, self.FEATURE_NAME_HASH[x])
                  for x in sorted(self.FEATURE_NAME_HASH.keys())])
        print()
        self.INFO("C) FEATURE ENCODING\n", [(x, self.FEATURE_NAME_CODE[x])
                  for x in sorted(self.FEATURE_NAME_CODE.keys())])
        return
    # #################################################################

    # #################################################################
    def print_distance_mat(self, jdistances, threshold=4, ncols=60):
        pd.set_option('display.max_columns', ncols)
        self.idistances = jdistances * 10 - 1
        for j in jdistances.columns:
            self.idistances[j] = [int(x) if x > threshold
                                  else "." for x in self.idistances[j]]
        self.INFO("QUANTIZED JACCARD INT. DISTANCES\n", self.idistances)
        self.HEADER("QUANTIZED JACCARD DISTANCES DONE")
        return self.idistances
    # #################################################################

    # #################################################################
    def setup(self, original_vectors):
        RECODED_VECTORS = MINHASH.rename_df_byfeature(original_vectors)
        self.print_feature_encoding()
        MINHASH.INFO("INPUT DATA SHAPE", "%s (M) rows/samples, %s (N) columns/features" % RECODED_VECTORS.shape)
        return RECODED_VECTORS
    # #################################################################

    # #################################################################
    def verify_distances(self, jdistances, jcsmat, threshold=0.2, use_abs=True, NMAX=40):
        N, M = jcsmat.shape
        NMAX = min(M, 40)
        C = [x for x in jcsmat.columns]
        differences = np.array(np.zeros((NMAX, NMAX)))

        for i in C[0:NMAX]:
            for j in C[0:NMAX]:
                val = MINHASH.get_jaccard_similarity(jcsmat, i, j, debug=False)
                print("%4.2f" % val, end=' ')
                differences[i][j] = val - jdistances[i][j]
            print()
        self.HEADER("COMPARATIVE DISPLAY OF TRUE JACCARD DISTANCES DONE")
        print()

        for i in C[0:NMAX]:
            for j in C[0:NMAX]:
                val = differences[i][j]
                if use_abs:
                    val = abs(differences[i][j])
                if abs(val) > threshold:
                    print("%4.2f" % val, end=' ')
                else:
                    print('....', end=' ')
            print()
        self.HEADER("ABSOLUTE ERROR B/W APPROXIMATE & TRUE JACCARD DISTANCES DONE")
        print()

        return differences
    # #################################################################


# #################################################################
if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog='minhash.py', description='This program computes minhash-based approx. jaccard distances along with detailed performance analytics.')
    parser.add_argument("--filename", default='datafile.csv', help='filepath to file containing the lines to be sampled.')
    parser.add_argument("--num_signatures", type=int, default=50, help='(10,..] number of hash signatures to use.')
    parser.add_argument("--num_samples", type=int, default=500, help='number of input records to read from datafile.')
    parser.add_argument("--verify", type=int, default=1, help='whether or not to compute true jaccard distances for verification purposes')
    parser.add_argument("--largediff", type=float, default=0.25, help='small float specifying what is large error in differene between |approx-true| jaccard distances')
    parser.add_argument("--vizthreshold", type=int, default=4, help='[0..9] integer threshold above which quantized jaccard distances are displayed for visualiation purposes')
    args = parser.parse_args()
    parser.print_help()

    print('-' * 80, file=sys.stderr)
    print("file to scan:", args.filename, file=sys.stderr)
    print("lines to read:", args.num_samples, file=sys.stderr)
    print("num. signatures:", args.num_signatures, file=sys.stderr)
    print("vizthreshold", args.vizthreshold, file=sys.stderr)
    print("cross-verify:", args.verify, file=sys.stderr)
    print("error threshold", args.largediff, file=sys.stderr)
    print('-' * 80, file=sys.stderr)

    MINHASH = MINHASH_CLASS()

    recoded_vectors = MINHASH.setup(pd.read_csv(args.filename,
                                    header=None, sep=",",
                                    nrows=args.num_samples))

    jcsmat = MINHASH.get_jaccard_column_similarity_matrix(recoded_vectors)
    del(recoded_vectors)

    jdistances = MINHASH.do_minhashing(jcsmat, num_signatures=args.num_signatures)
    idistances = MINHASH.print_distance_mat(jdistances, threshold=args.vizthreshold)

    if args.verify:
        differences = MINHASH.verify_distances(jdistances, jcsmat, threshold=args.largediff)

    MINHASH.HEADER('DONE')
