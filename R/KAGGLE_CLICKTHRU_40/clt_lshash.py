#!/usr/bin/python

# #################################################################
# copyright nelsonmanohar
__copyright__ = "nelson manohar c(2015)"
__license__= '''This module (version Feb/2015), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License.'''
__author__ = "NelsonManohar"
__date__   = "Feb2015"
__doc__    = '''minhash and lshash functions for comparing documents
(i.e., documents share elements/values from an implicitly common lexicon)
as well as dataframes (i.e., similar values across columns/featues do not
necessarily represent the same quantify, factor level, physical meaning)'''
print '-' * 80
print __doc__
print '-' * 80
print __license__
print '-' * 80
# #################################################################


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
import minhash
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
class LSH_CLASS (minhash.MINHASH_CLASS):

    # #################################################################
    @property
    def sigmat(self):
        return self.SignatureMatrix
    # #################################################################

    # #################################################################
    @sigmat.setter
    def sigmat(self, x):
        self.SignatureMatrix = x
    # #################################################################

    # #################################################################
    def hash_itemset(self, itemset, n, how="min"):
        if how == "min":
            if itemset:
                return min(itemset) % n
        else:
            return hash(itemset) % n
        return 0
    # #################################################################

    # #################################################################
    def partition_sigmat_into_blocks(self, b=10):
        N, M = self.sigmat.shape
        self.num_blocks_per_col = b
        self.num_rows_per_block = int(math.ceil(N/float(b)))
        self.CSIG = np.zeros((b, M))
        for col in self.sigmat.columns:
            for bnum in range(b):
                lo = bnum * self.num_rows_per_block
                hi = min((bnum+1) * self.num_rows_per_block, N)
                block_colvals = tuple([self.sigmat[col][x] for x in range(lo, hi)])
                self.CSIG[bnum][col] = self.hash_itemset(block_colvals, self.num_rows_per_block)
        self.CSIG = pd.DataFrame(self.CSIG)
        return
    # #################################################################

    # #################################################################
    def do_lshashing_via_approx_jaccard(self):
        ''' returns an upper triangular matrix of the approx. js distances
            computed using a non-iterative alternative computation.'''
        N, M = self.CSIG.shape
        timemark = time.time()
        self.approxJS = np.zeros((M, M))
        c = [0 for x in xrange(M)]
        for j in xrange(M):
            c[j] = np.array(self.CSIG[j])
        out = ""
        for i in xrange(M):
            for j in xrange(i, M):
                idxs = N - np.count_nonzero(c[i]-c[j])
                self.approxJS[j][i] = idxs
                self.approxJS[i][j] = self.approxJS[j][i]
            out += "(%s: %.4f), " % (i, time.time()-timemark)
            timemark = time.time()
        self.DEBUG("MINHASHING STATS\n", out)
        self.HEADER("MINHASHING COMPUTATION DONE")
        self.approxJS = pd.DataFrame(self.approxJS/float(N))
        self.INFO("~Jaccard Sim. Matrix\n", self.approxJS)
        self.HEADER("APPROX JACCARD COMPUTATION DONE")
        return self.approxJS
    # #################################################################

    # #################################################################
    def do_locality_sensing_hashing(self, b):
        self.partition_sigmat_into_blocks(b)
        return self.do_lshashing_via_approx_jaccard()
    # #################################################################


# #################################################################
if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog='minhash.py',
             description='This program computes minhash-based approx. jaccard distances along with detailed performance analytics.')
    parser.add_argument("--filename",
             default='datafile.csv', help='filepath to file containing the lines to be sampled.')
    parser.add_argument("--num_signatures", type=int,
             default=50, help='(10,..] number of hash signatures to use.')
    parser.add_argument("--num_samples", type=int,
             default=100, help='number of input records to read from datafile.')
    parser.add_argument("--verify", type=int,
             default=0, help='whether or not to compute true jaccard distances for verification purposes')
    parser.add_argument("--largediff", type=float,
             default=0.3, help='small float specifying what is large error in differene between |approx-true| jaccard distances')
    parser.add_argument("--visualize_patterns", type=int,
             default=0, help='[0/1] do quantized display to help visualize patterns in the distances')
    parser.add_argument("--vizthreshold", type=int,
             default=4, help='[0..9] integer threshold above which quantized jaccard distances are displayed for visualiation purposes')
    parser.add_argument("--lsh_nbins", type=int,
             default=10, help='number of bins x to which to hash-compress the resulting mapping of tuples of num_signatures/x column cell values')
    parser.add_argument("--debug", type=int,
             default=3, help='debug level of output details [ERROR=1, WARNING=2, INFO=3, DEBUG=4]')
    parser.add_argument("--op", type=str,
             default="lshash", help='number of bins x to which to hash-compress the resulting mapping of tuples of num_signatures/x column cell values')
    args = parser.parse_args()
    parser.print_help()

    print >>sys.stderr, '-' * 80
    print >>sys.stderr, "file to scan:", args.filename
    print >>sys.stderr, "lines to read:", args.num_samples
    print >>sys.stderr, "num. signatures:", args.num_signatures
    print >>sys.stderr, "vizthreshold", args.vizthreshold
    print >>sys.stderr, "cross-verify:", args.verify
    print >>sys.stderr, "error threshold", args.largediff
    print >>sys.stderr, '-' * 80

    MINHASH = LSH_CLASS(debug_level=args.debug)

    recoded_vectors = MINHASH.setup(pd.read_csv(args.filename,
                                    header=None, sep=",",
                                    nrows=args.num_samples))
    jcsmat = MINHASH.get_jaccard_column_similarity_matrix(recoded_vectors)
    del(recoded_vectors)

    if args.op == "minhash":
        jdistances = MINHASH.do_minhashing(jcsmat, num_signatures=args.num_signatures)
    else:
        MINHASH.do_minhashing_core(num_signatures=args.num_signatures)
        jdistances = MINHASH.do_locality_sensing_hashing(args.lsh_nbins)

    if args.visualize_patterns:
        idistances = MINHASH.print_distance_mat(jdistances, threshold=args.vizthreshold, ncols=60)

    if args.verify:
        MINHASH.verify_distances(jdistances, jcsmat, threshold=args.largediff, use_abs=True, NMAX=40)

    MINHASH.HEADER('DONE')
