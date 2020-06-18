cosmo:MINHASH nelsonr$ python3 minhash.py 
--------------------------------------------------------------------------------
minhash and lshash functions for comparing documents
(i.e., documents share elements/values from an implicitly common lexicon)
as well as dataframes (i.e., similar values across columns/featues do not
necessarily represent the same quantify, factor level, physical meaning)
--------------------------------------------------------------------------------
This module (version Feb/2015), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to
redistribute it under conditions of the GNU General Public License.
--------------------------------------------------------------------------------
usage: minhash.py [-h] [--filename FILENAME] [--num_signatures NUM_SIGNATURES]
                  [--num_samples NUM_SAMPLES] [--verify VERIFY]
                  [--largediff LARGEDIFF]
                  [--visualize_patterns VISUALIZE_PATTERNS]
                  [--vizthreshold VIZTHRESHOLD] [--lsh_nbins LSH_NBINS]
                  [--debug DEBUG] [--op OP]

This program computes minhash-based approx. jaccard distances along with
detailed performance analytics.

optional arguments:
  -h, --help            show this help message and exit
  --filename FILENAME   filepath to file containing the lines to be sampled.
  --num_signatures NUM_SIGNATURES
                        (10,..] number of hash signatures to use.
  --num_samples NUM_SAMPLES
                        number of input records to read from datafile.
  --verify VERIFY       whether or not to compute true jaccard distances for
                        verification purposes
  --largediff LARGEDIFF
                        small float specifying what is large error in
                        differene between |approx-true| jaccard distances
  --visualize_patterns VISUALIZE_PATTERNS
                        [0/1] do quantized display to help visualize patterns
                        in the distances
  --vizthreshold VIZTHRESHOLD
                        [0..9] integer threshold above which quantized jaccard
                        distances are displayed for visualiation purposes
  --lsh_nbins LSH_NBINS
                        number of bins x to which to hash-compress the
                        resulting mapping of tuples of num_signatures/x column
                        cell values
  --debug DEBUG         debug level of output details [ERROR=1, WARNING=2,
                        INFO=3, DEBUG=4]
  --op OP               number of bins x to which to hash-compress the
                        resulting mapping of tuples of num_signatures/x column
                        cell values
--------------------------------------------------------------------------------
file to scan: datafile.csv
lines to read: 100
num. signatures: 50
vizthreshold 4
cross-verify: 0
error threshold 0.3
