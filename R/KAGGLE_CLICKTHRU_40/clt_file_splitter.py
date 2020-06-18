
from collections import defaultdict

if __name__ == "__main__":

    i = 0
    N = 100000
    #stem = "TRAIN/training_chunk_%s.csv"
    stem = "testing/test_chunk_%s.csv"
    chunk_id = 1000

    fp = None
    #for line in open('KAGGLE/CLICKTHRU/train'):
    for line in open('KAGGLE/CLICKTHRU/test'):
        if i == 0:
            i = i + 1
            continue
        if ( i % N == 1 ): 
            if fp: fp.close()
            chunk_id = chunk_id + 1
            outfile = stem % chunk_id
            fp = open( outfile, "w")
        fp.write(line)
        i = i + 1
