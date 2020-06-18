import sys
import pandas as pd
from collections import defaultdict

train = "KAGGLE/CLICKTHRU/train"
test = "KAGGLE/CLICKTHRU/test"

test_ids = defaultdict(int)
train_ids = defaultdict(int)
idx = 8

N = 50
M = 10000

if __name__ == "__main__":
    i = 0
    with open(train) as fp1:
        lr1 = ( line1a for line1a in fp1.readlines())
        for line in lr1: 
            i = i + 1
            if i > M:
                break
            items = line.strip().split(',')
            sc = items[idx-1]
            train_ids[sc] += 1 

    i = 0
    with open(test) as fp1:
        lr1 = ( line1a for line1a in fp1.readlines())
        for line in lr1: 
            i = i + 1
            if i > M:s
                break
            items = line.strip().split(',')
            sc = items[idx-1]
            test_ids[sc] += 1 

    top_keys_during_training = sorted(test_ids, key=test_ids.__getitem__, reverse=True )
    top_keys_during_testing  = sorted(train_ids, key=test_ids.__getitem__, reverse=True )

    for key in top_keys_during_testing[0:N]:
        if key not in top_keys_during_training[0:N]:
            print key, top_keys_during_testing[key]

    # find_matching_key_in_training( key )
    with open(train) as fp1:
        lr1 = ( line for line in fp1.readlines() if line.split(',')[idx-1]==key )
    with open(test) as fp2:
        lr2 = ( line for line in fp2.readlines() if line.split(',')[idx-1]==key )
    lines1 = [ line for line in lr1 ]
    lines2 = [ line for line in lr2 ]
    # transform into cuts the original numbers so as to compute distance matrix with respect to x but only if there is high consensus on training value of y
    # might as well use knn ignoring any fields that are not ammenable and the field of interest and require high consensus k and l
