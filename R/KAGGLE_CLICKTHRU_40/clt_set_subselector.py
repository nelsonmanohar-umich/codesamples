from collections import defaultdict

if __name__ == "__main__":
    WRT = 'KAGGLE/CLICKTHRU/train'
    if 1:
        BURSTSIZE = 250
        BURSTFREQ = int(5E5)
        offsets = [x for x in range(1,BURSTSIZE,1)]
        subset = offsets[:]

        for SAMPLING_IDX in range(int(0E6),int(50E6),BURSTFREQ):
            idxs = [SAMPLING_IDX+x for x in offsets]
            subset.extend(idxs)

        SUBSET = defaultdict(int)
        SUBSET[0] = 0
        for i in subset:
            SUBSET[i] = i

        i = 0
        for line in open(WRT):
            if i in SUBSET:
                print line.strip()
            i = i + 1
