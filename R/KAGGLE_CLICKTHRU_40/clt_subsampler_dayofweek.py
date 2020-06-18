from collections import defaultdict

if __name__ == "__main__":
    PREFIX = "ta"
    WRT = 'KAGGLE/CLICKTHRU/train'

    if 1:
        BURSTSIZE = 1800
        BURSTFREQ = int(10000)
        offsets = [x for x in range(1,BURSTSIZE,1)]
        subset = offsets[:]

        for SAMPLING_IDX in range(int(0E6),int(50E6),BURSTFREQ):
            idxs = [SAMPLING_IDX+x for x in offsets]
            subset.extend(idxs)

        SUBSET = defaultdict(int)
        SUBSET[0] = 0
        for i in subset:
            SUBSET[i] = i

        fp21 = open( '%s-subsampled_141021.csv' % PREFIX, 'w' )
        fp22 = open( '%s-subsampled_141022.csv' % PREFIX, 'w' )
        fp23 = open( '%s-subsampled_141023.csv' % PREFIX, 'w' )
        fp24 = open( '%s-subsampled_141024.csv' % PREFIX, 'w' )
        fp25 = open( '%s-subsampled_141025.csv' % PREFIX, 'w' )
        fp26 = open( '%s-subsampled_141026.csv' % PREFIX, 'w' )
        fp27 = open( '%s-subsampled_141027.csv' % PREFIX, 'w' )
        fp28 = open( '%s-subsampled_141028.csv' % PREFIX, 'w' )
        fp29 = open( '%s-subsampled_141029.csv' % PREFIX, 'w' )
        fp30 = open( '%s-subsampled_141030.csv' % PREFIX, 'w' )
        fp31 = open( '%s-subsampled_141031.csv' % PREFIX, 'w' )

        i = 0
        for line in open(WRT):
            if i in SUBSET:
                items = line.split(',')
                date = items[2][0:6]
                if   date.endswith('21'): fp21.write( line )
                elif date.endswith('22'): fp22.write( line )
                elif date.endswith('23'): fp23.write( line )
                elif date.endswith('24'): fp24.write( line )
                elif date.endswith('25'): fp25.write( line )
                elif date.endswith('26'): fp26.write( line )
                elif date.endswith('27'): fp27.write( line )
                elif date.endswith('28'): fp28.write( line )
                elif date.endswith('29'): fp29.write( line )
                elif date.endswith('30'): fp30.write( line )
                elif date.endswith('31'): fp31.write( line )
            i = i + 1

        fp21.close()
        fp22.close()
        fp23.close()
        fp24.close()
        fp25.close()
        fp26.close()
        fp27.close()
        fp28.close()
        fp29.close()
        fp30.close()
        fp31.close()
    print i
