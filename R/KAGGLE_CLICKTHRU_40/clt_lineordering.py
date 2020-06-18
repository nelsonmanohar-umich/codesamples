import sys

if __name__ == "__main__":
    print "id,click"
    with open('KAGGLE/CLICKTHRU/test') as fp1:
        with open('evaluation_clt.csv') as fp2:
            lr1 = ( line1a for line1a in fp1.readlines())
            lr2 = ( line2a for line2a in fp2.readlines())
            i = 0
            for (line1, line2) in zip( lr1, lr2 ):
                i = i + 1
                if i == 1: continue
                line1 = line1.strip()
                line2 = line2.strip()
                items1 = line1.split(',')
                items2 = line2.split(',')
                if items1[0][1:-1] != items2[0]:
                    print >>sys.stderr, items1[0][1:-1],items2[0]
                val  = items1[0][1:-1]
                prob = items2[1]
                print "%s,%s" % (val, prob )
