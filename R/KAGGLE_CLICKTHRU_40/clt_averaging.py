import sys

from collections import defaultdict

'''
 Your team's final score will be the best private submission score from the 2 selected submissions.
Submission 	Files 	Public Score 	Selected?
Tue, 03 Feb 2015 04:05:33
Edit description 	averaging.out.gz 	0.4136448 	
Tue, 03 Feb 2015 01:23:13
Edit description 	evaluation_clt_jan031d.csv.gz 	0.4244580 	
Mon, 02 Feb 2015 13:36:45
Edit description 	evaluation_clt_jan031b.csv.gz 	0.4114245 	
Mon, 02 Feb 2015 10:51:01
Edit description 	evaluation_clt_jan031a.csv.gz 	0.4147491 	
Sun, 01 Feb 2015 12:49:23
Edit description 	evaluation_clt_jan030c.csv.gz 	0.4113803 	
Sun, 01 Feb 2015 01:38:17
Edit description 	evaluation_clt_jan030b.csv.gz 	0.4126498 	
Sun, 01 Feb 2015 00:35:52
Edit description 	evaluation_clt_jan030a.csv.gz 	0.4126498 	
Tue, 27 Jan 2015 14:47:11
Edit description 	evaluation_clt_jan020c.csv.gz 	0.4062351 	
Tue, 27 Jan 2015 04:38:22
Edit description 	evaluation_clt_jan020b.csv.gz 	0.4113981 	
Tue, 27 Jan 2015 04:26:21
Edit description 	evaluation_clt_jan020a.csv.gz 	0.4168380 	
Thu, 22 Jan 2015 22:12:14
Edit description 	evaluation_clt_jan019a.csv.gz 	0.4114406 	
Thu, 22 Jan 2015 16:12:37
Edit description 	evaluation_clt_jan017a.csv.gz 	0.4117807 	
Mon, 19 Jan 2015 03:22:13
Edit description 	evaluation_clt_jan012a.csv.gz 	0.4158915 	
Thu, 15 Jan 2015 13:49:09
Edit description 	evaluation_clt_jan09b.csv.gz 	0.4156492 	
Thu, 15 Jan 2015 00:00:21
Edit description 	evaluation_clt_jan09a.csv.gz 	0.4185532 	
Tue, 13 Jan 2015 15:39:00
Edit description 	evaluation_clt_jan08c.csv.gz 	0.4188544 	
Tue, 13 Jan 2015 09:23:28
'''

FILES=('evaluation_clt_jan017a.csv','evaluation_clt_jan019a.csv', 'evaluation_clt_jan020b.csv','evaluation_clt_jan020c.csv','evaluation_clt_jan02a.csv','evaluation_clt_jan030a.csv','evaluation_clt_jan030c.csv', 'evaluation_clt_jan031b.csv')

if __name__ == "__main__":
    FILES = [r"SUBMISSIONS/GZIP/"+filename for filename in FILES]
    means = defaultdict(list)
    filename = FILES[0]
    i = 0
    for line in open(filename):
        if i == 0: 
            i = i + 1
            next
        else:
            i = i + 1
            items = line.strip().split(',')
            adid = items[0]
            prob = float(items[1])
            means[adid] = prob
            #if i % 10000: print i, prob, means[adid]

    if ( 0 ): print '-' * 80

    for k,filename in enumerate(FILES[1:]):
        i = 0
        for line in open(filename):
            if i == 0:
                i = i + 1
                next
            else:
                i = i + 1
                items = line.strip().split(',')
                adid = items[0]
                prob = float(items[1])
                means[adid] = (means[adid]*(k+1) + prob)/float(k+2)
                #if i % 10000: print i, prob, means[adid]

    if ( 0 ): 
        print '-' * 80
        # for adid in means:
            # probs = means[adid]
            # print adid, probs

    filename = FILES[0]
    for line in open(filename):
        if i == 0:
            i = i + 1
            next
        else:
            i = i + 1
            items = line.strip().split(',')
            adid = items[0]
            print "%s,%s" % ( adid, means[adid] )

    #for adid in means:
        #probs = means[adid]
        #means_adid = sum(probs)/len(probs)
        #print >>sys.stderr, adid, means_adid, probs
