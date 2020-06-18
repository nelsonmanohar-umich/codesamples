from collections import defaultdict

CLICKFIELD  = 2
TIMEFIELD   = 3

SITE_CAT=('f028772b','50e219e0','28905ebd','3e814130','75fa27f6','335d28a8','dedf689d','f66779e6','c0dd3be3','70fb0e29','76b2941d','72722551','0569f928','42a36e14','8fd0aea4','e787de0e','bcf865d9','5378d028','a818d37a','9ccfa2ea','74073276','other')
TARGETED_IDS = SITE_CAT
TARGETFIELD = 8
output_file = "probs_site_cat.csv"

APP_CAT =('07d7df22','0f2161f8','f95efa07','cef3e649','8ded1f7a','d1327cf5','dc97ec06','09481d60','75d80bbe','4ce2e9fc','879c24eb','fc6fa53d','4681bb9d','0f9a328c','a3c42688','8df2e842','a86a3e89','a7fd01ec','79f0b860','2281a340','18b1e0be','0bfbc358','0d82db25','2fc4f2aa','7113d72a','71af18ce','5326cf99','other')
TARGETED_IDS = APP_CAT
TARGETFIELD = 11
output_file = "probs_app_cat.csv"

HOUR_RANGE = [ str(x)[1:] for x in range(100,124) ]

def validate_key(key):
    if key in TARGETED_IDS:
        return key
    return "other"

if __name__ == "__main__":
    WRT = 'KAGGLE/CLICKTHRU/train'

    yes = {}
    nop = {}

    for key in TARGETED_IDS:
        yes[key] = defaultdict(int)
        nop[key] = defaultdict(int)
        for hour in HOUR_RANGE:
            yes[key][hour] = 0
            nop[key][hour] = 0

    N = 0
    C = 0
    for line in open(WRT):
        N = N + 1
        if N == 1:
            continue
        items = line.split(',')
        click = items[CLICKFIELD-1].strip()
        hour = items[TIMEFIELD-1][6:].strip()
        field_id = items[TARGETFIELD-1].strip()
        field_id = validate_key(field_id)

        if "1" in click:
            C = C + 1
            yes[field_id][hour] = yes[field_id][hour] + 1.
        else:
            nop[field_id][hour] = nop[field_id][hour] + 1.

    for field in TARGETED_IDS:
        print [yes[field_id][x] for x in HOUR_RANGE]

    print N, C

    with open(output_file, 'w') as fp:
        fp.write("id,hour,poscount,totalcnt,prob\n")
        for key in TARGETED_IDS:
            for hour in HOUR_RANGE:
                poscount = float(yes[key][hour])
                negcount = float(nop[key][hour])
                totalcnt = poscount + negcount + 1.
                prob = float(poscount) / float(totalcnt)
                out = "%s:%s,%s,%s,%s\n" % (key, hour, poscount, totalcnt, prob)
                fp.write(out)
                print out.strip()

    print '-' * 80
