
if __name__ == "__main__":
    with open('evaluation_clt.csv') as fp:
        i = 0
        for line in fp.readlines():
            i = 1
            line = line.strip()
            items = line.split(',')
            val = items[0]
            if "e" in items[0]:
                val = long(float(items[0]))
            print "%s,%s" % (val, items[1])
