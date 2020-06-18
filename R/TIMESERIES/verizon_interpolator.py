import sys
separator = ','


# ###########################################################################
def get_count_from(line):
    count, date, dow, year, month, \
        quarter, week, doy, daynum, age = parse_line(line, output=False)
    return count


# ###########################################################################
def build_output_line(nline, count, date, daynum):
    line_out = " ".join([str(x) for x in
                         [nline, count, date, date[0:4],
                          date[5:7], date[8:10], daynum]])
    return line_out


# ###########################################################################
def insert_record(gap_daynum, wrt_daynum, nline, lines):
    minindx = max(1, nline-3)
    maxindx = min(len(lines), nline+3)
    vals = [int(get_count_from(line)) for line in lines[minindx:maxindx]]
    if len(vals):
        val_count = int(float(sum(vals))/len(vals))
    else:
        print "this line"
        val_count = min(vals)
    out = "%s %s i" % (val_count, gap_daynum)
    line = lines[nline].strip()
    count, date, dow, year, month, \
        quarter, week, doy, daynum, age = parse_line(line, output=False)
    line_out = build_output_line(nline, count, date, daynum)
    out_fp.write("%s %s\n" % (line_out, out))
    return out


# ###########################################################################
def parse_line(line, output=True):
    line = line.strip()
    items = line.split(separator)
    if output:
        count, date, dow, year, month, quarter, week, doy, daynum, age = items
        sys.stderr.write("%04d|" % nline)
        for i, f in enumerate(['count', 'date', 'dow', 'year', 'month',
                               'quarter', 'week', 'doy', 'daynum', 'age']):
            sys.stderr.write("%s:%s|" % (f, items[i]))
        sys.stderr.write("\n")
    return items


# ###########################################################################
with open('DATA/VERIZON_INTERPOLATED_NEW.csv', 'w') as out_fp:
    with open('DATA/VERIZON_TRANSACTIONS_EXTENDED.csv') as fp:
        lines = fp.readlines()
        old_count, old_daynum = 0, -1
        for nline, line in enumerate(lines):
            if nline == 0:
                continue

            items = parse_line(line)
            count, date, dow, year, month, \
                quarter, week, doy, daynum, age = items

            for gap_daynum in range(int(old_daynum)+1, int(daynum)):
                insert_record(gap_daynum, int(daynum), nline, lines)

            old_count, old_daynum = count, daynum
            out = "%s %s r" % (count, daynum)
            line_out = build_output_line(nline, count, date, daynum)
            out_fp.write("%s %s\n" % (line_out, out))
