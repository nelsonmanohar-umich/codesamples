# stackoverflow/1434016
what="$1"
/usr/bin/awk --assign=R="${what}" '
BEGIN { 
    min=1; nlines=0; max=0; count=0; tot=0; start=0; last=0; skip=0; 
    #print "----------------------------------------";
}
{ 
    nlines++;
    if ( $0 == "[1] \"PROBABILITY MIXTURE\"" ) {
        start=1;
    };
    if ( start==1 ) {
        if ( skip==R ) {
            if( count < 540 ) {
                if ( $3 < min ) {
                    min = $3;
                    #print "min", min;
                };
                if ( $3 > max ) {
                    max = $3;
                    #print "max", max;
                };
                tot += $3; 
                last = $3;
                count++;
                #print R, $3, "\t", tot/count, "\t", min, "\t", max, "\t", start, skip, NR, count, nlines, "\t", $0;
            };
            skip=0;
            start=0;
        } else {
            skip++;
        };
    };
}
END { 
    #print "----------------------------------------";
    print last, "\t", count, "\t", tot/count, "\t", min, "\t", max; 
    #print "----------------------------------------";
}
' $2
