cat $1 | 
/usr/bin/awk ' 
BEGIN { 
    lines=0 
}
{ 
    if( /EMSEMBLE/ ) {
        lines=25;
    };

    if (lines > 0) {
        print;
        --lines;
    };
} '
