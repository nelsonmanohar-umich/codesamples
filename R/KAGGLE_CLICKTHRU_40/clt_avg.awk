#!/usr/bin/awk
# stackoverflow/1434016
BEGIN { min=1; max=0 }
{ 
    if ( NR <= 540 ) { 
        if ( $1 < min ) {
            min = $1
        }
        if ( $1 > max ) {
            max = $1
        }
        tot += $1; 
        count++ 
    } 
}
END { print $1, "\t", count, "\t", tot/count, "\t", min, "\t", max }
