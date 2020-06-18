ls -Cths P[3-9]*.RData
echo "______________________________________________"
F=`grep LOG_LOSS output_clt.out | cut -f 3 -d " " | cut -f 1 -d '"' | awk ' { if ( $1<0.4075 ) print $1} ' | awk '{lines=lines+1; if (lines>2) print $0}'`
echo ${F}
echo "______________________________________________"

for i in `ls -t1r P*.RData | cut -f 2 -d '-'  | cut -f 1 -d'.'` ; do grep FACTOR output_clt.out | fgrep "[${i}]" ; done | sort | uniq
