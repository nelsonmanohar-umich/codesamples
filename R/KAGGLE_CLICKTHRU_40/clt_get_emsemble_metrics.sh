FILE="output_clt_e.out"
METRICS0=" YP "
METRICS1=" M1 M2 M3 M4 M5 M6 M7 M8 M9 "
METRICS2=" DT YF "
BANNER="--------------------------------------------------------------------"
HOPTS="2 3 4 5 6 7 8 9 10 11 12 13"
echo $BANNER
# grep tree_controls clt_prob_enhancer.R | grep -v "#" | tail -18
# echo $BANNER
if [ $1 ]; then
    FILE="$1";
fi
echo $BANNER
for i in ${METRICS0}; do echo "$i: `grep \"${i}: \" ${FILE} | cut -f 1 -d'\\' | cut -f 2 -d':' | awk -f clt_avg.awk`"; done
echo $BANNER
for i in ${METRICS1}; do echo "$i: `grep \"${i}: \" ${FILE} | cut -f 1 -d'\\' | cut -f 2 -d':' | awk -f clt_avg.awk`"; done
echo $BANNER
for i in ${METRICS2}; do echo "$i: `grep \"${i}: \" ${FILE} | cut -f 1 -d'\\' | cut -f 2 -d':' | awk -f clt_avg.awk`"; done
echo $BANNER
grep MEAN $FILE | tail -n 1
grep STD  $FILE | tail -n 1
echo $BANNER
for i in $HOPTS; do echo "$i: `./clt_get_mixture_metrics.awk $i ${FILE}`"; done
echo $BANNER
