dirname=`ls -td LOGDIR* | head -1`
dirname=`echo ${dirname} | cut -d: -f 1`
echo "${dirname}"
if [ -z $dirname ]; then
    dirname="LOGDIR_`date +%g_%h_%d_%H_%M`";
    mkdir ${dirname};
    mkdir ${dirname}/previous;
    echo "${dirname}";
fi

logfile="output_clt.out"
if [ $1 ]; then
    logfile="$1";
fi
output="${dirname}/metrics"

./clt_training_metrics.awk ${logfile} > "${output}.training"
./clt_get_emsemble_metrics.sh ${logfile} > "${output}.cvfolds"
ls -lsth P*ata > ${dirname}/factor_models.created
mv clt*png ${dirname}/
mv plot*clt*png ${dirname}/
mv plot*clt*pdf ${dirname}/
mv B*RData ${dirname}/previous
cp P*RData ${dirname}/previous
cp clt_cvfolds_metrics*csv ${dirname}/ 
cp ${logfile} ${dirname}/

ls -lsht ${dirname}
echo
grep LOG_LOSS "${output}.training"
echo
echo ${dirname}

