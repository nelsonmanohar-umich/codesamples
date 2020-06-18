WHERE="/home/ubuntu/"
WHERE="/WORKSPACE/SRC/R/"
INTO="${WHERE}/output_clt_aws.out"
CMD="/usr/bin/R CMD BATCH ${INTO}"
(cd $WHERE; nohup ${CMD} 1>${WHERE}/nohup.out 2>/${WHERE}/nohup.err )&
watch tail -n 30 ${INTO}
