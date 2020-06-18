dirname="LOGDIR_`date +%g_%h_%d_%H_%M`"
mkdir ${dirname}
mkdir ${dirname}/previous
cp -p clt_options.R clt_prob_enhancer.R clt_columns.R clt_traincoding.R clt_model_subsuming.R ${dirname}
mv P[1-9]*.RData ${dirname}/previous/
echo "${dirname}"
