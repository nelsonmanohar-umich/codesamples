## Kaggle clickthrough code ##
implements an emsemble classifier over 9 models which can be 
trained over arbitrary variants of the data, if so desired.

the modeling is general enough to allow customization to other
factor driven datasets and make reuse of the emsemble classifier.

it also implements a probability enhancer model, which allows
modeling the emsemble outputs

### -rwxr-xr-x 1 nrm nrm  18K Feb 24 20:00 clt.R ###

R> source('clt.R') 
runs the main entry point, from the  R prompt, type source('clt.R') assuming that the input files have been produced.  training files been chunked and placed on TRAINING_FILES

### -rwxr-xr-x 1 nrm nrm 8.4K Feb 24 18:50 clt_testcoding.R ###
applies encoding transformation to test input data in a manner that matches the training data transformations (precondition: test data set is loaded and train data is available)

### -rwxr-xr-x 1 nrm nrm 6.3K Feb 24 18:50 clt_test.R ###
runs the test driver, (precondition: after the models are trained)

### -rwxr-xr-x 1 nrm nrm  17K Feb 24 18:50 clt_traincoding.R ###
applies encoding transformations to train data

### -rwxr-xr-x 1 nrm nrm  22K Feb 24 18:50 clt_train.R ###
runs the model training driver

### -rwxr-xr-x 1 nrm nrm 6.0K Feb 24 18:50 clt_probs.R ###
computes probabilities for some features

### -rwxr-xr-x 1 nrm nrm  28K Feb 24 18:50 clt_prob_enhancer.R ###
implements the various models of the emsemble

### -rwxr-xr-x 1 nrm nrm 1.9K Feb 24 18:50 clt_intro.R ###
startup used to load the necessary dependency files 

### -rwxr-xr-x 1 nrm nrm 8.2K Feb 24 18:50 clt_nb_formula.R ###
naive bayes model (deprecated)

### -rwxr-xr-x 1 nrm nrm  16K Feb 24 18:50 clt_probability_predictor.R ###
implements a model atop probability predictions

### -rwxr-xr-x 1 nrm nrm 7.9K Feb 24 18:50 clt_cv.R ###
cross validation

### -rwxr-xr-x 1 nrm nrm 3.7K Feb 24 18:50 clt_dataloader.R ###
loads the training data

### -rwxr-xr-x 1 nrm nrm  68K Feb 24 18:50 clt_columns.R ###
defines which columns are activated for each model of the emsemble

### -rwxr-xr-x 1 nrm nrm 2.0K Feb 24 18:50 clt_activated_cols.R ###
defines which features need to be kept around during the analysis so as to drop all found to be irrelevat features

### -rwxr-xr-x 1 nrm nrm  19K Feb 24 18:50 clt_basic_functions.R ###
common functions such as recoding of factors, etc used by all modules of the system

### -rwxr-xr-x 1 nrm nrm  318 Feb 24 18:49 clt_avg.awk ###
compute online statistics from the logs produced by clt.R

### -rwxr-xr-x 1 nrm nrm  575 Feb 24 18:49 clt_file_splitter.py ###
generates training chunks which are to be placed on directory TRAIN when clt_cv is invoked to perform cross validation of against the entire training set

### -rwxr-xr-x 1 nrm nrm  923 Feb 24 18:49 clt_get_emsemble_metrics.sh ###
awk generates online statistics (as the clt.R runs) by scanning the log file and generating avg, mean, max log-loss estimates for each member of the emsemble classifier, e.g., watch clt_get_emsemble_metrics.sh output_clt.out

### -rwxr-xr-x 1 nrm nrm 1.1K Feb 24 18:49 clt_get_mixture_metrics.awk ###
used by the above to get additional metrics from the logs

### -rwxr-xr-x 1 nrm nrm 1.8K Feb 24 18:49 clt_id_analyzer.py ###
used to parse the train data fields and extract dictionary counts, frequencies and probabilities for the various factor levels of a given column/feature

### -rwxr-xr-x 1 nrm nrm 2.6K Feb 24 18:49 clt_id_frequency_extractor.py ###
used to parse the train data fields and extract dictionary counts, frequencies and probabilities for the various factor levels of a given column/feature

### -rwxr-xr-x 1 nrm nrm 2.3K Feb 24 18:49 clt_id_probability_builder.py ###
used to parse the train data fields and extract dictionary counts, frequencies and probabilities for the various factor levels of a given column/feature

### -rwxr-xr-x 1 nrm nrm 3.3K Feb 24 18:49 clt_averaging.py ###
used to parse the train data fields and extract dictionary counts, frequencies and probabilities for the various factor levels of a given column/feature

### -rwxr-xr-x 1 nrm nrm  632 Feb 24 18:49 clt_set_subselector.py ###
subsampler by feature activations

### -rwxr-xr-x 1 nrm nrm 2.3K Feb 24 18:49 clt_subsampler_dayofweek.py ###
subsampler by day of week, the files generated here for various levels of subsampling go into the directory TRAINING_FILES

### -rwxr-xr-x 1 nrm nrm  13K Feb 24 18:49 clt_chunk_extractor.py ###
subsampler for the test data

### -rwxr-xr-x 1 nrm nrm  35K Feb 24 18:49 utilities.R ###
general utilties

### -rwxr-xr-x 1 nrm nrm 3.1K Feb 24 18:49 intro.R ###
general intro setup

### -rwxr-xr-x 1 nrm nrm  27K Feb 24 18:49 fselect.R ###
deprecated

### -rwxr-xr-x 1 nrm nrm  17K Feb 24 18:49 decision_trees.R ###
used to implement some decision tree models, deprecated

### -rwxr-xr-x 1 nrm nrm  30K Feb 24 18:49 datasets.R ###
deprecated, w fselect.R

### -rwxr-xr-x 1 nrm nrm 4.9K Feb 24 18:50 clt_cv_bootstrap.R ###
(deprecated, incorrect bootstrap)

### datafiles ###
#### -rwxr-xr-x 1 nrm nrm  31K Feb 24 18:49 clt_site_ids.csv ####
#### -rwxr-xr-x 1 nrm nrm 115K Feb 24 18:49 clt_site_id.csv ####
#### -rwxr-xr-x 1 nrm nrm  808 Feb 24 18:49 clt_site_id_cat.csv ####
#### -rwxr-xr-x 1 nrm nrm  19K Feb 24 18:49 clt_probs_site_cat.csv ####
#### -rwxr-xr-x 1 nrm nrm  23K Feb 24 18:49 clt_probs_app_cat.csv ####
#### -rwxr-xr-x 1 nrm nrm  87K Feb 24 18:49 clt_app_id.csv ####
#### -rwxr-xr-x 1 nrm nrm  646 Feb 24 18:49 clt_app_id_cat.csv ####
#### -rwxr-xr-x 1 nrm nrm  214 Feb 24 18:49 clt_aws_rdriver.sh ####

### -rwxr-xr-x 1 nrm nrm  155 Feb 24 18:49 clt_clean.sh ###
used to assemble kaggle submissions

### -rwxr-xr-x 1 nrm nrm  354 Feb 24 18:49 clt_linecleaner.py ###
used to assemble kaggle submissions

### -rwxr-xr-x 1 nrm nrm  794 Feb 24 18:49 clt_lineordering.py ###
used to assemble kaggle submissions

### -rwxr-xr-x 1 nrm nrm 1.7K Feb 24 18:49 copyrigth.R ###
copyright

KAGGLE/CLICKTHRU : place train and test therein
TRAIN: create directory and then run scripts above to generate sample files from train dataset
CLT_TESTING: create directory and then run scripts above to chunk test dataset into chunks
CLT_TRAINING: create directory and tehn run script above to chunk train dataset into chunks
