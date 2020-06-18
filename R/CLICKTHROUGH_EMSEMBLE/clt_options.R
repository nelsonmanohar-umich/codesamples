# this module needs quite a bit of refinement and it is on its infancy.
# ##############################################################
CLT_OPTIONS = list()
# ##############################################################


# ##############################################################
# WHERE THE SUBSAMPLED DATASET PROJECTIONS ARE:  
# (just collections of subsampled chunks of the train dataset)
# ##############################################################
BALANCED_TRAINSET_9        = 'TRAINING_FILES/training_set9.csv'
BURST_ORIGINAL_TRAINSET_2  = 'TRAINING_FILES/subsampled_train_set_2.csv'
BURST_ORIGINAL_TRAINSET_10 = 'TRAINING_FILES/subsampled_train_set_10.csv'
BURST_BAD_PERFORMANCE_X    = 'TRAINING_FILES/bad_performance.csv'
AUGMENTATION_DATASETS      = 'TRAINING_FILES/augmentation_models_ready.csv'
# ##############################################################


# ##############################################################
# (just collections of subsampled chunks of the train dataset of increasing size)
# ##############################################################
PREFIX0="TRAINING_FILES/"
PREFIX1="TRAINING_FILES/t-"
PREFIX2="TRAINING_FILES/t2-"
PREFIX3="TRAINING_FILES/t3-"
PREFIX4="TRAINING_FILES/t4-"
PREFIX5="TRAINING_FILES/t5-"
PREFIX6="TRAINING_FILES/t6-"
PREFIX7="TRAINING_FILES/t7-"
PREFIX8="TRAINING_FILES/t8-"
PREFIX9="TRAINING_FILES/t9-"
PREFIX10="TRAINING_FILES/t0-"
PREFIXA="TRAINING_FILES/ta-"
# ##############################################################


# ##############################################################
# the entry point main dataset (size limited by R)
# ##############################################################
CLT_OPTIONS[['primary balanced dataset']]             = AUGMENTATION_DATASETS
NROWS                                                 = 50000              # load up to this number of rows only
# --------------------------------------------------------------
DO_FAST_TEST                                          = TRUE                # load only a portion of the dataset
FRACTION_TO_USE                                       = 0.1                 # subsample the rows above by these and return only this proportion
# ##############################################################


# ##############################################################
# optional add on datasets
# ##############################################################
CLT_OPTIONS[['secondary temporal dataset']]           = BALANCED_TRAINSET_9 
CLT_OPTIONS[['load small scale balanced set - 2']]    = FALSE               # load an add-on subsample dataset 
NROWS_2                                               = 400000              # how many rows from this other dataset
# --------------------------------------------------------------
CLT_OPTIONS[['tertiary temporal dataset']]            = BURST_ORIGINAL_TRAINSET_10
CLT_OPTIONS[['load large scale balanced set - 10']]   = FALSE               # another add on dataset
NROWS_10                                              = 1000000             # 1/500 of 100K longitudinal samples from balanced set
# --------------------------------------------------------------
CLT_OPTIONS[['bad temporal performance dataset']]     = BURST_BAD_PERFORMANCE_X    
CLT_OPTIONS[['load data chunks w/ known bad perf']]   = TRUE                # load another add on dataset, this comprises known slices with bad log loss 
NROWS_B                                               = 100                 # they are between 15k and 26k #NROWS_W = 10000; NROWS_W = 20000
# ##############################################################


# ##############################################################
# optional add on datasets ( day to day slices )
# ##############################################################
CLT_OPTIONS[['dataset scale prefix']]                 = PREFIX3             # for day by day data
CLT_OPTIONS[['load subsampled by day dataset']]       = TRUE                # load them
NROWS_W                                               = 9900000             # maximum size to load from each day slice
# --------------------------------------------------------------
CLT_OPTIONS[['dataset scale 2nd prefix']]             = PREFIX1             # a secondary augmentation instance of the above
CLT_OPTIONS[['load secondary dataset']]               = FALSE
# ##############################################################


# ##############################################################
CLT_OPTIONS[['workspace directory']]                  = "~/WORKSPACE/src/R/"
CLT_OPTIONS[['output logfile']]                       = "output_clt.out"
CLT_OPTIONS[['precomputed_aws_image']]                = "T4.RData"
# --------------------------------------------------------------
CLT_OPTIONS[['do options logging']]                   = TRUE
CLT_OPTIONS[['do state checkpointing']]               = TRUE
CLT_OPTIONS[['running on aws']]                       = FALSE               # currently used just to take snapshots not for remote processsing
# ##############################################################


# ##############################################################
CLT_OPTIONS[['use_ctypes_instead_of_htypes']]         = FALSE               # use continuous or interval cut representations for numericals
CLT_OPTIONS[['do model training']]                    = TRUE                # build models, requires feature selection
CLT_OPTIONS[['do feature selection']]                 = TRUE                # do feature selection exploration
CLT_OPTIONS[['do analysis cross validation dataset']] = TRUE
# ##############################################################


# ##############################################################
CLT_OPTIONS[['use subsumed models']]                  = TRUE
CLT_OPTIONS[['use individualized predictor']]         = TRUE
CLT_OPTIONS[['model_subsuming_definitions_file']]     = "clt_model_subsuming.R"
CLT_OPTIONS[['model_column_definitions_file']]        = "clt_columns.R"
# --------------------------------------------------------------
CLT_OPTIONS[['use_incremental_probability_builder']]  = FALSE
CLT_OPTIONS[['use_independent_probability_generator']]= !CLT_OPTIONS$'use_incremental_probability_builder'
# ##############################################################


# ##############################################################
CLT_OPTIONS[['aws_bucketkey']]                        = "nelsonmanohar-machlearn-clt-3141519"
# --------------------------------------------------------------
CLT_OPTIONS[['do_rje_processing']]                    = FALSE               # should attempt to build models via AWS/S3/EC2?
CLT_OPTIONS[['prefetch_remote_precomputed_models']]   = FALSE               # were models precomputed at AWS and stored at S3?
CLT_OPTIONS[['rebuild_models_already_existing']]      = FALSE               # if model exist locally, should be rebuilt?
# ##############################################################


# ##############################################################
CLT_OPTIONS[['max number of samples per main model']] = 7500000
CLT_OPTIONS[['max number of samples per cust. model']]=  500000
# --------------------------------------------------------------
CLT_OPTIONS[['min_subset_size_to_predict_on']]        = 63
CLT_OPTIONS[['min_subset_size_to_train_on']]          = 511
CLT_OPTIONS[['exploration_depths_for_optimization']]  = c(7,14,21,28)
CLT_OPTIONS[['num_crossval_folds_during_training']]   = 5
CLT_OPTIONS[['factorlevel_training_percentage_split']]= 0.720               # split between training and model-cv during training wrt training data
CLT_OPTIONS[['max_acceptable_training_logloss']]      = 0.409               # maximum tolerable log loss goal for customized models during training
CLT_OPTIONS[['logloss_threshold_to_abort_optimizing']]= 0.459               # logloss at which customized model exploration need not be continued
# ##############################################################


# ##############################################################
CLT_OPTIONS[['model configuration']]                  = "CUSTOMIZED"
CLT_OPTIONS[['use which model as default model']]     = 9                  # corresponding PROB_CORRECTION_MODEL$predictor BELOW must be set to ''
# --------------------------------------------------------------
if ( CLT_OPTIONS$'model configuration' == "GENERAL" ) {
    CLT_OPTIONS[['predictors']]                       = c('',               # PROB_CORRECTION_MODEL_1
                                                          '',               # PROB_CORRECTION_MODEL_2 
                                                          '',               # PROB_CORRECTION_MODEL_3
                                                          '',               # PROB_CORRECTION_MODEL_4
                                                          '',               # PROB_CORRECTION_MODEL_5
                                                          '',               # PROB_CORRECTION_MODEL_6
                                                          '',               # PROB_CORRECTION_MODEL_7
                                                          '',               # PROB_CORRECTION_MODEL_8
                                                          ''                # PROB_CORRECTION_MODEL_9
                                                          )
}
# --------------------------------------------------------------
if ( CLT_OPTIONS$'model configuration' == "CUSTOMIZED" ) {
    CLT_OPTIONS[['predictors']]                       = c(
                                                          'device_ip',      # PROB_CORRECTION_MODEL_1
                                                          'device_id',      # PROB_CORRECTION_MODEL_2
                                                          'device_model',   # PROB_CORRECTION_MODEL_8
                                                          'site_category',  # PROB_CORRECTION_MODEL_3
                                                          'site_domain',    # PROB_CORRECTION_MODEL_4
                                                          'app_domain',     # PROB_CORRECTION_MODEL_5
                                                          'H15',            # PROB_CORRECTION_MODEL_6
                                                          'H19',            # PROB_CORRECTION_MODEL_7
                                                          ''                # PROB_CORRECTION_MODEL_9
                                                          )
}
# --------------------------------------------------------------
if ( CLT_OPTIONS$'model configuration' == "CUSTOMIZED" ) {
    CLT_OPTIONS[['predictors']]                       = c(
                                                          'banner_pos',     # PROB_CORRECTION_MODEL_4
                                                          'device_id',      # PROB_CORRECTION_MODEL_2
                                                          'site_category',  # PROB_CORRECTION_MODEL_3
                                                          'device_type',    # PROB_CORRECTION_MODEL_1
                                                          'device_ip',      # PROB_CORRECTION_MODEL_7
                                                          'site_id',        # PROB_CORRECTION_MODEL_5
                                                          'shift',          # PROB_CORRECTION_MODEL_8
                                                          '',               # PROB_CORRECTION_MODEL_6
                                                          ''                # PROB_CORRECTION_MODEL_9
                                                          )
}
# ##############################################################


# ##############################################################
CLT_OPTIONS[['print_rje_computed_model']]             = FALSE               # display detailed feedback about remote model building
CLT_OPTIONS[['verify_rje_model_building']]            = FALSE               # verify validity of model for subsequent use (deprecated)
CLT_OPTIONS[['display_intermediary_trees']]           = FALSE
# ##############################################################


# ##############################################################
CLT_OPTIONS[['do visualization 1']]                   = FALSE
CLT_OPTIONS[['do visualization 2']]                   = FALSE
CLT_OPTIONS[['do visualization 3']]                   = FALSE
# ----------------------------------------------------------
CLT_OPTIONS[['do analysis missing cases']]            = FALSE
CLT_OPTIONS[['do exploration interaction terms']]     = FALSE
CLT_OPTIONS[['do exploration threshold values']]      = FALSE
# ----------------------------------------------------------
CLT_OPTIONS[['do release training dataset']]          = FALSE
CLT_OPTIONS[['do bootstrap analysis']]                = FALSE
CLT_OPTIONS[['bootstrapper']]                         = "DT"
# ----------------------------------------------------------
CLT_OPTIONS[['do grid search']]                       = FALSE
CLT_OPTIONS[['do incremental modeling on rank-fselect']] = FALSE
CLT_OPTIONS[['do incremental modeling on dt-fselect']]   = FALSE
# ----------------------------------------------------------
CLT_OPTIONS[['do analysis xval balanced dataset']]    = FALSE
CLT_OPTIONS[['do analysis test submission dataset']]  = FALSE
# ##############################################################


