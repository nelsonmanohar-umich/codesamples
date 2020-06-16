# ######################################################################################################
opts = options(width=120,digits=1, error = function() traceback(2))
LICENSETEXT = "Copyright (C) Nelson R. Manohar, comes with ABSOLUTELY NO WARRANTY."  
message( LICENSETEXT )
message("")
# ######################################################################################################


# ######################################################################################################
SOURCE = "Mon_09pm"
TARGET = "target"
TARGET = "Survived"
TARGET = "PredictedProb"
TARGET = "OutcomeType"
TARGET = "label"
DATASET = "Shelter"
DATASET = "Digits"
CLASSES = c('Adoption', 'Died', 'Euthanasia', 'Return_to_owner', 'Transfer')
CLASSES = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
SUBMISSION_NUMBER = sprintf("100_%s", DATASET)
# ######################################################################################################


# ######################################################################################################
PREDICTOR_OUTFILE = sprintf("yp_%s.csv", SOURCE)
FILENAME = sprintf("nrm_submission_%s_%s.csv", SUBMISSION_NUMBER, SOURCE)
print(sprintf("DATASET= %s", DATASET))
print(sprintf("SOURCE= %s", SOURCE))
print(sprintf("TARGET= %s", TARGET))
print(sprintf("INPUT= %s", PREDICTOR_OUTFILE))
print(sprintf("OUTPUT= %s", FILENAME))
# ######################################################################################################
PREDICT_BASED_ON = 'locally_computed_mean'
THRESHOLD = 0.50
# ######################################################################################################


# ######################################################################################################
DO_COMPARATIVE_HISTOGRAM = function() {
    graphics.off()
    pngfilename = sprintf("hist_%s.png", SUBMISSION_NUMBER)
    # M = as.integer(sqrt(length(AVERAGE_WHICH_PREDICTORS))+1)
    png( pngfilename, 1920, 1280)
        par(mfrow=c(4,3))
        for (predictor in AVERAGE_WHICH_PREDICTORS) {
            print ( predictor )
            hist(preds[,predictor], xlim=c(0,1), breaks=100) ; text( 0.5, 1000, predictor, cex=1.5, col='red')
            print ("---------------")
        }
    graphics.off()

    pngfilename = sprintf("hist_diffs_%s.png", SUBMISSION_NUMBER)
    png( pngfilename, 1920, 1280)
        par(mfrow=c(4,3))
        for (predictor in AVERAGE_WHICH_PREDICTORS) {
            print ( predictor )
            hist(preds[,"locally_computed_mean"] - preds[,predictor], xlim=c(0,1), breaks=100) ; text( 0.5, 1000, predictor, cex=1.5, col='red')
            print ("---------------")
        }
    graphics.off()
    print ("DONE")
    return()
}
# ######################################################################################################


# ######################################################################################################
AVERAGE_WHICH_PREDICTORS = c(
                'BasicGradientBoost_0',
                'BasicGradientBoost_1',
                'BasicGradientBoost_2',
                'BasicGradientBoost_3',
                'BasicGradientBoost_4',
                'BasicGradientBoost_5',
                'BasicGradientBoost_6',
                'BasicGradientBoost_7',
                'BasicGradientBoost_8',
                'BasicGradientBoost_9',
                'ensemble_mean_0',
                'ensemble_mean_1',
                'ensemble_mean_2',
                'ensemble_mean_3',
                'ensemble_mean_4',
                'ensemble_mean_5',
                'ensemble_mean_6',
                'ensemble_mean_7',
                'ensemble_mean_8',
                'ensemble_mean_9',
                'BasicDecisionTree_01_0',
                'BasicDecisionTree_01_1',
                'BasicDecisionTree_01_2',
                'BasicDecisionTree_01_3',
                'BasicDecisionTree_01_4',
                'BasicDecisionTree_01_5',
                'BasicDecisionTree_01_6',
                'BasicDecisionTree_01_7',
                'BasicDecisionTree_01_8',
                'BasicDecisionTree_01_9',
                'BasicRandomForest_0',
                'BasicRandomForest_1',
                'BasicRandomForest_2',
                'BasicRandomForest_3',
                'BasicRandomForest_4',
                'BasicRandomForest_5',
                'BasicRandomForest_6',
                'BasicRandomForest_7',
                'BasicRandomForest_8',
                'BasicRandomForest_9',
                'BasicGradientBoost_01_0',
                'BasicGradientBoost_01_1',
                'BasicGradientBoost_01_2',
                'BasicGradientBoost_01_3',
                'BasicGradientBoost_01_4',
                'BasicGradientBoost_01_5',
                'BasicGradientBoost_01_6',
                'BasicGradientBoost_01_7',
                'BasicGradientBoost_01_8',
                'BasicGradientBoost_01_9')
# ######################################################################################################


# ######################################################################################################
print ( '---------------------------------------------------------------' )
print ( AVERAGE_WHICH_PREDICTORS )
preds = read.table(PREDICTOR_OUTFILE, header=T, sep=',')
# ######################################################################################################
AVERAGE_WHICH_PREDICTORS = AVERAGE_WHICH_PREDICTORS[which(AVERAGE_WHICH_PREDICTORS %in% colnames(preds))]
print ( '---------------------------------------------------------------' )
print ( AVERAGE_WHICH_PREDICTORS )
print ( '---------------------------------------------------------------' )
# ######################################################################################################
# IDS = read.table('DATA/ids.csv', header=T, sep=',')
# data = cbind(IDS, preds[,AVERAGE_WHICH_PREDICTORS])
data = preds[,AVERAGE_WHICH_PREDICTORS]
# ######################################################################################################


# ######################################################################################################
INFO = list(PREDICT_BASED_ON=PREDICT_BASED_ON,
            SUBMISSION_NUMBER=SUBMISSION_NUMBER,
            THRESHOLD=THRESHOLD,
            SOURCE=SOURCE,
            PREDICTOR_OUTFILE=PREDICTOR_OUTFILE,
            CSV_FILENAME=FILENAME,
            AVERAGE_WHICH_PREDICTORS=AVERAGE_WHICH_PREDICTORS)
write.table(INFO, file=sprintf("nrm_submission_%s.dat", SUBMISSION_NUMBER))
# ######################################################################################################


# ######################################################################################################
print('---------------------------------------------------------------------------')
print(summary(preds))
print('---------------------------------------------------------------------------')
# ######################################################################################################


# ######################################################################################################
for (pclass in CLASSES) {
    print('---------------------------------------------------------------------------')
    print(pclass)
    which_classifiers = AVERAGE_WHICH_PREDICTORS[grep(pclass, AVERAGE_WHICH_PREDICTORS)]
    print(which_classifiers)
    colname = sprintf("locally_computed_mean_%s", pclass)
    predictor = which_classifiers[1]
    ensemble_mean = sprintf("ensemble_mean_%s", pclass)
    data[ensemble_mean] = preds[,predictor]
    if ( length(which_classifiers) > 1 ) {
        data[colname] = apply(preds[,which_classifiers], 1, mean)
    } else {
        data[colname] = preds[,predictor]
    }
}
# ######################################################################################################


# ######################################################################################################
for (i in 1:length(CLASSES)) {
    pclass = CLASSES[i]
    local_predictor_mean = sprintf("locally_computed_mean_%s", pclass)
    colname = sprintf("target_bit_%s", CLASSES[i])
    print('--------------------------------------------------------------')
    print(pclass)
    print(colname)
    print('--------------------------------------------------------------')
    which_classifiers = AVERAGE_WHICH_PREDICTORS[grep(pclass, AVERAGE_WHICH_PREDICTORS)]
    predictor = which_classifiers[1]
    ensemble_mean = sprintf("ensemble_mean_%s", pclass)
    print(which_classifiers)
    print(predictor)
    print('--------------------------------------------------------------')
    data[, pclass] = data[, local_predictor_mean]
    data[, colname] = as.integer(data[, local_predictor_mean] >= THRESHOLD)
    print(table(data[,pclass] >= THRESHOLD, data[,predictor] >= THRESHOLD))
    print('--------------------------------------------------------------')
}
# ######################################################################################################


# ######################################################################################################
# ######################################################################################################
print('--------------------------------------------------------------')
print(summary(preds))
print('--------------------------------------------------------------')

print('--------------------------------------------------------------')
print(summary(data))
print('--------------------------------------------------------------')

print('--------------------------------------------------------------')
print(str(data))
print('--------------------------------------------------------------')

print('--------------------------------------------------------------')
print(data[1:10,])
print('--------------------------------------------------------------')
# ######################################################################################################


# ######################################################################################################
SUBMISSION = data[, CLASSES]
write.table(SUBMISSION, file=FILENAME, quote=FALSE, row.names=FALSE, sep=",")
system(sprintf("/bin/gzip  %s", FILENAME))
# ######################################################################################################


# ######################################################################################################
print("DONE")
# ######################################################################################################

