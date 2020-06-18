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
DATASET = "Shelter"
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
                             # 'ensemble',
                             'BasicAdaBoost', 
                             'BasicKNN', 
                             'BasicLinearSVC', 
                             'BasicRadial',
                             'BasicLDA', 
                             'BasicQDA', 
                             'BasicSGD', 
                             'BasicDecisionTree',
                             'BasicExtraTrees',
                             'BasicDecisionTree_01',
                             'BasicGradientBoost', 
                             'BasicRandomForest',
                             'BasicForest_01', 
                             'BasicForest_02', 
                             'BasicForest_03', 
                             'BasicForest_04', 
                             'BasicForest_05', 
                             'BasicForest_06', 
                             'BasicForest_07', 
                             'BasicForest_08',
                             'BasicForest_09', 
                             'BasicGradientBoost_01',
                             'BasicGradientBoost_02',
                             'BasicGradientBoost_03',
                             'BasicGradientBoost_04',
                             'BasicGradientBoost_05',
                             'BasicGradientBoost_06',
                             'BasicGradientBoost_07',
                             'BasicGradientBoost_08',
                             'BasicGradientBoost_09',
                             'ensemble_mean')
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
IDS = read.table('DATA/ids.csv', header=T, sep=',')
data = cbind(IDS, preds[,AVERAGE_WHICH_PREDICTORS])
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
if ( length(AVERAGE_WHICH_PREDICTORS) > 1 ) {
    preds["locally_computed_mean"] = apply(preds[,AVERAGE_WHICH_PREDICTORS], 1, mean)
} else {
    predictor = AVERAGE_WHICH_PREDICTORS[1]
    preds["locally_computed_mean"] = preds[,predictor]
}
# ######################################################################################################


# ######################################################################################################
data[, "target_bit"] = as.integer(preds[, "ensemble_mean"] >= THRESHOLD)
data[, TARGET] = preds[, "ensemble_mean"]
# ######################################################################################################


# ######################################################################################################
data[, "ensemble_mean"] = preds[,'ensemble_mean']
data[, "locally_computed_mean"] = preds[,'locally_computed_mean']
data[, "target_bit"] = as.integer(data[, PREDICT_BASED_ON] >= THRESHOLD)
data[, TARGET] = data[, PREDICT_BASED_ON]
print(table(data[,'locally_computed_mean'] >= THRESHOLD, data[,'ensemble_mean'] >= THRESHOLD))
# ######################################################################################################


# ######################################################################################################
print('--------------------------------------------------------------')
print(summary(data))
print('--------------------------------------------------------------')

print('--------------------------------------------------------------')
print(str(data))
print('--------------------------------------------------------------')

print('--------------------------------------------------------------')
print(data[1:100,])
print('--------------------------------------------------------------')
# ######################################################################################################


# ######################################################################################################
SUBMISSION = data[, c("ID", TARGET)]
write.table(SUBMISSION, file=FILENAME, quote=FALSE, row.names=FALSE, sep=",")
system(sprintf("/bin/gzip  %s", FILENAME))
# ######################################################################################################


# ######################################################################################################
print("PLOTTING HISTOGRAM")
DO_COMPARATIVE_HISTOGRAM()
print("DONE")
# ######################################################################################################

