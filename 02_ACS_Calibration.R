# ---------------------------------------------------------------------------#
####                        File Description & Setup                      ####
#----------------------------------------------------------------------------#
# Please run the rproj file in the folder
# Dataset: Diabetes ACS (STEMI + NSTEMI) (Without TIMI Score)
# This script is to compare the raw performance and calibrated performance
  
##### Download eRic packages ####
# UNCOMMENT TO INSTALL FOR THE FIRST TIME
# install.packages("devtools")
# library("devtools")
# install_github("etlundquist/eRic")

##### Cleaning environment ####
rm(list=ls())

##### Loading Library ####
set.seed(2468)
source('./00_Functions.R')
library(readxl)
library(caret)
library(eRic)
library(ResourceSelection)
library(rms)
library(pROC)


# ---------------------------------------------------------------------------#
####                       Loading and Transformation                     ####
#----------------------------------------------------------------------------#  

##### Loading Dataset ####
calibration_ds <- read.csv('./dataset/processed/calibration_ds.csv', header = TRUE)
validation_ds <- read.csv('./dataset/processed/validation_ds.csv', header = TRUE)
  
##### Get all Features #####
all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')
  
##### Transform each feature #####
calibration_ds <- RF20_TransformationFunction(calibration_ds, all_features)
validation_ds <- RF20_TransformationFunction(validation_ds, all_features)

##### Removing Uneccessary Column ####
cols_to_remove <- c('acsstratum', 'timiscorestemi', 'timiscorenstemi')
calibration_ds <- calibration_ds[, -which(names(calibration_ds) %in% cols_to_remove)]
validation_ds <- validation_ds[, -which(names(validation_ds) %in% cols_to_remove)]
  
##### Separating features and label ####
X_calibration_ds <- calibration_ds[,-which(names(calibration_ds) == 'ptoutcome')]
X_validation_ds <- validation_ds[,-which(names(validation_ds) == 'ptoutcome')]
  
y_calibration_ds <- calibration_ds[,which(names(calibration_ds) == 'ptoutcome')]
y_validation_ds <- validation_ds[,which(names(validation_ds) == 'ptoutcome')]


# ---------------------------------------------------------------------------#
####                       Predicting Probabilities                       ####
#----------------------------------------------------------------------------#  

##### Load Model ####
model <- readRDS('./models/3_modelAll_rf20_dm.rds')$RF

##### Probailities ####  
pred_prob_calib <- predict(model, X_calibration_ds, type= 'prob')$Death
pred_prob_valid <- predict(model, X_validation_ds, type= 'prob')$Death
  

# ---------------------------------------------------------------------------#
####              Model Evaluation Before Calibration                     ####
#----------------------------------------------------------------------------#  

# Using Best F1 Score for Threshold
raw_threshold <- SearchBestThreshold(y_validation_ds, pred_prob_valid)

##### Evaluate model ####
acs_raw_result <- Evaluation(y_validation_ds, pred_prob_valid, raw_threshold, rowname = 'ACS_Raw')

  
# ---------------------------------------------------------------------------#
####                           Calibration                                ####
#----------------------------------------------------------------------------#

##### Platt Scalling ####
# -1 to make the labels to be in 0,1
res <- prCalibrate(r.calib = as.numeric(y_calibration_ds)-1, 
                    p.calib = pred_prob_calib,
                    r.valid = as.numeric(y_validation_ds)-1,
                    p.valid = pred_prob_valid, 
                    nbins=10)

##### Getting all evaluation scores  ####
raw_logloss = res$raw.logloss
cal_logloss = res$cal.logloss
  
raw_acc = sum((res$raw.probs > 0.5) == res$responses)/length(res$responses)
cal_acc = sum((res$cal.probs > 0.5) == res$responses)/length(res$responses)
  
raw_auc = roc(res$responses, res$raw.probs, auc = TRUE)
cal_auc = roc(res$responses, res$cal.probs, auc = TRUE)

calibrated_result <- data.frame(raw_logloss,cal_logloss,raw_auc$auc,cal_auc$auc,raw_acc, cal_acc, row.names = 'calibrated')

##### Getting calibrated probabilities and model ####
calibrated_prob <- res$cal.probs
calibrated_model<- res$cal.model

  
# ---------------------------------------------------------------------------#
####                Model Evaluation After Calibration                    ####
#----------------------------------------------------------------------------# 

# Using Best F1 Score for Threshold
calib_threshold <- SearchBestThreshold(y_validation_ds, calibrated_prob)

##### Evaluate Model ####
acs_calibrated_result <- Evaluation(y_validation_ds, calibrated_prob, calib_threshold, rowname = 'ACS_Calibrated')

# ---------------------------------------------------------------------------#
####                          Exporting Result                            ####
#----------------------------------------------------------------------------# 
##### Exporting Result ####
final_result <- rbind(acs_raw_result,acs_calibrated_result)
# write.csv(final_result, "./results/ACS_Calibration_Valid_F1.csv")

##### Exporting Model ####
acs_calibrated_model <- res$cal.model
#saveRDS(calibrated_model, file = "./results/calibrated_models/acs_calibrated_model.rds")



#iso

calibratedProbs1<-rfUtilities::probability.calibration(y_calibration_ds, pred_prob_calib, regularization = T)

calibratedProbs2<-CORElearn::calibrate(y_calibration_ds, pred_prob_calib, class1=1, 
          method = c("isoReg"), noBins=10, assumeProbabilities=T)
applyCalibration(y_calibration_ds, calibratedProbs2)

threshold <-SearchBestThreshold(y_calibration_ds, pred_prob_calib)  
a<-Evaluation(y_calibration_ds, calibratedProbs1,rowname = 'raw', threshold = threshold)
threshold <-SearchBestThreshold(y_calibration_ds, calibratedProbs1)  
b<-Evaluation(y_calibration_ds, calibratedProbs1,rowname = 'iso', threshold = threshold)

a
b

CORElearn::reliabilityPlot(pred_prob_calib, calibratedProbs2, titleText="", boxing="equipotent", 
                noBins=10, classValue = 1, printWeight=FALSE)

# Plot calibrated against original probability estimate
plot(density(pred_prob_calib), col="red", xlim=c(0,1), ylim=c(0,20), ylab="Density", xlab="probabilities",
     main="Calibrated probabilities" )
lines(density(calibratedProbs1), col="blue")
legend("topright", legend=c("original","calibrated"), 
       lty = c(1,1), col=c("red","blue"))



