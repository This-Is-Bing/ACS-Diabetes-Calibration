# ---------------------------------------------------------------------------#
####                        File Description & Setup                      ####
#----------------------------------------------------------------------------#
# Please run the rproj file in the folder                                     
# Dataset: Diabetes ACS (STEMI)                                     
# Description: To calibrate model for STEMI

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
####                      Loading and Transformation                      ####
#----------------------------------------------------------------------------#

##### Loading Datset ####
calibration_ds <- read.csv('./dataset/processed/calibration_ds.csv', header = TRUE)

##### Get STEMI Only ####
calibration_ds <- calibration_ds[calibration_ds$acsstratum ==1, ]

##### Get all Features ####
all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')

##### Structuring each feature ####
calibration_ds <- RF20_TransformationFunction(calibration_ds, all_features)

##### Removing Unecessary Columns ####
cols_to_remove <- c('acsstratum', 'timiscorestemi', 'timiscorenstemi')
calibration_ds <- calibration_ds[, -which(names(calibration_ds) %in% cols_to_remove)]

##### Separating features and label ####
X_calibration_ds <- calibration_ds[,-which(names(calibration_ds) == 'ptoutcome')]

y_calibration_ds <- calibration_ds[,which(names(calibration_ds) == 'ptoutcome')]

# ---------------------------------------------------------------------------#
####                      Predicting Probailities                         ####
#----------------------------------------------------------------------------#

##### Load Model ####
model <- readRDS('./models/3_modelAll_rf20_dm.rds')$RF

##### Predicting for calibration and validation dataset ####
pred_prob_calib <- predict(model, X_calibration_ds, type= 'prob')$Death

# ---------------------------------------------------------------------------#
####                 Performance Evaluation Before                        ####
#----------------------------------------------------------------------------#

##### Evaluate Performance ####
raw_threshold <- SearchBestThreshold(y_calibration_ds,pred_prob_calib)
stemi_atcual_raw_result <- Evaluation(y_calibration_ds, pred_prob_calib, raw_threshold,rowname = 'STEMI_Atcual_Raw')


# ---------------------------------------------------------------------------#
####                           Calibration                                ####
#----------------------------------------------------------------------------#

##### Platt Scalling ####
# -1 to make the labels to be in 0,1
res <- prCalibrate(r.calib = as.numeric(y_calibration_ds)-1, 
                   p.calib = pred_prob_calib,
                   # r.valid = as.numeric(y_validation_ds)-1,
                   # p.valid = pred_prob_valid, 
                   nbins=10)

##### Getting Metrics Scores ####
raw_logloss = res$raw.logloss
cal_logloss = res$cal.logloss

raw_acc = sum((res$raw.probs > 0.5) == res$responses)/length(res$responses)
cal_acc = sum((res$cal.probs > 0.5) == res$responses)/length(res$responses)

raw_auc = roc(res$responses, res$raw.probs, auc = TRUE)
cal_auc = roc(res$responses, res$cal.probs, auc = TRUE)

calibrated_result <- data.frame(raw_logloss,cal_logloss,raw_auc$auc,cal_auc$auc,raw_acc, cal_acc, row.names = 'calibrated')

#### Get Probabilities ####
calibrated_prob <- res$cal.probs

##### Performance After Calibrating ####
calibrated_threshold <- SearchBestThreshold(y_calibration_ds, calibrated_prob)
stemi_atcual_calibrated_result <- Evaluation(y_calibration_ds, calibrated_prob, calibrated_threshold,rowname = 'STEMI_Atcual_Calibrated')

# ---------------------------------------------------------------------------#
####                          Exporting Result                            ####
#----------------------------------------------------------------------------# 
##### Exporting Result ####
final_result <- rbind(stemi_atcual_raw_result,stemi_atcual_calibrated_result)
# write.csv(final_result, "./results/Stemi_Calibration_Valid_F1.csv")

##### Exporting Model ####
acs_calibrated_model <- res$cal.model
# saveRDS(acs_calibrated_model, file = "./results/calibrated_models/stemi_calibrated_model.rds")



