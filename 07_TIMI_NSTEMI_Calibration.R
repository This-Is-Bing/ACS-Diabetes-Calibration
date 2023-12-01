# ---------------------------------------------------------------------------#
####                        File Description & Setup                      ####
#----------------------------------------------------------------------------#
# Please run the rproj file in the folder                                     
# Dataset: Diabetes ACS (TIMI NSTEMI)                                     
# Description: To calibrate model for TIMI NSTEMI

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
validation_ds <- read.csv('./dataset/processed/validation_ds.csv', header = TRUE)

##### Get STEMI Only ####
calibration_ds <- calibration_ds[!calibration_ds$acsstratum ==1, ]
validation_ds <- validation_ds[!validation_ds$acsstratum ==1, ]

##### Get all Features ####
all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')

##### Removing Uneccessary Columns ####
cols_to_remove <- c('acsstratum', 'ptoutcome', 'timiscorestemi')
calibration_ds <- calibration_ds[, -which(names(calibration_ds) %in% cols_to_remove)]
validation_ds <- validation_ds[, -which(names(validation_ds) %in% cols_to_remove)]

##### Classifying outcome with timiscore ####
calibration_ds$timiscorenstemi <- ifelse(calibration_ds$timiscorenstemi >=5, 1, 0)
validation_ds$timiscorenstemi <- ifelse(validation_ds$timiscorenstemi >=5, 1, 0)

##### Changing the outcome column name to make the code reusable ####
calibration_ds$ptoutcome <- calibration_ds$timiscorenstemi
calibration_ds$timiscorenstemi<- NULL

validation_ds$ptoutcome <- validation_ds$timiscorenstemi
validation_ds$timiscorenstemi<- NULL

##### Structuring each feature ####
calibration_ds <- RF20_TransformationFunction(calibration_ds, all_features)
validation_ds <- RF20_TransformationFunction(validation_ds, all_features)

##### Separating features and label ####
X_calibration_ds <- calibration_ds[,-which(names(calibration_ds) == 'ptoutcome')]
X_validation_ds <- validation_ds[,-which(names(validation_ds) == 'ptoutcome')]

y_calibration_ds <- calibration_ds[,which(names(calibration_ds) == 'ptoutcome')]
y_validation_ds <- validation_ds[,which(names(validation_ds) == 'ptoutcome')]


# ---------------------------------------------------------------------------#
####                      Predicting Probailities                         ####
#----------------------------------------------------------------------------#

##### Load Model ####
model <- readRDS('./models/3_modelAll_rf20_dm.rds')$RF

##### Predicting for calibration and validation dataset ####
pred_prob_calib <- predict(model, X_calibration_ds, type= 'prob')$Death
pred_prob_valid <- predict(model, X_validation_ds, type= 'prob')$Death


# ---------------------------------------------------------------------------#
####                 Performance Evaluation Before                        ####
#----------------------------------------------------------------------------#

##### Evaluate Performance ####
raw_threshold <- SearchBestThreshold(y_validation_ds,pred_prob_valid)
timi_nstemi_atcual_raw_result <- Evaluation(y_validation_ds, pred_prob_valid, raw_threshold,rowname = 'TIMI_NSTEMI_Atcual_Raw')


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
calibrated_threshold <- SearchBestThreshold(y_validation_ds, calibrated_prob)
timi_nstemi_atcual_calibrated_result <- Evaluation(y_validation_ds, calibrated_prob, calibrated_threshold,rowname = 'TIMI_NSTEMI_Atcual_Calibrated')

# ---------------------------------------------------------------------------#
####                          Exporting Result                            ####
#----------------------------------------------------------------------------# 
##### Exporting Result ####
final_result <- rbind(timi_nstemi_atcual_raw_result,timi_nstemi_atcual_calibrated_result)
# write.csv(final_result, "./results/Timi_Nstemi_Calibration_Valid_F1.csv")

##### Exporting Model ####
acs_calibrated_model <- res$cal.model
# saveRDS(acs_calibrated_model, file = "./results/calibrated_models/timi_nstemi_calibrated_model.rds")



