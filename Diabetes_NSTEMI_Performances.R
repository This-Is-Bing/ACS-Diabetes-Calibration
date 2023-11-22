#### File Description & Setup####
# Please run the rproj file in the folder
# Dataset: Diabetes ACS (STEMI)
# Type : STEMI
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
source('./Functions.R')
library(readxl)
library(caret)
library(eRic)
library(ResourceSelection)
library(rms)
library(pROC)

#### STEMI ATCUAL Outcome ####

##### Loading and Transformation ####
calibration_ds <- read.csv('./dataset/processed/calibration_ds.csv', header = TRUE)
validation_ds <- read.csv('./dataset/processed/validation_ds.csv', header = TRUE)

# Get STEMI Only
calibration_ds <- calibration_ds[calibration_ds$acsstratum ==2 | calibration_ds$acsstratum == 3 , ]
validation_ds <- validation_ds[validation_ds$acsstratum ==2 | validation_ds$acsstratum ==3, ]

# Get all Features
all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')

# Structuring each feature
calibration_ds <- RF20_TransformationFunction(calibration_ds, all_features)
validation_ds <- RF20_TransformationFunction(validation_ds, all_features)

# Removing acsstratum, timiscorestemi, timiscorenstemi
cols_to_remove <- c('acsstratum', 'timiscorestemi', 'timiscorenstemi')
calibration_ds <- calibration_ds[, -which(names(calibration_ds) %in% cols_to_remove)]
validation_ds <- validation_ds[, -which(names(validation_ds) %in% cols_to_remove)]

##### Separating features and label ####
X_calibration_ds <- calibration_ds[,-which(names(calibration_ds) == 'ptoutcome')]
X_validation_ds <- validation_ds[,-which(names(validation_ds) == 'ptoutcome')]

y_calibration_ds <- calibration_ds[,which(names(calibration_ds) == 'ptoutcome')]
y_validation_ds <- validation_ds[,which(names(validation_ds) == 'ptoutcome')]

##### Predicting Probabilities ####
# Load Model
model <- readRDS('./models/3_modelAll_rf20_dm.rds')$RF

# Predicting for calibration and validation dataset
# pred_prob_testing <- as.data.frame(predict(model, X_test_testing, type= 'prob')$Death)
pred_prob_calib <- predict(model, X_calibration_ds, type= 'prob')$Death
pred_prob_valid <- predict(model, X_validation_ds, type= 'prob')$Death


##### Performance Before Calibrating ####
raw_NSTEMI_Atcual_result <- Evaluation(y_validation_ds, pred_prob_valid, rowname = 'NSTEMI_Atcual_Raw')


##### Platt Scalling ####
# -1 to make the labels to be in 0,1
res <- prCalibrate(r.calib = as.numeric(y_calibration_ds)-1, 
                   p.calib = pred_prob_calib,
                   r.valid = as.numeric(y_validation_ds)-1,
                   p.valid = pred_prob_valid, 
                   nbins=10)

raw_logloss = res$raw.logloss
cal_logloss = res$cal.logloss

raw_acc = sum((res$raw.probs > 0.5) == res$responses)/length(res$responses)
cal_acc = sum((res$cal.probs > 0.5) == res$responses)/length(res$responses)

raw_auc = roc(res$responses, res$raw.probs, auc = TRUE)
cal_auc = roc(res$responses, res$cal.probs, auc = TRUE)

calibrated_result <- data.frame(raw_logloss,cal_logloss,raw_auc$auc,cal_auc$auc,raw_acc, cal_acc, row.names = 'calibrated')

calibrated_prob <- res$cal.probs

##### Performance After Calibrating ####
calibrated_NSTEMI_Atcual_result <- Evaluation(y_validation_ds, calibrated_prob, rowname = 'NSTEMI_Atcual_Calibrated')




#### STEMI TIMI Outcome ####

##### Loading and Transformation ####
calibration_ds <- read.csv('./dataset/processed/calibration_ds.csv', header = TRUE)
validation_ds <- read.csv('./dataset/processed/validation_ds.csv', header = TRUE)

# Get STEMI Only
calibration_ds <- calibration_ds[calibration_ds$acsstratum ==2 | calibration_ds$acsstratum == 3 , ]
validation_ds <- validation_ds[validation_ds$acsstratum ==2 | validation_ds$acsstratum ==3, ]

# Get all Features
all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')

# Structuring each feature
calibration_ds <- RF20_TransformationFunction(calibration_ds, all_features)
validation_ds <- RF20_TransformationFunction(validation_ds, all_features)

# Removing acsstratum, timiscorestemi, timiscorenstemi
cols_to_remove <- c('acsstratum', 'timiscorestemi', 'ptoutcome')
calibration_ds <- calibration_ds[, -which(names(calibration_ds) %in% cols_to_remove)]
validation_ds <- validation_ds[, -which(names(validation_ds) %in% cols_to_remove)]

# Normalize outcome
calibration_ds$timiscorenstemi <- (calibration_ds$timiscorenstemi - min(calibration_ds$timiscorenstemi)) / (max(calibration_ds$timiscorenstemi) - min(calibration_ds$timiscorenstemi))
validation_ds$timiscorenstemi <- (validation_ds$timiscorenstemi - min(validation_ds$timiscorenstemi)) / (max(validation_ds$timiscorenstemi) - min(validation_ds$timiscorenstemi))

##### Separating features and label ####
X_calibration_ds <- calibration_ds[,-which(names(calibration_ds) == 'timiscorenstemi')]
X_validation_ds <- validation_ds[,-which(names(validation_ds) == 'timiscorenstemi')]

y_calibration_ds <- calibration_ds[,which(names(calibration_ds) == 'timiscorenstemi')]
y_calibration_ds <- ifelse(y_calibration_ds >= 0.5, 1, 0)
y_calibration_ds <- factor(y_calibration_ds)

y_validation_ds <- validation_ds[,which(names(validation_ds) == 'timiscorenstemi')]
y_validation_ds <- ifelse(y_validation_ds >= 0.5, 1, 0)
y_validation_ds <- factor(y_validation_ds)

##### Predicting Probabilities ####
# Load Model
model <- readRDS('./models/3_modelAll_rf20_dm.rds')$RF

# Predicting for calibration and validation dataset
# pred_prob_testing <- as.data.frame(predict(model, X_test_testing, type= 'prob')$Death)
pred_prob_calib <- predict(model, X_calibration_ds, type= 'prob')$Death
pred_prob_valid <- predict(model, X_validation_ds, type= 'prob')$Death


##### Performance Before Calibrating ####
raw_NSTEMI_TIMI_result <- Evaluation(y_validation_ds, pred_prob_valid, rowname = 'NSTEMI_TIMI_Raw')


##### Platt Scalling ####
# -1 to make the labels to be in 0,1
res <- prCalibrate(r.calib = as.numeric(y_calibration_ds)-1, 
                   p.calib = pred_prob_calib,
                   r.valid = as.numeric(y_validation_ds)-1,
                   p.valid = pred_prob_valid, 
                   nbins=10)

raw_logloss = res$raw.logloss
cal_logloss = res$cal.logloss

raw_acc = sum((res$raw.probs > 0.5) == res$responses)/length(res$responses)
cal_acc = sum((res$cal.probs > 0.5) == res$responses)/length(res$responses)

raw_auc = roc(res$responses, res$raw.probs, auc = TRUE)
cal_auc = roc(res$responses, res$cal.probs, auc = TRUE)

calibrated_result <- data.frame(raw_logloss,cal_logloss,raw_auc$auc,cal_auc$auc,raw_acc, cal_acc, row.names = 'calibrated')

calibrated_prob <- res$cal.probs

##### Performance After Calibrating ####
calibrated_NSTEMI_TIMI_result <- Evaluation(y_validation_ds, calibrated_prob, rowname = 'NSTEMI_TIMI_Calibarted')


#### Exporting Result ####
final_result <- rbind(raw_NSTEMI_Atcual_result,calibrated_NSTEMI_Atcual_result, raw_NSTEMI_TIMI_result ,calibrated_NSTEMI_TIMI_result)
# write.csv(final_result, "./results/NSTEMI_Calibration_Performance.csv")
