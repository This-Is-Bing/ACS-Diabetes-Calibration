# ---------------------------------------------------------------------------#
####                        File Description & Setup                      ####
#----------------------------------------------------------------------------#
# Please run the rproj file in the folder                                     
# Dataset: Diabetes ACS, STEMI, NSTEMI on Testing Dataset                                    
# Description: To compare the raw model & calibrated model performance 

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

##### Reading Testing Dataset
testing_ds <- read.csv('./dataset/processed/testing_ds.csv', header = TRUE)

##### Get all Features ####
all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')

##### Structuring each feature ####
testing_ds <- RF20_TransformationFunction(testing_ds, all_features)

##### Separating ACS, STEMI, NSTEMI
stemi_testing_ds <- testing_ds[testing_ds$acsstratum ==1, ]
nstemi_testing_ds <- testing_ds[!testing_ds$acsstratum ==1, ]

##### Removing acsstratum, timiscorestemi, timiscorenstemi ####
cols_to_remove <- c('acsstratum', 'timiscorestemi', 'timiscorenstemi')
testing_ds <- testing_ds[, -which(names(testing_ds) %in% cols_to_remove)]
stemi_testing_ds <- stemi_testing_ds[, -which(names(stemi_testing_ds) %in% cols_to_remove)]
nstemi_testing_ds <- nstemi_testing_ds[, -which(names(nstemi_testing_ds) %in% cols_to_remove)]

##### Separating features and label ####
X_testing_ds <- testing_ds[,-which(names(testing_ds) == 'ptoutcome')]
X_stemi_testing_ds <- stemi_testing_ds[,-which(names(stemi_testing_ds) == 'ptoutcome')]
X_nstemi_testing_ds <- nstemi_testing_ds[,-which(names(nstemi_testing_ds) == 'ptoutcome')]

y_testing_ds <- testing_ds[,which(names(testing_ds) == 'ptoutcome')]
y_stemi_testing_ds <- stemi_testing_ds[,which(names(stemi_testing_ds) == 'ptoutcome')]
y_nstemi_testing_ds <- nstemi_testing_ds[,which(names(nstemi_testing_ds) == 'ptoutcome')]

# ---------------------------------------------------------------------------#
####                        Predicting Probabilities                      ####
#----------------------------------------------------------------------------#

##### Loading Models ####
raw_model <- readRDS('./models/3_modelAll_rf20_dm.rds')$RF
acs_calibrated_model <- readRDS('./results/2/calibrated_models/acs_calibrated_model.rds')
stemi_calibrated_model <- readRDS('./results/2/calibrated_models/stemi_calibrated_model.rds')
nstemi_calibrated_model <- readRDS('./results/2/calibrated_models/nstemi_calibrated_model.rds')

##### Prediction ####

###### ACS ####
acs_raw_pred_probs <- predict(raw_model, X_testing_ds, type= 'prob')$Death
acs_calibrated_pred_probs <- predict(acs_calibrated_model, data.frame(y = y_testing_ds, x = acs_raw_pred_probs), type = 'response')

###### STEMI ####
stemi_raw_pred_probs <- predict(raw_model, X_stemi_testing_ds, type= 'prob')$Death
stemi_calibrated_pred_probs <- predict(stemi_calibrated_model, data.frame(y = y_stemi_testing_ds, x = stemi_raw_pred_probs), type = 'response')

###### NSTEMI ####
nstemi_raw_pred_probs <- predict(raw_model, X_nstemi_testing_ds, type= 'prob')$Death
nstemi_calibrated_pred_probs <- predict(nstemi_calibrated_model, data.frame(y = y_nstemi_testing_ds, x = nstemi_raw_pred_probs), type = 'response')


# ---------------------------------------------------------------------------#
####                            Result Evaluation.                        ####
#----------------------------------------------------------------------------#
# Threshold searched using the best F1 socre

##### ACS ####
###### Raw Model Evaluation ####
acs_raw_threshold <- SearchBestThreshold(y_testing_ds,acs_raw_pred_probs)
acs_raw_test_result <- Evaluation(y_testing_ds, acs_raw_pred_probs, acs_raw_threshold, rowname = 'ACS_Raw_Test')

###### Calibrated Model Evaluation ####
acs_calibrated_threshold <- SearchBestThreshold(y_testing_ds,acs_calibrated_pred_probs)
acs_calibrated_test_result <- Evaluation(y_testing_ds, acs_calibrated_pred_probs, acs_calibrated_threshold, rowname = 'ACS_Calibrated_Test')


##### STEMI ####
###### Raw Model Evaluation ####
stemi_raw_threshold <- SearchBestThreshold(y_stemi_testing_ds, stemi_raw_pred_probs)
stemi_raw_test_result <- Evaluation(y_stemi_testing_ds, stemi_raw_pred_probs, stemi_raw_threshold, rowname = 'STEMI_Raw_Test')

###### Calibrated Model Evaluation ####
stemi_calibrated_threshold <- SearchBestThreshold(y_stemi_testing_ds,stemi_calibrated_pred_probs)
stemi_calibrated_test_result <- Evaluation(y_stemi_testing_ds, stemi_calibrated_pred_probs, stemi_calibrated_threshold, rowname = 'STEMI_Calibrated_Test')


##### NSTEMI ####
###### Raw Model Evaluation ####
nstemi_raw_threshold <- SearchBestThreshold(y_nstemi_testing_ds, nstemi_raw_pred_probs)
nstemi_raw_test_result <- Evaluation(y_nstemi_testing_ds, nstemi_raw_pred_probs, nstemi_raw_threshold, rowname = 'NSTEMI_Raw_Test')

###### Calibrated Model Evaluation ####
nstemi_calibrated_threshold <- SearchBestThreshold(y_nstemi_testing_ds,nstemi_calibrated_pred_probs)
nstemi_calibrated_test_result <- Evaluation(y_nstemi_testing_ds, nstemi_calibrated_pred_probs, nstemi_calibrated_threshold, rowname = 'NSTEMI_Calibrated_Test')


# ---------------------------------------------------------------------------#
####                          Exporting Result                            ####
#----------------------------------------------------------------------------# 

##### Exporting Result ####
final_result <- rbind(acs_raw_test_result,acs_calibrated_test_result,
                      stemi_raw_test_result, stemi_calibrated_test_result,
                      nstemi_raw_test_result, nstemi_calibrated_test_result)

write.csv(final_result,"./results/2/overall_performance.csv")

