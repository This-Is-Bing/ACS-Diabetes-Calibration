# ---------------------------------------------------------------------------#
####                        File Description & Setup                      ####
#----------------------------------------------------------------------------#
# Please run the rproj file in the folder                                     
# Dataset: Diabetes TIMI STEMI, NSTEMI on Testing Dataset                                    
# Description: To compare the raw model & calibrated model performance on TIMI SCORE

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

##### Separating ACS, STEMI, NSTEMI
stemi_testing_ds <- testing_ds[testing_ds$acsstratum ==1, ]
nstemi_testing_ds <- testing_ds[!testing_ds$acsstratum ==1, ]

##### Removing Uneccessary Columns for STEMI ####
cols_to_remove <- c('acsstratum', 'ptoutcome', 'timiscorenstemi')
stemi_testing_ds <- stemi_testing_ds[, -which(names(stemi_testing_ds) %in% cols_to_remove)]

##### Removing Uneccessary Columns for SNTEMI ####
cols_to_remove <- c('acsstratum', 'ptoutcome', 'timiscorestemi')
nstemi_testing_ds <- nstemi_testing_ds[, -which(names(nstemi_testing_ds) %in% cols_to_remove)]

##### Classifying outcome with timiscore fot STEMI ####
stemi_testing_ds$timiscorestemi <- ifelse(stemi_testing_ds$timiscorestemi >=5, 1, 0)

##### Classifying outcome with timiscore fot NSTEMI ####
nstemi_testing_ds$timiscorenstemi <- ifelse(nstemi_testing_ds$timiscorenstemi >=5, 1, 0)

##### Changing the outcome column name to make the code reusable ####
stemi_testing_ds$ptoutcome <- stemi_testing_ds$timiscorestemi
stemi_testing_ds$timiscorestemi<- NULL

nstemi_testing_ds$ptoutcome <- nstemi_testing_ds$timiscorenstemi
nstemi_testing_ds$timiscorenstemi<- NULL

##### Structuring each feature ####
stemi_testing_ds <- RF20_TransformationFunction(stemi_testing_ds, all_features)
nstemi_testing_ds <- RF20_TransformationFunction(nstemi_testing_ds, all_features)

##### Separating features and label ####
# X_testing_ds <- testing_ds[,-which(names(testing_ds) == 'ptoutcome')]
X_stemi_testing_ds <- stemi_testing_ds[,-which(names(stemi_testing_ds) == 'ptoutcome')]
X_nstemi_testing_ds <- nstemi_testing_ds[,-which(names(nstemi_testing_ds) == 'ptoutcome')]

# y_testing_ds <- testing_ds[,which(names(testing_ds) == 'ptoutcome')]
y_stemi_testing_ds <- stemi_testing_ds[,which(names(stemi_testing_ds) == 'ptoutcome')]
y_nstemi_testing_ds <- nstemi_testing_ds[,which(names(nstemi_testing_ds) == 'ptoutcome')]



# ---------------------------------------------------------------------------#
####                        Predicting Probabilities                      ####
#----------------------------------------------------------------------------#

##### Loading Models ####
raw_model <- readRDS('./models/3_modelAll_rf20_dm.rds')$RF
# acs_calibrated_model <- readRDS('./results/calibrated_models/acs_calibrated_model.rds')
timi_stemi_calibrated_model <- readRDS('./results/calibrated_models/timi_stemi_calibrated_model.rds')
timi_nstemi_calibrated_model <- readRDS('./results/calibrated_models/timi_nstemi_calibrated_model.rds')

##### Prediction ####

###### ACS ####
# acs_raw_pred_probs <- predict(raw_model, X_testing_ds, type= 'prob')$Death
# acs_calibrated_pred_probs <- predict(acs_calibrated_model, data.frame(y = y_testing_ds, x = acs_raw_pred_probs), type = 'response')

###### STEMI ####
stemi_raw_pred_probs <- predict(raw_model, X_stemi_testing_ds, type= 'prob')$Death
stemi_calibrated_pred_probs <- predict(timi_stemi_calibrated_model, data.frame(y = y_stemi_testing_ds, x = stemi_raw_pred_probs), type = 'response')

###### NSTEMI ####
nstemi_raw_pred_probs <- predict(raw_model, X_nstemi_testing_ds, type= 'prob')$Death
nstemi_calibrated_pred_probs <- predict(timi_nstemi_calibrated_model, data.frame(y = y_nstemi_testing_ds, x = nstemi_raw_pred_probs), type = 'response')


# ---------------------------------------------------------------------------#
####                            Result Evaluation.                        ####
#----------------------------------------------------------------------------#
# Threshold searched using the best F1 socre

##### ACS ####
###### Raw Model Evaluation ####
# acs_raw_threshold <- SearchBestThreshold(y_testing_ds,acs_raw_pred_probs)
# acs_raw_test_result <- Evaluation(y_testing_ds, acs_raw_pred_probs, acs_raw_threshold, rowname = 'ACS_Raw_Test')

###### Calibrated Model Evaluation ####
# acs_calibrated_threshold <- SearchBestThreshold(y_testing_ds,acs_calibrated_pred_probs)
# acs_calibrated_test_result <- Evaluation(y_testing_ds, acs_calibrated_pred_probs, acs_calibrated_threshold, rowname = 'ACS_Calibrated_Test')


##### STEMI ####
###### Raw Model Evaluation ####
stemi_raw_threshold <- SearchBestThreshold(y_stemi_testing_ds, stemi_raw_pred_probs)
stemi_raw_test_result <- Evaluation(y_stemi_testing_ds, stemi_raw_pred_probs, stemi_raw_threshold, rowname = 'TIMI_STEMI_Raw_Test')

###### Calibrated Model Evaluation ####
stemi_calibrated_threshold <- SearchBestThreshold(y_stemi_testing_ds,stemi_calibrated_pred_probs)
stemi_calibrated_test_result <- Evaluation(y_stemi_testing_ds, stemi_calibrated_pred_probs, stemi_calibrated_threshold, rowname = 'TIMI_STEMI_Calibrated_Test')


##### NSTEMI ####
###### Raw Model Evaluation ####
nstemi_raw_threshold <- SearchBestThreshold(y_nstemi_testing_ds, nstemi_raw_pred_probs)
nstemi_raw_test_result <- Evaluation(y_nstemi_testing_ds, nstemi_raw_pred_probs, nstemi_raw_threshold, rowname = 'TIMI_NSTEMI_Raw_Test')

###### Calibrated Model Evaluation ####
nstemi_calibrated_threshold <- SearchBestThreshold(y_nstemi_testing_ds,nstemi_calibrated_pred_probs)
nstemi_calibrated_test_result <- Evaluation(y_nstemi_testing_ds, nstemi_calibrated_pred_probs, nstemi_calibrated_threshold, rowname = 'TIM_NSTEMI_Calibrated_Test')


# ---------------------------------------------------------------------------#
####                          Exporting Result                            ####
#----------------------------------------------------------------------------# 

##### Exporting Result ####
final_result <- rbind(stemi_raw_test_result, stemi_calibrated_test_result,
                      nstemi_raw_test_result, nstemi_calibrated_test_result)

# write.csv(final_result, "./results/TIMI_Performance_on_testing.csv")





