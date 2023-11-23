# ---------------------------------------------------------------------------#
####                        File Description & Setup                      ####
#----------------------------------------------------------------------------#
# Please run the rproj file in the folder                                     
# Dataset: Diabetes ACS (STEMI & NSTEMI) Testing Dataset                                    
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
source('./Functions.R')
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

##### Removing acsstratum, timiscorestemi, timiscorenstemi ####
cols_to_remove <- c('acsstratum', 'timiscorestemi', 'timiscorenstemi')
testing_ds <- testing_ds[, -which(names(testing_ds) %in% cols_to_remove)]

##### Separating features and label ####
X_testing_ds <- testing_ds[,-which(names(testing_ds) == 'ptoutcome')]
y_testing_ds <- testing_ds[,which(names(testing_ds) == 'ptoutcome')]


# ---------------------------------------------------------------------------#
####                        Predicting Probailities                       ####
#----------------------------------------------------------------------------#

##### Loading Models ####
raw_model <- readRDS('./models/3_modelAll_rf20_dm.rds')$RF
calibrated_model <- readRDS('./results/calibrated_models/acs_calibrated_model.rds')

##### Prediction ####
raw_pred_probs <- predict(raw_model, X_testing_ds, type= 'prob')$Death
calibrated_pred_probs <- predict(calibrated_model, data.frame(y = y_testing_ds, x = raw_pred_probs), type = 'response')

##### Comparing Result ####
test_result <- data.frame(raw_pred_probs, calibrated_pred_probs,y_testing_ds)
names(test_result) <- c('Before', 'After', 'Atcual')

# ---------------------------------------------------------------------------#
####                            Result Evaluation.                        ####
#----------------------------------------------------------------------------#
# Threshold searched using the best F1 socre

##### Raw Model Evaluation ####
raw_threshold <- SearchBestThreshold(y_testing_ds,raw_pred_probs)
acs_raw_test_result <- Evaluation(y_testing_ds, raw_pred_probs, raw_threshold, rowname = 'ACS_Raw_Test')

##### Calibrated Model Evaluation ####
calibrated_threshold <- SearchBestThreshold(y_testing_ds,calibrated_pred_probs)
acs_calibrated_test_result <- Evaluation(y_testing_ds, calibrated_pred_probs, calibrated_threshold, rowname = 'ACS_Calibrated_Test')


# ---------------------------------------------------------------------------#
####                          Exporting Result                            ####
#----------------------------------------------------------------------------# 

##### Exporting Result ####
final_result <- rbind(acs_raw_test_result,acs_calibrated_test_result)
write.csv(final_result, "./results/TestingSetPerformance.csv")



