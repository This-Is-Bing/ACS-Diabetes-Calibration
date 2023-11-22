  #### File Description ####
  # Please run the rproj file in the folder
  # Dataset: Diabetes ACS (STEMI + NSTEMI) (Without TIMI Score)
  # This script is to compare the raw performance and calibrated performance
  
  #### Download eRic packages ####
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
  
  #### Loading and Transformation ####
  calibration_ds <- read.csv('./dataset/processed/calibration_ds.csv', header = TRUE,)
  validation_ds <- read.csv('./dataset/processed/validation_ds.csv', header = TRUE)
  
  # Get all Features
  all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')
  
  # Structuring each feature
  calibration_ds <- RF20_TransformationFunction(calibration_ds, all_features)
  validation_ds <- RF20_TransformationFunction(validation_ds, all_features)
  
  # Removing acsstratum, timiscorestemi, timiscorenstemi
  cols_to_remove <- c('acsstratum', 'timiscorestemi', 'timiscorenstemi')
  calibration_ds <- calibration_ds[, -which(names(calibration_ds) %in% cols_to_remove)]
  validation_ds <- validation_ds[, -which(names(validation_ds) %in% cols_to_remove)]
  
  #### Separating features and label ####
  X_calibration_ds <- calibration_ds[,-which(names(calibration_ds) == 'ptoutcome')]
  X_validation_ds <- validation_ds[,-which(names(validation_ds) == 'ptoutcome')]
  
  y_calibration_ds <- calibration_ds[,which(names(calibration_ds) == 'ptoutcome')]
  y_validation_ds <- validation_ds[,which(names(validation_ds) == 'ptoutcome')]
  
  #### Predicting Probabilities ####
  # Load Model
  model <- readRDS('./models/3_modelAll_rf20_dm.rds')$RF
  
  # Predicting for calibration and validation dataset
  # pred_prob_testing <- as.data.frame(predict(model, X_test_testing, type= 'prob')$Death)
  pred_prob_calib <- predict(model, X_calibration_ds, type= 'prob')$Death
  pred_prob_valid <- predict(model, X_validation_ds, type= 'prob')$Death
  

  #### Performance Before Calibrating ####
  raw_acs_result <- Evaluation(y_validation_ds, pred_prob_valid, rowname = 'ACS_Raw')
  #### Performance Before Calibrating ##
  
  
  #### Platt Scalling ####
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

  #### Performance After Calibrating ####
  calibrated_acs_result <- Evaluation(y_validation_ds, pred_prob_valid, rowname = 'ACS_Calibrated')
  #### Performance After Calibrating ##
  
  #### Exporting Result ####
  final_result <- rbind(raw_acs_result,calibrated_acs_result)
  write.csv(final_result, "./results/ACS_Calibration_Performance.csv", row.names = FALSE)
  
  
  aaaa <- read.csv('./results/ACS_Calibration_Performance.csv')
aaaa  
