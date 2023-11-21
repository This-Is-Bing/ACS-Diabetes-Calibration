#### File Description ####
# Please run the rproj file in the folder
# This script is to run the HL test and Platt Scaling


##### Cleaning environment ####
rm(list=ls())

#### Download eric packages ####
# UNCOMMENT TO INSTALL FOR THE FIRST TIME
#install.packages("devtools")
#library("devtools")
#install_github("etlundquist/eRic")


##### Loading Library ####
set.seed(2468)
source('./Functions.R')
library(readxl)
library(caret)
library(eRic)
library(ResourceSelection)
library(rms)
library(pROC)


#### Loading and Change Types of All Datasets ####
# Include [-1] to remove "X" column of row number
# testing_ds <- read.csv('./dataset/processed/testing_ds.csv', header = TRUE)[-1]
calibration_ds <- read.csv('./dataset/processed/calibration_ds.csv', header = TRUE)[-1]
validation_ds <- read.csv('./dataset/processed/validation_ds.csv', header = TRUE)[-1]

# Select features
all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')
all_features <- all_features[which(all_features$rf20_fet %in% c("other", "yes")),]

# Get selected features names with types
all_features_names <- all_features$VarNames[which(all_features$rf20_fet %in% c("yes"))]
cat_nb_features_names <- all_features$VarNames[which(all_features$Cat_Cont_Ord == "categorical_nonBinary")]
cat_features_names<- all_features$VarNames[which(all_features$Cat_Cont_Ord == "categorical_binary")] 
cont_features_names <- all_features$VarNames[which(all_features$Cat_Cont_Ord %in% c("continuous", "ordinal"))]

# get selected features in DS
#testing_ds <- testing_ds[,c(all_features_names,"ptoutcome")]
calibration_ds <- calibration_ds[,c(all_features_names,"ptoutcome")]
validation_ds <- validation_ds[,c(all_features_names,"ptoutcome")]

# Change the types of each variables
# testing_ds <- StructuringFunction(dataset = testing_ds, cat_nb_features_names, cat_features_names, cont_features_names)
calibration_ds <- StructuringFunction(dataset = calibration_ds, cat_nb_features_names, cat_features_names, cont_features_names)
validation_ds <- StructuringFunction(dataset = validation_ds, cat_nb_features_names, cat_features_names, cont_features_names)

# X_test_testing <- testing_ds[, -which(names(testing_ds) == 'ptoutcome')]
X_test_calib <- calibration_ds[, -which(names(calibration_ds) == 'ptoutcome')]
X_test_valid <- validation_ds[, -which(names(validation_ds) == 'ptoutcome')]

#### Predicting Probabilities ####

# Load Model
model <- readRDS('./models/3_modelAll_rf20_dm.rds')
model <- model$RF

# Predicting for calibration and validation dataset
# pred_prob_testing <- as.data.frame(predict(model, X_test_testing, type= 'prob')$Death)
pred_prob_calib <- as.data.frame(predict(model, X_test_calib, type= 'prob')$Death)
pred_prob_valid <- as.data.frame(predict(model, X_test_valid, type= 'prob')$Death)

# pred_prob_testing$ptoutcome <- as.numeric(testing_ds$ptoutcome== 0)
pred_prob_calib$ptoutcome <- as.numeric(calibration_ds$ptoutcome)
pred_prob_calib$ptoutcome <- as.numeric(pred_prob_calib$ptoutcome-1)
pred_prob_valid$ptoutcome <- as.numeric(validation_ds$ptoutcome)
pred_prob_valid$ptoutcome <- as.numeric(pred_prob_valid$ptoutcome-1)


#### Platt Scaling ####
res <- prCalibrate(pred_prob_calib$ptoutcome, 
                   pred_prob_calib$`predict(model, X_test_calib, type = "prob")$Death`, 
                   pred_prob_valid$ptoutcome,
                   pred_prob_valid$`predict(model, X_test_valid, type = "prob")$Death`, 
                   nbins=10)

raw_logloss = res$raw.logloss
cal_logloss = res$cal.logloss

raw_acc = sum((res$raw.probs > 0.5) == res$responses)/length(res$responses)
cal_acc = sum((res$cal.probs > 0.5) == res$responses)/length(res$responses)

raw_auc = roc(res$responses, res$raw.probs, auc = TRUE)
cal_auc = roc(res$responses, res$cal.probs, auc = TRUE)

raw_logloss
cal_logloss
raw_acc
cal_acc
raw_auc$auc
cal_auc$auc

#### Hosmer Lemeshow Test ####
pred_risk <- res$cal.probs
ptoutcome <- res$responses

hoslem.test(pred_prob_valid$ptoutcome,pred_risk, g=10)

#	Hosmer and Lemeshow goodness of fit (GOF) test

# data:  pred_prob_valid$ptoutcome, pred_risk
# X-squared = 10.349, df = 8, p-value = 0.2414
