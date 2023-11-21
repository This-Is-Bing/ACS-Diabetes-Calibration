#### File Description ####
# Please run the rproj file in the folder
# Dataset: STEMI and NSTEMI
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
source('./StructuringFunction.R')
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

#### Separating STEMI and NSTEMI ####
stemi_calib_ds <- calibration_ds[calibration_ds$acsstratum==1,]
stemi_valid_ds <- validation_ds[validation_ds$acsstratum==1,]

nstemi_calib_ds<- calibration_ds[calibration_ds$acsstratum==2 | calibration_ds$acsstratum==3,]
nstemi_valid_ds <- validation_ds[validation_ds$acsstratum==2 | validation_ds$acsstratum==3,]

#### Normalize STEMI and NSTEMI timi score ####
stemi_calib_ds$timiscorestemi <- (stemi_calib_ds$timiscorestemi - min(stemi_calib_ds$timiscorestemi)) / (max(stemi_calib_ds$timiscorestemi) - min(stemi_calib_ds$timiscorestemi))
stemi_valid_ds$timiscorestemi <- (stemi_valid_ds$timiscorestemi - min(stemi_valid_ds$timiscorestemi)) / (max(stemi_valid_ds$timiscorestemi) - min(stemi_valid_ds$timiscorestemi))

nstemi_calib_ds$timiscorenstemi <- (nstemi_calib_ds$timiscorenstemi - min(nstemi_calib_ds$timiscorenstemi)) / (max(nstemi_calib_ds$timiscorenstemi) - min(nstemi_calib_ds$timiscorenstemi))
nstemi_valid_ds$timiscorenstemi <- (nstemi_valid_ds$timiscorenstemi - min(nstemi_valid_ds$timiscorenstemi)) / (max(nstemi_valid_ds$timiscorenstemi) - min(nstemi_valid_ds$timiscorenstemi))

# get selected features in DS
#testing_ds <- testing_ds[,c(all_features_names,"ptoutcome")]
stemi_calib_actual_ds <- stemi_calib_ds[,c(all_features_names,"ptoutcome")]
stemi_valid_actual_ds <- stemi_valid_ds[,c(all_features_names,"ptoutcome")]
stemi_calib_timi_ds <- stemi_calib_ds[,c(all_features_names,"timiscorestemi")]
stemi_valid_timi_ds <- stemi_valid_ds[,c(all_features_names,"timiscorestemi")]

nstemi_calib_actual_ds <- nstemi_calib_ds[,c(all_features_names,"ptoutcome")]
nstemi_valid_actual_ds <- nstemi_valid_ds[,c(all_features_names,"ptoutcome")]
nstemi_calib_timi_ds <- nstemi_calib_ds[,c(all_features_names,"timiscorenstemi")]
nstemi_valid_timi_ds <- nstemi_valid_ds[,c(all_features_names,"timiscorenstemi")]


# Change the types of each variables
# testing_ds <- StructuringFunction(dataset = testing_ds, cat_nb_features_names, cat_features_names, cont_features_names)
stemi_calib_actual_ds <- StructuringFunction(dataset = stemi_calib_actual_ds, cat_nb_features_names, cat_features_names, cont_features_names)
stemi_valid_actual_ds <- StructuringFunction(dataset = stemi_valid_actual_ds, cat_nb_features_names, cat_features_names, cont_features_names)
stemi_calib_timi_ds <- StemiStructuringFunction(dataset = stemi_calib_timi_ds, cat_nb_features_names, cat_features_names, cont_features_names)
stemi_valid_timi_ds <- StemiStructuringFunction(dataset = stemi_valid_timi_ds, cat_nb_features_names, cat_features_names, cont_features_names)
nstemi_calib_actual_ds <- StructuringFunction(dataset = nstemi_calib_actual_ds, cat_nb_features_names, cat_features_names, cont_features_names)
nstemi_valid_actual_ds <- StructuringFunction(dataset = nstemi_valid_actual_ds, cat_nb_features_names, cat_features_names, cont_features_names)
nstemi_calib_timi_ds <- NStemiStructuringFunction(dataset = nstemi_calib_timi_ds, cat_nb_features_names, cat_features_names, cont_features_names)
nstemi_valid_timi_ds <- NStemiStructuringFunction(dataset = nstemi_valid_timi_ds, cat_nb_features_names, cat_features_names, cont_features_names)

# X_test_testing <- testing_ds[, -which(names(testing_ds) == 'ptoutcome')]
X_stemi_actual_calib <- stemi_calib_actual_ds[, -which(names(stemi_calib_actual_ds) == 'ptoutcome')]
X_stemi_actual_valid <- stemi_valid_actual_ds[, -which(names(stemi_valid_actual_ds) == 'ptoutcome')]
X_stemi_timi_calib <- stemi_calib_timi_ds[, -which(names(stemi_calib_timi_ds) == 'timiscorestemi')]
X_stemi_timi_valid <- stemi_valid_timi_ds[, -which(names(validation_ds) == 'timiscorestemi')]

X_nstemi_actual_calib <- nstemi_calib_actual_ds[, -which(names(nstemi_calib_actual_ds) == 'ptoutcome')]
X_nstemi_actual_valid <- nstemi_valid_actual_ds[, -which(names(nstemi_valid_actual_ds) == 'ptoutcome')]
X_nstemi_timi_calib <- nstemi_calib_timi_ds[, -which(names(nstemi_calib_timi_ds) == 'timiscorenstemi')]
X_nstemi_timi_valid <- nstemi_valid_timi_ds[, -which(names(nstemi_valid_timi_ds) == 'timiscorenstemi')]

#### Predicting Probabilities ####

# Load Model
model <- readRDS('./models/3_modelAll_rf20_dm.rds')
model <- model$RF

# Predicting for calibration and validation dataset
# pred_prob_testing <- as.data.frame(predict(model, X_test_testing, type= 'prob')$Death)
pred_stemi_actual_calib <- as.data.frame(predict(model, X_stemi_actual_calib, type= 'prob')$Death)
pred_stemi_actual_valid <- as.data.frame(predict(model, X_stemi_actual_valid, type= 'prob')$Death)
pred_stemi_timi_calib <- as.data.frame(predict(model, X_stemi_timi_calib, type= 'prob')$Death)
pred_stemi_timi_valid <- as.data.frame(predict(model, X_stemi_timi_valid, type= 'prob')$Death)

pred_nstemi_actual_calib <- as.data.frame(predict(model, X_nstemi_actual_calib, type= 'prob')$Death)
pred_nstemi_actual_valid <- as.data.frame(predict(model, X_nstemi_actual_valid, type= 'prob')$Death)
pred_nstemi_timi_calib <- as.data.frame(predict(model, X_nstemi_timi_calib, type= 'prob')$Death)
pred_nstemi_timi_valid <- as.data.frame(predict(model, X_nstemi_timi_valid, type= 'prob')$Death)

# Append atcual result to predicted result
# Stemi actual calib
pred_stemi_actual_calib$ptoutcome <- as.numeric(stemi_calib_actual_ds$ptoutcome)
pred_stemi_actual_calib$ptoutcome <- as.numeric(pred_stemi_actual_calib$ptoutcome-1)

# Stemi actual valid
pred_stemi_actual_valid$ptoutcome <- as.numeric(stemi_valid_actual_ds$ptoutcome)
pred_stemi_actual_valid$ptoutcome <- as.numeric(pred_stemi_actual_valid$ptoutcome-1)

# Stemi timi calib
pred_stemi_timi_calib$timiscorestemi <- as.numeric(stemi_calib_timi_ds$timiscorestemi)
pred_stemi_timi_calib$timiscorestemi <- as.numeric(pred_stemi_timi_calib$timiscorestemi-1)

# Stemi timi valid
pred_stemi_timi_valid$timiscorestemi <- as.numeric(stemi_valid_timi_ds$timiscorestemi)
pred_stemi_timi_valid$timiscorestemi <- as.numeric(pred_stemi_timi_valid$timiscorestemi-1)


# NStemi actual calib
pred_nstemi_actual_calib$ptoutcome <- as.numeric(nstemi_calib_actual_ds$ptoutcome)
pred_nstemi_actual_calib$ptoutcome <- as.numeric(pred_nstemi_actual_calib$ptoutcome-1)

# NStemi actual valid
pred_nstemi_actual_valid$ptoutcome <- as.numeric(nstemi_valid_actual_ds$ptoutcome)
pred_nstemi_actual_valid$ptoutcome <- as.numeric(pred_nstemi_actual_valid$ptoutcome-1)

# NStemi timi calib
pred_nstemi_timi_calib$timiscorenstemi <- as.numeric(nstemi_calib_timi_ds$timiscorenstemi)
pred_nstemi_timi_calib$timiscorenstemi <- as.numeric(pred_nstemi_timi_calib$timiscorenstemi-1)

# NStemi timi valid
pred_nstemi_timi_valid$timiscorenstemi <- as.numeric(nstemi_valid_timi_ds$timiscorenstemi)
pred_nstemi_timi_valid$timiscorenstemi <- as.numeric(pred_nstemi_timi_valid$timiscorenstemi-1)


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

#### HL test for STEMI ####
pred_risk <- pred_stemi_timi_valid$`predict(model, X_stemi_timi_valid, type = "prob")$Death`
actual_risk <- pred_stemi_timi_valid$timiscorestemi

hoslem.test(actual_risk,pred_risk, g=10)

#### HL test for NSTEMI ####
pred_risk <- pred_nstemi_timi_valid$`predict(model, X_nstemi_timi_valid, type = "prob")$Death`
actual_risk <- pred_nstemi_timi_valid$timiscorenstemi

hoslem.test(actual_risk,pred_risk, g=10)










