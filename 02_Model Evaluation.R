#### File Description ####
# Please run the rproj file in the folder
# Dataset: Testing
# This script is to run and evaluate the model


##### Cleaning environment ####
rm(list=ls())


##### Loading Library ####
set.seed(2468)
source('./Functions.R')
library(readxl)
library(caret)
library(pROC)
library(PRROC)


#### Loading and Change Types of All Datasets ####
testing_ds <- read.csv('./dataset/processed/testing_ds.csv', header = TRUE)

# Select features
all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')
all_features <- all_features[which(all_features$rf20_fet %in% c("other", "yes")),]

# Get selected features names with types
all_features_names <- all_features$VarNames[which(all_features$rf20_fet %in% c("yes"))]
cat_nb_features_names <- all_features$VarNames[which(all_features$Cat_Cont_Ord == "categorical_nonBinary")]
cat_features_names<- all_features$VarNames[which(all_features$Cat_Cont_Ord == "categorical_binary")] 
cont_features_names <- all_features$VarNames[which(all_features$Cat_Cont_Ord %in% c("continuous", "ordinal"))]

# get selected features in DS
testing_ds <- testing_ds[,c(all_features_names,"ptoutcome")]

# Change the types of each variables
testing_ds <- StructuringFunction(dataset = testing_ds, cat_nb_features_names, cat_features_names, cont_features_names)

X_test <- testing_ds[, -which(names(testing_ds) == 'ptoutcome')]

#### Predicting Probabilities ####
# Description: Random Forest only

# Load Model
model <- readRDS('./models/3_modelAll_rf20_dm.rds')
rf_model <- model$RF

# Predict probabilities and class
rf_pred_prob <- as.data.frame(predict(rf_model, X_test, type = 'prob')$Death)

rf_pred_class <-as.data.frame(predict(rf_model, X_test))

# Adding atcual outcome to predicted result
rf_pred_prob$ptoutcome <- testing_ds$ptoutcome
rf_pred_class$ptoutcome <- testing_ds$ptoutcome
rf_pred_class$ptoutcome<- ifelse(rf_pred_class$ptoutcome == 0, 'Alive', ifelse(rf_pred_class$ptoutcome == 1, 'Death', NA))

#### Evaluating Result ####

# AUC
roc <- roc(rf_pred_prob$ptoutcome, rf_pred_prob$`predict(rf_model, X_test, type = "prob")$Death`)
auc <- roc$auc

# PR
pr <- pr.curve(rf_pred_prob$ptoutcome == 0 , rf_pred_prob$ptoutcome == 1, curve = T)

# CM
cm <- confusionMatrix(data = as.factor(rf_pred_class$`predict(rf_model, X_test)`), 
                      reference = as.factor(rf_pred_class$ptoutcome),
                      positive = 'Death')

#AUC: 0.8917
#Accuracy : 0.8405  




