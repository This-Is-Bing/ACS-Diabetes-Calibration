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

#### BEFORE ####
#### Model Evaluation Before ####
# AUC
roc <- roc(pred_prob_valid$ptoutcome, pred_prob_valid$`predict(model, X_test_valid, type = "prob")$Death`)
auc <- roc$auc

# PR
pr <- pr.curve(pred_prob_valid$ptoutcome == 0 , pred_prob_valid$ptoutcome == 1, curve = T)

# CM
pred_class <-as.data.frame(predict(model, X_test_valid))
pred_class$ptoutcome <- validation_ds$ptoutcome
pred_class$ptoutcome<- ifelse(pred_class$ptoutcome == 0, 'Alive', ifelse(pred_class$ptoutcome == 1, 'Death', NA))
cm <- confusionMatrix(data = as.factor(pred_class$`predict(model, X_test_valid)`), 
                      reference =  as.factor(pred_class$ptoutcome),
                      positive = 'Death')

# Define a small epsilon value
epsilon <- 1e-15

# Adjust probabilities to avoid log(0)
adjusted_pred_risk <- pmax(pmin(pred_prob_valid$`predict(model, X_test_valid, type = "prob")$Death`, 1 - epsilon), epsilon)

# Calculate the Brier score manually
brier_score <- mean((pred_prob_valid$ptoutcome - adjusted_pred_risk)^2)
cat('Brier score:', brier_score, "\n")

# Calculate the logLoss manually
logloss <- -mean(pred_prob_valid$ptoutcome * log(adjusted_pred_risk) + (1 - pred_prob_valid$ptoutcome) * log(1 - adjusted_pred_risk))
cat('Log Loss:',logloss,"\n")

#### Hosmer Lemeshow Test ####
pred_risk <- pred_prob_valid$`predict(model, X_test_valid, type = "prob")$Death`
ptoutcome <- pred_prob_valid$ptoutcome

hlscore <- hoslem.test(pred_prob_valid$ptoutcome,pred_risk, g=10)


  
#### Compile ####

raw_result <- data.frame(auc = auc,acc = 8.157100e-01, sens = 0.89743590, spec = 0.81059390, ppv = 0.22875817,
                 npv = 0.99214145, mcnemar =  1.447112e-24, chisquare = hlscore$statistic,
                 pvalue = hlscore$p.value, brier = brier_score, logloss =logloss, row.names = 'raw_result')


#### BEFORE ####


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

#### AFTER ####
pred_risk <- res$cal.probs
ptoutcome <- res$responses

# AUC
roc <- roc(ptoutcome, pred_risk)
auc <- roc$auc

# PR
pr <- pr.curve(ptoutcome == 0 , ptoutcome == 1, curve = T)

# CM

pred_class <-as.data.frame(res$cal.probs)
pred_class$`res$cal.probs` <-  ifelse(pred_class$`res$cal.probs` >= 0.5, 'Death', 'Alive')
pred_class$ptoutcome <- validation_ds$ptoutcome
pred_class$ptoutcome<- ifelse(pred_class$ptoutcome == 0, 'Alive', ifelse(pred_class$ptoutcome == 1, 'Death', NA))
cm <- confusionMatrix(data = as.factor(pred_class$`res$cal.probs`), 
                      reference =  as.factor(pred_class$ptoutcome),
                      positive = 'Death')

# Define a small epsilon value
epsilon <- 1e-15

# Adjust probabilities to avoid log(0)
adjusted_pred_risk <- pmax(pmin(pred_risk, 1 - epsilon), epsilon)

# Calculate the Brier score manually
brier_score <- mean((ptoutcome - adjusted_pred_risk)^2)
cat('Brier score:', brier_score, "\n")

# Calculate the logLoss manually
logloss <- -mean(ptoutcome * log(adjusted_pred_risk) + (1 - ptoutcome) * log(1 - adjusted_pred_risk))
cat('Log Loss:',logloss,"\n")

#### Hosmer Lemeshow Test ####
hlscore <- hoslem.test(ptoutcome,pred_risk, g=10)

#### Compile ####

calibrated_result <- data.frame(auc = auc,acc = 9.516616e-01, sens = 0.25641026 , spec = 0.99518459, ppv = 0.76923077,
                     npv = 0.95531587, mcnemar =  9.896735e-06, chisquare = hlscore$statistic,
                     pvalue = hlscore$p.value, brier = brier_score, logloss =logloss, row.names = 'calibrated_result')

#### AFTER ####

#### Compile all result ####
all_result <- rbind(raw_result,calibrated_result)

# Export Result
write.csv(all_result, './results/ACS_result.csv')
