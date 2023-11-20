##### File Description ####
# Please run the rproj of this file
# This script is to filter the selected features and split into training, calibration and validation dataset

##### Cleaning Environment ####
rm(list=ls())


##### Loading Library ####
set.seed(2468)
library(readxl)
library(caret)
source('./StructuringFunction.R')


##### Loading Training Dataset ####
# Source: Chien Wen
# Type. : Diabetes
# Split : Training
# Others: No NA, ROSE Balanced
ds <- read_xlsx("./dataset/dm_trainData_noNA_BALANCED_ROSEMethod_new.xlsx")


##### Dataset Preprocessing ####
# Description: Only taking the RF features and Model with 20 selected features
all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')
all_features <- all_features[which(all_features$rf20_fet %in% c("other", "yes")),]

# Get selected features names and types
all_features_names <- all_features$VarNames[which(all_features$rf20_fet %in% c("yes"))]
cat_nb_features_names <- all_features$VarNames[which(all_features$Cat_Cont_Ord == "categorical_nonBinary")]
cat_features_names<- all_features$VarNames[which(all_features$Cat_Cont_Ord == "categorical_binary")] 
cont_features_names <- all_features$VarNames[which(all_features$Cat_Cont_Ord %in% c("continuous", "ordinal"))]

# Select variables on DS
ds <- ds[,c(all_features_names,"ptoutcome")]
ds$ptoutcome <- ifelse(ds$ptoutcome == "Alive", 0, ifelse(ds$ptoutcome == "Death", 1, NA))


#### Dataset Splitting ####
# Description: Splitting the dataset into training, calibration and validation dataset (STEMI + NSTEMI)
#Split training to 80%
training_partition <- createDataPartition(ds$ptoutcome, p = 0.8, list = FALSE)
training_ds <- ds[training_partition,]
remaining_ds <- ds[-training_partition,]

#Split calibration and validation 10% each
calib_valid_partition <-createDataPartition(remaining_ds$ptoutcome, p = 0.5, list = FALSE)
calibration_ds <- remaining_ds[calib_valid_partition,]
validation_ds <- remaining_ds[-calib_valid_partition,]

#### Saving processed files ####
write.csv(training_ds, './dataset/processed/training_ds.csv')
write.csv(calibration_ds, './dataset/processed/calibration_ds.csv')
write.csv(validation_ds, './dataset/processed/validation_ds.csv')







