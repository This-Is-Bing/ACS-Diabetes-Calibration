##### File Description ####
# Please run the rproj of this file
# This script is to filter the selected features and split into training, calibration and validation dataset

##### Cleaning Environment ####
rm(list=ls())


##### Loading Library ####
set.seed(2468)
library(readxl)
library(caret)


##### Loading Training Dataset ####
# Source: Chien Wen
# Type. : Diabetes
# Split : Testing

ds <- read_xlsx("./dataset/dm_testData_noNA_new.xlsx")

##### Dataset Preprocessing ####
# Description: Only taking the RF features and Model with 20 selected features
all_features <- read_xlsx('./dataset/NCVD_features_dm.xlsx')
all_features <- all_features[which(all_features$rf20_fet %in% c("other", "yes")),]

# Get selected features names and types
all_features_names <- all_features$VarNames[which(all_features$rf20_fet %in% c("yes"))]

# Select variables on DS
ds <- ds[,c(all_features_names,'acsstratum', 'timiscorestemi',"timiscorenstemi", "ptoutcome")]
ds$ptoutcome <- ifelse(ds$ptoutcome == "Alive", 0, ifelse(ds$ptoutcome == "Death", 1, NA))


#### Dataset Splitting ####
# Description: Splitting the dataset into training, calibration and validation dataset (STEMI + NSTEMI)
#Split training to 80%
testing_partition <- createDataPartition(ds$ptoutcome, p = 0.4, list = FALSE)
testing_ds <- ds[testing_partition,]
remaining_ds <- ds[-testing_partition,]

#Split calibration and validation 10% each
calib_valid_partition <-createDataPartition(remaining_ds$ptoutcome, p = 0.5, list = FALSE)
calibration_ds <- remaining_ds[calib_valid_partition,]
validation_ds <- remaining_ds[-calib_valid_partition,]

#### Saving processed files ####
# write.csv(testing_ds, './dataset/processed/testing_ds.csv', row.names = FALSE)
# write.csv(calibration_ds, './dataset/processed/calibration_ds.csv', row.names = FALSE)
# write.csv(validation_ds, './dataset/processed/validation_ds.csv', row.names = FALSE)








