# All Functions used for this Project

#### Structuring Function ####
# This function is to structure ACS dataset into categorical and continuous dataset
StructuringFunction <- function(dataset, cat_nb_features_names, cat_features_names, cont_features_names) {
  # Change the types of variables in dataset
  #categorical non binary
  dataset[,cat_nb_features_names] <- lapply(dataset[,cat_nb_features_names], as.factor)
  
  #categorical
  dataset[,cat_features_names] <- ifelse(dataset[,cat_features_names] == 1, 1, 0)
  dataset[,cat_features_names] <- lapply(dataset[,cat_features_names], as.factor)
  
  #continuos & ordinal
  zscore_scale <- preProcess(dataset[,cont_features_names],method=c("center", "scale"))
  dataset[,cont_features_names] <- predict(zscore_scale, dataset[,cont_features_names])
  
  # Changing ptoutcome to binary
  dataset$ptoutcome <- as.factor(dataset$ptoutcome)
  
  return(dataset)
}

#### STEMI Structuring Function ####
StemiStructuringFunction <- function(dataset, cat_nb_features_names, cat_features_names, cont_features_names) {
  # Change the types of variables in dataset
  #categorical non binary
  dataset[,cat_nb_features_names] <- lapply(dataset[,cat_nb_features_names], as.factor)
  
  #categorical
  dataset[,cat_features_names] <- ifelse(dataset[,cat_features_names] == 1, 1, 0)
  dataset[,cat_features_names] <- lapply(dataset[,cat_features_names], as.factor)
  
  #continuos & ordinal
  zscore_scale <- preProcess(dataset[,cont_features_names],method=c("center", "scale"))
  dataset[,cont_features_names] <- predict(zscore_scale, dataset[,cont_features_names])
  
  # Changing ptoutcome to binary
  dataset$timiscorestemi <- as.factor(dataset$timiscorestemi)
  
  return(dataset)
}

#### NSTEMI Structuring Function ####
NStemiStructuringFunction <- function(dataset, cat_nb_features_names, cat_features_names, cont_features_names) {
  # Change the types of variables in dataset
  #categorical non binary
  dataset[,cat_nb_features_names] <- lapply(dataset[,cat_nb_features_names], as.factor)
  
  #categorical
  dataset[,cat_features_names] <- ifelse(dataset[,cat_features_names] == 1, 1, 0)
  dataset[,cat_features_names] <- lapply(dataset[,cat_features_names], as.factor)
  
  #continuos & ordinal
  zscore_scale <- preProcess(dataset[,cont_features_names],method=c("center", "scale"))
  dataset[,cont_features_names] <- predict(zscore_scale, dataset[,cont_features_names])
  
  # Changing ptoutcome to binary
  dataset$timiscorenstemi <- as.factor(dataset$timiscorenstemi)
  
  return(dataset)
}
