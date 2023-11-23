# All Functions used for this Project

#### Transformation Function ####
# This function is to transform ACS dataset into categorical and continuous dataset
RF20_TransformationFunction <- function(dataset, all_features) {
  #get all features for rf20
  all_features <- all_features[which(all_features$rf20_fet %in% c("other", "yes")),]
  
  # Get selected features names with types
  all_features_names <- all_features$VarNames[which(all_features$rf20_fet %in% c("yes"))]
  cat_nb_features_names <- all_features$VarNames[which(all_features$Cat_Cont_Ord == "categorical_nonBinary")]
  cat_features_names<- all_features$VarNames[which(all_features$Cat_Cont_Ord == "categorical_binary")] 
  cont_features_names <- all_features$VarNames[which(all_features$Cat_Cont_Ord %in% c("continuous", "ordinal"))]
  
  # Change the types of variables in dataset
  #categorical non binary
  dataset[,cat_nb_features_names]<- factor(dataset[,cat_nb_features_names])
  
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



#### Transformation Function ####
# This function is to structure ACS dataset into categorical and continuous dataset
TransformationFunction <- function(dataset, cat_nb_features_names, cat_features_names, cont_features_names) {
  # Change the types of variables in dataset
  #categorical non binary
  dataset[,cat_nb_features_names]<- factor(dataset[,cat_nb_features_names])
  
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

#### STEMI Transformation Function ####
StemiTransformationFunction <- function(dataset, cat_nb_features_names, cat_features_names, cont_features_names) {
  # Change the types of variables in dataset
  #categorical non binary
  dataset[,cat_nb_features_names]<- factor(dataset[,cat_nb_features_names])
  
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

#### NSTEMI Transformation Function ####
NStemiTransformationFunction <- function(dataset, cat_nb_features_names, cat_features_names, cont_features_names) {
  # Change the types of variables in dataset
  #categorical non binary
  dataset[,cat_nb_features_names]<- factor(dataset[,cat_nb_features_names])
  
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


#### Evaluation ####
Evaluation <- function(label, predicted, rowname){
  
  label <- as.numeric(label)-1
  predicted <- as.numeric(predicted)
  
  roc_curve <- roc(label, predicted)
  
  # Get the coordinates of the point on the ROC curve that is closest to the top-left corner
  coords_best <- coords(roc_curve, "best", best.method = "closest.topleft")
  
  # Access the threshold value from the coordinates
  auc <- roc_curve$auc
  
  # Get the best threshold from the roc curve
  threshold <-  coords_best$threshold
  
  # Confusion Matrix
  pred_class <- factor(ifelse(predicted >= threshold, 'Death', 'Alive'))
  
  label_fac <- factor(ifelse(label == 1, 'Death', 'Alive'))
  
  cm <- confusionMatrix(data = pred_class, reference = label_fac)
  acc <- cm[['overall']]['Accuracy']
  mcnemar <- cm[['overall']]['McnemarPValue']
  sensitivity <- cm[['byClass']]['Sensitivity']
  specificity <- cm[['byClass']]['Specificity']
  ppv <- cm[['byClass']]['Pos Pred Value']
  npv <- cm[['byClass']]['Neg Pred Value']
  
  # Hosmer Lemeshow Test 
  # Define a small epsilon value
  epsilon <- 1e-15
  
  # Adjust probabilities to avoid log(0)
  adjusted_pred_risk <- pmax(pmin(predicted, 1 - epsilon), epsilon)
  
  # Calculate the Brier score manually
  brier_score <- mean((label - adjusted_pred_risk)^2)
  
  # Calculate the logLoss manually
  logloss <- -mean(label * log(adjusted_pred_risk) + (1 - label) * log(1 - adjusted_pred_risk))

  #### Hosmer Lemeshow Test ####
  hlscore <- hoslem.test(label,predicted, g=10)
  chisquare = hlscore$statistic
  calib_pvalue = hlscore$p.value
  brier = brier_score
  logloss =logloss
  
  result <- data.frame(threshold, 
                       auc, 
                       acc, 
                       sensitivity, 
                       specificity, 
                       ppv, 
                       npv, 
                       mcnemar, 
                       chisquare, 
                       calib_pvalue, 
                       brier,
                       logloss, 
                       row.names = rowname )
  return (result)
}

#### Evaluation ####
Evaluation <- function(label, predicted, threshold, rowname){
  
  label <- as.numeric(label)-1
  predicted <- as.numeric(predicted)
  
  roc_curve <- roc(label, predicted)
  
  # Get the coordinates of the point on the ROC curve that is closest to the top-left corner
  coords_best <- coords(roc_curve, "best", best.method = "closest.topleft")
  
  # Access the threshold value from the coordinates
  auc <- roc_curve$auc
  
  # Get the best threshold from the roc curve
  # threshold <-  coords_best$threshold
  
  # Confusion Matrix
  pred_class <- factor(ifelse(predicted >= threshold, 'Death', 'Alive'))
  
  label_fac <- factor(ifelse(label == 1, 'Death', 'Alive'))
  
  cm <- confusionMatrix(data = pred_class, reference = label_fac)
  acc <- cm[['overall']]['Accuracy']
  mcnemar <- cm[['overall']]['McnemarPValue']
  sensitivity <- cm[['byClass']]['Sensitivity']
  specificity <- cm[['byClass']]['Specificity']
  ppv <- cm[['byClass']]['Pos Pred Value']
  npv <- cm[['byClass']]['Neg Pred Value']
  
  # Hosmer Lemeshow Test 
  # Define a small epsilon value
  epsilon <- 1e-15
  
  # Adjust probabilities to avoid log(0)
  adjusted_pred_risk <- pmax(pmin(predicted, 1 - epsilon), epsilon)
  
  # Calculate the Brier score manually
  brier_score <- mean((label - adjusted_pred_risk)^2)
  
  # Calculate the logLoss manually
  logloss <- -mean(label * log(adjusted_pred_risk) + (1 - label) * log(1 - adjusted_pred_risk))
  
  #### Hosmer Lemeshow Test ####
  hlscore <- hoslem.test(label,predicted, g=10)
  chisquare = hlscore$statistic
  calib_pvalue = hlscore$p.value
  brier = brier_score
  logloss =logloss
  
  result <- data.frame(threshold, 
                       auc, 
                       acc, 
                       sensitivity, 
                       specificity, 
                       ppv, 
                       npv, 
                       mcnemar, 
                       chisquare, 
                       calib_pvalue, 
                       brier,
                       logloss, 
                       row.names = rowname )
  return (result)
}

SearchBestThreshold <- function(label, predicted) {
  label <- as.numeric(label) - 1
  predicted <- as.numeric(predicted)
  
  best_threshold <- 0
  best_f1 <- 0
  
  for (threshold in seq(0, 1, by = 0.001)) {
    # Convert predicted probabilities to binary predictions using the threshold
    pred_class <- factor(ifelse(predicted >= threshold, 'Death', 'Alive'),
                         levels = c('Death', 'Alive'))
    label_fac <- factor(ifelse(label == 1, 'Death', 'Alive'),
                        levels = c('Death', 'Alive'))
    
    # Calculate precision, recall, and F1 score
    
    cm <- confusionMatrix(data = pred_class, reference = label_fac)
    if (!is.na(cm[['byClass']]['Pos Pred Value']) && !is.na(cm[['byClass']]['Sensitivity'])) {
      precision <- cm[['byClass']]['Pos Pred Value']
      recall <- cm[['byClass']]['Sensitivity']
      f1 <- 2 * (precision * recall) / (precision + recall)
    
      if (!is.na(f1) && f1 > best_f1) {
        best_f1 <- f1
        best_threshold <- threshold
        best_cm <- cm
      }
    }
  }
  
  # Print the best F1 score and corresponding threshold
  cat("Best F1 Score:", best_f1, "\n")
  cat("Best Threshold:", best_threshold, "\n")
  
  return(best_threshold)
}
