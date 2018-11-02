


#' Multiple Filter ensemble algorithm, parallel version
#' 
#' @param x The dataset to train the algorithm.
#' @param y The binary variable output.
#' @param library List of algorithm ID strings(corresponding to available caret package algorithm names for caret trainControl function) to use in the multiple filter ensemble.
#' @return A list containing: "MF", samples to be kept by majority filter; "CF", samples to be kept by consensus filter, "full_res", the full list of discordant predictions
make_ensemble_parallel <- function(x, y, library){
  
  algApplyFn <- function(algorithmL, x, y){
    ctrl <- caret::trainControl(method = "cv", savePred=T) #, classProb=T
    
    mod <- NULL
    if(algorithmL == "nnet"){
      mod <- caret::train(x, y, method = algorithmL, trControl = ctrl, maxit = 500)
    }else{
      mod <- caret::train(x, y, method = algorithmL, trControl = ctrl)
    }
    
    all_predictions <- mod$pred
    predictions_to_use <- all_predictions[rowSums(do.call(cbind, lapply(names(mod$bestTune), function(x){print(x); return(all_predictions[,x] == mod$bestTune[[x]])}))) == length(mod$bestTune),]
    predictions_to_use <- predictions_to_use[order(predictions_to_use$rowIndex),]   # MAJOR FIX 10/15
    sum(predictions_to_use$pred == predictions_to_use$obs) / length(predictions_to_use$pred)
    
    concordant <- predictions_to_use$pred == predictions_to_use$obs
    accuracy <- sum(predictions_to_use$pred == predictions_to_use$obs)/length(predictions_to_use$obs)
    logger.info(msg = sprintf("MAIN - ALGO - ENSEMBLE - %s, accuracy = %f", algorithmL, accuracy))
    
    discordant <- predictions_to_use$pred != predictions_to_use$obs
    
    return(discordant)
  }
  
  
  count = 0
  list_of_results <- list()
  
  list_of_results = parallel::mclapply(library, function(alg){return(algApplyFn(alg, x, y))})
  names(list_of_results) <- make.unique(library)
  print(list_of_results)
  
  r <- do.call(cbind, list_of_results)
  print(rowSums(r))
  
  majority_filter <- rowSums(r)/ncol(r) > .6
  consensus_filter <- rowSums(r)/ncol(r) == 1
  
  keep_majority_filter <- !majority_filter
  keep_consensus_filter <- !consensus_filter
  
  return(list("MF" = keep_majority_filter, "CF" = keep_consensus_filter, "full_res" = r) )
}


#' Generate predictions based on an ensemble of classification algorithms; 
#' 
#' @param x training data; matrix of features/covariates used for prediction.
#' @param y training labels; must be factor of non-integer values i.e. "one"/"two" instead of 1/2
#' @param library A vector of strings indicating the algorithms to be included in the ensemble. Available algorithms can be queried here: https://topepo.github.io/caret/available-models.html
#' @param multiple Boolean value indicating whether to run multiple iterations of cross-validation to generate ensemble predictions; default = FALSE; If TRUE, will perform 5 iterations 
#' @return A list containing: "MF", samples to be kept by majority filter; "CF", samples to be kept by consensus filter, "full_res", the full list of discordant predictions
make_ensemble <- function(x, y, library, multiple = FALSE){
  count = 0
  list_of_results <- list()
  for(algorithmL in library){
    ctrl <- caret::trainControl(method = "cv", savePred=T) 
    
    if(multiple){
      print("generating ensemble with repeats")
      ctrl <- caret::trainControl(method = "repeatedcv", savePred=T, number = 10, repeats = 5) #classProb=T,
    }
    
    mod <- NULL
    if(algorithmL == "nnet"){
      mod <- caret::train(x, y, method = algorithmL, trControl = ctrl, maxit = 500)
    }else{
      mod <- caret::train(x, y, method = algorithmL, trControl = ctrl)
    }
    all_predictions <- mod$pred
    predictions_to_use <- all_predictions[rowSums(do.call(cbind, lapply(names(mod$bestTune), function(x){print(x); return(all_predictions[,x] == mod$bestTune[[x]])}))) == length(mod$bestTune),]
    predictions_to_use <- predictions_to_use[order(predictions_to_use$rowIndex),]   # MAJOR FIX 10/15
    sum(predictions_to_use$pred == predictions_to_use$obs) / length(predictions_to_use$pred)
    
    if(multiple){
      # collapse predictions_to_use
      n <- do.call(rbind, lapply(unique(predictions_to_use$rowIndex), function(x){b = predictions_to_use[predictions_to_use$rowIndex == x,]; a = which.max(table(b$pred)); print(names(a)); return(b[b$pred == names(a), ])}))
      n <- n[,c("pred","obs","rowIndex")]
      n <- n[!duplicated(n), ]
      predictions_to_use <- n
    }
    
    concordant <- predictions_to_use$pred == predictions_to_use$obs
    accuracy <- sum(predictions_to_use$pred == predictions_to_use$obs)/length(predictions_to_use$obs)
    logger.info(msg = sprintf("MAIN - ALGO - ENSEMBLE - %s, accuracy = %f", algorithmL, accuracy))
    
    discordant <- predictions_to_use$pred != predictions_to_use$obs
    
    if((algorithmL) %in% names(list_of_results)){
      count = count + 1
      list_of_results[[paste(algorithmL, count, sep = "")]] = discordant
    }else{
      list_of_results[[algorithmL]] = discordant
    }
  }
  
  r <- do.call(cbind, list_of_results)
  print(rowSums(r))
  
  majority_filter <- rowSums(r)/ncol(r) > .6
  consensus_filter <- rowSums(r)/ncol(r) == 1
  
  keep_majority_filter <- !majority_filter
  keep_consensus_filter <- !consensus_filter
  
  return(list("MF" = keep_majority_filter, "CF" = keep_consensus_filter, "full_res" = r) )
}
