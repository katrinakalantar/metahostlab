
#' Create a cross-validation dataset from a known GEO dataset with a covariate of interest.
#' 
#' @param geo_dataset_name The dataset name set initially.
#' @param geo_dataset_list The pre-loaded list of GEO datasets.
#' @param pos_regex A regular expression (string) that selects for the positive class in the GEO dataset metadta field specified by source_variable arg
#' @param neg_regex A regular expression (string) that selects for the negative class in the GEO dataset metadta field specified by source_variable arg
#' @param source_variable The GEO dataset variable that will be used to split the dataset.
#' @param flip_i Decimal value indicating the proportion of samples of class B to flip to class A.
#' @param flip_j Decimal value indicating the proportion of samples of class B to flip to class A.
#' @return List of datasets split for cross-validation with training and test sets, including flipped labels at specified percentage.
subset_geo_cv <- function(geo_dataset_name, geo_dataset_list, pos_regex, neg_regex, source_variable, flip_i, flip_j){
  
  geo_data <- geo_dataset_list[[geo_dataset_name]]
    
  split <- strsplit(geo_dataset_name, "_")
  pos_regex <- split[[1]][1]
  neg_regex <- split[[1]][2]
    
  pos_split <- split_train_test_cv( geo_data[, grep(pos_regex, Biobase::pData(geo_data)[,source_variable])], pos_regex, source_variable, flip_i, flip_j)
  neg_split <- split_train_test_cv( geo_data[, grep(neg_regex, Biobase::pData(geo_data)[,source_variable])], pos_regex, source_variable, flip_i, flip_j)
  
  full_dataset <- list()
  for(i in seq(1:length(pos_split))){
    
    full_train <- Biobase::combine(pos_split[[i]]$training_set, neg_split[[i]]$training_set)
    full_test <- Biobase::combine(pos_split[[i]]$test_set, neg_split[[i]]$test_set)
    
    true_labels_train <- rep(0, length(Biobase::pData(full_train)[,source_variable]))
    true_labels_train[grep(pos_regex, Biobase::pData(full_train)[,source_variable])] <- 1
    true_labels_train <- true_labels_train + 1
    
    flipped_labels_train <- c((pos_split[[i]]$training_flipped_labels),(neg_split[[i]]$training_flipped_labels))
    flipped_markers_train <- c((pos_split[[i]]$training_flipped_markers),(neg_split[[i]]$training_flipped_markers))
    
    flipped_labels_test <- c((pos_split[[i]]$test_flipped_labels),(neg_split[[i]]$test_flipped_labels))
    flipped_markers_test <- c((pos_split[[i]]$test_flipped_markers),(neg_split[[i]]$test_flipped_markers))
    
    true_labels_test <- rep(0, length(Biobase::pData(full_test)[,source_variable]))
    true_labels_test[grep(pos_regex, Biobase::pData(full_test)[,source_variable])] <- 1
    true_labels_test <- true_labels_test + 1
    
    full_dataset[[i]] <- list("x" = t(as.matrix(Biobase::exprs(full_train))), "y" = as.matrix(true_labels_train), "yz" = as.matrix(flipped_labels_train), "ff" = as.matrix(flipped_markers_train),
                              "xx" = t(as.matrix(Biobase::exprs(full_test))), "tt" = as.matrix(true_labels_test), "tz" = as.matrix(flipped_labels_test), "tf" = as.matrix(flipped_markers_test))
    
  }
  return(full_dataset)
  
}


#' Given a dataset, split it into training and test sets randomly.
#' 
#' @param input Dataset
#' @param regex The regular expression for this class.
#' @param source_variable The GEO dataset variable that will be used to split the dataset.
#' @param flip_i Decimal value indicating the proportion of samples of class B to flip to class A.
#' @param flip_j Decimal value indicating the proportion of samples of class B to flip to class A.
#' @param cv The number of cross-validation splits to return; default = 10.
#' @return List of datasets split for cross-validation with training and test sets, including flipped labels at specified percentage.
split_train_test_cv <- function(input, regex, source_variable, flip_i, flip_j, cv=10){
  s <- permute::shuffle(seq(1:dim(input)[2]))
  cv_datasets <- list()
  test_splits <- split(s, sort(s%%cv))
  
  true_labels <- rep(0, length(Biobase::pData(input)[,source_variable]))
  true_labels[grep(regex, Biobase::pData(input)[,source_variable])] <- 1
  true_labels <- true_labels + 1
  
  a = inject_label_noiseR(true_labels, flip_i, flip_j)
  flipped_labels <- a$yz
  flipped_markers <- a$fd
  print(paste("flip_i = ", flip_i, " / flip_j = ", flip_j))
  print(table(flipped_markers)[2]/(table(flipped_markers)[1]+table(flipped_markers)[2]))
  
  for(i in seq(1:length(test_splits))){
    training_set <- input[,s[!(s %in% test_splits[[i]])]]
    training_flipped_labels <- flipped_labels[s[!(s %in% test_splits[[i]])]]
    training_flipped_markers <- flipped_markers[s[!(s %in% test_splits[[i]])]]
    
    test_flipped_labels <- flipped_labels[s[(s %in% test_splits[[i]])]]
    test_flipped_markers <- flipped_markers[s[(s %in% test_splits[[i]])]]
    
    test_set <- input[,test_splits[[i]]]
    cv_datasets[[i]] <- list("training_set" = training_set, "test_set" = test_set, "training_flipped_labels" = training_flipped_labels, "training_flipped_markers" = training_flipped_markers,
                             "test_flipped_labels" = test_flipped_labels, "test_flipped_markers" = test_flipped_markers)
  }
  return(cv_datasets)
}