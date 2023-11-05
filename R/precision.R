binary_precision <- function(preds, target, threshold=0.5,multidim_average = "global"){
  stopifnot(dim(preds)==dim(target))

  # transform probability into labels when necessary
  if(is.numeric(preds)&(!is.integer(preds))){
    preds <- as.numeric(preds>threshold)
  }

  cfs_mtx <- confusion_scores(preds, target, multidim_average)
  return((cfs_mtx$tp)/(cfs_mtx$tp+cfs_mtx$fp))
}

multiclass_precision <-function(preds, target, multidim_average = "global",
                                average = "micro"){
  # transform probability into labels when necessary
  if((length(dim(preds))==length(dim(target))+1)){
    # the last dimension always be the probabilities for each class
    preds = apply(preds, 1:(length(dim(preds))-1), which.max)
  }

  # validate the multiclass assumption
  ele_all <- unique(c(target, preds)) # element in the union of two vec
  stopifnot(length(ele_all)>=0)
  stopifnot(length(target)==length(preds))
  num_class = length(ele_all)

  if(length(ele_all)==1){
    tp <- length(preds)
    tn <- fp <- fn <-0
  }

  stopifnot(dim(preds)==dim(target))
  stopifnot(num_class>=length(unique(c(target))))

  if(multidim_average=="samplewise"|average=="micro"){
    precision_0 = (binary_precision(preds, target, multidim_average = multidim_average))
    return(precision_0)
  }
  else if(average=="macro"){
    label_precision = numeric(num_class)
    # label-wise accuracy calculation
    i = 1
    for(ele in ele_all){
      targetnew <- target[target==ele]
      predsnew <- preds[target==ele]
      label_precision[i] <- multiclass_confusion_scores(predsnew, targetnew, class=ele)
      i = i+1
    }
    return(mean(label_acc))
  }
}
