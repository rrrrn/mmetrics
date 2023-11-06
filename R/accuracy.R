#' binary_acc
#'
#' @description Calculate the binary class accuracy for a given predicted set of
#' values and corresponding targets
#'
#' @param preds Predicted label or predicted probability between 0 and 1,
#' same shape as target label
#' @param target Target label
#' @param threshold The numerical cut-off between 0 and 1 to transform
#' predicted probability into binary predicted labels
#' @param multidim_average Average model: global-average across all accuracies,
#' samplewise-average across the all but the first dimensions (calculated
#' independently for each sample)
#'
#' @return Binary accuracy for preds and target, with format dictated by
#' multidim_average command.
#'
#' @export
#'
#' @examples
#' binary_acc(c(0.8, 0.2), c(1,1), 0.3)
#' binary_acc(c(1,1), c(0,1))
binary_acc <- function(preds, target, threshold=0.5, multidim_average = "global"){

  stopifnot(dim(preds)==dim(target))

  # transform probability into labels if necessary
  if(is.numeric(preds)&(!is.integer(preds))){
    preds <- as.numeric(preds>threshold)
  }

  cfs_mtx <- confusion_scores(preds, target, multidim_average)

  if(multidim_average == "global"){
    return(sum(diag(cfs_mtx$matrix))/sum(cfs_mtx$matrix))
  }
  else{
    return((cfs_mtx$tp+cfs_mtx$tn)/(cfs_mtx$tp+cfs_mtx$tn+cfs_mtx$fn+cfs_mtx$fp))
  }
}

#' multiclass_acc
#'
#' @description Calculate the multiclass accuracy for a given predicted set of
#' values and corresponding targets
#'
#' @param preds Predicted label with the same shape as target label, or
#' predicted probability between 0 and 1 for each class that has one
#' additional dimension compared with target label
#' @param target Target label that has been transformed into dinstinct integers
#' to refer to each class
#' @param average Defines the reduction that is applied over labels.
#' Micro-sum over all class labels.
#' Macro-calculate class label-wise statistics and then take the average.
#' The parameter average makes an effect in calculation only when the accuracy
#' is required on a global level
#' @param multidim_average Average model: global-average across all accuracies,
#' samplewise-average across the all but the first dimensions (calculated
#' independently for each sample)
#'
#' @return Multiclass accuracy for preds and target, with format dictated by
#' multidim_average command and average methods choice.
#'
#' @export
#'
#' @examples
#' y_pred = matrix(c(0.1, 0.5, 0.4, 0.9, 0.2, 0.8), 2,3)
#' y_target = c(2,1)
#' multiclass_acc(y_pred, y_target)
multiclass_acc <- function(preds, target, multidim_average = "global",
                           average = "micro"){

  # transform probability into labels when necessary
  if((length(dim(preds))==length(dim(target))+1)|(length(dim(preds))>=2&is.null(dim(target)))){
    # the last dimension always be the probabilities for each class
    preds = apply(preds, 1:(length(dim(preds))-1), which.max)
  }

  ele_all = unique(c(preds, target))
  num_class = length(ele_all)

  stopifnot(dim(preds)[1]==dim(target)[1])
  stopifnot(dim(preds)==dim(target))
  stopifnot(num_class>0)

  if(multidim_average=="global"&average=="micro"){
    return(mean(preds==target))
  }
  else if(multidim_average=="global"&average=="macro"){
    label_acc = numeric(num_class)
    # label-wise accuracy calculation
    for(i in 1:num_class){
      targetnew <- target[target==ele_all[i]]
      predsnew <- preds[target==ele_all[i]]
      label_acc[i] <- ifelse(length(targetnew==predsnew)>0, mean(targetnew==predsnew),0)
    }
    return(mean(label_acc))
  }
  else if(multidim_average=="samplewise"){
    return(apply(target==preds, 1, mean))
  }
}
