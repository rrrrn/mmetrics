
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

  if(!is.factor(preds)){
    ele_all <- factor(unique(c(target, as.vector(preds)))) # element in the union of two vec
  }
  else{
    ele_all <- unique(c(levels(target), levels(preds)))
  }
  num_class = length(ele_all)

  stopifnot(dim(preds)[1]==dim(target)[1])
  stopifnot(dim(preds)==dim(target))
  stopifnot(num_class>0)

  # generalized steps for computing scores
  comp_assist = function(datamtx, average){
    if(length(dim(datamtx))==1|is.null(dim(datamtx))){
      n = length(datamtx)/2
      preds = datamtx[1:n]
      target = datamtx[(n+1):(2*n)]
    }
    else{
      n <- ncol(datamtx)/2
      preds <- datamtx[,1:n]
      target <- datamtx[,(n+1):(2*n)]
    }

    if(average=="micro"){
      return(mean(preds==target))
    }
    else if(average=="macro"){
      label_acc <- numeric(num_class)
      # label-wise accuracy calculation
      for(i in 1:num_class){
        predsnew = preds[target==i]
        targetnew = target[target==i]
        label_acc[i] <- ifelse(length(targetnew==predsnew)>0, mean(targetnew==predsnew),0)
      }
      return(mean(label_acc))
    }
  }

  if(multidim_average=="global"){
    return(comp_assist(cbind(preds,target), average))
  }
  else if(multidim_average=="samplewise"){
    return(apply(cbind(preds,target), 1, comp_assist, average = average))
  }
}
