#' multiclass_precision
#'
#' @description Calculate the multiclass precision value for a given predicted set of
#' values and corresponding targets
#'
#' @param preds Predicted label with the same shape as target label, or
#' predicted probability between 0 and 1 for each class that has one
#' additional dimension compared with target label
#' @param target Target label that has been transformed into dinstinct integers
#' to refer to each class
#' @param average Defines the reduction that is applied over labels.
#' Micro-sum over all class labels, that is all true positives for each class divided
#' by all positive predicted values for each class.
#' Macro-calculate class label-wise precision scores and then take the average.
#' @param multidim_average Average model: global-average across all precision scores,
#' samplewise-average across the all but the first dimensions (calculated
#' independently for each sample)
#'
#' @return Multiclass precision for preds and target, with format dictated by
#' multidim_average argument and average methods choice.
#'
#' @export
#'
#' @examples
#' y_pred = matrix(c(0.1, 0.5, 0.4, 0.9, 0.2, 0.8), 2,3)
#' y_target = c(2,1)
#' multiclass_precision(y_pred, y_target)
multiclass_precision <-function(preds, target, multidim_average = "global",
                                average = "micro"){
  # transform probability into labels when necessary
  if((length(dim(preds))==length(dim(target))+1)|(length(dim(preds))>=2&is.null(dim(target)))){
    # the last dimension always be the probabilities for each class
    preds = apply(preds, 1:(length(dim(preds))-1), which.max)
  }

  # validate the multiclass assumption
  # retrieve all unique labels occurred in prediction and target labels
  if(!is.factor(preds)){
    fact=FALSE
    ele_all <- (unique(c(target, as.vector(preds)))) # element in the union of two vec
  }
  else{
    fact=TRUE
    ele_all <- unique(c(levels(target), levels(preds)))
  }
  num_class = length(ele_all)

  stopifnot(dim(preds)[1]==dim(target)[1])
  stopifnot(dim(preds)==dim(target))
  stopifnot(num_class>0)

  if(length(ele_all)==1){
    tp <- length(preds)
    tn <- fp <- fn <-0
    return(1)
  }
  print(preds)

  # generalized steps for computing scores
  comp_assist = function(datamtx, average){
    if(length(dim(datamtx))==1|is.null(dim(datamtx))){
      n = length(datamtx)/2
      preds = datamtx[1:n]
      target = datamtx[(n+1):(2*n)]
    }
    else{
      n = ncol(datamtx)/2
      preds = datamtx[,1:n]
      target = datamtx[,(n+1):(2*n)]
    }

    # average methods with regard to multiclass labels
    if(average=="micro"|length(ele_all)==1){
      cfsmtx <- multiclass_confusion_scores(preds, target)
      tp <- sum(diag(cfsmtx))
      return((tp/sum(cfsmtx)))
    }
    else if(average=="macro"){
      label_prec = numeric(num_class)
      # label-wise accuracy calculation
      for(i in 1:num_class){
        classtype = ifelse(fact, i, ele_all[i])
        cfsmtx <- multiclass_confusion_scores(preds, target, classtype=classtype)
        label_prec[i] <- cfsmtx$tp/(cfsmtx$tp+cfsmtx$fp)
      }
      return(mean(label_prec))
    }
  }

  if(multidim_average=="global"){
    return(comp_assist(cbind(preds,target), average))
  }
  else if(multidim_average=="samplewise"){
    return(apply(cbind(preds,target),
                 1, comp_assist, average = average))
  }
}
