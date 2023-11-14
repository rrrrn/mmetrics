#' multiclass_f1
#'
#' @description Calculate the multiclass f1 value for a given predicted set of
#' values and corresponding targets
#'
#' @param preds Predicted label with the same shape as target label, or
#' predicted probability between 0 and 1 for each class that has one
#' additional dimension compared with target label
#' @param target Target label
#' @param average Defines the reduction that is applied over labels.
#' Micro-sum over all class labels, that is all true positives for each class divided
#' by all positive predicted values for each class.
#' Macro-calculate class label-wise f1 scores and then take the average.
#' @param multidim_average Average model: global-average across all f1 scores,
#' samplewise-average across the all but the first dimensions (calculated
#' independently for each sample)
#'
#' @return Multiclass f1-score for preds and target, with format dictated by
#' multidim_average argument and average methods choice.
#'
#' @export
#'
#' @examples
#' y_pred = matrix(c(0.1, 0.5, 0.4, 0.9, 0.2, 0.8), 2,3)
#' y_target = c(2,1)
#' multiclass_f1(y_pred, y_target)
multiclass_f1 <-function(preds, target, multidim_average = "global",
                             average = "micro"){
  # transform probability into labels when necessary
  if((length(dim(preds))==length(dim(target))+1)|(length(dim(preds))>=2&is.null(dim(target)))){
    # the last dimension always be the probabilities for each class
    preds = apply(preds, 1:(length(dim(preds))-1), which.max)
  }

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

    if(average=="micro"|length(ele_all)==1){
      cfsmtx <- multiclass_confusion_scores(preds, target)
      tp <- sum(diag(cfsmtx))
      return((tp/sum(cfsmtx)))
    }
    else if(average=="macro"){
      label_f1 = numeric(num_class)
      # label-wise accuracy calculation
      for(i in 1:num_class){
        classtype = ifelse(fact, i, ele_all[i])
        cfsmtx <- multiclass_confusion_scores(preds, target, classtype=classtype)
        recallscore = cfsmtx$tp/(cfsmtx$tp+cfsmtx$fn)
        precisscore = cfsmtx$tp/(cfsmtx$tp+cfsmtx$fp)
        label_f1[i] <- 2*recallscore*precisscore/(recallscore+precisscore)
      }
      return(mean(label_f1))
    }
  }

  if(multidim_average=="global"){
    return(comp_assist(cbind(preds,target), average))
  }
  else if(multidim_average=="samplewise"){
    result = apply(cbind(preds,target), 1, comp_assist, average = average)
    return(result)
  }
  else{
    message("Incompatible multidim-average mode")
    return(NA)
  }
}
