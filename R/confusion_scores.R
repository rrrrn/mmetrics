#' confusion_scores
#'
#' @description Calculate confusion matrix for a given predicted set of
#' values and corresponding targets, mainly suitable for binary classification task.
#'
#' @param preds Predicted label, same shape as target label
#' @param target Target label
#' @param multidim_average Average model: global-average across all accuracies,
#' samplewise-average across the all but the first dimensions (calculated
#' independently for each sample)
#'
#' @return A list contains matrix-confusion matrix, tp-true positives,
#' fn-false negatives, fp-false positives, tn-true negatives
#'
#' @export
#'
#' @examples
#'
#' y_pred = c(1,1,0,1,1)
#' y_target = c(0,1,0,1,0)
#' confusion_scores(y_pred, y_target)
confusion_scores <- function(preds, target, multidim_average="global"){
  # validate binary assumption
  ele_all <- unique(c(target, preds)) # element in the union of two vec
  stopifnot(length(ele_all)<=2)
  stopifnot(length(target)==length(preds))

  if(length(ele_all)==1){
    tp <- length(preds)
    tn <- fp <- fn <-0
  }
  else{
  # compute confusion matrix value
  ele1 = unique(ele_all)[1]
  ele2 = unique(ele_all)[2]
  tp = ((target == preds) & (target == ele1))
  fn = ((target != preds) & (target == ele1))
  fp = ((target != preds) & (target == ele2))
  tn = ((target == preds) & (target == ele2))
}
  # average according to multidim_average command
  if(multidim_average=="global"){
    tp = sum(tp); fn = sum(fn); fp = sum(fp); tn = sum(tn)
    cfsmtx = matrix(c(tp, fn, fp, tn), 2, 2)
    return(list(matrix = cfsmtx, tp=tp, fp=fp, fn=fn, tn=tn))
  }
  else if(multidim_average=="samplewise"){
    # sum over all but the first dimension to achieve sample-wise summation
    tp = apply(tp,1,sum); fn = apply(fn,1,sum); fp = apply(fp,1,sum); tn = apply(tn,1,sum)
    return(list(matrix=NULL, tp=tp, fp=fp, fn=fn, tn=tn))
  }
  else{
    errorCondition("Incompatible multidim-average mode")
  }
}

#' multiclass_confusion_scores
#'
#' @description Calculate confusion matrix for a given predicted set of
#' values and corresponding targets in multiclass classification task
#'
#' @param preds Predicted label, same shape as target label
#' @param target Target label
#' @param multidim_average Average model: global-average across all accuracies,
#' samplewise-average across the all but the first dimensions (calculated
#' independently for each sample)
#' @param num_class The number of classes involved in the task
#' @param class If multidim_average is set to "samplewise", this param specifies
#' particular class of interest to compute confusion matrix
#'
#' @return If under "global" average mode, the cross-sample multiclass confusion
#' matrix for all classes will be returned. Else if average is taken sample-wise,
#' only the confusion matrix for particular class-of-interest, as well as TP, TN,
#' FN, FP for all samples will be returned in a list.
#' @export
multiclass_confusion_scores <- function(preds, target, class=NULL,
                                        multidim_average = "global"){

  ele_all <- factor(unique(c(target, preds))) # element in the union of two vec
  stopifnot(length(ele_all)>0)
  stopifnot(length(target)==length(preds))

  if(multidim_average=="global"){
    preds <- factor(preds, levels = ele_all)
    target <- factor(target, levels = ele_all)
    cfs_mtx <- table(preds, target)
    if(length(class)==0){
      return(cfs_mtx)
    }
    else if(length(class)==1){
      tp = cfs_mtx[class, class]
      fp = sum(cfs_mtx[class, ])-tp
      fn = sum(cfs_mtx[, class])-tp
      tn = sum(cfs_mtx)-tp-fp-fn
      cfsmtx = matrix(c(tp, fn, fp, tn), 2, 2)
      return(list(matrix = cfsmtx, tp=tp, fn=fn, fp=fp, tn=tn))
    }
  }
  else if(multidim_average=="samplewise"&length(class)==1){
    dimpred = dim(preds)
    prednew <- matrix(as.numeric(preds==class), dimpred[1], dimpred[2])
    targetnew <- matrix(as.numeric(target==class), dimpred[1], dimpred[2])
    return(confusion_scores(prednew, targetnew, multidim_average = multidim_average))
  }
  else{
    errorCondition("Invalid Input")
  }
}
