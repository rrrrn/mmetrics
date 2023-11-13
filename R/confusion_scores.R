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
    # set the positive and negative values for input levels/labels
    if(is.logical(ele_all)|all(c(1, 0) %in% ele_all)){
      ele1 = TRUE
      ele2 = FALSE
    }
    else{
      ele1 = unique(ele_all)[1]
      ele2 = unique(ele_all)[2]
    }
  tp = ((preds == ele1) & (target == ele1))
  fn = ((preds == ele2) & (target == ele1))
  fp = ((preds == ele1) & (target == ele2))
  tn = ((preds == ele2) & (target == ele2))
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
    message("Incompatible multidim-average mode")
    return(NA)
  }
}
