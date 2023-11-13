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
#' @param classtype If multidim_average is set to "samplewise", this param specifies
#' particular class of interest to compute confusion matrix
#'
#' @return If under "global" average mode, the cross-sample multiclass confusion
#' matrix for all classes will be returned. Else if average is taken sample-wise,
#' only the confusion matrix for particular class-of-interest, as well as TP, TN,
#' FN, FP for all samples will be returned in a list.
#' @export
#' @examples
#'
#' y_pred = c("A","B","C","A","B")
#' y_target = rep("A", 5)
#' multiclass_confusion_scores(y_pred, y_target, classtype="A")
multiclass_confusion_scores <- function(preds, target, classtype=NULL,
                                        multidim_average = "global"){
  if(!is.factor(preds)){
    ele_all <- factor(unique(c(target, as.vector(preds)))) # element in the union of two vec
  }
  else{
    ele_all <- unique(c(levels(target), levels(preds)))
  }
  stopifnot(length(ele_all)>0)
  stopifnot(length(target)==length(preds))

  if(multidim_average=="global"){
    preds <- factor(preds, levels = ele_all)
    target <- factor(target, levels = ele_all)
    cfs_mtx <- table(preds, target)
    if(length(classtype)==0|length(ele_all)==1){
      return(cfs_mtx)
    }
    else if(length(classtype)==1){
      classtype=as.character(classtype)
      tp = cfs_mtx[classtype, classtype]
      fp = sum(cfs_mtx[classtype, ])-tp
      fn = sum(cfs_mtx[, classtype])-tp
      tn = sum(cfs_mtx)-tp-fp-fn
      cfsmtx = matrix(c(tp, fn, fp, tn), 2, 2)
      return(list(matrix = cfsmtx, tp=tp, fn=fn, fp=fp, tn=tn))
    }
  }
  else if(multidim_average=="samplewise"&length(classtype)==1){
    dimpred = dim(preds)
    prednew <- matrix(as.numeric(preds==classtype), dimpred[1], dimpred[2])
    targetnew <- matrix(as.numeric(target==classtype), dimpred[1], dimpred[2])
    return(confusion_scores(prednew, targetnew, multidim_average = multidim_average))
  }
  else{
    message("Invalid Input")
    return(NaN)
  }
}
