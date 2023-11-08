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
