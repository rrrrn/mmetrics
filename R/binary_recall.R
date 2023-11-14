#' binary_recall
#'
#' @description Calculate the binary classification recall for a given predicted set of
#' values and corresponding targets. In other words, this function estimate the
#' sensitivity of the model prediction.
#'
#' @param preds Predicted labels or predicted probability between 0 and 1,
#' same shape as target label
#' @param target Target label
#' @param threshold The numerical cut-off between 0 and 1 to transform
#' predicted probability into binary predicted labels
#' @param multidim_average Average model: global-average across all computed recall scores,
#' samplewise-average across the all but the first dimensions (calculated
#' independently for each sample)
#'
#' @return Binary recall value for preds and target, with format dictated by
#' multidim_average command.
#'
#' @export
#'
#' @examples
#' binary_recall(c(0.8, 0.2), c(1,1), 0.3)
#' binary_recall(c(1,1), c(0,1))
binary_recall <- function(preds, target, threshold=0.5, multidim_average = "global"){

  stopifnot(dim(preds)==dim(target))

  # transform probability into labels when necessary
  if(is.numeric(preds)&(!is.integer(preds))){
    preds <- as.numeric(preds>=threshold)
  }

  cfs_mtx <- confusion_scores(preds, target, multidim_average)
  if(any((cfs_mtx$tp+cfs_mtx$fn==0))){
    message("NaN generated due to lack of true positive labels")
  }
  return((cfs_mtx$tp)/(cfs_mtx$tp+cfs_mtx$fn))
}
