#' binary_f1
#'
#' @description Calculate the binary classification f1-score for a given predicted set of
#' values and corresponding targets. In other words, this function estimate how accurate
#' the true prediction value by the model is.
#'
#' @param preds Predicted labels or predicted probability between 0 and 1,
#' same shape as target label
#' @param target Target label
#' @param threshold The numerical cut-off between 0 and 1 to transform
#' predicted probability into binary predicted labels
#' @param multidim_average Average model: global-average across all f1s,
#' samplewise-average across the all but the first dimensions (calculated
#' independently for each sample)
#'
#' @return f1score value for binary preds and target input, with format dictated by
#' multidim_average command.
#'
#' @export
#'
#' @examples
#' binary_f1(c(0.8, 0.2), c(1,1), 0.3)
#' binary_f1(c(1,1), c(0,1))
binary_f1 <- function(preds, target, threshold=0.5, multidim_average = "global"){

  stopifnot(dim(preds)==dim(target))

  # transform probability into labels when necessary
  if(is.numeric(preds)&(!is.integer(preds))){
    preds <- as.numeric(preds>=threshold)
  }

  cfs_mtx <- confusion_scores(preds, target, multidim_average)
  # handle exceptions
  if(any((cfs_mtx$tp+cfs_mtx$fn==0))){
    message("NaN generated due to lack of true positive labels")
  }
  else if(any((cfs_mtx$tp+cfs_mtx$fp==0))){
    message("NaN generated due to lack of positively predicted labels")
  }
  else if(any(cfs_mtx$tp==0)){
    message("NaN generated due to lack of accurately positive predicted positive labels")
  }
  recall = (cfs_mtx$tp)/(cfs_mtx$tp+cfs_mtx$fn)
  precis = (cfs_mtx$tp)/(cfs_mtx$tp+cfs_mtx$fp)

  return(2*recall*precis/(precis+recall))
}
