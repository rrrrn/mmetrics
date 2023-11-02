pearson_corr <- function(preds, target){
  # check the validity of input


  d <- ifelse(length(dim(preds)) == 2, dim(preds)[1], 1)
  temp <- numeric(d)

  # check the input shape
}
