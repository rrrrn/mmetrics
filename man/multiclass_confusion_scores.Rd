% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiclass_confusion_scores.R
\name{multiclass_confusion_scores}
\alias{multiclass_confusion_scores}
\title{multiclass_confusion_scores}
\usage{
multiclass_confusion_scores(
  preds,
  target,
  classtype = NULL,
  multidim_average = "global"
)
}
\arguments{
\item{preds}{Predicted label, same shape as target label}

\item{target}{Target label}

\item{classtype}{If multidim_average is set to "samplewise", this param specifies
particular class of interest to compute confusion matrix}

\item{multidim_average}{Average model: global-average across all accuracies,
samplewise-average across the all but the first dimensions (calculated
independently for each sample)}
}
\value{
If under "global" average mode, the cross-sample multiclass confusion
matrix for all classes will be returned. Else if average is taken sample-wise,
only the confusion matrix for particular class-of-interest, as well as TP, TN,
FN, FP for all samples will be returned in a list.
}
\description{
Calculate confusion matrix for a given predicted set of
values and corresponding targets in multiclass classification task
}
\examples{

y_pred = c("A","B","C","A","B")
y_target = rep("A", 5)
multiclass_confusion_scores(y_pred, y_target, classtype="A")
}
