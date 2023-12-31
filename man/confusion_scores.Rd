% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confusion_scores.R
\name{confusion_scores}
\alias{confusion_scores}
\title{confusion_scores}
\usage{
confusion_scores(preds, target, multidim_average = "global")
}
\arguments{
\item{preds}{Predicted label, same shape as target label}

\item{target}{Target label}

\item{multidim_average}{Average model: global-average across all accuracies,
samplewise-average across the all but the first dimensions (calculated
independently for each sample)}
}
\value{
A list contains matrix-confusion matrix, tp-true positives,
fn-false negatives, fp-false positives, tn-true negatives
}
\description{
Calculate confusion matrix for a given predicted set of
values and corresponding targets, mainly suitable for binary classification task.
}
\examples{

y_pred = c(1,1,0,1,1)
y_target = c(0,1,0,1,0)
confusion_scores(y_pred, y_target)
}
