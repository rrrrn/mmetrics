% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary_recall.R
\name{binary_recall}
\alias{binary_recall}
\title{binary_recall}
\usage{
binary_recall(preds, target, threshold = 0.5, multidim_average = "global")
}
\arguments{
\item{preds}{Predicted labels or predicted probability between 0 and 1,
same shape as target label}

\item{target}{Target label}

\item{threshold}{The numerical cut-off between 0 and 1 to transform
predicted probability into binary predicted labels}

\item{multidim_average}{Average model: global-average across all computed recall scores,
samplewise-average across the all but the first dimensions (calculated
independently for each sample)}
}
\value{
Binary recall value for preds and target, with format dictated by
multidim_average command.
}
\description{
Calculate the binary classification recall for a given predicted set of
values and corresponding targets. In other words, this function estimate the
sensitivity of the model prediction.
}
\examples{
binary_recall(c(0.8, 0.2), c(1,1), 0.3)
binary_recall(c(1,1), c(0,1))
}
