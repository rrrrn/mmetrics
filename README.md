# mmetrics

<!-- badges: start -->

[![R-CMD-check](https://github.com/rrrrn/mmetrics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rrrrn/mmetrics/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/rrrrn/mmetrics/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rrrrn/mmetrics?branch=main)

<!-- badges: end -->

## Overview

mmetrics is a grammar of metrics calculation, providing a consistent set of functions that help you quantitatively assess the performance of statistical models:

-   `confusion_scores` and `multiclass_confusion_scores` provides confusion matrices given model predictions and ground truth labels.

-   accuracy function (`binary_acc` and `multiclass_acc`) estimates the correctness of predicted classification labels.

-   precision function (`binary_precision` and `multiclass_precision`) measures the accuracy of positive predictions.

-   recall function (`binary_recall` and `multiclass_recall`) measures the completeness of positive predictions.

## Installation

``` R
install.packages("mmetrics")
```

## Usage

Please refer to `doc/Tutorial.html` for example usage or consult function help page.
