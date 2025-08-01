% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tailor.R
\name{tailor}
\alias{tailor}
\title{Declare post-processing for model predictions}
\usage{
tailor()
}
\value{
An object of class \code{tailor} with elements:
\itemize{
\item \code{type}: The type of task (e.g., regression)
\item \code{adjustments}: A list containing the sequential options specified by the
user.
\item \code{columns}: the data set column names for the true outcome values and the
predictions of various types. If these are not specified, then \code{NULL}.
These are usual
\item \code{ptype}: a zero-row slice of the data containing the \code{columns}.
}

Most of these values are set when an adjustment is added to the tailor or
when \code{\link[=fit.tailor]{fit.tailor()}} is used.
}
\description{
Tailors compose iterative adjustments to model predictions. After
initializing a tailor with this function, add adjustment specifications
with \verb{adjust_*()} functions:
\itemize{
\item For probability distributions: \code{\link[=adjust_probability_calibration]{adjust_probability_calibration()}}
\item For transformation of probabilities to hard class predictions:
\code{\link[=adjust_probability_threshold]{adjust_probability_threshold()}}, \code{\link[=adjust_equivocal_zone]{adjust_equivocal_zone()}}
\item For numeric distributions: \code{\link[=adjust_numeric_calibration]{adjust_numeric_calibration()}},
\code{\link[=adjust_numeric_range]{adjust_numeric_range()}}
}

For ad-hoc adjustments, see \code{\link[=adjust_predictions_custom]{adjust_predictions_custom()}}.

Tailors must be trained with \link[=fit.tailor]{fit()} before being applied to
new data with \link[=predict.tailor]{predict()}.
}
\section{Ordering of adjustments}{


When composing multiple adjustments in a tailor object, the order matters
and must follow specific rules depending on the type of predictions being
adjusted (classification or regression).

For classification problems (binary and multiclass), adjustments that modify
probability estimates (e.g., \code{\link[=adjust_probability_calibration]{adjust_probability_calibration()}}) must be
applied before adjustments that change hard class predictions (including
\code{\link[=adjust_equivocal_zone]{adjust_equivocal_zone()}}). This ensures that class predictions are based
on the final calibrated probabilities.
For regression problems, \code{adjust_numeric_calibration()} must be applied
before other numeric adjustments. This ensures that subsequent adjustments
work with calibrated predictions.

Generally, adjustments cannot be duplicated (i.e. the same adjustment type
cannot be used multiple times in a tailor object), though
\code{adjust_predictions_custom()} can be used multiple times. Adjustments for
different prediction types cannot be mixed---numeric adjustments (for
regression) and probability adjustments (for classification) cannot be
used in the same tailor object.

If these ordering rules are violated, \code{\link[=tailor]{tailor()}} will raise an
error describing the issue.
}

\examples{
\dontshow{if (rlang::is_installed(c("probably", "modeldata"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(dplyr)
library(modeldata)

# `predicted` gives hard class predictions based on probabilities
two_class_example |> count(predicted)

# change the probability threshold to allot one class vs the other
tlr <-
  tailor() |>
  adjust_probability_threshold(threshold = .1)

tlr

# fit by supplying column names.
tlr_fit <- fit(
  tlr,
  two_class_example,
  outcome = c(truth),
  estimate = c(predicted),
  probabilities = c(Class1, Class2)
)

tlr_fit

# adjust hard class predictions
predict(tlr_fit, two_class_example) |> count(predicted)
\dontshow{\}) # examplesIf}
}
