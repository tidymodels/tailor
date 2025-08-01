% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tailor.R
\name{fit.tailor}
\alias{fit.tailor}
\alias{predict.tailor}
\title{Fit and predict from tailors}
\usage{
\method{fit}{tailor}(object, .data, outcome, estimate, probabilities = c(), ...)

\method{predict}{tailor}(object, new_data, ...)
}
\arguments{
\item{object}{A \code{\link[=tailor]{tailor()}}.}

\item{.data, new_data}{A data frame containing predictions from a model.}

\item{outcome}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}>
The column name of the outcome variable.}

\item{estimate}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}>}

\item{probabilities}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}> The column
names of class probability estimates. These
should be given in the order of the factor levels of the \code{estimate}.}

\item{...}{Currently ignored.}
}
\value{
An updated \code{\link[=tailor]{tailor()}} objects. Any estimates produced and saved by
\code{\link[=fit.tailor]{fit.tailor()}} are saved in the \code{adjustments} element of the tailor.
}
\description{
These functions apply \code{fit()} and \code{predict()} methods for each adjustment
added to a tailor, in the order in which they were applied.
}
\section{Data Usage}{


For adjustments that don't require estimating parameters, training with
\code{fit()} simply evaluates tidyselect expressions and logs column names.
For others, as in \code{\link[=adjust_numeric_calibration]{adjust_numeric_calibration()}}, adjustments actually
learn from data; in that case, separate subsets of data ought to be used
for training the tailor and evaluating its performance on predictions.

Note that if \code{.data} has zero or one row, the \code{method} is changed to \code{"none"}.
}

\examples{
\dontshow{if (rlang::is_installed(c("probably", "modeldata"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(modeldata)

# `predicted` gives hard class predictions based on probability threshold .5
head(two_class_example)

# use a threshold of .1 instead:
tlr <-
  tailor() |>
  adjust_probability_threshold(.1)

# fit by supplying column names.
tlr_fit <- fit(
  tlr,
  two_class_example,
  outcome = c(truth),
  estimate = c(predicted),
  probabilities = c(Class1, Class2)
)

# adjust hard class predictions
predict(tlr_fit, two_class_example) |> head()
\dontshow{\}) # examplesIf}
}
