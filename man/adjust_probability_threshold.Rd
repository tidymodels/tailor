% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjust-probability-threshold.R
\name{adjust_probability_threshold}
\alias{adjust_probability_threshold}
\title{Change the event threshold}
\usage{
adjust_probability_threshold(x, threshold = 0.5)
}
\arguments{
\item{x}{A \code{\link[=tailor]{tailor()}}.}

\item{threshold}{A numeric value (between zero and one) or \code{\link[hardhat:tune]{hardhat::tune()}}.}
}
\description{
Change the event threshold
}
\examples{
library(dplyr)
library(modeldata)

post_obj <-
  tailor() \%>\%
  adjust_probability_threshold(threshold = .1)

two_class_example \%>\% count(predicted)

post_res <- fit(
  post_obj,
  two_class_example,
  outcome = c(truth),
  estimate = c(predicted),
  probabilities = c(Class1, Class2)
)

predict(post_res, two_class_example) \%>\% count(predicted)
}
