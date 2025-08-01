#' Apply an equivocal zone to a binary classification model.
#'
#' Equivocal zones describe intervals of predicted probabilities that are deemed
#' too uncertain or ambiguous to be assigned a hard class. Rather than
#' predicting a hard class when the probability is very close to a threshold,
#' tailors using this adjustment predict `"[EQ]"`.
#'
#' @param x A [tailor()].
#' @param value A numeric value (between zero and 1/2) or [hardhat::tune()]. The
#' value is the size of the buffer around the threshold.
#' @param threshold A numeric value (between zero and one) or [hardhat::tune()].
#' Defaults to `adjust_probability_threshold(threshold)` if previously set
#' in `x`, or `1 / 2` if not.
#'
#' @section Data Usage:
#' This adjustment doesn't require estimation and, as such, the same data that's
#' used to train it with `fit()` can be predicted on with `predict()`; fitting
#' this adjustment just collects metadata on the supplied column names and does
#' not risk data leakage.
#'
#' @details
#' This function transforms the class prediction column `estimate` to have type
#' `class_pred` from `probably::class_pred()`. You can loosely think of this
#' column type as a factor, except there's a possible entry `"[EQ]"` that is
#' _not_ a level and will be excluded from performance metric calculations.
#' As a result, the output column has the same number of levels as the input,
#' except now has a possible entry `"[EQ]"` that tidymodels functions know to
#' exclude from further analyses.
#'
#' @return An updated [tailor()] containing the new operation.
#' @examplesIf rlang::is_installed(c("probably", "modeldata"))
#' library(dplyr)
#' library(modeldata)
#'
#' head(two_class_example)
#'
#' # `predicted` gives hard class predictions based on probabilities
#' two_class_example |> count(predicted)
#'
#' # when probabilities are within (.25, .75), consider them equivocal
#' tlr <-
#'   tailor() |>
#'   adjust_equivocal_zone(value = 1 / 4)
#'
#' tlr
#'
#' # fit by supplying column names.
#' tlr_fit <- fit(
#'   tlr,
#'   two_class_example,
#'   outcome = c(truth),
#'   estimate = c(predicted),
#'   probabilities = c(Class1, Class2)
#' )
#'
#' tlr_fit
#'
#' # adjust hard class predictions
#' predict(tlr_fit, two_class_example) |> count(predicted)
#' @export
adjust_equivocal_zone <- function(x, value = 0.1, threshold = NULL) {
  validate_probably_available()

  check_tailor(x)
  threshold <- infer_threshold(x = x, threshold = threshold)
  if (!is_tune(value)) {
    check_number_decimal(value, min = 0, max = 1 / 2)
  }

  adj <-
    new_adjustment(
      "equivocal_zone",
      inputs = "probability",
      outputs = "class",
      arguments = list(value = value, threshold = threshold),
      results = list(),
      trained = FALSE,
      requires_fit = FALSE
    )

  new_tailor(
    type = x$type,
    adjustments = c(x$adjustments, list(adj)),
    columns = x$dat,
    ptype = x$ptype,
    call = current_env()
  )
}

#' @export
print.equivocal_zone <- function(x, ...) {
  # check for tune() first

  if (is_tune(x$arguments$value)) {
    cli::cli_bullets(c("*" = "Add equivocal zone of optimized size."))
  } else {
    trn <- ifelse(x$trained, " [trained]", "")
    cli::cli_bullets(c(
      "*" = "Add equivocal zone of size
             {signif(x$arguments$value, digits = 3)}.{trn}"
    ))
  }
  invisible(x)
}

#' @export
fit.equivocal_zone <- function(object, data, tailor = NULL, ...) {
  validate_probably_available()

  new_adjustment(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(),
    trained = TRUE,
    requires_fit = object$requires_fit
  )
}

#' @export
predict.equivocal_zone <- function(object, new_data, tailor, ...) {
  validate_probably_available()

  est_nm <- tailor$columns$estimate
  prob_nm <- tailor$columns$probabilities[1]
  lvls <- levels(new_data[[est_nm]])
  col_syms <- syms(prob_nm[1])
  cls_pred <- probably::make_two_class_pred(
    new_data[[prob_nm]],
    levels = lvls,
    buffer = object$arguments$value,
    threshold = object$arguments$threshold
  )
  new_data[[est_nm]] <- cls_pred # todo convert to factor?
  new_data
}

#' @export
required_pkgs.equivocal_zone <- function(x, ...) {
  c("tailor", "probably")
}

#' @export
tunable.equivocal_zone <- function(x, ...) {
  tibble::new_tibble(
    list(
      name = "buffer",
      call_info = list(list(pkg = "dials", fun = "buffer")),
      source = "tailor",
      component = "equivocal_zone",
      component_id = "equivocal_zone"
    ),
    nrow = 1
  )
}

infer_threshold <- function(x, threshold, call = caller_env()) {
  if (!is.null(threshold) && !is_tune(threshold)) {
    check_number_decimal(threshold, min = 10^-10, max = 1 - 10^-10, call = call)
    return(threshold)
  }

  if (is_tune(threshold)) {
    return(threshold)
  }

  # use `map() |> unlist()` rather than `map_dbl` to handle NULLs
  thresholds <- purrr::map(
    x$adjustments,
    purrr::pluck,
    "arguments",
    "threshold"
  )
  thresholds <- unlist(thresholds)

  if (!is.null(thresholds)) {
    return(thresholds[length(thresholds)])
  }

  1 / 2
}
