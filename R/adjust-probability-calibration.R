#' Re-calibrate classification probability predictions
#'
#' @description
#' Calibration is the process of adjusting a model's outputted probabilities
#' to match the observed frequencies of events. This technique aims to
#' ensure that when a model predicts a certain probability for an outcome,
#' that probability accurately reflects the true likelihood of that outcome
#' occurring.
#'
#' @param x A [tailor()].
#' @param method Character. One of `"logistic"`, `"multinomial"`,
#' `"beta"`, `"isotonic"`, or `"isotonic_boot"`, corresponding to the
#' function from the \pkg{probably} package [probably::cal_estimate_logistic()],
#' [probably::cal_estimate_multinomial()], etc., respectively.
#'
#' @inheritSection adjust_numeric_calibration Data Usage
#'
#' @examplesIf FALSE
# @examplesIf rlang::is_installed("modeldata")
#' library(modeldata)
#'
#' # split example data
#' set.seed(1)
#' in_rows <- sample(c(TRUE, FALSE), nrow(two_class_example), replace = TRUE)
#' d_calibration <- two_class_example[in_rows, ]
#' d_test <- two_class_example[!in_rows, ]
#'
#' head(d_calibration)
#'
#' # specify calibration
#' tlr <-
#'   tailor() %>%
#'   adjust_probability_calibration(method = "logistic")
#'
#' # train tailor on a subset of data. situate in a modeling workflow with
#' # `workflows::add_tailor()` to avoid having to specify column names manually
#' tlr_fit <- fit(
#'   tlr,
#'   d_calibration,
#'   outcome = c(truth),
#'   estimate = c(predicted),
#'   probabilities = c(Class1, Class2)
#' )
#'
#' # apply to predictions on another subset of data
#' head(d_test)
#'
#' predict(tlr_fit, d_test)
#'
#' @export
adjust_probability_calibration <- function(x, method = NULL) {
  check_tailor(x, calibration_type = "probability")
  # wait to `check_method()` until `fit()` time
  if (!is.null(method)) {
    arg_match(
      method,
      c("logistic", "multinomial", "beta", "isotonic", "isotonic_boot")
    )
  }

  adj <-
    new_adjustment(
      "probability_calibration",
      inputs = "probability",
      outputs = "probability_class",
      arguments = list(method = method),
      results = list(),
      trained = FALSE,
      requires_fit = TRUE
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
print.probability_calibration <- function(x, ...) {
  trn <- ifelse(x$trained, " [trained]", "")
  cli::cli_bullets(c("*" = "Re-calibrate classification probabilities.{trn}"))
  invisible(x)
}

#' @export
fit.probability_calibration <- function(object, data, tailor = NULL, ...) {
  method <- check_method(object$arguments$method, tailor$type)
  # todo: adjust_probability_calibration() should take arguments to pass to
  # cal_estimate_* via dots
  fit <-
    eval_bare(
      call2(
        paste0("cal_estimate_", method),
        .data = expr(data),
        # todo: make getters for the entries in `columns`
        truth = tailor$columns$outcome,
        estimate = tailor$columns$probabilities,
        .ns = "probably"
      )
    )

  new_adjustment(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(fit = fit),
    trained = TRUE,
    requires_fit = object$requires_fit
  )
}

#' @export
predict.probability_calibration <- function(object, new_data, tailor, ...) {
  probably::cal_apply(
    .data = new_data,
    object = object$results$fit,
    pred_class = !!tailor$columns$estimate
  )
}

# todo probably needs required_pkgs methods for cal objects
#' @export
required_pkgs.probability_calibration <- function(x, ...) {
  c("tailor", "probably")
}

#' @export
tunable.probability_calibration <- function(x, ...) {
  no_param
}

# todo missing methods:
# todo tidy
# todo extract_parameter_set_dials
