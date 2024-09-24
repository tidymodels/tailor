#' Re-calibrate numeric predictions
#'
#' @description
#' Calibration for regression models involves adjusting the model's
#' predictions to adjust for correlated errors, ensuring that predicted
#' values align closely with actual observed values across the entire
#' range of outputs.
#'
#' @param x A [tailor()].
#' @param method Character. One of `"linear"`, `"isotonic"`, or
#' `"isotonic_boot"`, corresponding to the function from the \pkg{probably}
#' package [probably::cal_estimate_linear()],
#' [probably::cal_estimate_isotonic()], or
#' [probably::cal_estimate_isotonic_boot()], respectively.
#'
#' @section Data Usage:
#' This adjustment requires estimation and, as such, different subsets of data
#' should be used to train it and evaluate its predictions. See the section
#' by the same name in `?workflows::add_tailor()` for more information on
#' preventing data leakage with postprocessors that require estimation. When
#' situated in a workflow, tailors will automatically be estimated with
#' appropriate subsets of data.
#'
#' @examples
#' library(tibble)
#'
#' # create example data
#' set.seed(1)
#' d_calibration <- tibble(y = rnorm(100), y_pred = y/2 + rnorm(100))
#' d_test <- tibble(y = rnorm(100), y_pred = y/2 + rnorm(100))
#'
#' d_calibration
#'
#' # specify calibration
#' tlr <-
#'   tailor() %>%
#'   adjust_numeric_calibration(method = "linear")
#'
#' # train tailor on a subset of data. situate in a modeling workflow with
#' # `workflows::add_tailor()` to avoid having to specify column names manually
#' tlr_fit <- fit(tlr, d_calibration, outcome = y, estimate = y_pred)
#'
#' # apply to predictions on another subset of data
#' d_test
#'
#' predict(tlr_fit, d_test)
#' @export
adjust_numeric_calibration <- function(x, method = NULL) {
  check_tailor(x, calibration_type = "numeric")
  # wait to `check_method()` until `fit()` time
  if (!is.null(method)) {
    arg_match0(
      method,
      c("linear", "isotonic", "isotonic_boot")
    )
  }

  adj <-
    new_adjustment(
      "numeric_calibration",
      inputs = "numeric",
      outputs = "numeric",
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
print.numeric_calibration <- function(x, ...) {
  trn <- ifelse(x$trained, " [trained]", "")
  cli::cli_bullets(c("*" = "Re-calibrate numeric predictions.{trn}"))
  invisible(x)
}

#' @export
fit.numeric_calibration <- function(object, data, tailor = NULL, ...) {
  method <- check_method(object$arguments$method, tailor$type)
  # todo: adjust_numeric_calibration() should take arguments to pass to
  # cal_estimate_* via dots
  fit <-
    eval_bare(
      call2(
        paste0("cal_estimate_", method),
        .data = data,
        truth = tailor$columns$outcome,
        estimate = tailor$columns$estimate,
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
predict.numeric_calibration <- function(object, new_data, tailor, ...) {
  probably::cal_apply(new_data, object$results$fit)
}

# todo probably needs required_pkgs methods for cal objects
#' @export
required_pkgs.numeric_calibration <- function(x, ...) {
  c("tailor", "probably")
}

#' @export
tunable.numeric_calibration <- function(x, ...) {
  no_param
}

# todo missing methods:
# todo tune_args
# todo tidy
# todo extract_parameter_set_dials
