#' Re-calibrate numeric predictions
#'
#' @param x A [tailor()].
#' @param method Character. One of `"linear"`, `"isotonic"`, or
#' `"isotonic_boot"`, corresponding to the function from the \pkg{probably}
#' package [probably::cal_estimate_linear()],
#' [probably::cal_estimate_isotonic()], or
#' [probably::cal_estimate_isotonic_boot()], respectively.
#' @examples
#' library(modeldata)
#' library(probably)
#' library(tibble)
#'
#' # create example data
#' set.seed(1)
#' dat <- tibble(y = rnorm(100), y_pred = y/2 + rnorm(100))
#'
#' dat
#'
#' # specify calibration
#' reg_tailor <-
#'   tailor() %>%
#'   adjust_numeric_calibration(method = "linear")
#'
#' # train tailor
#' reg_tailor_trained <- fit(reg_tailor, dat, outcome = y, estimate = y_pred)
#'
#' predict(reg_tailor_trained, dat)
#' @export
adjust_numeric_calibration <- function(x, method = NULL) {
  # to-do: add argument specifying `prop` in initial_split
  check_tailor(x, calibration_type = "numeric")
  # wait to `check_method()` until `fit()` time
  if (!is.null(method)) {
    arg_match0(
      method,
      c("linear", "isotonic", "isotonic_boot")
    )
  }

  op <-
    new_operation(
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
    operations = c(x$operations, list(op)),
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
  method <- check_method(object$method, tailor$type)
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

  new_operation(
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
