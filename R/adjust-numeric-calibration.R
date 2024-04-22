#' Re-calibrate numeric predictions
#'
#' @param x A [container()].
#' @param calibrator A pre-trained calibration method from the \pkg{probably}
#' package, such as [probably::cal_estimate_linear()].
#' @export
adjust_numeric_calibration <- function(x, calibrator) {
  if (!inherits(calibrator, "cal_regression")) {
    cli_abort("The {.arg calibrator} argument should be an object of //
                   class {.val 'cal_regression'}.")
  }

  op <-
    new_operation(
      "numeric_calibration",
      inputs = "numeric",
      outputs = "numeric",
      arguments = list(calibrator = calibrator),
      results = list(trained = FALSE)
    )

  new_container(
    mode = x$mode,
    type = x$type,
    operations = c(x$operations, list(op)),
    columns = x$dat,
    ptype = x$ptype,
    call = current_env()
  )
}

#' @export
print.numeric_calibration <- function(x, ...) {
  trn <- ifelse(x$results$trained, " [trained]", "")
  cli_inform(c("Re-calibrate numeric predictions{trn}"))
  invisible(x)
}

#' @export
fit.numeric_calibration <- function(object, data, parent = NULL, ...) {
  new_operation(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(trained = TRUE)
  )
}

#' @export
predict.numeric_calibration <- function(object, new_data, parent, ...) {
  probably::cal_apply(new_data, object$argument$calibrator)
}

# todo probably needs required_pkgs methods for cal objects
#' @export
required_pkgs.numeric_calibration <- function(x, ...) {
  c("container", "probably")
}

#' @export
tunable.numeric_calibration <- function(x, ...) {
  no_param
}

# todo missing methods:
# todo tune_args
# todo tidy
# todo extract_parameter_set_dials