#' Re-calibrate classification probability predictions
#'
#' @param x A [container()].
#' @param calibrator A pre-trained calibration method from the \pkg{probably}
#' package, such as [probably::cal_estimate_logistic()].
#' @export
adjust_probability_calibration <- function(x, calibrator) {

  cls <- c("cal_binary", "cal_multinomial")
  if (!inherits(calibrator, cls)) {
    cli::cli_abort("The {.arg calibrator} argument should be an object of //
                   class {.val cls}.")
  }

  op <-
    new_operation(
      "probability_calibration",
      inputs = "probability",
      outputs = "probability_class",
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
print.probability_calibration <- function(x, ...) {
  trn <- ifelse(x$results$trained, " [trained]", "")
  cli::cli_inform(c("Re-calibrate classification probabilities{trn}"))
  invisible(x)
}

#' @export
fit.probability_calibration <- function(object, data, parent = NULL, ...) {
  new_operation(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(trained = TRUE)
  )
}

#' @export
predict.probability_calibration <- function(object, new_data, parent, ...) {
  probably::cal_apply(new_data, object$argument$calibrator)
}

# todo probably needs required_pkgs methods for cal objects
#' @export
required_pkgs.probability_calibration <- function(x, ...) {
  c("container", "probably")
}

#' @export
tunable.probability_calibration <- function (x, ...) {
  no_param
}

# todo missing methods:
# todo tune_args
# todo tidy
# todo extract_parameter_set_dials
