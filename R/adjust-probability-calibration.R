#' Re-calibrate classification probability predictions
#'
#' @param x A [container()].
#' @param calibrator A pre-trained calibration method from the \pkg{probably}
#' package, such as [probably::cal_estimate_logistic()].
#' @export
adjust_probability_calibration <- function(x, calibrator) {
  check_container(x)
  cls <- c("cal_binary", "cal_multinomial")
  check_required(calibrator)
  if (!inherits_any(calibrator, cls)) {
    cli_abort(
      "{.arg calibrator} should be a \\
       {.help [<cal_binary> or <cal_multinomial> object](probably::cal_estimate_logistic)}, \\
       not {.obj_type_friendly {calibrator}}."
    )
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
  cli::cli_bullets(c("*" = "Re-calibrate classification probabilities.{trn}"))
  invisible(x)
}

#' @export
fit.probability_calibration <- function(object, data, container = NULL, ...) {
  new_operation(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(trained = TRUE)
  )
}

#' @export
predict.probability_calibration <- function(object, new_data, container, ...) {
  probably::cal_apply(new_data, object$argument$calibrator)
}

# todo probably needs required_pkgs methods for cal objects
#' @export
required_pkgs.probability_calibration <- function(x, ...) {
  c("container", "probably")
}

#' @export
tunable.probability_calibration <- function(x, ...) {
  no_param
}

# todo missing methods:
# todo tune_args
# todo tidy
# todo extract_parameter_set_dials
