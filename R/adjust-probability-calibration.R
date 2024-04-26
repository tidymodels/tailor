#' Re-calibrate classification probability predictions
#'
#' @param x A [container()].
#' @param type Character. One of `"logistic"`, `"multinomial"`,
#' `"beta"`, `"isotonic"`, or `"isotonic_boot"`, corresponding to the
#' function from the \pkg{probably} package [probably::cal_estimate_logistic()],
#' [probably::cal_estimate_multinomial()], etc., respectively.
#' @export
adjust_probability_calibration <- function(x, type = NULL) {
  # to-do: add argument specifying `prop` in initial_split
  check_container(x)
  type <- check_type(type, x$type)

  op <-
    new_operation(
      "probability_calibration",
      inputs = "probability",
      outputs = "probability_class",
      arguments = list(type = type),
      results = list(),
      trained = FALSE
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
  trn <- ifelse(x$trained, " [trained]", "")
  cli::cli_bullets(c("*" = "Re-calibrate classification probabilities.{trn}"))
  invisible(x)
}

#' @export
fit.probability_calibration <- function(object, data, container = NULL, ...) {
  # todo: adjust_probability_calibration() should take arguments to pass to
  # cal_estimate_* via dots
  # to-do: add argument specifying `prop` in initial_split
  fit <-
    eval_bare(
      call2(
        paste0("cal_estimate_", object$type),
        .data = data,
        # todo: make getters for the entries in `columns`
        truth = container$columns$outcome,
        estimate = container$columns$estimate,
        .ns = "probably"
      )
    )

  new_operation(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(fit = fit),
    trained = TRUE
  )
}

#' @export
predict.probability_calibration <- function(object, new_data, container, ...) {
  probably::cal_apply(new_data, object$results$fit)
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
