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
#' function from the \pkg{probably} package `probably::cal_estimate_logistic()`,
#' `probably::cal_estimate_multinomial()`, etc., respectively.
#'
#' @inheritSection adjust_numeric_calibration Data Usage
#'
#' @examplesIf FALSE
# @examplesIf rlang::is_installed(c("probably", "modeldata"))
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
#'   tailor() |>
#'   adjust_probability_calibration(method = "logistic")
#'
#' # train tailor on a subset of data.
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
  validate_probably_available()

  check_tailor(x, calibration_type = "probability")
  # wait to `check_method()` until `fit()` time
  if (!is.null(method) & !is_tune(method)) {
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

  if (is_tune(x$arguments$method)) {
    method <- "(method marked for optimization)"
  } else {
    if (is.null(x$argument$method)) {
      method <- "unknown"
    } else {
      method <- x$argument$method
    }
    method <- paste("using", x$argument$method, "method")
  }

  cli::cli_bullets(c(
    "*" = "Re-calibrate classification probabilities {method}.{trn}"
  ))
  invisible(x)
}

#' @export
fit.probability_calibration <- function(object, data, tailor = NULL, ...) {
  validate_probably_available()

  method <- check_method(object$arguments$method, tailor$type)
  # todo: adjust_probability_calibration() should take arguments to pass to
  # cal_estimate_* via dots

  cl <- rlang::call2(
    paste0("cal_estimate_", method),
    .data = expr(data),
    # todo: make getters for the entries in `columns`
    truth = tailor$columns$outcome,
    estimate = tailor$columns$probabilities,
    .ns = "probably"
  )

  fit <- try(eval_bare(cl), silent = TRUE)
  if (inherits(fit, "try-error")) {
    cli::cli_alert(
      "The {object$method} calibration failed: {fit}. No calibration is applied.")
  }

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
  validate_probably_available()

  if (inherits(object$results$fit, "try-error")) {
    return(new_data)
  }

  probably::cal_apply(
    .data = new_data,
    object = object$results$fit,
    pred_class = !!tailor$columns$estimate
  )
}

#' @export
required_pkgs.probability_calibration <- function(x, ...) {
  res <- c("tailor", "probably")

  if (x$trained) {
    res <- c(res, required_pkgs(x$results$fit))
  }
  sort(unique(res))
}

#' @export
tunable.probability_calibration <- function(x, ...) {
  tibble::new_tibble(
    list(
      name = "method",
      call_info = list(list(pkg = "dials", fun = "cal_method_class")),
      source = "tailor",
      component = "probability_calibration",
      component_id = "probability_calibration"
    ),
    nrow = 1
  )
}
