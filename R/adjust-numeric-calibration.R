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
#' package `probably::cal_estimate_linear()`,
#' `probably::cal_estimate_isotonic()`, or
#' `probably::cal_estimate_isotonic_boot()`, respectively.
#' @param ... Optional arguments to pass to the corresponding function in the
#' \pkg{probably} package. These arguments must be named.
#'
#' @section Data Usage:
#' This adjustment requires estimation and, as such, different subsets of data
#' should be used to train it and evaluate its predictions. See the section
#' by the same name in `?workflows::add_tailor()` for more information on
#' preventing data leakage with postprocessors that require estimation. When
#' situated in a workflow, tailors will automatically be estimated with
#' appropriate subsets of data.
#'
#' @examplesIf rlang::is_installed("probably")
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
#'   tailor() |>
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
adjust_numeric_calibration <- function(x, method = NULL, ...) {
  validate_probably_available()

  check_tailor(x, calibration_type = "numeric")
  # wait to `check_method()` until `fit()` time
  if (!is.null(method) & !is_tune(method)) {
    arg_match0(
      method,
      c("linear", "isotonic", "isotonic_boot")
    )
  }

  args <- list(...)
  nms <- names(args)
  if (length(args) > 0 & (is.null(nms) || any(nms == ""))) {
    cli::cli_abort(
      "All calibration arguments passed to {.arg ...} should have names."
    )
  }
  args$method <- method

  adj <-
    new_adjustment(
      "numeric_calibration",
      inputs = "numeric",
      outputs = "numeric",
      arguments = args,
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

  if (is_tune(x$arguments$method)) {
    method <- "(method marked for optimization)"
  } else {
    if (is.null(x$arguments$method)) {
      method <- "unknown"
    } else {
      method <- x$arguments$method
    }
    method <- paste("using", x$arguments$method, "method")
  }

  cli::cli_bullets(c(
    "*" = "Re-calibrate numeric predictions {method}.{trn}"
  ))
  invisible(x)
}

#' @export
fit.numeric_calibration <- function(object, data, tailor = NULL, ...) {
  validate_probably_available()

  method <- check_method(object$arguments$method, tailor$type)

  cl <- rlang::call2(
    paste0("cal_estimate_", method),
    .data = data,
    truth = tailor$columns$outcome,
    estimate = tailor$columns$estimate,
    .ns = "probably"
  )

  other_args <- object$arguments[names(object$arguments) != "method"]
  if (length(other_args) > 0) {
    cl <- rlang::call_modify(cl, !!!other_args)
  }

  fit <- try(eval_bare(cl), silent = TRUE)
  if (inherits(fit, "try-error")) {
    cli::cli_warn(
      c(
        "The {method} calibration failed. No calibration is applied.",
        i = fit
      )
    )
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
predict.numeric_calibration <- function(object, new_data, tailor, ...) {
  validate_probably_available()

  if (inherits(object$results$fit, "try-error")) {
    return(new_data)
  }

  probably::cal_apply(new_data, object$results$fit)
}

#' @export
required_pkgs.numeric_calibration <- function(x, ...) {
  res <- c("tailor", "probably")

  if (x$trained) {
    res <- c(res, required_pkgs(x$results$fit))
  }
  sort(unique(res))
}

#' @export
tunable.numeric_calibration <- function(x, ...) {
  tibble::new_tibble(
    list(
      name = "method",
      call_info = list(list(pkg = "dials", fun = "cal_method_reg")),
      source = "tailor",
      component = "numeric_calibration",
      component_id = "numeric_calibration"
    ),
    nrow = 1
  )
}
