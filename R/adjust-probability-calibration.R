#' Re-calibrate classification probability predictions
#'
#' @description
#' Calibration is the process of adjusting a model's outputted probabilities
#' to match the observed frequencies of events. This technique aims to
#' ensure that when a model predicts a certain probability for an outcome,
#' that probability accurately reflects the true likelihood of that outcome
#' occurring.
#'
#' @inheritParams adjust_numeric_calibration
#' @param method Character. One of `"logistic"`, `"multinomial"`,
#' `"beta"`, `"isotonic"`, `"isotonic_boot"`, or `"none"`, corresponding to the
#' function from the \pkg{probably} package `probably::cal_estimate_logistic()`,
#' `probably::cal_estimate_multinomial()`, etc., respectively.  The default is to
#' use `"logistic"` which, despite its name, fits a generalized additive model.
#' Note that when [fit.tailor()] is called, the value may be changed to `"none"`
#' if there is insufficient data.
#'
#' @details
#' The "logistic" and "multinomial" methods fit models that predict the observed
#' classes as a function of the predicted class probabilities. These models
#' remove any overt systematic trends from the linear predictor and correct new
#' predictions. The underlying code fits that model using [mgcv::gam()].
#' If `smooth = FALSE` is passed to the `...`, it uses [stats::glm()] for binary
#' outcomes or [nnet::multinom()] for 3+ classes.
#'
#' The isotonic method uses [stats::isoreg()] to force the predicted
#' probabilities to increase with the observed outcome class. This creates a
#' step function that will map new predictions to values that are monotonically
#' increasing with the binary (0/1) form of the outcome. One side effect is
#' that there are fewer, perhaps far fewer, unique predicted probabilities.
#' For 3+ classes, this is done using a one-versus-all strategy that ensures
#' that the probabilities add to 1.0. The "isotonic boot" method resamples the
#' data and generates multiple isotonic regressions that are averaged and used
#' to correct the predictions. This may not be perfectly monotonic, but the
#' number of unique calibrated predictions increases with the number of
#' bootstrap samples (controlled by passing the `times` argument to `...`).
#'
#' Beta calibration (Kull _et al_, 2017) assumes that the probability estimates
#' follow a Beta distribution. This leads to a sigmoidal model that can be fit
#' to the data via maximum likelihood. There are a few different ways to fit
#' the model; see [betacal:: beta_calibration()] options `parameters` to select
#' a specific sigmoidal model.
#'
#' @inheritSection adjust_numeric_calibration Data Usage
#'
#' @return An updated [tailor()] containing the new operation.
#'
#' @references
#' Kull, Meelis, Telmo Silva Filho, and Peter Flach. "Beta calibration: a
#' well-founded and easily implemented improvement on logistic calibration
#' for binary classifiers." Artificial intelligence and statistics. PMLR, 2017.
#'
#' \url{https://aml4td.org/chapters/cls-metrics.html#sec-cls-calibration}
#'
#' @examplesIf rlang::is_installed(c("probably", "modeldata"))
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
adjust_probability_calibration <- function(x, method = NULL, ...) {
  validate_probably_available()

  check_tailor(x, calibration_type = "probability")
  # We will check the method again during `fit()` using `check_cal_method()`
  if (!is.null(method) & !is_tune(method)) {
    arg_match(
      method,
      c("logistic", "multinomial", "beta", "isotonic", "isotonic_boot", "none")
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
      "probability_calibration",
      inputs = "probability",
      outputs = "probability_class",
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

  method <- check_cal_method(
    object$arguments$method,
    type = tailor$type,
    cal_data = data
  )

  cl <- rlang::call2(
    paste0("cal_estimate_", method),
    .data = quote(data),
    # TODO: make getters for the entries in `columns`
    truth = tailor$columns$outcome,
    estimate = tailor$columns$probabilities,
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
