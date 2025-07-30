#' Declare post-processing for model predictions
#'
#' @description
#'
#' Tailors compose iterative adjustments to model predictions. After
#' initializing a tailor with this function, add adjustment specifications
#' with `adjust_*()` functions:
#'
#' * For probability distributions: [adjust_probability_calibration()]
#' * For transformation of probabilities to hard class predictions:
#' [adjust_probability_threshold()], [adjust_equivocal_zone()]
#' * For numeric distributions: [adjust_numeric_calibration()],
#' [adjust_numeric_range()]
#'
#' For ad-hoc adjustments, see [adjust_predictions_custom()].
#'
#' Tailors must be trained with [fit()][fit.tailor()] before being applied to
#' new data with [predict()][predict.tailor()].
#'
#' @section Ordering of adjustments:
#'
#' When composing multiple adjustments in a tailor object, the order matters
#' and must follow specific rules depending on the type of predictions being
#' adjusted (classification or regression).
#'
#' For classification problems (binary and multiclass), adjustments that modify
#' probability estimates (e.g., [adjust_probability_calibration()]) must be
#' applied before adjustments that change hard class predictions (including
#' [adjust_equivocal_zone()]). This ensures that class predictions are based
#' on the final calibrated probabilities.

#' For regression problems, `adjust_numeric_calibration()` must be applied
#' before other numeric adjustments. This ensures that subsequent adjustments
#' work with calibrated predictions.
#'
#' Generally, adjustments cannot be duplicated (i.e. the same adjustment type
#' cannot be used multiple times in a tailor object), though
#' `adjust_predictions_custom()` can be used multiple times. Adjustments for
#' different prediction types cannot be mixed---numeric adjustments (for
#' regression) and probability adjustments (for classification) cannot be
#' used in the same tailor object.
#'
#' If these ordering rules are violated, [tailor()] will raise an
#' error describing the issue.
#'
#' @return An object of class `tailor` with elements:
#'
#' - `type`: The type of task (e.g., regression)
#' - `adjustments`: A list containing the sequential options specified by the
#'    user.
#' - `columns`: the data set column names for the true outcome values and the
#'    predictions of various types. If these are not specified, then `NULL`.
#'    These are usual
#' - `ptype`: a zero-row slice of the data containing the `columns`.
#'
#' Most of these values are set when an adjustment is added to the tailor or
#' when [fit.tailor()] is used.
#' @examplesIf rlang::is_installed(c("probably", "modeldata"))
#' library(dplyr)
#' library(modeldata)
#'
#' # `predicted` gives hard class predictions based on probabilities
#' two_class_example |> count(predicted)
#'
#' # change the probability threshold to allot one class vs the other
#' tlr <-
#'   tailor() |>
#'   adjust_probability_threshold(threshold = .1)
#'
#' tlr
#'
#' # fit by supplying column names.
#' tlr_fit <- fit(
#'   tlr,
#'   two_class_example,
#'   outcome = c(truth),
#'   estimate = c(predicted),
#'   probabilities = c(Class1, Class2)
#' )
#'
#' tlr_fit
#'
#' # adjust hard class predictions
#' predict(tlr_fit, two_class_example) |> count(predicted)
#' @export
tailor <- function() {
  new_tailor(
    "unknown",
    adjustments = list(),
    columns = null_columns(),
    ptype = tibble::new_tibble(list()),
    call = current_env()
  )
}

null_columns <- function() {
  list(
    outcome = NULL,
    estimate = NULL,
    probabilities = NULL
  )
}

new_tailor <- function(type, adjustments, columns, ptype, call) {
  if (!is.list(adjustments)) {
    cli_abort("The {.arg adjustments} argument should be a list.", call = call)
  }

  is_adjustment <- purrr::map_lgl(adjustments, \(.x) inherits(.x, "adjustment"))
  if (length(is_adjustment) > 0 && !any(is_adjustment)) {
    bad_adjustment <- names(is_adjustment)[!is_adjustment]
    cli_abort(
      "The following {.arg adjustments} do not have the class {.val adjustment}: {bad_adjustment}.",
      call = call
    )
  }

  orderings <- adjustment_orderings(adjustments)

  if (type == "unknown") {
    type <- infer_type(orderings)
  }

  # validate adjustment order and check duplicates
  validate_order(orderings, type, call)

  # check columns
  res <- list(
    type = type,
    adjustments = adjustments,
    columns = columns,
    ptype = ptype
  )
  class(res) <- "tailor"
  res
}

#' @export
print.tailor <- function(x, ...) {
  cli::cli_h1("tailor")

  num_adj <- length(x$adjustments)
  cli::cli_text(
    "A {ifelse(x$type == 'unknown', '', x$type)} postprocessor \\
     with {num_adj} adjustment{?s}{cli::qty(num_adj+1)}{?./:}"
  )

  if (num_adj > 0) {
    cli::cli_text("\n")
    res <- purrr::map(x$adjustments, print)
  }

  invisible(x)
}

#' Fit and predict from tailors
#'
#' @description
#' These functions apply `fit()` and `predict()` methods for each adjustment
#' added to a tailor, in the order in which they were applied.
#'
#' @section Data Usage:
#'
#' For adjustments that don't require estimating parameters, training with
#' `fit()` simply evaluates tidyselect expressions and logs column names.
#' For others, as in [adjust_numeric_calibration()], adjustments actually
#' learn from data; in that case, separate subsets of data ought to be used
#' for training the tailor and evaluating its performance on predictions.
#'
#' Note that if `.data` has zero or one row, the `method` is changed to `"none"`.
#'
#' @param object A [tailor()].
#' @param .data,new_data A data frame containing predictions from a model.
#' @param outcome <[`tidy-select`][dplyr::dplyr_tidy_select]>
#' The column name of the outcome variable.
#' @param estimate <[`tidy-select`][dplyr::dplyr_tidy_select]>
#' @param probabilities <[`tidy-select`][dplyr::dplyr_tidy_select]> The column
#' names of class probability estimates. These
#' should be given in the order of the factor levels of the `estimate`.
#' @param ... Currently ignored.
#'
#' @return An updated [tailor()] objects. Any estimates produced and saved by
#' [fit.tailor()] are saved in the `adjustments` element of the tailor.
#'
#' @examplesIf rlang::is_installed(c("probably", "modeldata"))
#' library(modeldata)
#'
#' # `predicted` gives hard class predictions based on probability threshold .5
#' head(two_class_example)
#'
#' # use a threshold of .1 instead:
#' tlr <-
#'   tailor() |>
#'   adjust_probability_threshold(.1)
#'
#' # fit by supplying column names.
#' tlr_fit <- fit(
#'   tlr,
#'   two_class_example,
#'   outcome = c(truth),
#'   estimate = c(predicted),
#'   probabilities = c(Class1, Class2)
#' )
#'
#' # adjust hard class predictions
#' predict(tlr_fit, two_class_example) |> head()
#' @export
fit.tailor <- function(
  object,
  .data,
  outcome,
  estimate,
  probabilities = c(),
  ...
) {
  # ------------------------------------------------------------------------------
  # set columns via tidyselect

  columns <- list()
  columns$outcome <- names(tidyselect::eval_select(enquo(outcome), .data))
  check_selection(enquo(outcome), columns$outcome, "outcome")
  columns$estimate <- names(tidyselect::eval_select(enquo(estimate), .data))
  check_selection(enquo(estimate), columns$estimate, "estimate")
  columns$probabilities <- names(
    tidyselect::eval_select(enquo(probabilities), .data)
  )
  # For type = "binary", update based on number of probability estimates
  object$type <- update_type(object$type, columns$probabilities)

  if (
    "probability" %in%
      purrr::map_chr(object$adjustments, purrr::pluck, "inputs")
  ) {
    check_selection(
      enquo(probabilities),
      columns$probabilities,
      "probabilities"
    )
    for (col in columns$probabilities) {
      check_variable_type(.data[[col]], "probability", "probabilities")
    }
  }

  .data <- .data[, names(.data) %in% unlist(columns)]
  if (!tibble::is_tibble(.data)) {
    .data <- tibble::as_tibble(.data)
  }
  ptype <- .data[0, ]

  object <- set_tailor_type(object, .data[[columns$outcome]])

  check_variable_type(.data[[columns$outcome]], object$type, "outcome")
  check_variable_type(.data[[columns$estimate]], object$type, "estimate")

  object <- new_tailor(
    object$type,
    adjustments = object$adjustments,
    columns = columns,
    ptype = ptype,
    call = current_env()
  )

  num_adjustment <- length(object$adjustments)
  for (adj in seq_len(num_adjustment)) {
    object$adjustments[[adj]] <- fit(object$adjustments[[adj]], .data, object)
    .data <- predict(object$adjustments[[adj]], .data, object)
  }

  object
}

#' @rdname fit.tailor
#' @export
predict.tailor <- function(object, new_data, ...) {
  # validate levels/classes
  num_adjustment <- length(object$adjustments)
  for (adj in seq_len(num_adjustment)) {
    new_data <- predict(object$adjustments[[adj]], new_data, object)
  }
  if (!tibble::is_tibble(new_data)) {
    new_data <- tibble::as_tibble(new_data)
  }
  new_data
}

set_tailor_type <- function(object, y, call = caller_env()) {
  if (object$type != "unknown") {
    check_variable_type(y, object$type, "outcome", call = call)
    return(object)
  }
  if (is.factor(y)) {
    lvls <- levels(y)
    if (length(lvls) == 2) {
      object$type <- "binary"
    } else {
      object$type <- "multiclass"
    }
  } else if (is.numeric(y)) {
    object$type <- "regression"
  } else {
    cli_abort(
      "Only factor and numeric outcomes are currently supported.",
      call = call
    )
  }
  object
}

# todo: where to validate #levels?
# todo setup eval_time

#' @export
tune_args.tailor <- function(object, full = FALSE, ...) {
  adjustments <- object$adjustments

  if (length(adjustments) == 0L) {
    return(tune_tbl())
  }

  res <- purrr::map(object$adjustments, tune_args, full = full)
  res <- purrr::list_rbind(res)

  tune_tbl(
    res$name,
    res$tunable,
    res$id,
    res$source,
    res$component,
    res$component_id,
    full = full
  )
}

#' @export
tunable.tailor <- function(x, ...) {
  if (length(x$adjustments) == 0) {
    res <- no_param
  } else {
    res <- purrr::map(x$adjustments, tunable)
    res <- vctrs::vec_rbind(!!!res)
    if (nrow(res) > 0) {
      res <- res[!is.na(res$name), ]
    }
  }
  res
}

#' @export
required_pkgs.tailor <- function(x, ...) {
  res <- purrr::map(x$adjustments, required_pkgs)
  res <- c("tailor", unlist(res))
  sort(unique(res))
}
