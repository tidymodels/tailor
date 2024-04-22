#' Declare post-processing for model predictions
#'
#' @param mode The model's mode, one of `"unknown"`, `"classification"`, or
#' `"regression"`. Modes of `"censored regression"` are not currently supported.
#' @param type The model sub-type. Possible values are `"unknown"`, `"regression"`,
#' `"binary"`, or `"multiclass"`.
#' @param outcome The name of the outcome variable.
#' @param estimate The name of the point estimate (e.g. predicted class)
#' @param probabilities The names of class probability estimates (if any). For
#' classification, these should be given in the order of the factor levels of
#' the `estimate`.
#' @param time The name of the predicted event time. (not yet supported)
#' @examples
#'
#' container()
#' @export
container <- function(mode = "unknown", type = "unknown", outcome = character(0),
                      estimate = character(0), probabilities = character(0),
                      time = character(0)) {
  dat <-
    list(
      outcome = outcome,
      type = type,
      estimate = estimate,
      probabilities = probabilities,
      time = time
    )

  new_container(
    mode,
    type,
    operations = list(),
    columns = dat,
    ptype = tibble::new_tibble(list()),
    call = current_env()
  )
}

new_container <- function(mode, type, operations, columns, ptype, call) {
  mode <- arg_match0(mode, c("unknown", "regression", "classification", "censored regression"))

  if (mode == "regression") {
    type <- "regression"
  }

  type <- arg_match0(type, c("unknown", "regression", "binary", "multiclass"))

  if (!is.list(operations)) {
    cli_abort("The {.arg operations} argument should be a list.", call = call)
  }

  is_oper <- purrr::map_lgl(operations, ~ inherits(.x, "operation"))
  if (length(is_oper) > 0 & !any(is_oper)) {
    bad_oper <- names(is_oper)[!is_oper]
    cli_abort("The following {.arg operations} do not have the class \\
                   {.val operation}: {bad_oper}.", call = call)
  }

  # validate operation order and check duplicates
  validate_oper_order(operations, mode, call)

  # check columns
  res <- list(
    mode = mode, type = type, operations = operations,
    columns = columns, ptype = ptype
  )
  class(res) <- "container"
  res
}

#' @export
print.container <- function(x, ...) {
  # todo emulate Emil's recipe printing

  num_op <- length(x$operations)
  cli_inform("{x$type} post-processing object with {num_op} operation{?s}")

  if (num_op > 0) {
    cat("\n")
    res <- purrr::map(x$operations, ~ print(.x))
  }

  invisible(x)
}

#' @export
fit.container <- function(object, .data, outcome, estimate, probabilities = c(),
                          time = c(), ...) {
  # ------------------------------------------------------------------------------
  # set columns via tidyselect

  dat <- list()
  dat$outcome <- names(tidyselect::eval_select(enquo(outcome), .data))
  dat$estimate <- names(tidyselect::eval_select(enquo(estimate), .data))

  probabilities <- tidyselect::eval_select(enquo(probabilities), .data)
  if (length(probabilities) > 0) {
    dat$probabilities <- names(probabilities)
  } else {
    dat$probabilities <- character(0)
  }

  time <- tidyselect::eval_select(enquo(time), .data)
  if (length(time) > 0) {
    dat$time <- names(time)
  } else {
    dat$time <- character(0)
  }

  .data <- .data[, names(.data) %in% unlist(dat)]
  if (!tibble::is_tibble(.data)) {
    .data <- tibble::as_tibble(.data)
  }
  ptype <- .data[0, ]

  object <- set_container_type(object, .data[[dat$outcome]])

  object <- new_container(
    object$mode,
    object$type,
    operations = object$operations,
    columns = dat,
    ptype = ptype,
    call = current_env()
  )

  num_oper <- length(object$operations)
  for (op in 1:num_oper) {
    object$operations[[op]] <- fit(object$operations[[op]], data, object)
    .data <- predict(object$operations[[op]], .data, object)
  }

  # todo Add a fitted container class?
  object
}

#' @export
predict.container <- function(object, new_data, ...) {
  # validate levels/classes
  num_oper <- length(object$operations)
  for (op in 1:num_oper) {
    new_data <- predict(object$operations[[op]], new_data, object)
  }
  if (!tibble::is_tibble(new_data)) {
    new_data <- tibble::as_tibble(new_data)
  }
  new_data
}

set_container_type <- function(object, y) {
  if (object$type != "unknown") {
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
    cli_abort("Only factor and numeric outcomes are currently supported.")
  }
  object
}

# todo: where to validate #levels?
# todo setup eval_time
# todo missing methods:
# todo tune_args
# todo tidy
# todo extract_parameter_set_dials
