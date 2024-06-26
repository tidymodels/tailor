#' Declare post-processing for model predictions
#'
#' @param type The model sub-type. Possible values are `"unknown"`, `"regression"`,
#' `"binary"`, or `"multiclass"`.
#' @param outcome The name of the outcome variable.
#' @param estimate The name of the point estimate (e.g. predicted class). In
#' tidymodels, this corresponds to column names `.pred`, `.pred_class`, or
#' `.pred_time`.
#' @param probabilities The names of class probability estimates (if any). For
#' classification, these should be given in the order of the factor levels of
#' the `estimate`.
#' @examples
#'
#' tailor()
#' @export
tailor <- function(type = "unknown", outcome = NULL, estimate = NULL,
                      probabilities = NULL) {
  columns <-
    list(
      outcome = outcome,
      type = type,
      estimate = estimate,
      probabilities = probabilities
    )

  new_tailor(
    type,
    adjustments = list(),
    columns = columns,
    ptype = tibble::new_tibble(list()),
    call = current_env()
  )
}

new_tailor <- function(type, adjustments, columns, ptype, call) {
  type <- arg_match0(type, c("unknown", "regression", "binary", "multiclass"))

  if (!is.list(adjustments)) {
    cli_abort("The {.arg adjustments} argument should be a list.", call = call)
  }

  is_adjustment <- purrr::map_lgl(adjustments, ~ inherits(.x, "adjustment"))
  if (length(is_adjustment) > 0 && !any(is_adjustment)) {
    bad_adjustment <- names(is_adjustment)[!is_adjustment]
    cli_abort("The following {.arg adjustments} do not have the class \\
                   {.val adjustment}: {bad_adjustment}.", call = call)
  }

  # validate adjustment order and check duplicates
  validate_order(adjustments, type, call)

  # check columns
  res <- list(
    type = type, adjustments = adjustments,
    columns = columns, ptype = ptype
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

#' @export
fit.tailor <- function(object, .data, outcome, estimate, probabilities = c(),
                       ...) {
  # ------------------------------------------------------------------------------
  # set columns via tidyselect

  columns <- list()
  columns$outcome <- names(tidyselect::eval_select(enquo(outcome), .data))
  check_selection(enquo(outcome), columns$outcome, "outcome")
  columns$estimate <- names(tidyselect::eval_select(enquo(estimate), .data))
  check_selection(enquo(estimate), columns$estimate, "estimate")
  columns$probabilities <- names(tidyselect::eval_select(enquo(probabilities), .data))
  if (any(c("probability", "everything") %in%
          purrr::map_chr(object$adjustments, purrr::pluck, "inputs"))) {
    check_selection(enquo(probabilities), columns$probabilities, "probabilities")
  }

  .data <- .data[, names(.data) %in% unlist(columns)]
  if (!tibble::is_tibble(.data)) {
    .data <- tibble::as_tibble(.data)
  }
  ptype <- .data[0, ]

  object <- set_tailor_type(object, .data[[columns$outcome]])

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

  # todo Add a fitted tailor class?
  object
}

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

set_tailor_type <- function(object, y) {
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
