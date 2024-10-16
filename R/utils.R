#' Internal tailor functions
#'
#' Utilities for use in downstream packages.
#'
#' @keywords internal
#' @name tailor-internals
NULL

# tuning machinery -------------------------------------------------------------
is_tune <- function(x) {
  if (!is.call(x)) {
    return(FALSE)
  }
  isTRUE(identical(quote(tune), x[[1]]))
}

tune_tbl <- function(name = character(), tunable = logical(), id = character(),
                     source = character(), component = character(),
                     component_id = character(), full = FALSE, call = caller_env()) {
  complete_id <- id[!is.na(id)]
  dups <- duplicated(complete_id)

  if (any(dups)) {
    offenders <- unique(complete_id[dups])
    cli::cli_abort(
      "{.val {offenders}} {?has a/have} duplicate {.field id} value{?s}.",
      call = call
    )
  }

  vry_tbl <-
    tibble::new_tibble(list(
      name = as.character(name),
      tunable = as.logical(tunable),
      id = as.character(id),
      source = as.character(source),
      component = as.character(component),
      component_id = as.character(component_id)
    ))

  if (!full) {
    vry_tbl <- vry_tbl[vry_tbl$tunable, ]
  }

  vry_tbl
}

#' @export
tune_args.adjustment <- function(object, full = FALSE, ...) {
  adjustment_id <- object$id
  # Grab the adjustment class before the subset, as that removes the class
  adjustment_type <- class(object)[1]

  tune_param_list <- tunable(object)$name

  # remove the non-tunable arguments as they are not important
  object <- object[tune_param_list]

  # Remove NULL argument adjustments. These are reserved
  # for deprecated args or those set at fit() time.
  object <- object[!purrr::map_lgl(object, is.null)]

  res <- purrr::map_chr(object, find_tune_id)
  res <- ifelse(res == "", names(res), res)

  tune_tbl(
    name = names(res),
    tunable = unname(!is.na(res)),
    id = unname(res),
    source = "tailor",
    component = adjustment_type,
    component_id = adjustment_id,
    full = full
  )
}

# for adjustments with no tunable parameters
no_param <-
  tibble::tibble(
    name = character(0),
    call_info = list(),
    source = character(0),
    component = character(0),
    component_id = character(0)
  )

find_tune_id <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (length(x) == 0L) {
    return(NA_character_)
  }
  if (rlang::is_quosures(x)) {
    .x <- try(purrr::map(x, rlang::eval_tidy), silent = TRUE)
    if (inherits(.x, "try-error")) {
      x <- purrr::map(x, rlang::quo_get_expr)
    } else {
      x <- .x
    }
  }
  id <- tune_id(x, call = call)

  if (!is.na(id)) {
    return(id)
  }

  if (is.atomic(x) | is.name(x) | length(x) == 1) {
    return(NA_character_)
  }

  tunable_elems <- vector("character", length = length(x))
  for (i in seq_along(x)) {
    tunable_elems[i] <- find_tune_id(x[[i]], call = call)
  }
  tunable_elems <- tunable_elems[!is.na(tunable_elems)]

  if (length(tunable_elems) == 0) {
    tunable_elems <- NA_character_
  }

  if (sum(tunable_elems == "", na.rm = TRUE) > 1) {
    offenders <- paste0(deparse(x), collapse = "")
    cli::cli_abort(
      c(
        "Only one tunable value is currently allowed per argument.",
        "{.arg {arg}} has {.code {offenders}}."
      ),
      call = call
    )
  }

  return(tunable_elems)
}

tune_id <- function(x, call = caller_env()) {
  if (is.null(x)) {
    return(NA_character_)
  } else {
    if (rlang::is_quosures(x)) {
      .x <- try(purrr::map(x, rlang::eval_tidy), silent = TRUE)
      if (inherits(.x, "try-error")) {
        x <- purrr::map(x, rlang::quo_get_expr)
      } else {
        x <- .x
      }
      if (is.null(x)) {
        return(NA_character_)
      }
    }

    if (is.call(x)) {
      if (rlang::call_name(x) == "tune") {
        if (length(x) > 1) {
          return(x[[2]])
        } else {
          return("")
        }

        return(x$id)
      } else {
        return(NA_character_)
      }
    }
  }

  NA_character_
}

# new_adjustment -------------------------------------------------------------
# These values are used to specify "what will we need for the adjustment?" and
# "what will we change?". For the outputs, we cannot change the probabilities
# without changing the classes. This is important because we are going to have
# to define constrains on the order of adjustments.

input_vals <- c("numeric", "probability", "class", "everything")
output_vals <- c("numeric", "probability_class", "class", "everything")

new_adjustment <- function(cls, inputs, outputs, arguments, results = list(),
                          trained, requires_fit, ...) {
  inputs <- arg_match0(inputs, input_vals)
  outputs <- arg_match0(outputs, output_vals)

  check_logical(trained)

  res <-
    list(
      inputs = inputs,
      outputs = outputs,
      arguments = arguments,
      results = results,
      trained = trained,
      requires_fit = requires_fit
    )
  class(res) <- c(cls, "adjustment")
  res
}

# predicates -------------------------------------------------------------------
is_tailor <- function(x) {
  inherits(x, "tailor")
}

#' @export
#' @keywords internal
#' @rdname tailor-internals
tailor_fully_trained <- function(x) {
  check_tailor(x)

  if (length(x$adjustments) == 0L) {
    return(FALSE)
  }

  all(purrr::map_lgl(x$adjustments, tailor_adjustment_trained))
}

tailor_adjustment_trained <- function(x) {
  isTRUE(x$trained)
}

#' @export
#' @keywords internal
#' @rdname tailor-internals
tailor_requires_fit <- function(x) {
  check_tailor(x)

  any(purrr::map_lgl(x$adjustments, tailor_adjustment_requires_fit))
}

tailor_adjustment_requires_fit <- function(x) {
  isTRUE(x$requires_fit)
}

# an tidy-esque method for adjustment lists, used in validating
# compatibility of adjustments
adjustment_orderings <- function(adjustments) {
  tibble::new_tibble(list(
    name = purrr::map_chr(adjustments, ~ class(.x)[1]),
    input = purrr::map_chr(adjustments, ~ .x$inputs),
    output_numeric = purrr::map_lgl(adjustments, ~ grepl("numeric", .x$outputs)),
    output_prob = purrr::map_lgl(adjustments, ~ grepl("probability", .x$outputs)),
    output_class = purrr::map_lgl(adjustments, ~ grepl("class", .x$outputs)),
    output_all = purrr::map_lgl(adjustments, ~ grepl("everything", .x$outputs))
  ))
}

# ad-hoc checking --------------------------------------------------------------
check_tailor <- function(x, calibration_type = NULL, call = caller_env(), arg = caller_arg(x)) {
  if (!is_tailor(x)) {
    cli_abort(
      "{.arg {arg}} should be a {.help [{.cls tailor}](tailor::tailor)}, \\
       not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  # check that the type of calibration ("numeric" or "probability") is
  # compatible with the tailor type
  if (!is.null(calibration_type)) {
    type <- x$type
    switch(
      type,
      regression =
        check_calibration_type(calibration_type, "numeric", type, call = call),
      binary = , multiclass =
        check_calibration_type(calibration_type, "probability", type, call = call)
    )
  }

  invisible()
}

check_calibration_type <- function(calibration_type, calibration_type_expected,
                                   tailor_type, call) {
  if (!identical(calibration_type, calibration_type_expected)) {
    cli_abort(
      "A {.field {tailor_type}} tailor is incompatible with the adjustment \\
       {.fun {paste0('adjust_', calibration_type, '_calibration')}}.",
      call = call
    )
  }
}

types_regression <- c("linear", "isotonic", "isotonic_boot")
types_binary <- c("logistic", "beta", "isotonic", "isotonic_boot")
types_multiclass <- c("multinomial", "beta", "isotonic", "isotonic_boot")
# a check function to be called when a tailor is being `fit()`ted.
# by the time a tailor is fitted, we have:
# * `method`, the `method` argument passed to an `adjust_*` function
#     * this argument has already been checked to agree with the kind of
#       `adjust_*()` function via `arg_match0()`.
# * `tailor_type`, the `type` argument either specified in `tailor()`
#   or inferred in `fit.tailor()`.
check_method <- function(method,
                       type,
                       arg = caller_arg(method),
                       call = caller_env()) {
  # if no `method` was supplied, infer a reasonable one based on the `type`
  if (is.null(method)) {
    switch(
      type,
      regression = return("linear"),
      binary = return("logistic"),
      multiclass = return("multinomial")
    )
  }

  switch(
    type,
    regression = arg_match0(
      method,
      types_regression,
      arg_nm = arg,
      error_call = call
    ),
    binary = arg_match0(
      method,
      types_binary,
      arg_nm = arg,
      error_call = call
    ),
    multiclass = arg_match0(
      method,
      types_multiclass,
      arg_nm = arg,
      error_call = call
    ),
    arg_match0(
      method,
      unique(c(types_regression, types_binary, types_multiclass)),
      arg_nm = arg,
      error_call = call
    )
  )

  method
}

# at `fit()` time, we check the type of the outcome vs the type
# supported by the applied adjustments. where this is called currently,
# we know already that `type` is not "unknown"
check_outcome_type <- function(outcome, type, call) {
  outcome_is_compatible <-
    switch(
      type,
      regression = is.numeric(outcome),
      binary = , multiclass = is.factor(outcome),
      FALSE
    )

  if (!outcome_is_compatible) {
    cli_abort(
      "Tailors with {type} adjustments are not compatible
       with {.cls {class(outcome)}} outcomes.",
      call = call
    )
  }
}

check_selection <- function(selector, result, arg, call = caller_env()) {
  if (length(result) == 0) {
    cli_abort(
      c(
        "!" = "{.arg {arg}} must select at least one column.",
        "x" = "Selector {.code {as_label(selector)}} did not match any columns \\
               in {.arg .data}."
      ),
      call = caller_env()
    )
  }
}
