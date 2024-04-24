is_tune <- function(x) {
  if (!is.call(x)) {
    return(FALSE)
  }
  isTRUE(identical(quote(tune), x[[1]]))
}

# for operations with no tunable parameters

no_param <-
  tibble::tibble(
    name = character(0),
    call_info = list(),
    source = character(0),
    component = character(0),
    component_id = character(0)
  )

# These values are used to specify "what will we need for the adjustment?" and
# "what will we change?". For the outputs, we cannot change the probabilities
# without changing the classes. This is important because we are going to have
# to define constrains on the order of adjustments.

input_vals <- c("numeric", "probability", "class", "everything")
output_vals <- c("numeric", "probability_class", "class", "everything")

new_operation <- function(cls, inputs, outputs, arguments, results = list(trained = FALSE), ...) {
  inputs <- arg_match0(inputs, input_vals)
  outputs <- arg_match0(outputs, output_vals)

  if (!any(names(results) == "trained")) {
    cli_abort("The {.arg results} slot requires a logical variable called \\
                   {.val trained}")
  } else {
    check_logical(results$trained)
  }

  res <-
    list(
      inputs = inputs,
      outputs = outputs,
      arguments = arguments,
      results = results
    )
  class(res) <- c(cls, "operation")
  res
}

# predicates -------------------------------------------------------------------
is_container <- function(x) {
  inherits(x, "container")
}

# ad-hoc checking --------------------------------------------------------------
check_container <- function(x, call = caller_env(), arg = caller_arg(x)) {
  if (!is_container(x)) {
    cli::cli_abort(
      "{.arg {arg}} should be a {.help [{.cls container}](container::container)}, \\
       not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  invisible()
}
