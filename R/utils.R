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

new_operation <- function(cls, inputs, outputs, arguments, results = list(),
                          trained, ...) {
  inputs <- arg_match0(inputs, input_vals)
  outputs <- arg_match0(outputs, output_vals)

  check_logical(trained)

  res <-
    list(
      inputs = inputs,
      outputs = outputs,
      arguments = arguments,
      results = results,
      trained = trained
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

types_regression <- c("linear", "isotonic", "isotonic_boot")
types_binary <- c("logistic", "beta", "isotonic", "isotonic_boot")
types_multiclass <- c("multinomial", "beta", "isotonic", "isotonic_boot")
check_type <- function(adjust_type,
                       container_type,
                       arg = caller_arg(adjust_type),
                       call = caller_env()) {
  # to-do: handle unknown container type (#11 ish)
  if (is.null(adjust_type)) {
    switch(
      container_type,
      regression = return("linear"),
      binary = return("logistic"),
      multiclass = return("multinomial")
    )
  }

  switch(
    container_type,
    regression = arg_match0(
      adjust_type,
      types_regression,
      arg_nm = arg,
      error_call = call
    ),
    binary = arg_match0(
      adjust_type,
      types_binary,
      arg_nm = arg,
      error_call = call
    ),
    multiclass = arg_match0(
      adjust_type,
      types_multiclass,
      arg_nm = arg,
      error_call = call
    ),
    arg_match0(
      adjust_type,
      unique(c(types_regression, types_binary, types_multiclass)),
      arg_nm = arg,
      error_call = call
    )
  )

  adjust_type
}

