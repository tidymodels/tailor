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
check_container <- function(x, calibration_type = NULL, call = caller_env(), arg = caller_arg(x)) {
  if (!is_container(x)) {
    cli_abort(
      "{.arg {arg}} should be a {.help [{.cls container}](container::container)}, \\
       not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  # check that the type of calibration ("numeric" or "probability") is
  # compatible with the container type
  if (!is.null(calibration_type)) {
    container_type <- x$type
    switch(
      container_type,
      regression =
        check_calibration_type(calibration_type, "numeric", container_type, call = call),
      binary = , multinomial =
        check_calibration_type(calibration_type, "probability", container_type, call = call)
    )
  }

  invisible()
}

check_calibration_type <- function(calibration_type, calibration_type_expected,
                                   container_type, call) {
  if (!identical(calibration_type, calibration_type_expected)) {
    cli_abort(
      "A {.field {container_type}} container is incompatible with the operation \\
       {.fun {paste0('adjust_', calibration_type, '_calibration')}}.",
      call = call
    )
  }
}

types_regression <- c("linear", "isotonic", "isotonic_boot")
types_binary <- c("logistic", "beta", "isotonic", "isotonic_boot")
types_multiclass <- c("multinomial", "beta", "isotonic", "isotonic_boot")
# a check function to be called when a container is being `fit()`ted.
# by the time a container is fitted, we have:
# * `adjust_type`, the `type` argument passed to an `adjust_*` function
#     * this argument has already been checked to agree with the kind of
#       `adjust_*()` function via `arg_match0()`.
# * `container_type`, the `type` argument either specified in `container()`
#   or inferred in `fit.container()`.
check_type <- function(adjust_type,
                       container_type,
                       arg = caller_arg(adjust_type),
                       call = caller_env()) {
  # if no `adjust_type` was supplied, infer a reasonable one based on the
  # `container_type`
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

