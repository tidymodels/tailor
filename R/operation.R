
# These values are used to specify "what will we need for the adjustment?" and
# "what will we change?". For the outputs, we cannot change the probabilities
# without changing the classes. This is important because we are going to have
# to define constrains on the order of adjustments.

input_vals  <- c("numeric", "probability", "class", "everything")
output_vals <- c("numeric", "probability_class", "class", "everything")

new_operation <- function(cls, inputs, outputs, arguments, results = list(trained = FALSE), ...) {
  inputs  <- rlang::arg_match0(inputs,  input_vals)
  outputs <- rlang::arg_match0(outputs, output_vals)

  if ( !any(names(results) == "trained") ){
    cli::cli_abort("The {.arg results} slot requires a logical variable called \\
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
  class(res) <- cls
  res
}
