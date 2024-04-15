
input_vals  <- c("numeric", "probability", "class")
output_vals <- c("numeric", "probability", "class")

new_operation <- function(cls, inputs, outputs, arguments, results = list(trained = FALSE), ...) {
  # check outputs too: multi choice
  inputs <- rlang::arg_match0(inputs, input_vals)
  res <-
    list(
      inputs = inputs,
      outputs = sort(outputs),
      arguments = arguments,
      results = results
    )
  class(res) <- cls
  res
}
