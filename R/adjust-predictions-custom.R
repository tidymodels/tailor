#' Change or add variables
#'
#' @param x A [container()].
#' @param .pkgs A character string of extra packages that are needed to execute
#' the commands.
#' @param ... Name-value pairs of expressions. See [dplyr::mutate()].
#' @examples
#' library(dplyr)
#' library(modeldata)
#'
#' post_obj <-
#'   container(mode = "classification") %>%
#'   adjust_equivocal_zone() %>%
#'   adjust_predictions_custom(linear_predictor = binomial()$linkfun(Class2))
#'
#'
#' post_res <- fit(
#'   post_obj,
#'   two_class_example,
#'   outcome = c(truth),
#'   estimate = c(predicted),
#'   probabilities = c(Class1, Class2)
#' )
#'
#' predict(post_res, two_class_example)
#' @export
adjust_predictions_custom <- function(x, ..., .pkgs = character(0)) {
  cmds <- enquos(...)

  op <-
    new_operation(
      "predictions_custom",
      inputs = "everything",
      outputs = "everything",
      arguments = list(commands = cmds, pkgs = .pkgs),
      results = list(trained = FALSE)
    )

  new_container(
    mode = x$mode,
    type = x$type,
    operations = c(x$operations, list(op)),
    columns = x$dat,
    ptype = x$ptype,
    call = current_env()
  )
}

#' @export
print.predictions_custom <- function(x, ...) {
  trn <- ifelse(x$results$trained, " [trained]", "")
  cli_inform("Adjust predictions using custom code{trn}.")
  invisible(x)
}

#' @export
fit.predictions_custom <- function(object, data, parent = NULL, ...) {
  new_operation(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(trained = TRUE)
  )
}

#' @export
predict.predictions_custom <- function(object, new_data, parent, ...) {
  dplyr::mutate(new_data, !!!object$arguments$commands)
}

#' @export
required_pkgs.predictions_custom <- function(x, ...) {
  unique(c("container", x$arguments$pkgs))
}

#' @export
tunable.predictions_custom <- function(x, ...) {
  no_param
}

# todo missing methods:
# todo tune_args
# todo tidy
# todo extract_parameter_set_dials