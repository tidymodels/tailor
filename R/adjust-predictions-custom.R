#' Change or add variables
#'
#' This adjustment functions allows for arbitrary transformations of model
#' predictions using [dplyr::mutate()] statements.
#'
#' @param x A [tailor()].
#' @param .pkgs A character string of extra packages that are needed to execute
#' the commands.
#' @param ... Name-value pairs of expressions. See [dplyr::mutate()].
#'
#' @section Data-dependent transformations:
#' Note that custom adjustments should not carry out estimation. If they do,
#' the estimation steps will be carried out independently at `fit()`
#' and `predict()` time. For example, if your transformation includes a mean
#' shift, the postprocessor will take the mean of the column supplied in the
#' training data at `fit()` and, rather than reusing that mean at `predict()`
#' will take the mean again of the dataset supplied at `predict()` time.
#'
#' @inheritSection adjust_equivocal_zone Data Usage
#'
#' @return An updated [tailor()] containing the new operation.
#'
#' @examplesIf rlang::is_installed(c("probably", "modeldata"))
#' library(modeldata)
#'
#' head(two_class_example)
#'
#' tlr <-
#'   tailor() |>
#'   adjust_equivocal_zone() |>
#'   adjust_predictions_custom(linear_predictor = binomial()$linkfun(Class2))
#'
#' tlr_fit <- fit(
#'   tlr,
#'   two_class_example,
#'   outcome = c(truth),
#'   estimate = c(predicted),
#'   probabilities = c(Class1, Class2)
#' )
#'
#' predict(tlr_fit, two_class_example) |> head()
#' @export
adjust_predictions_custom <- function(x, ..., .pkgs = character(0)) {
  check_tailor(x)
  cmds <- enquos(...)

  adj <-
    new_adjustment(
      "predictions_custom",
      inputs = "everything",
      outputs = "everything",
      arguments = list(commands = cmds, pkgs = .pkgs),
      results = list(),
      trained = FALSE,
      requires_fit = FALSE
    )

  new_tailor(
    type = x$type,
    adjustments = c(x$adjustments, list(adj)),
    columns = x$dat,
    ptype = x$ptype,
    call = current_env()
  )
}

#' @export
print.predictions_custom <- function(x, ...) {
  trn <- ifelse(x$trained, " [trained]", "")
  cli::cli_bullets(c("*" = "Adjust predictions using custom code.{trn}"))
  invisible(x)
}

#' @export
fit.predictions_custom <- function(object, data, tailor = NULL, ...) {
  new_adjustment(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(),
    trained = TRUE,
    requires_fit = object$requires_fit
  )
}

#' @export
predict.predictions_custom <- function(object, new_data, tailor, ...) {
  dplyr::mutate(new_data, !!!object$arguments$commands)
}

#' @export
required_pkgs.predictions_custom <- function(x, ...) {
  unique(c("tailor", x$arguments$pkgs))
}

#' @export
tunable.predictions_custom <- function(x, ...) {
  no_param
}
