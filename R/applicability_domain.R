#' Add model applicability scores
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
#'   adjust_applicability_domain(linear_predictor = binomial()$linkfun(Class2))
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
adjust_applicability_domain <- function(ctr, method = "isolation", ...) {

  method <- rlang::arg_match0(method, c("isolation", "pca", "hat", "similarity"))

  # check for elements in `...`

  input_objs <- rlang::enquos(...)
  if ( length(input_objs) == 0 ) {
    cli::cli_abort("Please pass arguments to for applicability function of \\
                   interest in the {.arg ...}.")
  }

  op <-
    new_operation(
      "applicability_domain",
      inputs = "everything",
      outputs = "everything",
      arguments = list(method = method, inputs = input_objs),
      results = list(trained = FALSE)
    )
  ctr$operations <- c(ctr$operations, list(op))
  ctr
}

#' @export
print.applicability_domain <- function(x, ...) {
  trn <- ifelse(x$results$trained, " [trained]", "")
  cli::cli_inform("Add applicability score{trn}.")
  invisible(x)
}

#' @export
fit.applicability_domain <- function(object, data, parent = NULL, ...) {

  .fn <- paste0("apd_", object$arguments$method)
  cl <- rlang::call2(.ns = "applicable", .fn = .fn, !!!object$arguments$inputs)
  ad_fit <- rlang::eval_tidy(cl)
  object$arguments$inputs <- list()

  new_operation(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(trained = TRUE, fit = ad_fit)
  )
}

#' @export
predict.applicability_domain <- function(object, new_data, parent, ...) {
  cl <- rlang::call2(.ns = "applicable", .fn = "score",
                     object = object$results$fit, new_data = new_data)
  apd_cols <- rlang::eval_tidy(cl)
  dplyr::bind_cols(new_data, apd_cols)
}

#' @export
required_pkgs.applicability_domain <- function(x, ...) {
  unique(c("container")) # todo add applibable packages
}

#' @export
tunable.applicability_domain <- function(x, ...) {
  no_param
}

# todo missing methods:
# todo tune_args
# todo tidy
# todo extract_parameter_set_dials
