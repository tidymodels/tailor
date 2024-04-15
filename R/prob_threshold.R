#' Change the event threshold
#'
#' @param x A [container()].
#' @param threshold A numeric value (between zero and one) or [hardhat::tune()].
#' @examples
#' library(dplyr)
#' library(modeldata)
#'
#' post_obj <-
#'   container(mode = "classification") %>%
#'   adjust_prob_threshold(threshold = .1)
#'
#' two_class_example %>% count(predicted)
#'
#' post_res <- fit(
#'   post_obj,
#'   two_class_example,
#'   outcome = c(truth),
#'   estimate = c(predicted),
#'   probabilities = c(Class1, Class2)
#' )
#'
#' predict(post_res, two_class_example) %>% count(predicted)
#' @export
adjust_prob_threshold <- function(x, threshold = 0.5) {
  op <-
    new_operation(
      "prob_threshold",
      inputs = "probability",
      outputs = "class",
      arguments = list(threshold = threshold),
      results = list(trained = FALSE)
    )
  x$operations <- c(x$operations, list(op))
  x
}

#' @export
print.prob_threshold <- function(x, ...) {
  # check for tune() first

  trn <- ifelse(x$results$trained, " [trained]", "")

  cli::cli_inform(c("Adjust probability threshold to  \\
                    {signif(x$arguments$threshold, digits = 3)}{trn}"))
  invisible(x)
}

#' @export
fit.prob_threshold <- function(object, data, parent = NULL, ...) {
  new_operation(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(trained = TRUE)
  )
}

#' @export
predict.prob_threshold <- function(object, new_data, parent, ...) {
  est_nm <- parent$columns$estimate
  prob_nm <- parent$columns$probabilities[1]
  lvls <- levels(new_data[[ est_nm ]])

  new_data[[ est_nm ]] <-
    ifelse(new_data[[ prob_nm ]] >= object$arguments$threshold, lvls[1], lvls[2])
  new_data[[ est_nm ]] <- factor(new_data[[ est_nm ]], levels = lvls)
  new_data
}

#' @export
required_pkgs.prob_threshold <- function(x, ...) {
  c("container")
}

#' @export
tunable.prob_threshold <- function(x, ...) {
  tibble::tibble(
    name = "threshold",
    call_info = list(list(pkg = "dials", fun = "threshold")),
    source = "container",
    component = "prob_threshold",
    component_id = "prob_threshold")
}

# todo missing methods:
# todo tune_args
# todo tidy
# todo extract_parameter_set_dials
