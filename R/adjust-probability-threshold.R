#' Change the event threshold
#'
#' @param x A [tailor()].
#' @param threshold A numeric value (between zero and one) or [hardhat::tune()].
#' @examplesIf rlang::is_installed("modeldata")
#' library(dplyr)
#' library(modeldata)
#'
#' post_obj <-
#'   tailor() %>%
#'   adjust_probability_threshold(threshold = .1)
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
adjust_probability_threshold <- function(x, threshold = 0.5) {
  check_tailor(x)
  if (!is_tune(threshold)) {
    check_number_decimal(threshold, min = 10^-10, max = 1 - 10^-10)
  }

  adj <-
    new_adjustment(
      "probability_threshold",
      inputs = "probability",
      outputs = "class",
      arguments = list(threshold = threshold),
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
print.probability_threshold <- function(x, ...) {
  # check for tune() first

  if (is_tune(x$arguments$threshold)) {
    cli::cli_bullets(c("*" = "Adjust probability threshold to optimized value."))
  } else {
    trn <- ifelse(x$trained, " [trained]", "")
    cli::cli_bullets(c(
      "*" = "Adjust probability threshold to \\
             {signif(x$arguments$threshold, digits = 3)}.{trn}"
    ))
  }
  invisible(x)
}

#' @export
fit.probability_threshold <- function(object, data, tailor = NULL, ...) {
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
predict.probability_threshold <- function(object, new_data, tailor, ...) {
  est_nm <- tailor$columns$estimate
  prob_nm <- tailor$columns$probabilities[1]
  lvls <- levels(new_data[[est_nm]])

  new_data[[est_nm]] <-
    ifelse(new_data[[prob_nm]] >= object$arguments$threshold, lvls[1], lvls[2])
  new_data[[est_nm]] <- factor(new_data[[est_nm]], levels = lvls)
  new_data
}

#' @export
required_pkgs.probability_threshold <- function(x, ...) {
  c("tailor")
}

#' @export
tunable.probability_threshold <- function(x, ...) {
  tibble::new_tibble(list(
    name = "threshold",
    call_info = list(list(pkg = "dials", fun = "threshold")),
    source = "tailor",
    component = "probability_threshold",
    component_id = "probability_threshold"
  ))
}

# todo missing methods:
# todo tune_args
# todo tidy
# todo extract_parameter_set_dials
