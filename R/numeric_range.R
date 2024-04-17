#' Truncate the range of numeric predictions
#'
#' @param x A [container()].
#' @param upper_limit,lower_limit A numeric value, NA (for no truncation) or
#' [hardhat::tune()].

#' @export
adjust_numeric_range <- function(x, lower_limit = -Inf, upper_limit = Inf) {

  # input checks are done via probably::bound_prediction
  op <-
    new_operation(
      "numeric_range",
      inputs = "numeric",
      outputs = "numeric",
      arguments = list(lower_limit = lower_limit, upper_limit = upper_limit),
      results = list(trained = FALSE)
    )

  new_container(
    mode = x$mode,
    type = x$type,
    operations = c(x$operations, list(op)),
    columns = x$dat,
    ptype = x$ptype,
    call = rlang::current_env()
  )
}

#' @export
print.numeric_range <- function(x, ...) {

  # todo could be na
  if ( !is_tune(x$arguments$lower_limit) ) {
    if ( !is_tune(x$arguments$upper_limit) ) {
      rng_txt <-
        paste0(
          "between [" ,
          signif(x$arguments$lower_limit, 3),
          ", ",
          signif(x$arguments$upper_limit, 3),
          "]"
        )
    } else {
      rng_txt <- paste0("between [" , signif(x$arguments$lower_limit, 3), ", ?]")
    }
  } else {
    if ( !is_tune(x$arguments$upper_limit) ) {
      rng_txt <- paste0("between [?, ", signif(x$arguments$upper_limit, 3), "]")
    } else {
      rng_txt <- "between [?, ?]"
    }

  }

  cli::cli_inform("Constrain numeric predictions to be {rng_txt}.")
  invisible(x)
}

#' @export
fit.numeric_range <- function(object, data, parent = NULL, ...) {
  new_operation(
    class(object),
    inputs = object$inputs,
    outputs = object$outputs,
    arguments = object$arguments,
    results = list(trained = TRUE)
  )
}

#' @export
predict.numeric_range <- function(object, new_data, parent, ...) {
  est_nm <- parent$columns$estimate
  lo <- object$arguments$lower_limit
  hi <- object$arguments$upper_limit

  # todo depends on tm predict col names
  new_data[[ est_nm ]] <-
    probably::bound_prediction(new_data, lower_limit = lo, upper_limit = hi)[[ est_nm ]]
  new_data
}

#' @export
required_pkgs.numeric_range <- function(x, ...) {
  c("container", "probably")
}

#' @export
tunable.numeric_range <- function(x, ...) {
  tibble::tibble(
    name = c("lower_limit", "upper_limit"),
    call_info = list(list(pkg = "dials", fun = "lower_limit"), # todo make these dials functions
                     list(pkg = "dials", fun = "upper_limit")),
    source = "container",
    component = "numeric_range",
    component_id = "numeric_range")
}

# todo missing methods:
# todo tune_args
# todo tidy
# todo extract_parameter_set_dials
