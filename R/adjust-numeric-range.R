#' Truncate the range of numeric predictions
#'
#' @description
#' Truncating ranges involves limiting the output of a model to a specific
#' range of values, typically to avoid extreme or unrealistic predictions.
#' This technique can help improve the practical applicability of a model's
#' outputs by constraining them within reasonable bounds based on domain
#' knowledge or physical limitations.
#'
#' @param x A [tailor()].
#' @param upper_limit,lower_limit A numeric value, NA (for no truncation) or
#' [hardhat::tune()].
#'
#' @inheritSection adjust_equivocal_zone Data Usage
#'
#' @examples
#' library(tibble)
#'
#' # create example data
#' set.seed(1)
#' d <- tibble(y = rnorm(100), y_pred = y/2 + rnorm(100))
#' d
#'
#' # specify calibration
#' tlr <-
#'   tailor() |>
#'   adjust_numeric_range(lower_limit = 1)
#'
#' # train tailor by passing column names.
#' tlr_fit <- fit(tlr, d, outcome = y, estimate = y_pred)
#'
#' predict(tlr_fit, d)
#' @export
adjust_numeric_range <- function(x, lower_limit = -Inf, upper_limit = Inf) {
  validate_probably_available()

  # remaining input checks are done via probably::bound_prediction
  check_tailor(x)

  adj <-
    new_adjustment(
      "numeric_range",
      inputs = "numeric",
      outputs = "numeric",
      arguments = list(lower_limit = lower_limit, upper_limit = upper_limit),
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
print.numeric_range <- function(x, ...) {
  trn <- ifelse(x$trained, " [trained]", "")

  if (is_tune(x$arguments$lower_limit)) {
    lower_limit <- "?"
  } else {
    lower_limit <- signif(x$arguments$lower_limit, 3)
  }
  if (is_tune(x$arguments$upper_limit)) {
    upper_limit <- "?"
  } else {
    upper_limit <- signif(x$arguments$upper_limit, 3)
  }

  rng_txt <- paste0("between [", lower_limit, ", ", upper_limit, "]")

  cli::cli_bullets(c(
    "*" = "Constrain numeric predictions to be {rng_txt}.{trn}"
  ))
  invisible(x)
}

#' @export
fit.numeric_range <- function(object, data, tailor = NULL, ...) {
  validate_probably_available()

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
predict.numeric_range <- function(object, new_data, tailor, ...) {
  validate_probably_available()

  est_nm <- tailor$columns$estimate
  lo <- object$arguments$lower_limit
  hi <- object$arguments$upper_limit

  new_data[[est_nm]] <-
    probably::bound_prediction(
      rename_prediction_column(new_data, est_nm),
      lower_limit = lo,
      upper_limit = hi
    )[[".pred"]]
  new_data
}

rename_prediction_column <- function(data, est_nm) {
  if (identical(est_nm, ".pred")) {
    return(data)
  }
  data[[".pred"]] <- data[[est_nm]]
  data[[est_nm]] <- NULL
  data
}

#' @export
required_pkgs.numeric_range <- function(x, ...) {
  c("tailor", "probably")
}

#' @export
tunable.numeric_range <- function(x, ...) {
  tibble::new_tibble(
    list(
      name = c("lower_limit", "upper_limit"),
      call_info = list(
        list(pkg = "dials", fun = "lower_limit"),
        list(pkg = "dials", fun = "upper_limit")
      ),
      source = c("tailor", "tailor"),
      component = c("numeric_range", "numeric_range"),
      component_id = c("numeric_range", "numeric_range")
    ),
    nrow = 2
  )
}
