
#' Tidy a tailor object
#'
#' @description
#' Describe a tailor's adjustments in a tibble with one row per adjustment.
#'
#' @param x A [tailor()] object.
#' @param number Optional. A single integer between 1 and the number of
#' adjustments.
#' @param ... Currently unused; must be empty.
#'
#' @returns
#' A tibble containing information about the tailor's adjustments including
#' their ordering, whether they've been trained, and whether they require
#' training with a separate calibration set.
#'
#' @export
tidy.tailor <- function(x, number = NA, ...) {
  n_adjustments <- length(x$adjustments)
  check_number_whole(
    number, min = 1, max = as.double(n_adjustments), allow_na = TRUE
  )
  check_dots_empty()
  if (is.na(number)) {
    number <- seq_len(n_adjustments)
  }

  res <- adjustment_orderings(x$adjustments[number])

  res <- vctrs::vec_cbind(
    number = number,
    res,
    trained = purrr::map_lgl(x$adjustments[number], purrr::pluck, "trained"),
    requires_training = purrr::map_lgl(
      x$adjustments[number], purrr::pluck, "requires_fit"
    )
  )

  tibble::new_tibble(res)
}
