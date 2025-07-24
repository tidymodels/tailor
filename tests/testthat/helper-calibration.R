library(dplyr)

set.seed(1)
d_reg_calibration <- dplyr::tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))
d_reg_test <- dplyr::tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))

# ------------------------------------------------------------------------------

set.seed(1)
d_bin_calibration <-
  dplyr::tibble(y = factor(rep(letters[1:2], each = 50)), a = runif(100)) |>
  dplyr::mutate(b = 1 - a, predicted = sample(y))
d_bin_test <-
  dplyr::tibble(y = factor(rep(letters[1:2], each = 50)), a = runif(100)) |>
  dplyr::mutate(b = 1 - a, predicted = sample(y))

# ------------------------------------------------------------------------------

set.seed(1)
probs <- matrix(runif(2 * 3 * 3 * 50), ncol = 3)
probs <- apply(probs, 1, function(x) x/sum(x))

d_mlt_calibration <-
  dplyr::tibble(
    y = factor(rep(letters[1:3], each = 50)),
    a = probs[1, 1:150],
    b = probs[2, 1:150],
    c = probs[3, 1:150]
  ) |>
  dplyr::mutate(predicted = sample(y))
d_mlt_test <-
  dplyr::tibble(
    y = factor(rep(letters[1:3], each = 50)),
    a = probs[1, 151:300],
    b = probs[2, 151:300],
    c = probs[3, 151:300]
  ) |>
  dplyr::mutate(predicted = sample(y))
