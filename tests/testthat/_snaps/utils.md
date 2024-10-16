# check_tailor raises informative error

    Code
      adjust_probability_threshold("boop")
    Condition
      Error in `adjust_probability_threshold()`:
      ! `x` should be a <tailor> (`?tailor::tailor()`), not a string.

# fit.tailor() errors informatively with incompatible outcome

    Code
      fit(tailor() %>% adjust_probability_threshold(0.1), two_class_example, outcome = c(
        test_numeric), estimate = c(predicted), probabilities = c(Class1, Class2))
    Condition
      Error in `fit()`:
      ! Tailors with binary adjustments are not compatible with <numeric> outcomes.

---

    Code
      fit(tailor() %>% adjust_numeric_range(lower_limit = 0.1), two_class_example,
      outcome = c(truth), estimate = c(Class1))
    Condition
      Error in `fit()`:
      ! Tailors with regression adjustments are not compatible with <factor> outcomes.

---

    Code
      fit(tailor() %>% adjust_probability_threshold(0.1), two_class_example, outcome = c(
        test_date), estimate = c(predicted), probabilities = c(Class1, Class2))
    Condition
      Error in `fit()`:
      ! Tailors with binary adjustments are not compatible with <POSIXct/POSIXt> outcomes.

---

    Code
      fit(tailor() %>% adjust_predictions_custom(hey = "there"), two_class_example,
      outcome = c(test_date), estimate = c(predicted), probabilities = c(Class1))
    Condition
      Error in `fit()`:
      ! Only factor and numeric outcomes are currently supported.

# find_tune_id() works

    Code
      find_tune_id(x)
    Condition
      Error:
      ! Only one tunable value is currently allowed per argument.
      `x` has `list(a = tune(), b = tune())`.

