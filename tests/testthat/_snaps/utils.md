# check_tailor raises informative error

    Code
      adjust_probability_threshold("boop")
    Condition
      Error in `adjust_probability_threshold()`:
      ! `x` should be a <tailor> (`?tailor::tailor()`), not a string.

# check_calibration_type errors informatively

    Code
      check_calibration_type("probability", "numeric", "regression")
    Condition
      Error in `check_calibration_type()`:
      ! A regression tailor is incompatible with the adjustment `adjust_probability_calibration()`.

---

    Code
      check_calibration_type("numeric", "probability", "binary")
    Condition
      Error in `check_calibration_type()`:
      ! A binary tailor is incompatible with the adjustment `adjust_numeric_calibration()`.

---

    Code
      check_calibration_type("numeric", "probability", "multiclass")
    Condition
      Error in `check_calibration_type()`:
      ! A multiclass tailor is incompatible with the adjustment `adjust_numeric_calibration()`.

# errors informatively without probably installed

    Code
      tailor() %>% adjust_numeric_calibration()
    Condition
      Error in `adjust_numeric_calibration()`:
      ! The probably package must be available to use this adjustment.

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

# check_selection() errors informatively

    Code
      check_selection(quote(contains("boop")), numeric(0), ".data")
    Condition
      Error:
      ! `.data` must select at least one column.
      x Selector `contains("boop")` did not match any columns in `.data`.

