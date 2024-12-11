# validation of adjustments (regression)

    Code
      tailor() %>% adjust_numeric_range(lower_limit = 2) %>%
        adjust_numeric_calibration() %>% adjust_predictions_custom(squared = .pred^2)
    Condition
      Error in `adjust_numeric_calibration()`:
      ! Calibration should come before other adjustments.

# validation of adjustments (classification)

    Code
      tailor() %>% adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_calibration()`:
      ! Adjustments that change the hard class predictions must come after adjustments that update the class probability estimates.

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_calibration()`:
      ! Adjustments that change the hard class predictions must come after adjustments that update the class probability estimates.

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_threshold(threshold = 0.5) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_threshold()`:
      ! Adjustment `probability_threshold()` was duplicated.

---

    Code
      tailor() %>% adjust_equivocal_zone(value = 0.2) %>%
        adjust_probability_threshold(threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Equivocal zone addition should come after adjustments that update the class probability estimates or hard class predictions.

# validation of adjustments (incompatible types)

    Code
      tailor() %>% adjust_numeric_calibration() %>% adjust_probability_threshold()
    Condition
      Error in `adjust_probability_threshold()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustment `adjust_numeric_calibration()` operates on numerics while `adjust_probability_threshold()` operates on probabilities.

---

    Code
      tailor() %>% adjust_probability_calibration("logistic") %>%
        adjust_probability_threshold(threshold = 0.4) %>% adjust_numeric_range(
        lower_limit = 2)
    Condition
      Error in `adjust_numeric_range()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustment `adjust_numeric_range()` operates on numerics while `adjust_probability_calibration()` and `adjust_probability_threshold()` operate on probabilities.

---

    Code
      tailor() %>% adjust_numeric_calibration() %>% adjust_numeric_range(lower_limit = 2) %>%
        adjust_probability_threshold(threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustments `adjust_numeric_calibration()` and `adjust_numeric_range()` operate on numerics while `adjust_probability_threshold()` operates on probabilities.

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_numeric_calibration() %>% adjust_probability_threshold()
    Condition
      Error in `adjust_probability_threshold()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustment `adjust_numeric_calibration()` operates on numerics while `adjust_probability_threshold()` operates on probabilities.

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_calibration("logistic") %>% adjust_probability_threshold(
        threshold = 0.4) %>% adjust_numeric_range(lower_limit = 2)
    Condition
      Error in `adjust_numeric_range()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustment `adjust_numeric_range()` operates on numerics while `adjust_probability_calibration()` and `adjust_probability_threshold()` operate on probabilities.

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_numeric_calibration() %>% adjust_numeric_range(lower_limit = 2) %>%
        adjust_probability_threshold(threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustments `adjust_numeric_calibration()` and `adjust_numeric_range()` operate on numerics while `adjust_probability_threshold()` operates on probabilities.

