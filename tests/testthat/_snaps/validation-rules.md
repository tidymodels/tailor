# validation of adjustments (regression)

    Code
      adjust_predictions_custom(adjust_numeric_calibration(adjust_numeric_range(
        tailor(), lower_limit = 2)), squared = .pred^2)
    Condition
      Error in `adjust_numeric_calibration()`:
      ! Calibration should come before other adjustments.

# validation of adjustments (classification)

    Code
      adjust_probability_calibration(adjust_probability_threshold(tailor(),
      threshold = 0.4))
    Condition
      Error in `adjust_probability_calibration()`:
      ! Adjustments that change the hard class predictions must come after adjustments that update the class probability estimates.

---

    Code
      adjust_probability_calibration(adjust_probability_threshold(
        adjust_predictions_custom(tailor(), veg = "potato"), threshold = 0.4))
    Condition
      Error in `adjust_probability_calibration()`:
      ! Adjustments that change the hard class predictions must come after adjustments that update the class probability estimates.

---

    Code
      adjust_probability_calibration(adjust_probability_threshold(
        adjust_probability_threshold(adjust_predictions_custom(tailor(), veg = "potato"),
        threshold = 0.4), threshold = 0.5))
    Condition
      Error in `adjust_probability_threshold()`:
      ! Adjustment `probability_threshold()` was duplicated.

---

    Code
      adjust_probability_threshold(adjust_equivocal_zone(tailor(), value = 0.2),
      threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Equivocal zone addition should come after adjustments that update the class probability estimates or hard class predictions.

# validation of adjustments (incompatible types)

    Code
      adjust_probability_threshold(adjust_numeric_calibration(tailor()))
    Condition
      Error in `adjust_probability_threshold()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustment `adjust_numeric_calibration()` operates on numerics while `adjust_probability_threshold()` operates on probabilities.

---

    Code
      adjust_numeric_range(adjust_probability_threshold(
        adjust_probability_calibration(tailor(), "logistic"), threshold = 0.4),
      lower_limit = 2)
    Condition
      Error in `adjust_numeric_range()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustment `adjust_numeric_range()` operates on numerics while `adjust_probability_calibration()` and `adjust_probability_threshold()` operate on probabilities.

---

    Code
      adjust_probability_threshold(adjust_numeric_range(adjust_numeric_calibration(
        tailor()), lower_limit = 2), threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustments `adjust_numeric_calibration()` and `adjust_numeric_range()` operate on numerics while `adjust_probability_threshold()` operates on probabilities.

---

    Code
      adjust_probability_threshold(adjust_numeric_calibration(
        adjust_predictions_custom(tailor(), veg = "potato")))
    Condition
      Error in `adjust_probability_threshold()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustment `adjust_numeric_calibration()` operates on numerics while `adjust_probability_threshold()` operates on probabilities.

---

    Code
      adjust_numeric_range(adjust_probability_threshold(
        adjust_probability_calibration(adjust_predictions_custom(tailor(), veg = "potato"),
        "logistic"), threshold = 0.4), lower_limit = 2)
    Condition
      Error in `adjust_numeric_range()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustment `adjust_numeric_range()` operates on numerics while `adjust_probability_calibration()` and `adjust_probability_threshold()` operate on probabilities.

---

    Code
      adjust_probability_threshold(adjust_numeric_range(adjust_numeric_calibration(
        adjust_predictions_custom(tailor(), veg = "potato")), lower_limit = 2),
      threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Can't compose adjustments for different prediction types.
      i Adjustments `adjust_numeric_calibration()` and `adjust_numeric_range()` operate on numerics while `adjust_probability_threshold()` operates on probabilities.

