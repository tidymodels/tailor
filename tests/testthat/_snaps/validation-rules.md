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
      ! adjustments that change the hard class predictions must come after adjustments that update the class probability estimates.

---

    Code
      tailor() %>% adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_calibration()`:
      ! adjustments that change the hard class predictions must come after adjustments that update the class probability estimates.

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_calibration()`:
      ! adjustments that change the hard class predictions must come after adjustments that update the class probability estimates.

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_calibration()`:
      ! adjustments that change the hard class predictions must come after adjustments that update the class probability estimates.

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_threshold(threshold = 0.5) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_threshold()`:
      ! adjustments cannot be duplicated: "probability_threshold"

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_threshold(threshold = 0.5) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_threshold()`:
      ! adjustments cannot be duplicated: "probability_threshold"

---

    Code
      tailor() %>% adjust_equivocal_zone(value = 0.2) %>%
        adjust_probability_threshold(threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Equivocal zone addition should come after adjustments that update the class probability estimates or hard class predictions.

---

    Code
      tailor() %>% adjust_equivocal_zone(value = 0.2) %>%
        adjust_probability_threshold(threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Equivocal zone addition should come after adjustments that update the class probability estimates or hard class predictions.

