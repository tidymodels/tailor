# validation of operations (regression)

    Code
      tailor(type = "regression") %>% adjust_numeric_range(lower_limit = 2) %>%
        adjust_numeric_calibration() %>% adjust_predictions_custom(squared = .pred^2)
    Condition
      Error in `adjust_numeric_calibration()`:
      ! Calibration should come before other operations.

# validation of operations (classification)

    Code
      tailor(type = "binary") %>% adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_calibration()`:
      ! Operations that change the hard class predictions must come after operations that update the class probability estimates.

---

    Code
      tailor() %>% adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_calibration()`:
      ! Operations that change the hard class predictions must come after operations that update the class probability estimates.

---

    Code
      tailor(type = "binary") %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_calibration()`:
      ! Operations that change the hard class predictions must come after operations that update the class probability estimates.

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_calibration()`:
      ! Operations that change the hard class predictions must come after operations that update the class probability estimates.

---

    Code
      tailor(type = "binary") %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_threshold(threshold = 0.5) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_threshold()`:
      ! Operations cannot be duplicated: "probability_threshold"

---

    Code
      tailor() %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_threshold(threshold = 0.5) %>%
        adjust_probability_calibration()
    Condition
      Error in `adjust_probability_threshold()`:
      ! Operations cannot be duplicated: "probability_threshold"

---

    Code
      tailor(type = "binary") %>% adjust_equivocal_zone(value = 0.2) %>%
        adjust_probability_threshold(threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Equivocal zone addition should come after operations that update the class probability estimates or hard class predictions.

---

    Code
      tailor() %>% adjust_equivocal_zone(value = 0.2) %>%
        adjust_probability_threshold(threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Equivocal zone addition should come after operations that update the class probability estimates or hard class predictions.

