# validation of operations (regression)

    Code
      container(mode = "regression") %>% adjust_numeric_range(lower_limit = 2) %>%
        adjust_numeric_calibration(dummy_reg_cal) %>% adjust_predictions_custom(
        squared = .pred^2)
    Condition
      Error in `adjust_numeric_calibration()`:
      ! Calibration should come before other operations.

# validation of operations (classification)

    Code
      container(mode = "classification") %>% adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration(dummy_cls_cal)
    Condition
      Error in `adjust_probability_calibration()`:
      ! Operations that change the hard class predictions must come after operations that update the class probability estimates.

---

    Code
      container(mode = "classification") %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_calibration(dummy_cls_cal)
    Condition
      Error in `adjust_probability_calibration()`:
      ! Operations that change the hard class predictions must come after operations that update the class probability estimates.

---

    Code
      container(mode = "classification") %>% adjust_predictions_custom(veg = "potato") %>%
        adjust_probability_threshold(threshold = 0.4) %>%
        adjust_probability_threshold(threshold = 0.5) %>%
        adjust_probability_calibration(dummy_cls_cal)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Operations cannot be duplicated: "probability_threshold"

---

    Code
      container(mode = "classification") %>% adjust_equivocal_zone(value = 0.2) %>%
        adjust_probability_threshold(threshold = 0.4)
    Condition
      Error in `adjust_probability_threshold()`:
      ! Equivocal zone addition should come after operations that update the class probability estimates or hard class predictions.
