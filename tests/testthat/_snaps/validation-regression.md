# validation of regression operations

    Code
      container(mode = "regression") %>% adjust_numeric_range(lower_limit = 2) %>%
        adjust_numeric_calibration(dummy_reg_cal) %>% adjust_predictions_custom(
        squared = .pred^2)
    Condition
      Error in `adjust_numeric_calibration()`:
      ! Calibration should come before other operations.

