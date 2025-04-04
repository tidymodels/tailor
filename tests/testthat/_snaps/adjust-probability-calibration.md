# adjustment printing

    Code
      tailor() %>% adjust_probability_calibration("logistic")
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Re-calibrate classification probabilities.

# errors informatively with bad input

    Code
      adjust_probability_calibration(tailor(), "boop")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `method` must be one of "logistic", "multinomial", "beta", "isotonic", or "isotonic_boot", not "boop".

---

    Code
      tailor() %>% adjust_probability_calibration("linear")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `method` must be one of "logistic", "multinomial", "beta", "isotonic", or "isotonic_boot", not "linear".

# tuning the calibration method

    Code
      fit(tlr, d_calibration, outcome = c(truth), estimate = c(predicted),
      probabilities = c(Class1, Class2))
    Condition
      Error in `check_method()`:
      ! The calibration method cannot be a value of `tune()` at `fit()` time.

