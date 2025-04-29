# adjustment printing

    Code
      adjust_numeric_calibration(tailor())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Re-calibrate numeric predictions.

# errors informatively with bad input

    Code
      adjust_numeric_calibration(tailor(), "boop")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `method` must be one of "linear", "isotonic", or "isotonic_boot", not "boop".

---

    Code
      adjust_numeric_calibration(tailor(), "binary")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `method` must be one of "linear", "isotonic", or "isotonic_boot", not "binary".
      i Did you mean "linear"?

# tuning the calibration method

    Code
      fit(tlr, d_calibration, outcome = y, estimate = y_pred)
    Condition
      Error in `fit()`:
      ! The calibration method cannot be a value of `tune()` at `fit()` time.

