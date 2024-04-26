# adjustment printing

    Code
      ctr_reg %>% adjust_numeric_calibration()
    Message
      
      -- Container -------------------------------------------------------------------
      A regression postprocessor with 1 operation:
      
      * Re-calibrate numeric predictions.

# errors informatively with bad input

    Code
      adjust_numeric_calibration(ctr_reg, "boop")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `type` must be one of "linear", "isotonic", or "isotonic_boot", not "boop".

---

    Code
      container("classification", "binary") %>% adjust_numeric_calibration("linear")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `type` must be one of "logistic", "beta", "isotonic", or "isotonic_boot", not "linear".

