# adjustment printing

    Code
      tailor() %>% adjust_numeric_calibration()
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
      tailor() %>% adjust_numeric_calibration("binary")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `method` must be one of "linear", "isotonic", or "isotonic_boot", not "binary".
      i Did you mean "linear"?

