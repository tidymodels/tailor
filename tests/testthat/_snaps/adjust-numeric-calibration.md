# adjustment printing

    Code
      tailor() %>% adjust_numeric_calibration()
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Re-calibrate numeric predictions.

# errors informatively with bad input

    Code
      adjust_numeric_calibration(tailor(), "boop")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `method` must be one of "linear", "isotonic", or "isotonic_boot", not "boop".

---

    Code
      tailor("binary") %>% adjust_numeric_calibration("linear")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! A binary tailor is incompatible with the operation `adjust_numeric_calibration()`.

---

    Code
      tailor("regression") %>% adjust_numeric_calibration("binary")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `method` must be one of "linear", "isotonic", or "isotonic_boot", not "binary".
      i Did you mean "linear"?

