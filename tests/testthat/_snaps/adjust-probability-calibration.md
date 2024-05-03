# adjustment printing

    Code
      tailor() %>% adjust_probability_calibration("logistic")
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Re-calibrate classification probabilities.

# errors informatively with bad input

    Code
      adjust_probability_calibration(tailor(), "boop")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `method` must be one of "logistic", "multinomial", "beta", "isotonic", or "isotonic_boot", not "boop".

---

    Code
      tailor("regression") %>% adjust_probability_calibration("binary")
    Condition
      Error in `adjust_probability_calibration()`:
      ! A regression tailor is incompatible with the operation `adjust_probability_calibration()`.

---

    Code
      tailor("binary") %>% adjust_probability_calibration("linear")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `method` must be one of "logistic", "multinomial", "beta", "isotonic", or "isotonic_boot", not "linear".

