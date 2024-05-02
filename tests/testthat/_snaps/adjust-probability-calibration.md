# adjustment printing

    Code
      container() %>% adjust_probability_calibration("logistic")
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Re-calibrate classification probabilities.

# errors informatively with bad input

    Code
      adjust_probability_calibration(container(), "boop")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `method` must be one of "logistic", "multinomial", "beta", "isotonic", or "isotonic_boot", not "boop".

---

    Code
      container("regression") %>% adjust_probability_calibration("binary")
    Condition
      Error in `adjust_probability_calibration()`:
      ! A regression container is incompatible with the operation `adjust_probability_calibration()`.

---

    Code
      container("binary") %>% adjust_probability_calibration("linear")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `method` must be one of "logistic", "multinomial", "beta", "isotonic", or "isotonic_boot", not "linear".

