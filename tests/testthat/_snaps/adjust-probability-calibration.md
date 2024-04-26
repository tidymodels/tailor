# adjustment printing

    Code
      ctr_cls %>% adjust_probability_calibration("logistic")
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Re-calibrate classification probabilities.

# errors informatively with bad input

    Code
      adjust_probability_calibration(ctr_cls, "boop")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `type` must be one of "linear", "isotonic", "isotonic_boot", "logistic", "beta", or "multinomial", not "boop".

---

    Code
      container("regression", "regression") %>% adjust_probability_calibration(
        "binary")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `type` must be one of "linear", "isotonic", or "isotonic_boot", not "binary".
      i Did you mean "linear"?

