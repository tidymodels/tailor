# adjustment printing

    Code
      adjust_probability_calibration(tailor())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Re-calibrate classification probabilities using method.

---

    Code
      adjust_probability_calibration(tailor(), method = "logistic")
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Re-calibrate classification probabilities using logistic method.

---

    Code
      adjust_probability_calibration(tailor(), method = hardhat::tune())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Re-calibrate classification probabilities (method marked for optimization).

---

    Code
      fit(adjust_probability_calibration(tailor()), two_class_example, outcome = c(
        truth), estimate = c(predicted), probabilities = c(Class1, Class2))
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Re-calibrate classification probabilities using method.  [trained]

# errors informatively with bad input

    Code
      adjust_probability_calibration(tailor(), "boop")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `method` must be one of "logistic", "multinomial", "beta", "isotonic", or "isotonic_boot", not "boop".

---

    Code
      adjust_probability_calibration(tailor(), "linear")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `method` must be one of "logistic", "multinomial", "beta", "isotonic", or "isotonic_boot", not "linear".

# tuning the calibration method

    Code
      fit(tlr, d_calibration, outcome = c(truth), estimate = c(predicted),
      probabilities = c(Class1, Class2))
    Condition
      Error in `fit()`:
      ! The calibration method cannot be a value of `tune()` at `fit()` time.

