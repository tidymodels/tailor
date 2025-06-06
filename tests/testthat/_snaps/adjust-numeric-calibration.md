# adjustment printing

    Code
      adjust_numeric_calibration(tailor())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Re-calibrate numeric predictions using method.

---

    Code
      adjust_numeric_calibration(tailor(), method = "isotonic")
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Re-calibrate numeric predictions using isotonic method.

---

    Code
      adjust_numeric_calibration(tailor(), method = hardhat::tune())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Re-calibrate numeric predictions (method marked for optimization).

---

    Code
      fit(adjust_numeric_calibration(tailor()), mtcars, outcome = mpg, estimate = disp)
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Re-calibrate numeric predictions using method. [trained]

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

