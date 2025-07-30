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
      ! `method` must be one of "linear", "isotonic", "isotonic_boot", or "none", not "boop".

---

    Code
      adjust_numeric_calibration(tailor(), "binary")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `method` must be one of "linear", "isotonic", "isotonic_boot", or "none", not "binary".
      i Did you mean "linear"?

# tuning the calibration method

    Code
      fit(tlr, d_reg_calibration, outcome = y, estimate = y_pred)
    Condition
      Error in `fit()`:
      ! The calibration method cannot be a value of `tune()` at `fit()` time.

# too few data

    Code
      fit(tlr, d_reg_calibration[0, ], outcome = y, estimate = y_pred)
    Condition
      Warning:
      The calibration data has 0 rows. There is not enough data for calibration so `method` is changed from "linear" to "none".
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Re-calibrate numeric predictions using linear method. [trained]

---

    Code
      fit(tlr, d_reg_calibration[1, ], outcome = y, estimate = y_pred)
    Condition
      Warning:
      The calibration data has 1 row. There is not enough data for calibration so `method` is changed from "linear" to "none".
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Re-calibrate numeric predictions using linear method. [trained]

# passing arguments to adjust_numeric_calibration

    Code
      tlr_fit <- adjust_numeric_calibration(tailor(), method = "linear", FALSE)
    Condition
      Error in `adjust_numeric_calibration()`:
      ! All calibration arguments passed to `...` should have names.

---

    Code
      tlr_fit <- adjust_numeric_calibration(tailor(), method = "linear", FALSE,
      select = TRUE)
    Condition
      Error in `adjust_numeric_calibration()`:
      ! All calibration arguments passed to `...` should have names.

# harden against calibration model failure

    Code
      pred_fit <- fit(tlr, d_calibration_pred, outcome = y, estimate = y_pred)
    Condition
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning in `glm.fit()`:
      no observations informative at iteration 1
      Warning:
      glm.fit: algorithm did not converge
      Warning:
      The linear calibration failed. No calibration is applied.
      i Error in fit$rank : object of type 'closure' is not subsettable

---

    Code
      y_fit <- fit(tlr, d_calibration_y, outcome = y, estimate = y_pred)
    Condition
      Warning:
      The linear calibration failed. No calibration is applied.
      i Error in mgcv::gam(f_model, data = .data, ...) : Not enough (non-NA) data to do anything meaningful

