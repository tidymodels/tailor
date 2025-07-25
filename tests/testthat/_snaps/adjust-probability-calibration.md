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
      fit(adjust_probability_calibration(tailor()), d_bin_calibration, outcome = c(y),
      estimate = c(predicted), probabilities = c(a, b))
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Re-calibrate classification probabilities using method. [trained]

# errors informatively with bad input

    Code
      adjust_probability_calibration(tailor(), "boop")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `method` must be one of "logistic", "multinomial", "beta", "isotonic", "isotonic_boot", or "none", not "boop".

---

    Code
      adjust_probability_calibration(tailor(), "linear")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `method` must be one of "logistic", "multinomial", "beta", "isotonic", "isotonic_boot", or "none", not "linear".

# tuning the calibration method

    Code
      fit(tlr, d_bin_calibration, outcome = c(y), estimate = c(predicted),
      probabilities = c(a, b))
    Condition
      Error in `fit()`:
      ! The calibration method cannot be a value of `tune()` at `fit()` time.

# too few data

    Code
      tlr_fit <- fit(tlr, d_bin_calibration[0, ], outcome = c(y), estimate = c(
        predicted), probabilities = c(a, b))
    Message
      The calibration data has 0 rows. There is not enough data for calibration so `method` is changed from "logistic" to "none".

---

    Code
      tlr_fit <- fit(tlr, d_bin_calibration[1, ], outcome = c(y), estimate = c(
        predicted), probabilities = c(a, b))
    Message
      The calibration data has 1 row. There is not enough data for calibration so `method` is changed from "logistic" to "none".

