# adjustment printing

    Code
      ctr_reg %>% adjust_numeric_calibration(dummy_reg_cal)
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Re-calibrate numeric predictions.

# errors informatively with bad input

    Code
      adjust_numeric_calibration(ctr_reg)
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `calibrator` is absent but must be supplied.

---

    Code
      adjust_numeric_calibration(ctr_reg, "boop")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `calibrator` should be a <cal_regression> object (`?probably::cal_estimate_linear()`), not a string.

---

    Code
      adjust_numeric_calibration(ctr_cls, dummy_cls_cal)
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `calibrator` should be a <cal_regression> object (`?probably::cal_estimate_linear()`), not a <cal_binary> object.

