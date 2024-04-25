# adjustment printing

    Code
      ctr_cls %>% adjust_probability_calibration(dummy_cls_cal)
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Re-calibrate classification probabilities.

# errors informatively with bad input

    Code
      adjust_probability_calibration(ctr_cls)
    Condition
      Error in `adjust_probability_calibration()`:
      ! `calibrator` is absent but must be supplied.

---

    Code
      adjust_probability_calibration(ctr_cls, "boop")
    Condition
      Error in `adjust_probability_calibration()`:
      ! `calibrator` should be a <cal_binary> or <cal_multinomial> object (`?probably::cal_estimate_logistic()`), not a string.

---

    Code
      adjust_probability_calibration(ctr_cls, dummy_reg_cal)
    Condition
      Error in `adjust_probability_calibration()`:
      ! `calibrator` should be a <cal_binary> or <cal_multinomial> object (`?probably::cal_estimate_logistic()`), not a <cal_regression> object.

