# errors informatively with bad input

    Code
      adjust_numeric_calibration(container())
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `calibrator` is absent but must be supplied.

---

    Code
      adjust_numeric_calibration(container(), "boop")
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `calibrator` should be a <cal_regression> object (`?probably::cal_estimate_linear()`), not a string.

---

    Code
      adjust_numeric_calibration(container(), dummy_cls_cal)
    Condition
      Error in `adjust_numeric_calibration()`:
      ! `calibrator` should be a <cal_regression> object (`?probably::cal_estimate_linear()`), not a <cal_binary> object.

