# tailor printing

    Code
      tailor()
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 0 adjustments.

---

    Code
      tailor()
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 0 adjustments.

---

    Code
      adjust_probability_threshold(tailor(), 0.2)
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Adjust probability threshold to 0.2.

---

    Code
      adjust_equivocal_zone(adjust_probability_threshold(tailor(), 0.2))
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 2 adjustments:
      
      * Adjust probability threshold to 0.2.
      * Add equivocal zone of size 0.1.

# error informatively with empty tidyselections

    Code
      tailor_fit <- fit(adjust_probability_threshold(tailor(), 0.5),
      two_class_example, outcome = "truth_WRONG", estimate = "predicted",
      probabilities = tidyselect::contains("Class"))
    Condition
      Error in `fit()`:
      ! Can't select columns that don't exist.
      x Column `truth_WRONG` doesn't exist.

---

    Code
      tailor_fit <- fit(adjust_probability_threshold(tailor(), 0.5),
      two_class_example, outcome = contains("truth_WRONG"), estimate = "predicted",
      probabilities = tidyselect::contains("Class"))
    Condition
      Error in `fit()`:
      ! `outcome` must select at least one column.
      x Selector `contains("truth_WRONG")` did not match any columns in `.data`.

---

    Code
      tailor_fit <- fit(adjust_probability_threshold(tailor(), 0.5),
      two_class_example, outcome = "truth", estimate = "predicted_WRONG",
      probabilities = tidyselect::contains("Class"))
    Condition
      Error in `fit()`:
      ! Can't select columns that don't exist.
      x Column `predicted_WRONG` doesn't exist.

---

    Code
      tailor_fit <- fit(adjust_probability_threshold(tailor(), 0.5),
      two_class_example, outcome = "truth", estimate = contains("predicted_WRONG"),
      probabilities = tidyselect::contains("Class"))
    Condition
      Error in `fit()`:
      ! `estimate` must select at least one column.
      x Selector `contains("predicted_WRONG")` did not match any columns in `.data`.

---

    Code
      tailor_fit <- fit(adjust_probability_threshold(tailor(), 0.5),
      two_class_example, outcome = contains("truth"), estimate = "predicted",
      probabilities = tidyselect::contains("Class_WRONG"))
    Condition
      Error in `fit()`:
      ! `probabilities` must select at least one column.
      x Selector `tidyselect::contains("Class_WRONG")` did not match any columns in `.data`.

---

    Code
      tailor_fit <- fit(adjust_numeric_range(tailor(), 0.5), two_class_example,
      outcome = "Class1", estimate = ".pred", probabilities = tidyselect::contains(
        "Class_WRONG"))
    Condition
      Error in `fit()`:
      ! Can't select columns that don't exist.
      x Column `.pred` doesn't exist.

