# adjustment printing

    Code
      adjust_probability_threshold(tailor())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Adjust probability threshold to 0.5.

---

    Code
      adjust_probability_threshold(tailor(), hardhat::tune())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Adjust probability threshold to optimized value.

---

    Code
      fit(adjust_probability_threshold(tailor()), two_class_example, outcome = c(
        truth), estimate = c(predicted), probabilities = c(Class1, Class2))
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Adjust probability threshold to 0.5.  [trained]

