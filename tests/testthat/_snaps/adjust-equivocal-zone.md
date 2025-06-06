# adjustment printing

    Code
      adjust_equivocal_zone(tailor())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Add equivocal zone of size 0.1.

---

    Code
      adjust_equivocal_zone(tailor(), hardhat::tune())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Add equivocal zone of optimized size.

---

    Code
      fit(adjust_equivocal_zone(tailor()), two_class_example, outcome = c(truth),
      estimate = c(predicted), probabilities = c(Class1, Class2))
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Add equivocal zone of size 0.1. [trained]

