# adjustment printing

    Code
      tailor() %>% adjust_probability_threshold()
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Adjust probability threshold to 0.5.

---

    Code
      tailor() %>% adjust_probability_threshold(hardhat::tune())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Adjust probability threshold to optimized value.

