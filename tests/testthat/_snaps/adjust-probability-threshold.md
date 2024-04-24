# adjustment printing

    Code
      container() %>% adjust_probability_threshold()
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Adjust probability threshold to 0.5.

---

    Code
      container() %>% adjust_probability_threshold(hardhat::tune())
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Adjust probability threshold to optimized value.

