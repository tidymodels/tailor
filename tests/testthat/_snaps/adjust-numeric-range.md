# adjustment printing

    Code
      ctr_reg %>% adjust_numeric_range()
    Message
      
      -- Container -------------------------------------------------------------------
      A regression postprocessor with 1 operation:
      
      * Constrain numeric predictions to be between [-Inf, Inf].

---

    Code
      ctr_reg %>% adjust_numeric_range(hardhat::tune())
    Message
      
      -- Container -------------------------------------------------------------------
      A regression postprocessor with 1 operation:
      
      * Constrain numeric predictions to be between [?, Inf].

---

    Code
      ctr_reg %>% adjust_numeric_range(-1, hardhat::tune())
    Message
      
      -- Container -------------------------------------------------------------------
      A regression postprocessor with 1 operation:
      
      * Constrain numeric predictions to be between [-1, ?].

---

    Code
      ctr_reg %>% adjust_numeric_range(hardhat::tune(), 1)
    Message
      
      -- Container -------------------------------------------------------------------
      A regression postprocessor with 1 operation:
      
      * Constrain numeric predictions to be between [?, 1].

