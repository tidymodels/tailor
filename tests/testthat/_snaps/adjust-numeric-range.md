# adjustment printing

    Code
      container() %>% adjust_numeric_range()
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Constrain numeric predictions to be between [-Inf, Inf].

---

    Code
      container() %>% adjust_numeric_range(hardhat::tune())
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Constrain numeric predictions to be between [?, Inf].

---

    Code
      container() %>% adjust_numeric_range(-1, hardhat::tune())
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Constrain numeric predictions to be between [-1, ?].

---

    Code
      container() %>% adjust_numeric_range(hardhat::tune(), 1)
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 1 operation:
      
      * Constrain numeric predictions to be between [?, 1].

