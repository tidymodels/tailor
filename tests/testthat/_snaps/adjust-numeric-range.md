# adjustment printing

    Code
      adjust_numeric_range(tailor())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [-Inf, Inf].

---

    Code
      adjust_numeric_range(tailor(), hardhat::tune())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [?, Inf].

---

    Code
      adjust_numeric_range(tailor(), -1, hardhat::tune())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [-1, ?].

---

    Code
      adjust_numeric_range(tailor(), hardhat::tune(), 1)
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [?, 1].

---

    Code
      fit(adjust_numeric_range(tailor()), mtcars, outcome = mpg, estimate = disp)
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [-Inf, Inf].  [trained]

