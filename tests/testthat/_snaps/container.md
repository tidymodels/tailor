# container printing

    Code
      ctr_cls
    Message
      
      -- Container -------------------------------------------------------------------
      A postprocessor with 0 operations.

---

    Code
      container(mode = "classification", type = "binary")
    Message
      
      -- Container -------------------------------------------------------------------
      A binary postprocessor with 0 operations.

---

    Code
      container(mode = "classification", type = "binary") %>%
        adjust_probability_threshold(0.2)
    Message
      
      -- Container -------------------------------------------------------------------
      A binary postprocessor with 1 operation:
      
      * Adjust probability threshold to 0.2.

---

    Code
      container(mode = "classification", type = "binary") %>%
        adjust_probability_threshold(0.2) %>% adjust_equivocal_zone()
    Message
      
      -- Container -------------------------------------------------------------------
      A binary postprocessor with 2 operations:
      
      * Adjust probability threshold to 0.2.
      * Add equivocal zone of size 0.1.

