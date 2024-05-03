# tailor printing

    Code
      tailor()
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 0 operations.

---

    Code
      tailor(type = "binary")
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 0 operations.

---

    Code
      tailor(type = "binary") %>% adjust_probability_threshold(0.2)
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 operation:
      
      * Adjust probability threshold to 0.2.

---

    Code
      tailor(type = "binary") %>% adjust_probability_threshold(0.2) %>%
        adjust_equivocal_zone()
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 2 operations:
      
      * Adjust probability threshold to 0.2.
      * Add equivocal zone of size 0.1.

