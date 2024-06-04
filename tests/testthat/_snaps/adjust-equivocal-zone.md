# adjustment printing

    Code
      tailor() %>% adjust_equivocal_zone()
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 1 adjustment:
      
      * Add equivocal zone of size 0.1.

---

    Code
      tailor() %>% adjust_equivocal_zone(hardhat::tune())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 1 adjustment:
      
      * Add equivocal zone of optimized size.

