# adjustment printing

    Code
      adjust_predictions_custom(tailor())
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 1 adjustment:
      
      * Adjust predictions using custom code.

---

    Code
      fit(adjust_predictions_custom(tailor(), linear_predictor = binomial()$linkfun(
        Class2)), two_class_example, outcome = c(truth), estimate = c(predicted),
      probabilities = c(Class1, Class2))
    Message
      
      -- tailor ----------------------------------------------------------------------
      A binary postprocessor with 1 adjustment:
      
      * Adjust predictions using custom code. [trained]

