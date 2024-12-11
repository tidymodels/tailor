validate_order <- function(orderings, type, call = caller_env()) {
  if (nrow(orderings) < 2) {
    return(invisible(orderings))
  }

  check_incompatible_types(orderings, call)

  switch(
    type,
    regression = check_regression_order(orderings, call),
    binary = , multiclass = check_classification_order(orderings, call),
    invisible()
  )

  invisible(orderings)
}

check_incompatible_types <- function(orderings, call) {
  if (all(c("numeric", "probability") %in% orderings$input)) {
    numeric_adjustments <- orderings$name[which(orderings$input == "numeric")]
    probability_adjustments <- orderings$name[which(orderings$input == "probability")]
    cli_abort(
      c(
        "Can't compose adjustments for different prediction types.",
        "i" = "{cli::qty(numeric_adjustments)}
               Adjustment{?s} {.fn {paste0('adjust_', numeric_adjustments)}}
               {cli::qty(numeric_adjustments[-1])} operate{?s} on numerics while
               {.fn {paste0('adjust_', probability_adjustments)}}
               {cli::qty(probability_adjustments[-1])} operate{?s} on probabilities."
      ),
      call = call
    )
  }
}

check_classification_order <- function(x, call) {
  cal_ind <- which(grepl("calibration$", x$name))
  eq_ind <- which(grepl("equivocal", x$name))
  prob_ind <- which(x$output_prob)
  class_ind <- which(x$output_class)

  # does probability steps come after steps that change the hard classes?
  if (length(prob_ind) > 0) {
    if (any(class_ind < prob_ind)) {
      cli_abort(
        "Adjustments that change the hard class predictions must come after
         adjustments that update the class probability estimates.",
        call = call
      )
    }
  }

  # todo ? calibration should _probably_ come before anything that is not a mutate

  # do any steps come before Eq zones
  if (length(eq_ind) > 0) {
    if (any(eq_ind < class_ind) | any(eq_ind < prob_ind)) {
      cli_abort(
        "Equivocal zone addition should come after adjustments that update the
         class probability estimates or hard class predictions.",
        call = call
      )
    }
  }

  # besides mutates, are there duplicate steps?
  check_duplicates(x, call)

  invisible(x)
}

check_regression_order <- function(x, call) {
  cal_ind <- which(grepl("calibration$", x$name))
  num_ind <- which(x$output_numeric)

  # does calibration come after other steps?
  # currently excluding mutates form this check
  if (length(cal_ind) > 0) {
    if (any(num_ind < cal_ind)) {
      cli_abort(
        "Calibration should come before other adjustments.",
        call = call
      )
    }
  }

  # besides mutates, are there duplicate steps?
  check_duplicates(x, call)

  invisible(x)
}

check_duplicates <- function(x, call) {
  non_mutates <- table(x$name[x$name != "predictions_custom"])
  if (any(non_mutates > 1)) {
    bad_adjustment <- names(non_mutates[non_mutates > 1])
    cli_abort("adjustments cannot be duplicated: {.val {bad_adjustment}}", call = call)
  }
  invisible(x)
}

infer_type <- function(orderings) {
  if (all(orderings$output_all)) {
    return("unknown")
  }

  if (all(orderings$output_numeric | orderings$output_all)) {
    return("regression")
  }

  if (all(orderings$output_prob | orderings$output_class | orderings$output_all)) {
    return("binary")
  }

  "unknown"
}
