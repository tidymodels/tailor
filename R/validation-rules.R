validate_oper_order <- function(ops, mode, call) {

  oper_data <-
    tibble::tibble(
      name = purrr::map_chr(ops, ~ class(.x)[1]),
      input = purrr::map_chr(ops, ~ .x$inputs),
      output_numeric = purrr::map_lgl(ops, ~ grepl("numeric", .x$outputs)),
      output_prob    = purrr::map_lgl(ops, ~ grepl("probability", .x$outputs)),
      output_class   = purrr::map_lgl(ops, ~ grepl("class", .x$outputs)),
      output_all     = purrr::map_lgl(ops, ~ grepl("everything", .x$outputs))
    )

  if (length(ops) < 2) {
    return(invisible(oper_data))
  }

  if (mode == "classification") {
    check_classification_order(oper_data, call)
  } else {
    check_regression_order(oper_data, call)
  }

  invisible(oper_data)
}

check_classification_order <- function(x, call) {
  cal_ind <- which(grepl("calibration$", x$name))
  eq_ind <- which(grepl("equivocal", x$name))
  prob_ind <- which(x$output_prob)
  class_ind <- which(x$output_class)

  # does probability steps come after steps that change the hard classes?
  if (length(prob_ind) > 0) {
    if (any(class_ind < prob_ind)) {
      cli::cli_abort("Operations that change the hard class predictions \\
                     must come after operations that update the class \\
                     probability estimates.", call = call)
    }
  }

  # todo ? calibration should _probably_ come before anything that is not a mutate

  # do any steps come before Eq zones
  if (length(eq_ind) > 0) {
    if (any(eq_ind < class_ind) | any(eq_ind < prob_ind)) {
      cli::cli_abort("Equivocal zone addition should come after operations \\
                     that update the class probability estimates or hard \\
                     class predictions.", call = call)
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
      cli::cli_abort("Calibration should come before other operations.",
                     call = call)
    }
  }

  # besides mutates, are there duplicate steps?
  check_duplicates(x, call)

  invisible(x)
}

check_duplicates <- function(x, call) {
  non_mutates <- table(x$name[x$name != "predictions_custom"])
  if (any(non_mutates > 1)) {
    bad_oper <- names(non_mutates[non_mutates > 1])
    cli::cli_abort("Operations cannot be duplicated: {.val {bad_oper}}", call = call)
  }
  invisible(x)
}
