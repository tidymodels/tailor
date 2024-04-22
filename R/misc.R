is_tune <- function(x) {
  if (!is.call(x)) {
    return(FALSE)
  }
  isTRUE(identical(quote(tune), x[[1]]))
}

# for operations with no tunable parameters

no_param <-
  tibble::tibble(
    name = character(0),
    call_info = list(),
    source = character(0),
    component = character(0),
    component_id = character(0)
  )
