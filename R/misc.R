is_tune <- function(x) {
  if ( !is.call(x) ) {
    return(FALSE)
  }
  isTRUE(identical(quote(tune), x[[1]]))
}

