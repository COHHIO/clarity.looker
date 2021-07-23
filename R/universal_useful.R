
is_legit <- function(x) {
  !(is.null(x) || rlang::is_empty(x) || is.na(x))
}


.mode <- function(.) {
  .u <- unique(.)
  tab <- tabulate(match(., .u))
  .u[tab == max(tab)]
}
