knn_row <- function(x, y) {
  purrr::imap_dfc(dplyr::select(x, ~inherits(.x, c("character", "numeric"))), ~{
    knn_dist(.x, y[[.y]])
  }) %>%
    dplyr::mutate(across(.fns = dplyr::percent_rank)) %>%
    slider::slide_dbl(sum, na.rm = TRUE) %>%
    which.min()
}

#' @export
knn_dist <- function(x, y) {
  UseMethod("knn_dist")
}

#' @export
knn_dist.character <- function(x, y) {
  stringdist::stringdist(x, y)
}

#' @export
knn_dist.factor <- function(x, y) {
  stringdist::stringdist(as.character(x), as.character(y))
}

#' @export
knn_dist.default <- function(x, y) {
  log(x - y)
}


