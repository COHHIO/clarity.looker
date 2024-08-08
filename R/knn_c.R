knn_row <- function(x, y) {
  purrr::imap_dfc(dplyr::select(x, ~inherits(.x, c("character", "numeric"))), ~{
    knn_dist(.x, y[[.y]])
  }) %>%
    dplyr::mutate(across(.fns = dplyr::percent_rank)) %>%
    slider::slide_dbl(sum, na.rm = TRUE) %>%
    which.min()
}

#' @title K-Nearest Neighbors Distance Generic Function
#' @description A generic function for computing distances between data points for k-nearest neighbors analysis.
#' @param x The first argument for method dispatch. The type of this argument determines which method is called.
#' @param y The second argument for method dispatch. The type of this argument determines which method is called.
#' @return The return value depends on the method implemented for the specific classes of `x` and `y`.
#' @details This function is a generic for computing k-nearest neighbors distances. Methods should be implemented for specific classes to define how distances are calculated.
#' @seealso \code{\link{knn_dist.default}} for the default method.
#' @export
knn_dist <- function(x, y) {
  UseMethod("knn_dist")
}

#' @title K-Nearest Neighbors Distance for Character Vectors
#' @description Computes the distance between two character vectors using string distance metrics.
#' @param x A character vector.
#' @param y A character vector.
#' @return A numeric vector of distances between the elements of `x` and `y` computed using the `stringdist` package.
#' @details This method uses `stringdist::stringdist` to compute distances between elements of `x` and `y`. The distance metric used is the default from the `stringdist` package.
#' @export
knn_dist.character <- function(x, y) {
  stringdist::stringdist(x, y)
}

#' @title K-Nearest Neighbors Distance for Factor Vectors
#' @description Computes the distance between two factor vectors by converting them to character vectors.
#' @param x A factor vector.
#' @param y A factor vector.
#' @return A numeric vector of distances between the elements of `x` and `y`, calculated using `stringdist::stringdist` on their character representations.
#' @details This method first converts the factors to character vectors and then computes distances using `stringdist::stringdist`.
#' @export
knn_dist.factor <- function(x, y) {
  stringdist::stringdist(as.character(x), as.character(y))
}

#' @title Default Method for K-Nearest Neighbors Distance
#' @description Computes the distance between two numeric vectors using the logarithmic difference.
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @return A numeric vector of distances between the elements of `x` and `y`, calculated as `log(x - y)`.
#' @details This method calculates the logarithmic difference between elements of `x` and `y`. Note that this method assumes `x` and `y` are numeric and may produce warnings if there are negative values or zero.
#' @export
knn_dist.default <- function(x, y) {
  log(x - y)
}


