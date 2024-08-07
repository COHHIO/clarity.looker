% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knn_c.R
\name{knn_dist.character}
\alias{knn_dist.character}
\title{K-Nearest Neighbors Distance for Character Vectors}
\usage{
\method{knn_dist}{character}(x, y)
}
\arguments{
\item{x}{A character vector.}

\item{y}{A character vector.}
}
\value{
A numeric vector of distances between the elements of \code{x} and \code{y} computed using the \code{stringdist} package.
}
\description{
Computes the distance between two character vectors using string distance metrics.
}
\details{
This method uses \code{stringdist::stringdist} to compute distances between elements of \code{x} and \code{y}. The distance metric used is the default from the \code{stringdist} package.
}
