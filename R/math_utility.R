
#' Calculate the squared Euclidean Norm of a Vector.
#' @description \code{euclidean_norm_squared} calculates the squared Euclidean norm of a vector.
#'
#' @param x a vector representing a (mathematical) vector
#'
#' @return double; squared Euclidean norm of \code{x}
#' @export
#'
#' @examples
#' x = c(1,2,3)
#' euclidean_norm_squared(x)
#' # 14
euclidean_norm_squared <- function (x) {
  sum(x^2)
}


#' Calculate the squared Euclidean Norm of a Vector.
#' @description \code{euclidean_norm} calculates the Euclidean norm of a vector.
#'
#' @param x a vector representing a (mathematical) vector
#'
#' @return double; Euclidean norm of \code{x}
#' @export
#'
#' @examples
#' x = c(1,2,3)
#' euclidean_norm(x)
#' # 3.741657
euclidean_norm <- function (x) {
  sqrt(euclidean_norm_squared(x))
}


#' Calculate the Euclidean distance between two vectors.
#' @description \code{euclidean_distance} calculates the squared Euclidean norm of a vector.
#'
#' @param x a vector representing a (mathematical) vector
#' @param y a vector representing a (mathematical) vector
#'
#' @return double; Euclidean distance between \code{x} and \code{y}
#' @export
#'
#' @examples
#' x = c(1,2,3)
#' y = c(2,3,4)
#' euclidean_distance(x, y)
#' # 1.732051
euclidean_distance <- function(x,y) {
  euclidean_norm(x-y)
}


#' Calculate the Gaussian Core for Two Input Vectors
#' @description \code{gaussian_core} calculates the gaussian core of two given vectors \code{x} and \code{y} of the same length using \code{gamma}.
#'
#' @param x vector
#' @param y vector
#' @param gamma double; has to be a positive number; defaults to \code{7.5}
#'
#' @return returns the gaussian code of \code{x} and \code{y} using \code{gamma}
#' @export
#'
#' @examples
gaussian_core <- function(x, y, gamma = 7.5) {
  stopifnot("gamma has to be a positive number" = gamma > 0)
  stopifnot("x and y have to be of equal length" = length(x) == length(y))

  exp(-gamma * euclidean_norm_squared(x - y))
}
