
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
#' @description \code{euclidean_distance} calculates the  Euclidean norm of a vector.
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
#' Calculate the squared Euclidean distance between two vectors.
#' @description \code{euclidean_distance_sqared} calculates the squared Euclidean norm of a vector.
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
#' euclidean_distance_squared(x, y)
euclidean_distance_squared <- function(x,y) {
  euclidean_norm_squared(x-y)
}
#' Calculate the Gaussian kernel for Two Input Vectors
#' @description \code{gaussian_core} calculates the gaussian kernel of two given vectors \code{x} and \code{y} of the same length using \code{gamma}.
#'
#' @param x vector
#' @param y vector
#' @param gamma double; has to be a positive number; defaults to \code{7.5}
#'
#' @return returns the gaussian kernel of \code{x} and \code{y} using \code{gamma}
#' @export
#'
# #' @examples
gaussian_kernel <- function(x, y, gamma = 7.5) {
  stopifnot("gamma has to be a positive number" = gamma > 0)
  stopifnot("x and y have to be of equal length" = length(x) == length(y))

  exp(-gamma * euclidean_norm_squared(x - y))
}


#' Gaussian Kernel factory
#' @description Returns a function that calculates the Gaussian Kernel with \code{gamma} fixed to the passed value.
#'
#' @param gamma The \code{gamma} value to be used for the Gaussien Kernel.
#'
#' @return A function that calculates the Gaussian Kernel for input vector \code{x}, \code{y} with a fixed \code{gamma}.
#' @export
#'
#' @examples
#' f <- gaussian_kernel_with_fixed_gamma(3)
gaussian_kernel_with_fixed_gamma <- function(gamma) {
  stopifnot("gamma has to be a positive number" = gamma > 0)

  bound_kernel <- function (x, y) {
    return(gaussian_kernel(x, y, gamma));
  }

  return(bound_kernel);
}

#' Calculate the Infinity-Norm of a Vector.
#' @description \code{infinity_norm} calculates the infinity-norm of a vector. Also known as maximum norm.
#'
#' @param x a vector representing a (mathematical) vector
#'
#' @return double; infinity-norm of \code{x}
#' @export
#'
#' @examples
#' x = c(1,2,3)
#' infinity_norm(x)
#' # 3
infinity_norm <- function(x) {
  return(max(sapply(x, abs)));
}
maximum_norm <- infinity_norm

#' Calculate the 1-Norm of a Vector.
#' @description \code{one_norm} calculates the Euclidean norm of a vector. Also known as Taxicab norm or Manhattan norm.
#'
#' @param x a vector representing a (mathematical) vector
#'
#' @return double; 1-norm of \code{x}
#' @export
#'
#' @examples
#' x = c(1,2,3)
#' one_norm(x)
#' # 6
one_norm <- function(x) {
  return(sum(sapply(x, abs)));
}
taxicab_norm <- one_norm
manhattan_norm <- one_norm

#' Calculate the p-Norm of a Vector.
#' @description \code{p_norm} calculates the p-norm of a vector for a given \code{p}.
#'
#' @param x a vector representing a (mathematical) vector
#' @param p integer; the p value
#'
#' @return double; p-norm of \code{x}
#' @export
#'
#' @examples
#' x = c(1,2,3)
#' p_norm(x, 2)
#' # 3.741657
p_norm <- function(x, p) {
  return(sum( sapply(x,abs)**p )**(1/p));
}


#' p-Norm Function Factory
#' @description Generate a function calculating the p-norm for the given p.
#'
#' @param p integer; specify which p-norm the returned function will compute.
#'
#' @return function; A function which computes the p-norm for the given \code{p}.
#' @export
#'
#' @examples
#' two_norm = p_norm_factory(2)
#' two_norm(c(1,2,3))
#' # 3.741657
p_norm_factory <- function(p) {
  function(x) {
    p_norm(x, p);
  }
}

#' Induced Metric Factory
#' @description This function returns a function calculating the metric induced by the given vector norm.
#'
#' @param vector_norm function; a vector norm
#'
#' @return function; calculates the metric induced by \code{vector_norm}.
#' @export
#'
#' @examples
#' euclidean_metric <- induced_metric_factory(euclidean_norm)
#' x = c(1,2,3)
#' y = c(2,3,4)
#' euclidean_metric(x, y)
#' # 1.732051
induced_metric_factory <- function(vector_norm) {
  stopifnot("vector_norm has to be a function." = is.function(vector_norm))
  function (x, y) {
    return(vector_norm(x-y));
  }
}
