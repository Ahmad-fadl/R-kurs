
#' Generate a Single Training Vector
#' @description This function generates a single training vector as specified in "Beispiel 10.23" in reference "Richter19"
#' @return returns a single training vector
#' @example create_training_vector()
#' @references Richter, S. (2019). Statistisches und maschinelles Lernen. Springer Spektrum.
generate_training_vector <- function () {
  R <- function(z) {
    Delta <- runif(1,-0.1, 0.1)
    if (z == 1L) {
      return(Delta + .3)
    } else {
      stopifnot("z should be either 1 or 2" = z == 2L)
      return(Delta + 1)
    }
  }

  Z <- sample.int(2, size = 1)
  U <- runif(1, 0, 2 * pi)
  X_1 <- R(Z) * c(cos(U), sin(U))

  return(X_1)
}

#' Generate Training Data.
#' @description This function generates training data as specified in "Beispiel 10.23" in reference "Richter19". It is meant for testing purposes.
#' @param n integer; the number of vectors in the returned training data.
#' @return returns a matrix with two rows and \code{n} columns; each column is a training vector.
generate_training_data <- function (n) {
  replicate(n, generate_training_vector())
}

#' Calculate the squared Euclidean Norm of a Vector.
#' @description \code{euclidean_norm} calculates the Euclidean norm of a vector.
#'
#' @param x a vector representing a vector
#'
#' @return double; Euclidean norm of \code{x}
#' #' @export
#'
#' @examples
#' x = c(1,2,3)
#' euclidean_norm(x)
#' # 3.741657
euclidean_norm <- function (x) {
  sqrt(sum(x^2))
}

#' Calculate the squared Euclidean Norm of a Vector.
#'  @description \code{euclidean_norm_squared} calculates the squared Euclidean norm of a vector.
#'
#' @param x a vector representing a vector
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

# expecting input vectors to be the columns of a matrix
#' Spectral Clustering over a Matrix's Column-Vectors.
#'
#' @description Performs the spectral clustering algorithm describe in "Definition 10.50" in reference Richter19
#' @references Richter19 -- Richter, S. (2019). Statistisches und maschinelles Lernen. Springer Spektrum.
#'
#' @param data matrix whose columns are training vectors
#' @param k integer; projection target dimension; defaults to \code{1}
#' @param mercer_core function; a function object to a mercer_core (see "Definition 4.32" in Richter19); defaults to the gaussian core (see p.116, Richter19)
#'
#' @return a matrix containing the optimal \code{k}-dimensional projections of the input vectors as its columns
#' @export
#'
#' @examples
#' X <- create_training_data(400)
#' optimal_projections <- spectral_clustering(X)
spectral_clustering <- function (data, k=1, mercer_core=gaussian_core) {
  n <- ncol(data);
  W_tilde <- matrix(0, ncol=n, nrow=n);

  # mercer_core is a symmetric function
  # => W_tilde is symmetric
  # TODO: optimize this
  for (i in seq(n)) {
    for (j in seq(n)) {
      W_tilde[i,j] <- mercer_core(data[,i], data[, j]);
    }
  }

  D_tilde <- matrix(0, ncol=n, nrow=n);
  for (i in seq(n)) {
    D_tilde[i,i] <- sum(W_tilde[i,]);
  }

  L_tilde <- D_tilde - W_tilde;

  D_inverse_sqrt <- D_tilde;
  for (i in seq(n)) {
    D_inverse_sqrt[i,i] <- 1 / sqrt(D_tilde[i,i]);
  }

  # eigen returns a list containing two attributes:
  # values is a vector of the eigenvalues
  # vectors corresponding eigenvectors of (euclidian) length 1
  p <- eigen(D_inverse_sqrt %*% L_tilde %*% D_inverse_sqrt);

  b <- p$vectors[, order(p$values)];

  beta <- apply(b, 2, function(j) { sqrt(n) * D_inverse_sqrt %*% j });

  alpha <- matrix(0, ncol=n, nrow=k);
  for (i in seq(k)) {
    alpha[i,] = beta[, i+1];
  }

  return(alpha);
}

test <- function() {
  X <- generate_training_data(400)
  erg <- spectral_clustering(X, k=2)
  plot(erg[1,], erg[2,])
}
